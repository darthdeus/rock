use std::{
    collections::{HashMap, HashSet},
    panic::AssertUnwindSafe,
    str::FromStr,
};

use anyhow::Result;
use ariadne::{Report, Source};
use log::{debug, error, info};
use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{
    notification::Notification,
    request::{Completion, GotoDefinition, GotoTypeDefinitionParams, HoverRequest},
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionOptions,
    CompletionResponse, Diagnostic, DiagnosticSeverity, Documentation, GotoDefinitionResponse,
    Hover, HoverContents, HoverProviderCapability, InitializeParams, Location, MarkupContent,
    MarkupKind, OneOf, Position, Range, SaveOptions, ServerCapabilities,
    TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Uri, WorkDoneProgressOptions,
};

use rock::*;
use source_code::{LineCol, SourceFile, SourceFiles, Span};
use symbol_table::SymbolTable;

fn main() -> Result<()> {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .format_timestamp(None)
        .init();

    info!("Starting Rock LSP server ...");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: None,
            all_commit_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(false),
            },
            completion_item: Some(lsp_types::CompletionOptionsCompletionItem {
                label_details_support: Some(false),
            }),
        }),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: Some(false),
                will_save_wait_until: Some(false),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(false),
                })),
            },
        )),
        ..Default::default()
    })?;

    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }

            return Err(e.into());
        }
    };

    main_loop(connection, initialization_params)?;

    io_threads.join()?;
    info!("Shutting down Rock LSP server");

    Ok(())
}

fn main_loop(connection: Connection, params: serde_json::Value) -> Result<()> {
    let _params: InitializeParams = serde_json::from_value(params)?;
    info!("Starting example main loop");

    let mut rock_context = CompilerContext::new();
    let mut sources = SourceFiles::new(vec![]);

    for msg in &connection.receiver {
        info!("got msg: {msg:?}");

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                debug!("got request: {req:?}");

                let req = match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        info!("gotoDefinition request #{id}: {params:?}");
                        let result = go_to_definition(&rock_context, &sources, &params)
                            .map(GotoDefinitionResponse::Array);
                        send_response(&connection, id, result)?;
                        continue;
                    }

                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast::<HoverRequest>(req) {
                    Ok((id, params)) => {
                        info!("hover request #{id}: {params:?}");
                        let result = do_hover(&rock_context, &sources, &params);
                        send_response(&connection, id, result)?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let _req = match cast::<Completion>(req) {
                    Ok((id, params)) => {
                        info!("completion request #{id}: {params:?}");
                        let result = do_completion(&mut rock_context, &mut sources, &params);
                        send_response(&connection, id, result)?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
            }

            Message::Response(resp) => {
                info!("got response: {resp:?}");
            }

            Message::Notification(not) => match not.method.as_str() {
                "textDocument/didSave" => {
                    info!("got textDocument/didSave notification {not:?}");
                    reload_sources_on_notification(
                        &connection,
                        &notification_uri(&not)?,
                        &mut rock_context,
                        &mut sources,
                    )?;
                }

                "textDocument/didOpen" => {
                    info!("got textDocument/didOpen notification {not:?}");
                    reload_sources_on_notification(
                        &connection,
                        &notification_uri(&not)?,
                        &mut rock_context,
                        &mut sources,
                    )?;
                }

                "textDocument/didChange" => {
                    let uri = notification_uri(&not)?;

                    // info!("got textDocument/didChange notification {not:?}");
                    info!(
                        "got textDocument/didChange notification {}",
                        uri.path().as_str()
                    );

                    let contents = not
                        .params
                        .as_object()
                        .and_then(|o| o.get("contentChanges"))
                        .and_then(|v| v.as_array())
                        .and_then(|a| a.first())
                        .and_then(|v| v.get("text"))
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string())
                        .ok_or_else(|| {
                            anyhow::anyhow!("Failed to get contents from notification")
                        })?;

                    // info!("**** URI: {:?}\n\n{}\n\n******", uri, contents);

                    reload_sources_on_change(&connection, &uri, contents, &mut rock_context, &mut sources)?;
                }

                _ => {
                    info!("UNEXPECTED notification: {not:?}");
                }
            },
        }
    }

    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn text_document_position_to_linecol(
    sources: &SourceFiles,
    params: &TextDocumentPositionParams,
) -> Result<LineCol, anyhow::Error> {
    let file_path = params.text_document.uri.path().as_str();
    // .map_err(|()| anyhow::anyhow!("Only file:// is supported"))?;
    let file_path = std::fs::canonicalize(file_path)?;

    let line = params.position.line;
    let col = params.position.character + 1;

    LineCol::from_file_line_col(
        sources,
        file_path.to_str().unwrap(),
        line as usize,
        col as usize,
    )
    .map_err(|s| anyhow::anyhow!("Failed to find line/col: {s}"))
}

fn go_to_definition(
    c: &CompilerContext,
    sources: &SourceFiles,
    params: &GotoTypeDefinitionParams,
) -> Result<Vec<Location>> {
    let query_loc =
        text_document_position_to_linecol(sources, &params.text_document_position_params)?;

    info!("Query location: {:?}", query_loc);

    if let Some(module) = c.get_module() {
        info!("... module");

        if let Some(symbol) = module.semantic.symbol_table.query_definition_at(query_loc) {
            info!("... symbol: {:?}", symbol);

            let span = module.semantic.symbol_table.span_of_symbol(symbol);
            let response_loc = span_to_location(&span);

            info!(
                "Found definition of {} at {:?}",
                module
                    .semantic
                    .symbol_table
                    .get_symbol_info(symbol)
                    .ident_text,
                response_loc
            );
            return Ok(vec![response_loc]);
        }
    }

    Ok(vec![])
}

fn do_hover(
    context: &CompilerContext,
    sources: &SourceFiles,
    params: &lsp_types::HoverParams,
) -> Result<Hover, anyhow::Error> {
    // We never return an error to avoid spamming the user. Instead, we return
    // an empty hover
    let empty_hover = Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::PlainText,
            value: "".into(),
        }),
        range: None,
    };

    if let Some(module) = context.get_module() {
        let Ok(query_loc) =
            text_document_position_to_linecol(sources, &params.text_document_position_params)
        else {
            info!("Failed to find");
            return Ok(empty_hover);
        };

        print_symbol_table(&module.semantic.symbol_table, sources, Some(&query_loc));

        info!("Query location: {:?}", query_loc);

        if let Some(definition) = module.semantic.symbol_table.query_definition_at(query_loc) {
            info!("Definition: {:?}", definition);

            let symbol_info = module.semantic.symbol_table.get_symbol_info(definition);
            let ident = &symbol_info.ident_text;
            let ty_str = module.semantic.symbol_table.type_to_string(symbol_info.ty);
            let ty_str_html_escaped = ty_str.replace("<", "&lt;").replace(">", "&gt;");
            let contents = format!("**{}** : {}", ident, ty_str_html_escaped);

            Ok(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: contents,
                }),
                range: None,
            })
        } else {
            info!("No definition found in symbol table");

            Ok(empty_hover)
        }
    } else {
        Err(anyhow::anyhow!("No module found"))
    }
}

fn do_completion(
    context: &mut CompilerContext,
    sources: &mut SourceFiles,
    params: &lsp_types::CompletionParams,
) -> Result<CompletionResponse, anyhow::Error> {
    let mut completions = vec![];
    // info!("COMPLETION PARAMS: {:#?}", params);

    // let query_uri = &params.text_document_position.text_document.uri;
    // reload_sources_on_notification(query_uri, context, sources)?;

    let query_loc = text_document_position_to_linecol(sources, &params.text_document_position)?;
    let line = sources.get_line(&query_loc);

    if let Some(line) = line {
        info!("Query location: {:?} ... line: {}", query_loc, line);
    }

    eprintln!("goob");

    if let Some(module) = context.get_module() {
        for _ in module.semantic.symbol_table.symbols.values() {
            let result = CompletionItem {
                label: "rockfun".to_string(),
                label_details: Some(CompletionItemLabelDetails {
                    detail: Some("label-ROCK-DETAIL".to_string()),
                    description: Some("label-ROCK-DESC".to_string()),
                }),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some("item-detail: Very good information about the rock".to_string()),
                // TODO: once we have markup, use that instead.
                documentation: Some(Documentation::String(
                    "item-doc: DETAILED DOCUMENTATION OF ROCKS\n\nRocks are made of rock."
                        .to_string(),
                )),
                deprecated: Some(false),
                preselect: Some(true),
                sort_text: None,          // Sort by label
                filter_text: None,        // Filter by label
                insert_text: None,        // VS Code does this wrong, so we ignore it.
                insert_text_format: None, // When we have snippets, we may want this.
                insert_text_mode: None,
                text_edit: None, // For now pretend this is whatever.
                additional_text_edits: None,
                command: None,
                commit_characters: None,
                data: None,
                tags: None,
            };

            completions.push(result);
        }
    }

    Ok(CompletionResponse::Array(completions))
}

fn span_to_location(span: &Span) -> Location {
    Location {
        // uri: Url::from_file_path(&span.file.as_str()).unwrap(),
        uri: Uri::from_str(&format!("file://{}", span.file.as_str())).unwrap(),
        range: lsp_types::Range {
            start: lsp_types::Position {
                line: span.start().line as u32,
                character: span.start().col.saturating_sub(1) as u32,
            },
            end: lsp_types::Position {
                line: span.end().line as u32,
                character: span.end().col.saturating_sub(1) as u32,
            },
        },
    }
}

fn notification_uri(not: &lsp_server::Notification) -> Result<Uri> {
    not.params
        .as_object()
        .and_then(|o| o.get("textDocument"))
        .and_then(|o| o.get("uri"))
        .and_then(|v| v.as_str())
        .map(|s| Uri::from_str(s).unwrap())
        .ok_or_else(|| anyhow::anyhow!("Failed to get uri from notification"))
    // let uri = not
    //     .params
    //     .as_object()
    //     .and_then(|o| o.get("textDocument"))
    //     .and_then(|o| o.get("uri"))
    //     .and_then(|v| v.as_str())
    //     .map(|s| Uri::from_str(s).unwrap())
    //     .unwrap();
}

fn reload_sources_on_notification(
    connection: &Connection,
    uri: &Uri,
    context: &mut CompilerContext,
    sources: &mut SourceFiles,
) -> Result<()> {
    let saved_file_path = uri.path().as_str();

    sources.add_or_update_file(SourceFile::from_path(saved_file_path)?);

    compile_and_send_diagnostics(connection, saved_file_path, context, sources)?;

    Ok(())
}

fn reload_sources_on_change(
    connection: &Connection,
    uri: &Uri,
    contents: String,
    context: &mut CompilerContext,
    sources: &mut SourceFiles,
) -> Result<()> {
    let saved_file_path = uri.path().as_str();

    sources.add_or_update_file(SourceFile::from_path_and_contents(
        saved_file_path,
        contents,
    )?);

    compile_and_send_diagnostics(connection, saved_file_path, context, sources)?;

    Ok(())
}

fn compile_and_send_diagnostics(
    connection: &Connection,
    saved_file_path: &str,
    context: &mut CompilerContext,
    sources: &SourceFiles,
) -> Result<Vec<Message>> {
    let compile_result =
        std::panic::catch_unwind(AssertUnwindSafe(|| context.compile_sources(sources)));

    let mut diagnostics_by_file = HashMap::<ustr::Ustr, Vec<Diagnostic>>::new();
    for file in sources.iter() {
        diagnostics_by_file.insert(file.path().into(), vec![]);
    }

    match compile_result {
        Ok(Ok(_)) => (),

        // Compiler normal errors
        Ok(Err(errors)) => {
            for error in errors {
                let range = if let Some(label) = error.labels.first() {
                    Range {
                        start: Position {
                            line: label.span.start().line as u32,
                            character: label.span.start().col.saturating_sub(1) as u32,
                        },
                        end: Position {
                            line: label.span.end().line as u32,
                            character: label.span.end().col.saturating_sub(1) as u32,
                        },
                    }
                } else {
                    Range {
                        start: Position {
                            line: error.location.line as u32,
                            character: error.location.col.saturating_sub(1) as u32,
                        },
                        end: Position {
                            line: error.location.line as u32,
                            character: error.location.col as u32,
                        },
                    }
                };

                let mut message = error.message.clone();
                for label in &error.labels {
                    message.push_str(&format!("\n\n{}: {}", label.span.file, label.message));
                }
                if let Some(note) = error.note {
                    message.push_str(&format!("\n\n{}", note.message));
                }

                diagnostics_by_file
                    .entry(error.location.file)
                    .and_modify(|diagnostics| {
                        diagnostics.push(Diagnostic {
                            range,
                            severity: Some(DiagnosticSeverity::ERROR),
                            code: None,
                            code_description: None,
                            source: Some("RockCompiler".to_string()),
                            message,
                            related_information: None,
                            tags: None,
                            data: None,
                        })
                    });
            }
        }

        // Compiler panic
        Err(err) => {
            let message = if let Some(msg) = err.downcast_ref::<&str>() {
                msg.to_string()
            } else if let Some(msg) = err.downcast_ref::<String>() {
                msg.clone()
            } else {
                format!("{:?}", err)
            };

            diagnostics_by_file
                .entry(saved_file_path.into())
                .and_modify(|diagnostics| {
                    diagnostics.push(Diagnostic {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 0,
                            },
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("RebelCompiler".to_string()),
                        message: format!("Compiler panic: {}", message),
                        related_information: None,
                        tags: None,
                        data: None,
                    })
                });
        }
    }

    let mut messages = vec![];
    for (path, diagnostics) in diagnostics_by_file {
        let notification = lsp_types::notification::PublishDiagnostics::METHOD;

        // The compiler sometimes generates files that we don't want to show
        // diagnostics for. In those cases the file gets this non-path name.
        if path == "<autogenerated>" {
            continue;
        }
        let params = lsp_types::PublishDiagnosticsParams {
            uri: Uri::from_str(&format!("file://{}", path.as_str())).unwrap(),
            version: None,
            diagnostics,
        };

        messages.push(Message::Notification(lsp_server::Notification {
            method: notification.to_string(),
            params: serde_json::to_value(params).unwrap(),
        }));
    }

    info!("GOT {} MESSAGES", messages.len());

    for message in &messages {
        info!("{:#?}", message);
        connection.sender.send(message.clone())?;
    }

    Ok(messages)
}

fn send_response<T: serde::Serialize>(
    connection: &Connection,
    id: RequestId,
    t: Result<T, anyhow::Error>,
) -> Result<(), anyhow::Error> {
    match t {
        Ok(t) => {
            let result = serde_json::to_value(&t).unwrap();
            let resp = Response {
                id,
                result: Some(result),
                error: None,
            };
            connection.sender.send(Message::Response(resp))?;
        }

        // When there's an actual error, we notify the client as such
        Err(err) => {
            error!("error: {}", err);

            let resp = Response {
                id,
                result: None,
                error: Some(lsp_server::ResponseError {
                    code: lsp_server::ErrorCode::RequestFailed as i32,
                    message: err.to_string(),
                    data: None,
                }),
            };
            connection.sender.send(Message::Response(resp))?;
        }
    }
    Ok(())
}

fn print_symbol_table(table: &SymbolTable, sources: &SourceFiles, query_loc: Option<&LineCol>) {
    let mut report = Report::build(ariadne::ReportKind::Advice, "gud.rock", 0);

    let file = &sources.files[0];

    let mut seen_syms = HashSet::new();

    if let Some(query_loc) = query_loc {
        let span = Span {
            file: query_loc.file,
            line_range: (query_loc.line, query_loc.line + 1),
            col_range: (query_loc.col, query_loc.col + 1),
            offset_range: (query_loc.offset, query_loc.offset + 1),
        };

        report.add_label(span.to_label(file.path(), ">>> Query location <<<".to_string()));
    }

    for (sym_id, sym) in table.symbols.iter() {
        seen_syms.insert(sym_id.to_u32());

        report.add_label(sym.span.to_label(
            file.path(),
            format!("symbol[#{}]: '{}'", sym_id.to_u32(), sym.ident_text),
        ));
    }

    for (ref_id, sym_ref) in table.symbol_refs.iter() {
        if seen_syms.contains(&ref_id.to_u32()) {
            continue;
        }

        report.add_label(sym_ref.span.to_label(
            file.path(),
            format!("ref[#{}] -> #{}", ref_id.to_u32(), sym_ref.symbol.to_u32()),
        ));
    }

    report
        .finish()
        .eprint((file.path().to_string(), Source::from(file.contents())))
        .unwrap();
}

// print_symbol_table(
//     &context
//         .compiled_module
//         .as_ref()
//         .unwrap()
//         .semantic
//         .symbol_table,
//     sources,
//     None,
// );
