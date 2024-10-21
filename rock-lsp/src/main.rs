use std::{collections::HashSet, str::FromStr};

use anyhow::Result;
use ariadne::{Report, Source};
use log::{debug, error, info};
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    request::{GotoDefinition, GotoTypeDefinitionParams, HoverRequest},
    GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability, InitializeParams,
    Location, MarkupContent, MarkupKind, OneOf, SaveOptions, ServerCapabilities,
    TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Uri,
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
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::NONE),
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
        debug!("got msg: {msg:?}");

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

                let _req = match cast::<HoverRequest>(req) {
                    Ok((id, params)) => {
                        info!("hover request #{id}: {params:?}");
                        let result = do_hover(&rock_context, &sources, &params);
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
                    reload_sources_on_notification(&not, &mut rock_context, &mut sources)?;
                }

                "textDocument/didOpen" => {
                    info!("got textDocument/didOpen notification {not:?}");
                    reload_sources_on_notification(&not, &mut rock_context, &mut sources)?;
                }

                _ => {
                    info!("got notification: {not:?}");
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

fn span_to_location(span: &Span) -> Location {
    Location {
        // uri: Url::from_file_path(&span.file.as_str()).unwrap(),
        uri: Uri::from_str(span.file.as_str()).unwrap(),
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

fn reload_sources_on_notification(
    not: &Notification,
    context: &mut CompilerContext,
    sources: &mut SourceFiles,
) -> Result<()> {
    let uri = not
        .params
        .as_object()
        .and_then(|o| o.get("textDocument"))
        .and_then(|o| o.get("uri"))
        .and_then(|v| v.as_str())
        .map(|s| Uri::from_str(s).unwrap())
        .unwrap();

    let file_path = uri.path().as_str();
    sources.add_or_update_file(SourceFile::from_path(file_path)?);

    context
        .compile_sources(sources)
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;

    print_symbol_table(
        &context
            .compiled_module
            .as_ref()
            .unwrap()
            .semantic
            .symbol_table,
        sources,
    );

    Ok(())
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

fn print_symbol_table(table: &SymbolTable, sources: &SourceFiles) {
    let mut report = Report::build(ariadne::ReportKind::Advice, "gud.rock", 0);

    let file = &sources.files[0];

    let mut seen_syms = HashSet::new();

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
            format!(
                "ref[#{}] -> #{}",
                ref_id.to_u32(),
                sym_ref.symbol.to_u32()
            ),
        ));
    }

    report
        .finish()
        .eprint((file.path().to_string(), Source::from(file.contents())))
        .unwrap();
}
