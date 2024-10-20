use std::str::FromStr;

use anyhow::Result;
use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{
    request::{GotoDefinition, GotoTypeDefinitionParams},
    GotoDefinitionResponse, InitializeParams, Location, OneOf, SaveOptions, ServerCapabilities,
    TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Uri,
};

use rock::*;
use source_code::{LineCol, SourceFile, SourceFiles, Span};

fn main() -> Result<()> {
    eprintln!("Starting Rock LSP server ...");
    // panic!("sad");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
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
    eprintln!("Shutting down Rock LSP  server");

    Ok(())
}

fn main_loop(connection: Connection, params: serde_json::Value) -> Result<()> {
    let _params: InitializeParams = serde_json::from_value(params)?;
    eprintln!("Starting example main loop");

    let rock_context = CompilerContext::new();
    let mut sources = SourceFiles::new(vec![]);

    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");

        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                eprintln!("got request: {req:?}");

                match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        eprintln!("got gotoDefinition request #{id}: {params:?}");

                        let result = go_to_definition(&rock_context, &sources, &params)?;
                        let result = Some(GotoDefinitionResponse::Array(result));
                        let result = serde_json::to_value(&result)?;
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };

                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }

                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
            }

            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }

            Message::Notification(not) => {
                if not.method == "textDocument/didSave" {
                    eprintln!("got textDocument/didSave notification {not:?}");

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

                    eprintln!("got uri: {uri:?}");
                } else {
                    eprintln!("got notification: {not:?}");
                }
            }
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

    if let Some(symbol) = c.query_definition_at(query_loc) {
        let span = c.span_of_symbol(symbol);
        let response_loc = span_to_location(&span);
        return Ok(vec![response_loc]);
    }

    Ok(vec![])
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
