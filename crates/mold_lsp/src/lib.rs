//! A generic PostgreSQL language server built on the mold engine.
//!
//! Synchronous (no async runtime): the server loop reads JSON-RPC over stdio
//! via `lsp-server`. A single [`CachedSchemaProvider`], when supplied, drives
//! schema-aware diagnostics and completion; without one the server still offers
//! formatting, syntax diagnostics and the lint rules that need no schema.

mod convert;

use std::collections::HashMap;
use std::error::Error;

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionProviderCapability,
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DocumentFormattingParams, OneOf, Position, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url, WorkspaceEdit,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
        PublishDiagnostics,
    },
    request::{CodeActionRequest, Completion, Formatting},
};
use mold_config::MoldConfig;
use mold_hir::{
    AnalysisOptions, BuiltinLintPack, Diagnostic as HirDiagnostic, NullSchemaProvider,
    SchemaProvider, analyze_query_with_options,
};
use mold_schema::CachedSchemaProvider;
use text_size::TextSize;

use convert::LineIndex;

/// Runs the language server over stdio until the client disconnects.
pub fn run_stdio(
    config: MoldConfig,
    schema: Option<CachedSchemaProvider>,
) -> anyhow::Result<()> {
    let (connection, io_threads) = Connection::stdio();
    let capabilities = serde_json::to_value(server_capabilities())?;
    let _init = connection.initialize(capabilities)?;

    let mut server = Server {
        config,
        schema,
        docs: HashMap::new(),
        connection,
    };
    let result = server.run();
    // Drop the server (and its connection sender) so the writer thread sees the
    // channel close; otherwise `join` blocks forever.
    drop(server);
    io_threads.join()?;
    result
}

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".into(), " ".into(), "(".into(), ",".into()]),
            ..Default::default()
        }),
        document_formatting_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
        ..Default::default()
    }
}

struct Server {
    config: MoldConfig,
    schema: Option<CachedSchemaProvider>,
    docs: HashMap<Url, String>,
    connection: Connection,
}

impl Server {
    fn run(&mut self) -> anyhow::Result<()> {
        // Drain the receiver. `recv` errors only when the channel is closed.
        while let Ok(msg) = self.connection.receiver.recv() {
            match msg {
                Message::Request(req) => {
                    if self.connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    self.handle_request(req)?;
                }
                Message::Notification(note) => self.handle_notification(note)?,
                Message::Response(_) => {}
            }
        }
        Ok(())
    }

    fn handle_request(&mut self, req: Request) -> anyhow::Result<()> {
        let req = match cast::<Completion>(req) {
            Ok((id, params)) => return self.on_completion(id, params),
            Err(req) => req,
        };
        let req = match cast::<Formatting>(req) {
            Ok((id, params)) => return self.on_formatting(id, params),
            Err(req) => req,
        };
        let req = match cast::<CodeActionRequest>(req) {
            Ok((id, params)) => return self.on_code_action(id, params),
            Err(req) => req,
        };
        // Unhandled method: reply with an empty result so the client is not left
        // waiting.
        self.respond(Response::new_ok(req.id, serde_json::Value::Null))
    }

    fn handle_notification(&mut self, note: lsp_server::Notification) -> anyhow::Result<()> {
        match note.method.as_str() {
            DidOpenTextDocument::METHOD => {
                let p: lsp_types::DidOpenTextDocumentParams =
                    serde_json::from_value(note.params)?;
                let uri = p.text_document.uri.clone();
                self.docs.insert(uri.clone(), p.text_document.text);
                self.publish_diagnostics(&uri)?;
            }
            DidChangeTextDocument::METHOD => {
                let p: lsp_types::DidChangeTextDocumentParams =
                    serde_json::from_value(note.params)?;
                // FULL sync: last change holds the whole document.
                if let Some(change) = p.content_changes.into_iter().last() {
                    let uri = p.text_document.uri.clone();
                    self.docs.insert(uri.clone(), change.text);
                    self.publish_diagnostics(&uri)?;
                }
            }
            DidCloseTextDocument::METHOD => {
                let p: lsp_types::DidCloseTextDocumentParams =
                    serde_json::from_value(note.params)?;
                self.docs.remove(&p.text_document.uri);
            }
            _ => {}
        }
        Ok(())
    }

    // ---- analysis -----------------------------------------------------------

    fn hir_provider(&self) -> Option<&dyn SchemaProvider> {
        self.schema.as_ref().map(|p| p as &dyn SchemaProvider)
    }

    fn lint_options(&self) -> AnalysisOptions {
        let mut packs = vec![BuiltinLintPack::Core, BuiltinLintPack::Jsonb];
        if self.config.lint.is_rule_enabled("CP01") {
            packs.push(BuiltinLintPack::Capitalisation);
        }
        AnalysisOptions::new().with_builtin_lint_packs(packs)
    }

    /// Runs analysis and returns the diagnostics surviving rule selection.
    fn analyze(&self, text: &str) -> Vec<HirDiagnostic> {
        let parse = mold_parser::parse(text);
        let options = self.lint_options();
        let null = NullSchemaProvider;
        let provider = self.hir_provider().unwrap_or(&null);
        let schema_aware = self.schema.is_some();

        let mut out: Vec<HirDiagnostic> = analyze_query_with_options(&parse, provider, &options)
            .diagnostics
            .into_iter()
            .filter(|d| match &d.code {
                None => schema_aware,
                Some(code) => self.config.lint.is_rule_enabled(code),
            })
            .collect();

        // Parse errors as their own diagnostics.
        for err in parse.errors() {
            out.push(HirDiagnostic::error(err.message.clone()).with_range(err.range));
        }
        out
    }

    fn publish_diagnostics(&self, uri: &Url) -> anyhow::Result<()> {
        let Some(text) = self.docs.get(uri) else {
            return Ok(());
        };
        let index = LineIndex::new(text);
        let diags: Vec<Diagnostic> = self
            .analyze(text)
            .iter()
            .map(|d| convert::diagnostic(&index, d))
            .collect();

        let params = PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: diags,
            version: None,
        };
        self.notify::<PublishDiagnostics>(params)
    }

    // ---- requests -----------------------------------------------------------

    fn on_completion(&mut self, id: RequestId, params: CompletionParams) -> anyhow::Result<()> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let items = self
            .docs
            .get(&uri)
            .map(|text| self.completions(text, pos))
            .unwrap_or_default();
        self.respond(Response::new_ok(
            id,
            CompletionResponse::Array(items),
        ))
    }

    fn completions(&self, text: &str, pos: Position) -> Vec<CompletionItem> {
        let index = LineIndex::new(text);
        let offset = TextSize::from(index.offset(pos));
        let mut request = mold_completion::CompletionRequest::new(text, offset);
        if let Some(p) = &self.schema {
            request = request.with_schema_provider(p).with_function_provider(p);
        }
        let result = mold_completion::complete(request);
        result
            .items
            .into_iter()
            .map(|item| CompletionItem {
                label: item.label,
                detail: item.detail,
                insert_text: item.insert_text,
                ..Default::default()
            })
            .collect()
    }

    fn on_formatting(
        &mut self,
        id: RequestId,
        params: DocumentFormattingParams,
    ) -> anyhow::Result<()> {
        let uri = params.text_document.uri;
        let edits = self
            .docs
            .get(&uri)
            .map(|text| {
                let formatted = mold_format::format(text, &self.config.format_config());
                if formatted == *text {
                    Vec::new()
                } else {
                    let index = LineIndex::new(text);
                    vec![TextEdit {
                        range: index.range(text_size::TextRange::up_to(TextSize::of(text.as_str()))),
                        new_text: formatted,
                    }]
                }
            })
            .unwrap_or_default();
        self.respond(Response::new_ok(id, edits))
    }

    fn on_code_action(
        &mut self,
        id: RequestId,
        params: CodeActionParams,
    ) -> anyhow::Result<()> {
        let uri = params.text_document.uri.clone();
        let actions = self
            .docs
            .get(&uri)
            .map(|text| self.code_actions(&uri, text, &params))
            .unwrap_or_default();
        self.respond(Response::new_ok(id, actions))
    }

    fn code_actions(
        &self,
        uri: &Url,
        text: &str,
        params: &CodeActionParams,
    ) -> Vec<CodeActionOrCommand> {
        let index = LineIndex::new(text);
        let want = params.range;
        let mut actions = Vec::new();

        for diag in self.analyze(text) {
            let Some(range) = diag.range else { continue };
            let lsp_range = index.range(range);
            // Only offer actions overlapping the requested range.
            if lsp_range.end < want.start || lsp_range.start > want.end {
                continue;
            }
            for fix in &diag.fixes {
                let edits: Vec<TextEdit> =
                    fix.edits.iter().map(|e| convert::text_edit(&index, e)).collect();
                let mut changes = HashMap::new();
                changes.insert(uri.clone(), edits);
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: fix.title.clone(),
                    kind: Some(CodeActionKind::QUICKFIX),
                    diagnostics: Some(vec![convert::diagnostic(&index, &diag)]),
                    edit: Some(WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    }),
                    ..Default::default()
                }));
            }
        }
        actions
    }

    // ---- transport ----------------------------------------------------------

    fn respond(&self, response: Response) -> anyhow::Result<()> {
        self.connection
            .sender
            .send(Message::Response(response))
            .map_err(|e| anyhow::anyhow!("failed to send response: {e}"))
    }

    fn notify<N: lsp_types::notification::Notification>(
        &self,
        params: N::Params,
    ) -> anyhow::Result<()> {
        let note = lsp_server::Notification::new(N::METHOD.to_string(), params);
        self.connection
            .sender
            .send(Message::Notification(note))
            .map_err(|e| anyhow::anyhow!("failed to send notification: {e}"))
    }
}

/// Attempts to extract a typed request; returns the original on method mismatch.
fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    match req.extract::<R::Params>(R::METHOD) {
        Ok(value) => Ok(value),
        Err(ExtractError::MethodMismatch(req)) => Err(req),
        Err(ExtractError::JsonError { method, error }) => {
            // Malformed params: log and swallow by faking a mismatch so the
            // dispatcher can fall through to the null response.
            let _: (&str, Box<dyn Error>) = (&method, Box::new(error));
            Err(Request {
                id: RequestId::from(0),
                method: String::new(),
                params: serde_json::Value::Null,
            })
        }
    }
}
