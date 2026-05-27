//! Signature help (`textDocument/signatureHelp`).
//!
//! Finds the innermost function call surrounding the cursor, then renders its
//! signature with the active parameter highlighted, using built-in function
//! metadata (and any function provider).

use lsp_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureInformation,
};
use mold_completion::providers::FunctionProvider;
use mold_syntax::{SyntaxKind, SyntaxNode};

/// Computes signature help at `offset`, if the cursor is inside a call.
pub fn signature_help(
    root: &SyntaxNode,
    offset: u32,
    provider: Option<&dyn FunctionProvider>,
) -> Option<SignatureHelp> {
    let (call, active_param) = enclosing_call(root, offset)?;
    let name = call_name(&call)?;
    let info = mold_completion::generators::functions::get_function_signature(provider, &name)?;

    let parameters: Vec<ParameterInformation> = info
        .args
        .iter()
        .map(|a| {
            let label = match &a.name {
                Some(n) => format!("{} {}", n, a.data_type),
                None => a.data_type.clone(),
            };
            ParameterInformation {
                label: ParameterLabel::Simple(label),
                documentation: None,
            }
        })
        .collect();

    let active = if parameters.is_empty() {
        0
    } else {
        active_param.min(parameters.len() as u32 - 1)
    };

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label: info.signature(),
            documentation: None,
            parameters: Some(parameters),
            active_parameter: Some(active),
        }],
        active_signature: Some(0),
        active_parameter: Some(active),
    })
}

/// Finds the innermost FUNC_CALL whose parentheses enclose `offset`, returning
/// it with the active (zero-based) parameter index.
fn enclosing_call(root: &SyntaxNode, offset: u32) -> Option<(SyntaxNode, u32)> {
    let mut best: Option<SyntaxNode> = None;
    for node in root.descendants() {
        if node.kind() != SyntaxKind::FUNC_CALL {
            continue;
        }
        let Some(open_end) = l_paren_end(&node) else {
            continue;
        };
        let node_end = u32::from(node.text_range().end());
        // Cursor must be after `(` and within the call's extent.
        if offset < open_end || offset > node_end {
            continue;
        }
        // Innermost wins (smallest range).
        if best
            .as_ref()
            .is_none_or(|b| node.text_range().len() < b.text_range().len())
        {
            best = Some(node.clone());
        }
    }

    let call = best?;
    let open_end = l_paren_end(&call)?;
    // Count top-level commas between `(` and the cursor.
    let active = call
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| t.kind() == SyntaxKind::COMMA)
        .filter(|t| {
            let p = u32::from(t.text_range().start());
            p >= open_end && p < offset
        })
        .count() as u32;
    Some((call, active))
}

/// End offset of the call's opening `(`, if present.
fn l_paren_end(node: &SyntaxNode) -> Option<u32> {
    node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::L_PAREN)
        .map(|t| u32::from(t.text_range().end()))
}

/// The function name: the identifier or keyword token before the `(`.
fn call_name(call: &SyntaxNode) -> Option<String> {
    let mut name = None;
    for element in call.children_with_tokens() {
        let Some(token) = element.into_token() else {
            continue;
        };
        match token.kind() {
            SyntaxKind::L_PAREN => break,
            SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT => {
                name = Some(token.text().to_string());
            }
            k if k.is_keyword() => name = Some(token.text().to_string()),
            _ => {}
        }
    }
    name
}
