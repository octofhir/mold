---
title: Editor setup
description: Use banshee as a language server in your editor.
sidebar:
  order: 6
---

`banshee lsp` speaks LSP over stdio: diagnostics with quick-fix code actions,
completion (keywords, schema columns/tables, JSONB paths), hover, document and
range formatting, document symbols, semantic tokens, and signature help.

## VS Code

A local extension lives in
[`editors/code`](https://github.com/octofhir/banshee/tree/main/editors/code).
Put `banshee` on your `PATH` (or set `banshee.path`), then from `editors/code`
run `npm install && npm run compile` and press **F5** to launch it.

## Neovim (built-in LSP)

```lua
vim.lsp.start({
  name = 'banshee',
  cmd = { 'banshee', 'lsp' },
  root_dir = vim.fs.dirname(vim.fs.find({ 'banshee.toml', '.git' }, { upward = true })[1]),
})
```

Any editor that speaks LSP can launch `banshee lsp` the same way.
