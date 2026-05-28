import { workspace, ExtensionContext, window } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext): void {
  const config = workspace.getConfiguration("mold");
  const command = config.get<string>("path", "mold");

  // mold speaks LSP over stdio via `mold lsp`.
  const serverOptions: ServerOptions = {
    run: { command, args: ["lsp"], transport: TransportKind.stdio },
    debug: { command, args: ["lsp"], transport: TransportKind.stdio },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "sql" }],
    synchronize: {
      // Re-resolve config when a mold.toml changes.
      fileEvents: workspace.createFileSystemWatcher("**/mold.toml"),
    },
  };

  client = new LanguageClient(
    "mold",
    "mold language server",
    serverOptions,
    clientOptions,
  );

  client.start().catch((err) => {
    window.showErrorMessage(
      `mold: failed to start the language server (${command} lsp): ${err}`,
    );
  });
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
