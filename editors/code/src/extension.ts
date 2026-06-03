import { workspace, ExtensionContext, window } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext): void {
  const config = workspace.getConfiguration("banshee");
  const command = config.get<string>("path", "banshee");

  // banshee speaks LSP over stdio via `banshee lsp`.
  const serverOptions: ServerOptions = {
    run: { command, args: ["lsp"], transport: TransportKind.stdio },
    debug: { command, args: ["lsp"], transport: TransportKind.stdio },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "sql" }],
    synchronize: {
      // Re-resolve config when a banshee.toml changes.
      fileEvents: workspace.createFileSystemWatcher("**/banshee.toml"),
    },
  };

  client = new LanguageClient(
    "banshee",
    "banshee language server",
    serverOptions,
    clientOptions,
  );

  client.start().catch((err) => {
    window.showErrorMessage(
      `banshee: failed to start the language server (${command} lsp): ${err}`,
    );
  });
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
