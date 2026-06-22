const fs = require("fs");
const path = require("path");
const vscode = require("vscode");
const { LanguageClient, TransportKind } = require("vscode-languageclient/node");

let client;
let outputChannel;

async function activate(context) {
  outputChannel = vscode.window.createOutputChannel("RustyLR LSP");
  context.subscriptions.push(outputChannel);

  context.subscriptions.push(
    vscode.commands.registerCommand("rustylr.restartServer", async () => {
      await stopClient();
      try {
        await startClient(context);
        vscode.window.showInformationMessage("RustyLR language server restarted.");
      } catch (error) {
        reportStartError(error);
      }
    })
  );

  try {
    await startClient(context);
  } catch (error) {
    reportStartError(error);
  }
}

async function deactivate() {
  await stopClient();
}

async function startClient(context) {
  const config = vscode.workspace.getConfiguration("rustylr.server");
  const workspaceFolder =
    vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0
      ? vscode.workspace.workspaceFolders[0].uri.fsPath
      : undefined;
  const repoRoot = findRustyLrRoot(workspaceFolder) || findRustyLrRoot(context.extensionPath);

  const configuredCwd = config.get("cwd", "");
  const cwd = configuredCwd
    ? expandPath(configuredCwd, { workspaceFolder, extensionPath: context.extensionPath, repoRoot })
    : repoRoot || workspaceFolder || context.extensionPath;

  const configuredCommand = config.get("command", "");
  const configuredArgs = config.get("args", []);
  const server = resolveServerCommand(configuredCommand, configuredArgs, {
    workspaceFolder,
    extensionPath: context.extensionPath,
    repoRoot,
    cwd,
  });

  const patterns = config.get("documentPatterns", [
    "**/grammar.rs",
    "**/src/parser.rs",
    "**/*.rustylr.rs",
    "**/*.rustylr",
    "**/*.lr",
  ]);

  const documentSelector = [
    { scheme: "file", language: "rustylr" },
    ...patterns.map((pattern) => ({ scheme: "file", pattern })),
  ];

  outputChannel.appendLine(`Starting RustyLR LSP: ${server.command} ${server.args.join(" ")}`);
  outputChannel.appendLine(`RustyLR LSP cwd: ${cwd}`);

  client = new LanguageClient(
    "rustylr",
    "RustyLR Language Server",
    {
      command: server.command,
      args: server.args,
      options: { cwd },
      transport: TransportKind.stdio,
    },
    {
      documentSelector,
      outputChannel,
      synchronize: {
        configurationSection: "rustylr",
      },
    }
  );

  await client.start();
}

async function stopClient() {
  if (!client) {
    return;
  }

  const activeClient = client;
  client = undefined;
  try {
    await activeClient.stop();
  } catch (error) {
    const message = error && error.message ? error.message : String(error);
    if (outputChannel) {
      outputChannel.appendLine(`Ignoring RustyLR LSP stop error: ${message}`);
    }
  }
}

function expandPath(value, vars) {
  return value
    .split("${workspaceFolder}")
    .join(vars.workspaceFolder || "")
    .split("${extensionPath}")
    .join(vars.extensionPath || "")
    .split("${repoRoot}")
    .join(vars.repoRoot || "");
}

function resolveServerCommand(configuredCommand, configuredArgs, vars) {
  if (configuredCommand) {
    return {
      command: expandPath(configuredCommand, vars),
      args: configuredArgs.map((arg) => expandPath(arg, vars)),
    };
  }

  const binaryName = process.platform === "win32" ? "rusty_lr_lsp.exe" : "rusty_lr_lsp";
  const candidates = [
    vars.repoRoot && path.join(vars.repoRoot, "target", "debug", binaryName),
    vars.repoRoot && path.join(vars.repoRoot, "target", "release", binaryName),
  ].filter(Boolean);

  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      return { command: candidate, args: [] };
    }
  }

  return {
    command: "cargo",
    args: ["run", "--quiet", "--package", "rusty_lr_lsp"],
  };
}

function findRustyLrRoot(startPath) {
  if (!startPath) {
    return undefined;
  }

  let current = fs.statSync(startPath).isDirectory() ? startPath : path.dirname(startPath);
  while (true) {
    if (
      fs.existsSync(path.join(current, "Cargo.toml")) &&
      fs.existsSync(path.join(current, "rusty_lr_lsp", "Cargo.toml"))
    ) {
      return current;
    }

    const parent = path.dirname(current);
    if (parent === current) {
      return undefined;
    }
    current = parent;
  }
}

function reportStartError(error) {
  const message = error && error.stack ? error.stack : String(error);
  if (outputChannel) {
    outputChannel.appendLine("Failed to start RustyLR LSP.");
    outputChannel.appendLine(message);
    outputChannel.show(true);
  }
  vscode.window.showErrorMessage("Failed to start RustyLR language server. See Output: RustyLR LSP.");
}

module.exports = {
  activate,
  deactivate,
};
