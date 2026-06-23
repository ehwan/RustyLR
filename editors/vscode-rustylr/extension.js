const fs = require("fs");
const path = require("path");
const vscode = require("vscode");
const { LanguageClient, TransportKind } = require("vscode-languageclient/node");

let client;
let outputChannel;
let startingClient;

async function activate(context) {
  outputChannel = vscode.window.createOutputChannel("RustyLR LSP", { log: true });
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

  startClient(context).catch(reportStartError);
}

async function deactivate() {
  await stopClient();
}

async function startClient(context) {
  if (startingClient) {
    return startingClient;
  }
  if (client) {
    return;
  }

  startingClient = doStartClient(context);
  try {
    await startingClient;
  } finally {
    startingClient = undefined;
  }
}

async function doStartClient(context) {
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
    "**/*.rustylr",
    "**/rustylr.rs",
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
  if (startingClient) {
    try {
      await startingClient;
    } catch (_error) {
      // The start failure will already be reported by the original caller.
    }
  }

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

  const binaryName = process.platform === "win32" ? "rustylr.exe" : "rustylr";
  const lspArgs = ["lsp"];

  // 1. Try local repository targets if inside RustyLR repository (for development)
  if (vars.repoRoot) {
    const candidates = [
      path.join(vars.repoRoot, "target", "debug", binaryName),
      path.join(vars.repoRoot, "target", "release", binaryName),
    ];
    for (const candidate of candidates) {
      if (fs.existsSync(candidate)) {
        return { command: candidate, args: lspArgs };
      }
    }
  }

  // 2. Try searching the system PATH
  const pathEnv = process.env.PATH || "";
  const pathDirs = pathEnv.split(path.delimiter);
  for (const dir of pathDirs) {
    if (!dir) continue;
    const fullPath = path.join(dir, binaryName);
    try {
      if (fs.existsSync(fullPath) && fs.statSync(fullPath).isFile()) {
        return { command: fullPath, args: lspArgs };
      }
    } catch (_e) {
      // Ignore filesystem errors for inaccessible path entries
    }
  }

  // 3. Fallback to cargo run ONLY if we are inside the RustyLR repo
  if (vars.repoRoot) {
    return {
      command: "cargo",
      args: ["run", "--quiet", "--package", "rustylr", "--", "lsp"],
    };
  }

  // 4. Default fallback: assume it is on the PATH and let it fail gracefully
  return {
    command: binaryName,
    args: lspArgs,
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
      fs.existsSync(path.join(current, "rusty_lr_executable", "Cargo.toml"))
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

  vscode.window.showErrorMessage(
    "Failed to start RustyLR language server. Please make sure you have installed 'rustylr' by running 'cargo install rustylr'.",
    "Open README"
  ).then((selection) => {
    if (selection === "Open README") {
      vscode.env.openExternal(vscode.Uri.parse("https://github.com/ehwan/RustyLR/tree/main/editors/vscode-rustylr#readme"));
    }
  });
}

module.exports = {
  activate,
  deactivate,
};
