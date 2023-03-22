// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import axios from 'axios';
import { WebSocket } from 'ws';
const _ = require('lodash');

type AppState = {
    diagnostics: vscode.Diagnostic[];
    symbols: vscode.SymbolInformation[];
    openFiles: vscode.Uri[];
    currentFile: vscode.Uri;
};

type RequestLoc = {
    character: number;
    line: number;
};

type SnippetWebSocketRequest = {
    id: number;
    uri: { fsPath: string };
    range: RequestLoc[] | null;
};

async function pingEndpoint(): Promise<boolean> {
  const endpoint = 'http://localhost:4080/ping';
  try {
    const response = await axios.get(endpoint);
    return response.data === 'pong';
  } catch (error: any) {
    console.error('Error pinging endpoint:', error.message);
    return false;
  }
}

async function sendAppState(appState: AppState) {
    const endpoint = 'http://localhost:4080/appState';
    try {
        const response = await axios.post(endpoint, appState);
        console.log('App state sent successfully:', response.data);
    } catch (error: any) {
        console.error('Error sending app state:', error.message);
    }
}

async function fulfillSnippetRequest(request: SnippetWebSocketRequest) {
    const rawRange = request.range;
    let range = null;
    if (rawRange) {
        range = new vscode.Range(
            rawRange[0].line,
            rawRange[0].character,
            rawRange[1].line,
            rawRange[1].character
        );
    }
    const codeSnippet = await getCodeSnippetFromFile(vscode.Uri.file(request.uri.fsPath), range);
    const req = {
        id: request.id,
        snippet: codeSnippet
    };
    const endpoint = 'http://localhost:4080/fulfillSnippet';
    try {
        const response = await axios.post(endpoint, req);
        console.log('Snippet sent successfully:', response.data);
    } catch (error: any) {
        console.error('Error sending snippet:', error.message);
    }
}

async function getCodeSnippetFromFile(uri: vscode.Uri, range: null | vscode.Range) {
    // Open the text document of the file using its URI
    const document = await vscode.workspace.openTextDocument(uri);
    // Get the text of the code snippet within the specified range
    const codeSnippet = range ? document.getText(range) : document.getText();

    return codeSnippet;
}

async function createAppStateFromEvent(event: vscode.TextDocumentChangeEvent) {
    const uri = event.document.uri;
    const currentFile = uri;
    const diagnostics = vscode.languages.getDiagnostics(uri);
    const symbols = await collectSymbols();
    const openFiles = vscode.workspace.textDocuments.map((doc) => doc.uri);
    return { diagnostics, symbols, openFiles, currentFile };
}

async function collectSymbols(): Promise<vscode.SymbolInformation[]> {
    let symbols: vscode.SymbolInformation[] = [];
    for (const document of vscode.workspace.textDocuments) {
        let docSymbols: vscode.SymbolInformation[] = await vscode.commands.executeCommand(
            'vscode.executeDocumentSymbolProvider',
            document.uri
        );
        if (docSymbols) {
            symbols.push(...docSymbols);
        }
    }
    return symbols;
}

async function appStateHandler(event: vscode.TextDocumentChangeEvent) {
    const appState = await createAppStateFromEvent(event);
    await sendAppState(appState);
}

const debouncedAppStateHandler = _.debounce(appStateHandler, 1000);

let socket: WebSocket;
let connected = false;

export function activate(context: vscode.ExtensionContext) {
    // The command has been defined in the package.json file
    let disposable = vscode.commands.registerCommand('mkprompt.connectToApp', async () => {
        let ping = await pingEndpoint();
        if (!ping) {
            vscode.window.showErrorMessage('mkprompt app is not running!');
            return;
        }
        socket = new WebSocket(`ws://localhost:4080`);
        socket.on('message', async function (message) {
            const messageData: SnippetWebSocketRequest = JSON.parse(message.toString());
            await fulfillSnippetRequest(messageData);
        });
        connected = true;
        vscode.window.showInformationMessage('Connected to mkprompt app!');
    });

    // Register a listener for document changes
    vscode.workspace.onDidChangeTextDocument(async (event) => {
        if (connected) {
            await debouncedAppStateHandler(event);
        }
    });

    context.subscriptions.push(disposable);
}

// This method is called when your extension is deactivated
export function deactivate() {}
