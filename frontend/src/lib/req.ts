// Import vscode types
import type * as vscode from 'vscode';

export type Diagnostic = {
    severity: 'Error' | 'Warning' | 'Information' | 'Hint'; // Vscode serializes to the string equivalents
    message: string;
    range: Loc[];
};

export type AppState = {
    diagnostics: Diagnostic[];
    symbols: vscode.SymbolInformation[];
    openFiles: vscode.Uri[];
    currentFile: null | vscode.Uri;
};

export type Loc = {
    character: number;
    line: number;
};

export async function getAppState(): Promise<AppState> {
    // fetch the app state
    let resp = await fetch('http://127.0.0.1:4080/appState');
    return resp.json();
}

export async function getSnippet(uri: vscode.Uri, range: null | Loc[] = null): Promise<string> {
    let resp = await fetch('http://127.0.0.1:4080/snippet', {
        method: 'POST',
        body: JSON.stringify({
            uri: uri,
            range: range
        })
    });
    let json = await resp.json();
    return json.snippet;
}

export function rangeToString(range: Loc[]): string {
    return `${range[0].line}:${range[0].character}-${range[1].line}:${range[1].character}`;
}
