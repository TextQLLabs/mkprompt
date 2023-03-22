<script lang="ts">
    import type * as vscode from 'vscode';
    import { getAppState } from '$lib/req.js';
    import type { AppState } from '$lib/req.js';
    import { onMount } from 'svelte';
    import Diagnostic from '$lib/Diagnostic.svelte';
    import Symbol from '$lib/Symbol.svelte';
    import File from '$lib/File.svelte';
    import PromptBlock from '$lib/PromptBlock.svelte';

    type PromptBlock = {
        content: string;
        isCode: boolean;
    };

    let promptBlocks: PromptBlock[] = [];
    let newBlockContent = '';

    function mkPrompt(): string {
        let promptLines: string[] = [];
        for (let block of promptBlocks) {
            if (block.isCode) {
                promptLines.push('```');
                promptLines.push(block.content);
                promptLines.push('```');
            } else {
                promptLines.push(block.content);
            }
        }
        return promptLines.join('\n');
    }

    function copyPrompt() {
        navigator.clipboard.writeText(mkPrompt());
    }

    function addBlock(content: string, isCode = false) {
        let newBlock: PromptBlock = {
            content: content,
            isCode: isCode
        };
        promptBlocks = [...promptBlocks, newBlock];
    }

    function submitBlock() {
        addBlock(newBlockContent, false);
        newBlockContent = '';
    }

    function deleteBlock(event: { detail: number }) {
        promptBlocks.splice(event.detail, 1);
        promptBlocks = [...promptBlocks]; // Force Svelte to re-render the list
    }

    function clearBlocks() {
        promptBlocks = [];
    }

    let appState: AppState;
    let symbolSearchTerm = '';
    let filteredSymbols: vscode.SymbolInformation[] = [];
    $: if (appState)
        filteredSymbols = appState.symbols.filter((symbol: vscode.SymbolInformation) =>
            symbol.name.toLowerCase().includes(symbolSearchTerm.toLowerCase())
        );

    let fileSearchTerm = '';
    let filteredFiles: vscode.Uri[] = [];
    $: if (appState)
        filteredFiles = appState.openFiles.filter((file: vscode.Uri) =>
            file.fsPath.toLowerCase().includes(fileSearchTerm.toLowerCase())
        );

    function isCurrent(file: vscode.Uri, appState: AppState) {
        return file.fsPath === appState.currentFile?.fsPath;
    }

    async function refreshAppState() {
        appState = await getAppState();
    }

    function addDiagnostic(event: {
        detail: { message: string; snippet: string; location: string };
    }) {
        addBlock(event.detail.message, true);
        addBlock(event.detail.location, false);
        addBlock(event.detail.snippet, true);
    }

    function addSymbol(event: { detail: { snippet: string; location: string } }) {
        addBlock(event.detail.location, false);
        addBlock(event.detail.snippet, true);
    }

    function addFile(event: { detail: string }) {
        addBlock(event.detail, true);
    }

    onMount(() => {
        // Call refreshAppState immediately
        refreshAppState();
        // Call refreshAppState every 2 seconds
        setInterval(refreshAppState, 2000);
    });
</script>

<div class="p-8 flex flex-row">
    <div class="w-3/4 mr-8">
        <div class="mb-4">
            <form on:submit|preventDefault={submitBlock}>
                <input
                    class="border border-gray-200 rounded p-2 w-full"
                    placeholder="Type a new block or select an existing one to edit"
                    bind:value={newBlockContent}
                />
            </form>
            <button class="bg-blue-500 text-white px-4 py-2 mt-2" on:click={submitBlock}
                >Add Block</button
            >
            <button class="bg-blue-500 text-white px-4 py-2 mt-2 ml-2" on:click={copyPrompt}
                >Copy Prompt</button
            >
            <button class="bg-red-500 text-white px-4 py-2 mt-2 ml-2" on:click={clearBlocks}
                >Clear</button
            >
        </div>
        <div class="space-y-4">
            {#each promptBlocks as block, index (block)}
                <PromptBlock
                    {index}
                    content={block.content}
                    renderAsCode={block.isCode}
                    on:deleteBlock={deleteBlock}
                />
            {/each}
        </div>
        <div class="invisible">
            <PromptBlock
                index={42424242}
                content="console.log(you.shouldnt.see.this)"
                renderAsCode={true}
            />
        </div>
    </div>
    <div class="flex flex-col justify-start h-screen space-y-4 w-1/4 divide-y-2">
        {#if appState && appState.currentFile}
            <div class="flex flex-col space-y-2">
                <h3 class="text-lg font-semibold mb-2">Diagnostics</h3>
                <div class="overflow-y-auto max-h-[20rem] space-y-2">
                    {#each appState.diagnostics as diagnostic (diagnostic)}
                        <Diagnostic
                            {diagnostic}
                            currentUri={appState.currentFile}
                            on:addDiagnostic={addDiagnostic}
                        />
                    {/each}
                </div>
            </div>

            <div class="flex flex-col space-y-2">
                <h3 class="text-lg font-semibold mb-2">Symbols</h3>
                <input
                    type="text"
                    class="border border-gray-200 rounded p-2 mb-2"
                    placeholder="Search symbols..."
                    bind:value={symbolSearchTerm}
                />
                <div class="overflow-y-auto max-h-[20rem] space-y-2">
                    {#each filteredSymbols as symbol}
                        <Symbol {symbol} on:addSymbol={addSymbol} />
                    {/each}
                </div>
            </div>

            <div class="flex flex-col space-y-2">
                <h3 class="text-lg font-semibold mb-2">Open Files</h3>
                <div class="overflow-y-auto max-h-[20rem] space-y-2">
                    <input
                        type="text"
                        class="border border-gray-200 rounded p-2 mb-2"
                        placeholder="Search files..."
                        bind:value={fileSearchTerm}
                    />
                    <File uri={appState.currentFile} isCurrent={true} on:addFile={addFile} />
                    {#each filteredFiles as file}
                        {#if file.scheme === 'file' && file.fsPath !== appState.currentFile.fsPath}
                            <File uri={file} isCurrent={false} />
                        {/if}
                    {/each}
                </div>
            </div>
        {:else}
            <h2 class="text-lg font-semibold mb-2">
                Not Connected. Run "Connect to MkPrompt Web App from VSCode." to connect.
            </h2>
        {/if}
    </div>
</div>
