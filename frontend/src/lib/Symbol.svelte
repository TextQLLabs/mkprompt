<script lang="ts">
    import type * as vscode from 'vscode';
    import type { Loc } from './req.js';
    import { rangeToString, getSnippet } from './req.js';
    import { createEventDispatcher } from 'svelte';

    export let symbol: vscode.SymbolInformation;

    const dispatch = createEventDispatcher();

    type Locs = Loc[];

    let range: Locs = symbol.location.range as unknown as Locs;
    $: range = symbol.location.range as unknown as Locs;

    async function onAddSnippet() {
        const snippet = await getSnippet(symbol.location.uri, range);
        dispatch('addSymbol', snippet);
    }
</script>

<div class="p-2 border border-gray-200 rounded">
    <div class="font-semibold text-sm">{symbol.name}</div>
    <div class="text-xs text-gray-600">Kind: {symbol.kind}</div>
    <div class="text-xs text-gray-600">Path: {symbol.location.uri.path}</div>
    <div class="text-xs text-gray-600">
        Location: {rangeToString(range)}
    </div>
    <button on:click={onAddSnippet} class="bg-slate-100 text-black rounded p-1 text-xs"
        >+Definition</button
    >
</div>
