<script lang="ts">
    import type * as vscode from 'vscode';
    import { getSnippet } from '$lib/req.js';
    import { createEventDispatcher } from 'svelte';

    export let uri: vscode.Uri;
    export let isCurrent: boolean;

    const dispatch = createEventDispatcher();

    let classes = `border rounded-lg p-2 border-gray-200 ${isCurrent ? 'bg-sky-50' : 'bg-white'}`;
    async function getContents() {
        const snippet = await getSnippet(uri, null);
        dispatch('addFile', snippet);
    }
</script>

<div class={classes}>
    <div class="font-semibold text-sm">{uri.fsPath}</div>
    <button on:click={getContents} class="bg-slate-100 text-black rounded p-1 text-xs"
        >+Contents</button
    >
</div>
