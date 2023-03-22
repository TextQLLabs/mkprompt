<script lang="ts">
    import type * as vscode from 'vscode';
    import { rangeToString, getSnippet } from './req.js';
    import type { Diagnostic } from './req.js';
    import { createEventDispatcher } from 'svelte';
    export let diagnostic: Diagnostic;
    export let currentUri: vscode.Uri;

    const dispatch = createEventDispatcher();

    let color = () => {
        switch (diagnostic.severity as string) {
            case 'Error':
                return 'bg-red-200';
            case 'Warning':
                return 'bg-yellow-200';
            case 'Information':
                return 'bg-blue-200';
            case 'Hint':
                return 'bg-gray-200';
            default:
                return 'bg-gray-200';
        }
    };

    let classes = `rounded-lg p-2 ${color()}`;

    async function addError() {
        let range = diagnostic.range;
        const location = currentUri.path + ':' + rangeToString(diagnostic.range) + ':';
        range[0].character = 0;
        range[1].character = Number.MAX_SAFE_INTEGER;
        const snippet = await getSnippet(currentUri, diagnostic.range);
        dispatch('addDiagnostic', {
            message: diagnostic.message,
            snippet: snippet,
            location: location
        });
    }
</script>

<div class={classes}>
    <p class="font-bold text-sm">{diagnostic.message}</p>
    <p class="text-sm">Location: {rangeToString(diagnostic.range)}</p>
    <button on:click={addError} class="bg-slate-100 text-black rounded p-1 text-xs">+Error</button>
</div>
