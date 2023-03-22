<script lang="ts">
    import { createEventDispatcher } from 'svelte';
    import {HighlightAuto} from 'svelte-highlight';

    export let content = "";
    export let index: number;
    export let renderAsCode = true;
    let isEditing = false;

    function deleteBlock() {
        dispatch('deleteBlock', index);
    }

    function saveBlock() {
        isEditing = false;
        dispatch('saveBlock', { index, content });
    }

    function enableEditing() {
        isEditing = true;
    }

    const dispatch = createEventDispatcher();
</script>

<svelte:head>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css">
</svelte:head>

<div class="border p-2 rounded mb-2 relative">
    {#if isEditing}
        <textarea
            class="border border-gray-200 rounded p-2 w-full"
            rows="4"
            bind:value={content}
        ></textarea>
        <button class="absolute top-0 right-0 bg-green-500 text-white p-1" on:click={saveBlock}>Save</button>
    {:else}
        {#if renderAsCode}
            <HighlightAuto code={content} class="bg-gray-100 p-2 rounded" />
        {:else}
            <p>{content}</p>
        {/if}
        <button class="absolute top-0 right-0 bg-red-500 text-white p-1" on:click={deleteBlock}>Delete</button>
        <button class="absolute top-0 right-20 bg-blue-500 text-white p-1" on:click={enableEditing}>Edit</button>
    {/if}
</div>
