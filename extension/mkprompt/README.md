# mkprompt VSCode extension

VS Code extension that acts as a companion to the [mkprompt](https://github.com/TextQLLabs/mkprompt) web app. Lets you build ChatGPT prompts by injecting code and diagnostics from your open VS Code projects.

How to use this:
1. Download this VSCode Extension.
2. Run the `mkprompt` web app by following the instructions [here](https://github.com/TextQLLabs/mkprompt). Or, just run this:
```
docker run -it --rm -p 4080:4080 ahaym/mkprompt:latest
```
(You might need to `sudo` run the above command.)
3. Ctrl-Shift-P and run "Connect to MkPrompt Web App"
4. Navigate to `localhost:4080` and start building prompts!
