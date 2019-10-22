# Installing Haskell

Note: this file is still a WIP.

## Installation via Stack

We'll install Haskell via Stack. However, we'll do it "correctly" in that we won't install a specific version of Haskell across your entire computer. Rather, we'll install a specific version that can be used in projects that wish to use that specific version of Haskell later on.

```bash
# 1. Install Stack
curl -sSL https://get.haskellstack.org/ | sh
# 2. Upgrade Stack
stack upgrade
# 3. Create a new Stack-based project
#     in the `~/Projects/haskell-project/` directory
cd ~/Projects
stack new haskell-project

# 4. Change directory into the project's root folder
cd haskell-project/

# 5. Download a version of the GHC without installing it globally on your computer
#    This will take a long time to do the first time, but will be quick every
#    time thereafter.
stack setup

# 5. Build the sample project
stack build
```

## Installing Developer Tools

Once the above commands have been run, you should install some additional tools to help with development. **WARNING: If you were trying to install a tool called `someTool` (made-up name), most blog posts will tell you to run `stack install someTool`. Don't do that!** This will install that GHC-specific-version package globally. If you ever use a different GHC version, you will get conflicts. New learners think `stack install someTool` means there is a corresponding `stack uninstall someTool`. That is not the case. `stack install someTool` is an alias for `stack build --copy-bins someTool`. **The safer route is to run `stack build --copy-compiler-tool someTool` instead.** (~ [source: an opinionated guide to Haskell in 2018](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/))

Tools we'll be installing:
- hlint - provides suggestions for how to fix errors and otherwise improve the quality of your code
- hoogle - search Haskell's API docs
- weeder - removes dead code

Tools we won't be installing
- `ghc-mod` because it doesn't appear to work on more recent versions of Haskell (e.g. `8.4.x` and up) due to requiring a specific version range of `base`.

```bash
# 6. Install useful tools in the project, but not globally on your computer*
stack build --copy-compiler-tool hlint hoogle weeder
```

## Generating Hoogle database

```bash
stack hoogle --server
# You can now open up a Hoogle instance for your project and its dependencies
# by opening a browser to `http://127.0.0.1:<port number it prints out>`
```

## Editor Support

GHC's REPL (i.e. GHCi, which can be started via `stack ghci`) can get you pretty far. However, if you want more IDE integration, follow these instructions

I tried setting up editor support for Atom, but couldn't get the plugins to work. I'm not entirely sure why.

Thus, this shows how to set up an IDE via the Language Server Protocol and Neovim.

### NeoVim

1. [Install neovim](https://github.com/neovim/neovim/wiki/Installing-Neovim)
2. [Install `vim-plugin`](https://github.com/junegunn/vim-plug)
3. Start NeoVim `nvim`
4. Type `:edit $MYVIMRC` to edit the `init.vim` file
5. Type `i` to go to Insert mode
6. Paste [my `init.vim` file](https://github.com/JordanMartinez/_dotfiles/blob/master/init.vim) via `CTRL+SHIFT+V`
    - [Top 50 `init.vim` settings](https://www.shortcutfoo.com/blog/top-50-vim-configuration-options/)
    - [List of all `init.vim` settings](https://stackoverflow.com/questions/30290685/complete-list-of-all-vimrc-configuration-options)
7. Type `ESC` to return to Commande mode
8. Type `:wq` to save the changes and exit NeoVim

Note: if you are new to NeoVim, refer to the below instructions. Learn NeoVim by using it, not by reading about it. Muscle memory takes time and usage:
1. Start NeoVim via `nvim`
2. Type `:Tutor` and press Enter
3. Read the instructions.
    - `h` is the left arrow, `j` down arrow, `k` up arrow, and `l` right arrow.
    - type `:close` to close windows. Sometimes, their instructions will open up a new window but they don't tell you how to close them via this command until later.
    - type `CTRL+W` two times to switch to a different window
    - type `:q` to quit NeoVim
4. After completing it, see the [Vim Cheatsheet](https://www.fcodelabs.com/2018/12/08/Vim-Cheats/)

### Haskell IDE Engine

Haskell IDE Engine (i.e. HIE for short) is what I used because I couldn't figure out how to get `ghcide` to work.

- [Install haskell-ide-engine](https://github.com/haskell/haskell-ide-engine#installation-from-source)
    - Install HIE via stack by running `stack ./install.hs stack-hie-8.6.5` while in the project's directory.
- Start NeoVim via `nvim`
- Type `:CocInfo` and press Enter
- Copy and paste the following to get both Haskell and PureScript language servers ([haskell source](https://github.com/haskell/haskell-ide-engine#Coc) & [purescript source](https://github.com/neoclide/coc.nvim/wiki/Language-servers#purescript))

```json
{
  "languageserver": {
    "haskell": {
      "command": "hie-wrapper",
      "rootPatterns": [
        ".stack.yaml",
        "cabal.config",
        "package.yaml"
      ],
      "filetypes": [
        "hs",
        "lhs",
        "haskell"
      ],
      "initializationOptions": {
        "languageServerHaskell": {
        }
      }
    },
    "purescript": {
         "command": "purescript-language-server",
         "args": ["--stdio"],
         "filetypes": ["purescript"],
         "rootPatterns": ["bower.json", "psc-package.json", "spago.dhall"]
    }
  }
}
```
