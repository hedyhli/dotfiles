# dotfiles

Welcome to my personal collection of weirdish sometimes unmained configuration and
lose organization of cool scripts!

These are the setup I have on almost all machines I work in, and because I work
on many [tildes](https://tildeverse.org), I make this set of configuration as
cross-platform as possible.

Mirrors: [tildegit (gitea)](https://tildegit.org/hedy/dotfiles)
| [GitHub](https://github.com/hedyhli/dotfiles)

**Table of contents**

- [Basic list of things in the repo](#basics)
- [Features](#features)
- [Installation](#installation)
- [Details](#details)
  - [shell](#shell)
  - [editor](#editor)
    - [vim and nvim](#vim-and-nvim)
    - [emacs](#emacs)
  - [gemini/spartan client](#gemini-spartan-client) 
- [todo](#todo)

## Basics

- Manager: [yadm](https://yadm.io) ([config](.config/yadm))
- Shell: [fish](https://fishshell.com) ([config](.config/fish))
- Theme: Dracula ((neo)vim and terminal)
- Email: [aerc](https://aerc-mail.org) ([config](.config/aerc))
- Browser:
   - Chrome (has nothing to do with this repo though)
   - w3m
- Gemini client:
   - [amfora](https://github.com/makeworld-the-better-one/amfora) ([config](.config/amfora))
   - [gelim (also spartan)](https://sr.ht/~hedy/gelim) ([config](.config/gelim))
- Editor:
   - vim ([config](.vimrc))
   - neovim ([config](.config/nvim))
   - emacs ([config](.config/emacsd)) (with [chemacs](.emacs-profiles.el))
   - doom ([config](.config/doom))

## Features

These are more like "what I tend to do" in this repo

- Mostly bash shebangs
- Shared aliases and env between shells
- Setup and install scripts
- Private configuration tracked with yadm

## Details

Here are detailed information for each compenent of my dotfiles

### Shell

I don't track `bashrc` or `bash_profile` because I like to keep it to the system's defaults.

The fish shell configuration is at `.config/fish/` and `config.fish` doesn't really anything specific,
it just sources the shared environment variable file (`.exportenvs`) and the shared aliases (`.aliases`).

I have a symlink `.bash_aliases` pointing to `.aliases` because bash likes to look for that file.

`.exportenvs` is basically a bunch of environment variables exports. A whole ton of installation scripts
on the internet likes to add `export something=something` to `bashrc`, so when that happens I tend to just
move it into my `.exportenvs`.

#### oh my fish

oh my fish is like a plugin manager for fish, I don't have a lot of plugins, just these utilities:
- `z`: quickly access a common dir
- `bass`: source bash scripts and expressions in fish (I use this for sourcing `.exportenvs`)
- `pj`: access projects

fish theme is based entirely on the terminal and the
[prompt](.config/fish/functions/fish_prompt.fish) is copied from a particular pre-existing
prompt style that shows error status and git status.

#### fish functions

fish functions (located at `.config/fish/functions`) are mostly aliases that require some checking or
additional logic.

cat, rm, ll are aliased to ccat, trash, and exa respectively if those programs are installed.

#### local

The `.config/fish/config_local.fish` file is for configuration specific for a computer, this could
be setting a special $EDITOR, etc.

**Setup**: `_scripts/setup-fish`

### editor

- vim: only for systems that don't have neovim installed
- nvim: I use this as my primary editor and IDE
- emacs: just to play around and learn elisp
- doom: looking for IDE features and inspiration for my nvim setup. (Currenting editting READ using doom).
This *may* become my primary IDE for GUI.
- vscode: I only use this when I'm too stressed to remember vim/emacs's key binds, or I'm remote-editting
my [site](https://hedy.tilde.cafe)'s markdown posts.

#### vim and nvim

The [bin/nv](./bin/nv) script is an alias to neovim, and runs vim if neovim is not installed.

For vim I use Vundle as my plugin manager just because I discovered vim before nvim and Vundle before Plug.
My [`vimrc`](.vimrc) is largely unmaintained pieces of configuration copied from other people's configs. The
set of plugins are mostly maintained (as in me modifying what plugins to use), except for auto-pairs if I remember correctly.

Sometimes when I change a plugin for nvim and change it for vim too, if I have extra time.

I use vim-plug as my plugin manager for nvim because it is shorter to type. Completion with CoC is set up
for languages that I commonly work in. CoC keybinds configuration is mostly copied from their readme.

My nvim config directory (`.config/nvim`) has separate files for different types of configuration. `general.vim`
has mostly the same content as `.vimrc`.

The isWSL function checks whether the system is WSL in my (n)vim configs
I rebind some keys to make it work on WSL, but I don't use it much anymore.

**Setup**: `_scripts/nvim-setup_`

#### emacs

I use chemacs2 for switching emacs profiles and it allows me to use both vanilla emacs and doom emacs
at the same time. See [`.emacs-profiles.el`](.emacs-profiles.el).

**Setup**: `_scripts/emacs-setup_`. This script is mostly tested and it should set up everything you need
to start using both emacs and doom normally. It installs chemacs and doom if not already.

##### Vanilla emacs

They should probably work for version 26 too, but I use 27+ on almost all my machines.

Stuff in `.config/emacsd` are largely copied from various emacs distributions to suit my needs.
The [`modules`](.config/emacsd/modules) directory is work-in-progress.

##### Doom emacs

Doom itself requires at least emacs 27+ and git 2.23+ among other things. More info on the
[github repo](https://github.com/hlissner/doom-emacs).

There isn't much personal configuration in `.config/doom` yet, it's mostly just enabling or disabling
doom modules and add some packages.

The emacs directory would be at `.doomemacs` (because `.emacs.d` is for chemacs).

### Gemini and Spartan client

My amfora config at `.config/amfora` uses the default theme with mostly default settings. I don't use
amfora all that often because I also use elpher, lagrange, and gelim. I plan to have a custom newtab page
as well as a custom theme in the future. Because gelim doesn't support client certificates yet, I only
mostly use amfora for capsules that want a client certificate.

gelim config is at `.config/gelim`. gelim is my own gemini and spartan client for the terminal that isn't
a full-blown TUI and tries to stay simple whilst offering many features and extensibility. It's still WIP
so there won't be much configuration in there yet.

## TODO

- [ ] add LSP and completion to emacs
- [ ] (n)vim mapping for gg=``
- [ ] `.addpath` file
