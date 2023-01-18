# dotfiles

Welcome to my personal collection of weirdish—sometimes
unmaintained—configuration and lose organization of cool scripts!

These are the setup I have on almost all machines I work in, and because I work
on many [tildes](https://tildeverse.org), I make this set of configuration as
cross-platform as possible.

Used on (aka loosely tested on):
- Debian/Ubuntu
- Arch
- MacOS
- WSL 1 (may be deprecated soon because I no longer use that)

WSL 2 would probably be a similar experience to any of those corresponding
linux distros.

Repository: [SourceHut](https://sr.ht/~hedy/dotfiles)

Mirrors: [tildegit (gitea)](https://tildegit.org/hedy/dotfiles) |
[GitHub](https://github.com/hedyhli/dotfiles)

**Table of Contents**

<!-- vim-markdown-toc GFM -->

* [Overview](#overview)
* [Features](#features)
* [Installation](#installation)
* [Shell](#shell)
    * [Oh My Fish](#oh-my-fish)
    * [Fish functions](#fish-functions)
    * [Local](#local)
* [Editor](#editor)
    * [Vim and nvim](#vim-and-nvim)
        * [TODO for neovim](#todo-for-neovim)
    * [Vanilla Emacs](#vanilla-emacs)
    * [Doom emacs](#doom-emacs)
* [Terminal](#terminal)
* [Gemini and Spartan client](#gemini-and-spartan-client)
* [TODO](#todo)

<!-- vim-markdown-toc -->


## Overview

- Manager: [yadm](https://yadm.io) ([`.config/yadm`](.config/yadm))
- Shell: [fish](https://fishshell.com) ([`.config/fish`](.config/fish))
- Theme: Dracula ((neo)vim and terminal)
- Email: [aerc](https://aerc-mail.org) ([`.config/aerc`](.config/aerc))
- Browser:
   - Qutebrowser
   - Firefox (has nothing to do with this repo though)
- Terminal: [Kitty]() ([`.config/kitty`](.config/kitty))
- Editor:
   - Vim ([`.vimrc`](.vimrc))
   - Neovim ([`.config/nvim`](.config/nvim))
   - Emacs ([`.config/emacsd`](.config/emacsd)) (with chemacs: [.emacs-profiles.el](.emacs-profiles.el))
   - Doom ([`.config/doom`](.config/doom))
- Gemini client:
   - [amfora](https://github.com/makeworld-the-better-one/amfora) ([`.config/amfora`](.config/amfora))
   - [gelim (also spartan)](https://sr.ht/~hedy/gelim) ([`.config/gelim`](.config/gelim))

## Features

These are more like "what I tend to do":

- Mostly bash shebangs
- Shared aliases and environment variables between shells (fish, bash, sh)
- Setup and install scripts

## Installation

1. Install yadm
1. Clone the repo with yadm
1. Fix conflicts as you please
1. Reload sessions
  - fish
  - `tmux source ~/.tmux.conf`
1. Selectively run scripts in [`dotscripts`](./dotscripts/) using `dot <type>
   <target>` such as `dot setup fish` to suppress those not-found errors.

---

Here are detailed information for each component of my dotfiles

## Shell

My default shell is fish because it comes with auto-suggestions and
highlighting without the need of additional plugins. It does have its own
seemingly friendlier syntax, but many times I find myself dropping into `bash`
while writing shell scripts, so I realized... the only thing that is keeping me
to fish may as well be the auto-suggestions and syntax highlighting...
[imrsh](https://git.sr.ht/~sircmpwn/imrsh) seems quite attractive looking at
its goals, plus the fact that it's POSIX-compatible, so if development there
has any progress I may even consider switching.

Despite fish being my primary shell, I still like to keep all my aliases,
scripts, and environment variables synced with the other shells. This is done
in the [`.startup.sh`](.startup.sh) script. It should be sourced for
POSIX-compatible shells, eg:

```sh
echo 'source ~/.startup.sh' >> ~/.bashrc
echo 'source ~/.startup.sh' >> ~/.profile
```

I don't track `bashrc` or `bash_profile` because I like to keep it to the
system's defaults. Maintaining cross-platform versions of them when I use them
rarely is a pain.

The fish shell configuration lives at `.config/fish/`. Of which, `config.fish`
doesn't really anything specific, it just sources the shared environment
variable file (`.exportenvs.fish`) and the shared aliases (`.aliases`).

The `.exportenvs.fish` file is generated by
[`dotscripts/gen/fish-exportenvs`](dotscripts/gen/fish-exportenvs).

It takes [`.exportenvs`](.exportenvs) and translates it into fish syntax.
Instead of using bass to source `.exportenvs` on the fly during fish's startup,
fish can source the generated `.exportenvs.fish` directly which improves
performance significantly.

I have a symlink `.bash_aliases` pointing to `.aliases` because bash likes to
look for that file.

`.exportenvs` is basically a bunch of environment variables exports. A whole
ton of installation scripts on the internet likes to add `export
something=something` to `bashrc`, so when that happens I tend to just move it
into my `.exportenvs`.

Every time `.exportenvs` is updated, run `dot setup fish` to regenerate
`.exportenvs.fish` and reload the environment.

**TLDR**:
- `.startup.sh` for POSIX-compatible shell configuration
- Alternatively, do the aliases in `.aliases`,
- do the environment variables in `.exportenvs`, and
- do the PATH-updates in `.addpath`.

See below section "Local" for machine-specific configuration (untracked in the
repo).


### Oh My Fish

Oh My Fish is like a plugin manager for fish.

I don't have a lot of plugins and nor do I use it for theme and prompt, just
these utilities:
- `z`: quickly access a common dir
- `bass`: source bash scripts and expressions in fish (I use this for giving
  fish support to `nvm`, see [the `nvm`
  function](.config/fish/functions/nvm.fish))
- `pj`: quickly access projects

The fish theme is entirely dependent on the terminal color settings and the
[prompt](.config/fish/functions/fish_prompt.fish) is copied from a particular
pre-existing prompt style that shows error status and git status. I've modifid
the prompt style and added right prompt to show user@hostname.

**Setup**: `dotscripts/setup/fish`

### Fish functions

Fish functions (located at `.config/fish/functions`) are mostly aliases that
require some checking or additional logic.

cat, rm, ll are aliased to ccat, trash, and exa respectively if those programs
are installed.

The nvm function adds fish support to nvm (Node Version Manager).

### Local

The `.config/fish/config_local.fish` file is for configuration specific for a
computer, this could be setting a special $EDITOR, etc.

Other local files recognized:
- `.addpath_local` - machine specific software that changes PATH
- `.gitconfig-local` - email/name, sendmail
- `.config/kitty/local.conf` - fonts

**Setup**: `dotscripts/gen/localfiles`

## Editor

- Vim: only for systems that don't have neovim installed
- Neovim: I use this as my primary editor and IDE
- Emacs: just to play around and learn elisp
- Doom: looking for IDE features and inspiration for my nvim setup. I might
  drop this soon - takes so much time and resources to set it up and maintain
  the millions of packages only to leave it lying there and never get used.
- ~~VScode: I only use this when I'm too stressed to remember vim/emacs's key
  binds, or sometimes when I'm remote-editting.~~ Too heavy, not even as good
  as doom so there's no way I'd use this.

### Vim and nvim

The [bin/nv](./bin/nv) script is an alias to neovim, and runs vim if neovim is
not installed.

For vim I use Vundle as my plugin manager just because I discovered vim before
nvim and Vundle before vim-plug.  My [`vimrc`](.vimrc) is largely unmaintained
pieces of configuration copied from other people's configs. The set of plugins
are mostly maintained (as in me modifying what plugins to use), except for
auto-pairs if I remember correctly.

Sometimes when I change a plugin for nvim and change it for vim too, if I have
extra time.

I use vim-plug as my plugin manager for nvim because it is shorter to type. And
it installs plugins asynchronously.

My nvim config directory (`.config/nvim`) has separate files for different types
of configuration. `general.vim` has mostly the same content as `.vimrc`.

The isWSL function checks whether the system is WSL in my (n)vim configs. This
is to set up the clipboard correctly and rebind some keys to make <kbd>alt</kbd>
work correctly in vim.

**Lua and LSP for Neovim**

If the neovim version on the system >=0.5, LSP will be set up with lua
configuration, otherwise (and if node is installed), CoC will be used. I don't
use any system with neovim \<0.5, so the CoC set up is unmaintained. I might
remove it in the future.

LSP servers - See comments in `.config/nvim/lua/lsp.lua`.

I'll probably also optimize my vim config so that it can be fast and clean -
available for quick editting. No auto-complete, no fancy themes, just some
must-have utilities.

**Setup**:
- `dotscripts/setup/nvim`
- `dotscripts/install/misc` - Includes Vundle setup together with other
software.

#### TODO for neovim

- [ ] Drop CoC and \<v0.5 support
- [ ] Use Lazy.nvim package manager
- [ ] Replace lightline with lualine
- [ ] Set up snippets
- [ ] Replace NERDTree with neo-tree
- [ ] Use Tree sitter


### Vanilla Emacs

I use chemacs2 for switching emacs profiles and it allows me to use both
vanilla emacs and doom emacs at the same time. See
[`.emacs-profiles.el`](.emacs-profiles.el).

**Setup**: `dotscripts/setup/emacs`. This script is prettty heavily tested and
it should set up everything you need to start using both emacs and doom
normally. It installs chemacs and doom if not already.

Configuration for Vanilla should probably work for version 26 too, but I use 27+
on almost all my machines.

Stuff in `.config/emacsd` are largely copied from various emacs distributions to
suit my needs.  The [`modules`](.config/emacsd/modules) directory is
work-in-progress.

### Doom emacs

Doom itself requires at least emacs 27+ and git 2.23+ among other things. More
info on the [github repo](https://github.com/hlissner/doom-emacs).

There isn't much personal configuration in `.config/doom` yet, it's mostly just
enabling or disabling doom modules and add some packages.

The emacs directory would be at `.doomemacs` (because `.emacs.d` is for
chemacs).

## Terminal

The shell theme depends on the terminal theme. I use Kitty Terminal, where its
config at `.config/kitty` uses the Dracula theme. Font configuration varies
depending on the system hence it should be in a separate file `font.conf`. A
`local.conf` file is recognized. Both of which are not tracked in this repo as
they are platform-dependent.

## Gemini and Spartan client

My amfora config at `.config/amfora` uses the default theme with mostly default
settings. I don't use amfora all that often because I also use elpher,
lagrange, and gelim. I plan to have a custom newtab page as well as a custom
theme in the future. Because gelim doesn't support client certificates yet, I
only mostly use amfora for capsules that want a client certificate.

Gelim config is at `.config/gelim`. gelim is my own gemini and spartan client
for the terminal that isn't a full-blown TUI and tries to stay simple whilst
offering many features and extensibility. It's still WIP so there won't be much
configuration in there yet.

## TODO

- [ ] add LSP and completion to emacs
- [x] (n)vim mapping for gg=<backtick><backtick> (not needed anymore since we had `g@` mapping for LSP format)
- [x] `.addpath` file
- [ ] Wait for nvim 0.5 to because more widely adopted in system packages, then
  do the things in [nvim TODO](#todo-for-neovim)
