# dotfiles

[Source (SourceHut)](https://git.sr.ht/~hedy/dotfiles) |
[Patches](https://lists.sr.ht/~hedy/inbox) |
[Questions/Feedback](https://lists.sr.ht/~hedy/inbox) |
Chat ([Libera](https://web.libera.chat/#hedy)/[Tilde](https://tilde.chat/kiwi/##hedy))

Welcome to my personal collection of weirdish—sometimes
unmaintained—configuration and lose organization of cool scripts!

These are the setup I have on almost all machines I work in, and because I work
on many [tildes](https://tildeverse.org), I make this set of configuration as
cross-platform as possible.

I also try to use `$HOME` or `~` in place of `/home/name` because on Mac it's
`/Users/name` and my username is not always the same. This also aids
portability for others looking to reference my config.

Used on (aka loosely tested on):
- MacOS
- Debian/Ubuntu
- Arch
- ~WSL 1~ (I no longer use that)

WSL 2 would probably be a similar experience to any of those corresponding
linux distros.

Note that WSL support of clipboard pastes is deprecated on NVIM as of
2023-06-30. Before which, you need to have a `pbcopy`/`pbpaste` binary in path
that handles clipboard operations with windows.

## CHANGELOG

- 2023-09: I've switched from neovim to Doom, and to my own Emacs!
- 2023-06: (nvim) Switched from vim to full-lua set up

---

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
            * [New NVIM setup (lua)](#new-nvim-setup-lua)
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
   - Emacs ([`.config/emacs`](.config/emacs)) (with chemacs: [.emacs-profiles.el](.emacs-profiles.el))
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
- Neovim: used to be my primary editor, but now **I've switched to emacs** 😈
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

<details>
<summary> Legacy NVIM setup </summary>

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
</details>

##### New NVIM setup (lua)

Since 2023-06-30, I've switched to `init.lua` 🎉

- Plugin manager: Lazy.nvim
- Completion: nvim-cmp
- File explorer: nvim-tree and mini.files
- Icons: nvim-web-devicons
- Status line: lualine

A decision like this was as a difficult one to make as projects like fish
switching from C++ to Rust (like WTF?)

But my reasoning is like theirs, my startup time has improved significantly and
this switch came with many other advantages including opening up a world of many
modern, speedy plugins with very useful (not exactly due to being trendy)
functionalities.

Yes, I still use dracula theme.

Yes, I still don't use fancy separators for my status line.

Yes, I still don't use a dashboard, nor open up a sidebar of file explorer
automatically.

I like to choose plugins that are fast, customizable, and generally does not
have feature creep.

**Regarding bufferline**

lualine provides tabline support, which displays the list of buffers at the top
of the window where vim tabs are supposed to be.

If there is another plugin that provides similar customization capabilities as
lualine but does not have tabline, I would happily switch to that.

Even though I don't use tabs much, I prefer to have the list of buffers at the
bottom on the command line rather than taking up space at the top.

No, I don't wish to have hover events, "x"-close buttons, filetype icons on the
buffer line. My nvim should be a lightweight editor that has particular useful
capabilities from IDEs.

**LSP and completion**

I use the official lsp-config plus nvim-cmp.

For cmp, I have sources for latex (useful symbol input), emojis (useful for
docs), buffer (sometimes useful, mostly noisy), git (oh well), calc (also useful
for docs), path (useful for shell scripts and docs), cmdline (avoid opening the
browser search too much), and snippy.

**Linting**

Linting is triggered on save. I use nvim-lint.

**Treesitter**

I plan to use treesitter soon. Features I'm particularly looking for is speed,
indent context highlight (indicates current indent level), and perhaps block
navigation.

Unfortunately it requires newer nvim releases, so I wait for a bit first, like I
did for nvim-0.5 to become more ubiquitous for switching to lua config.

**Telescope**

No, I don't use telescope yet due to the version requirement.

For fuzzy opening of files I have mini.files and `:e fuzzy/path/autocompletion`
provided by nvim-cmp.

Or I would use a file browser outside of neovim like ranger and open from there.

No, I don't use neovim like an IDE, and no I am not one of those emacs-as-an-OS
people. It's only an editor :)

**Setup**:
- `dotscripts/setup/nvim` (no longer needed as I no longer use vim-plug)
- `dotscripts/install/misc` - Includes Vundle setup together with other
software.

#### TODO for neovim

(I might never complete these since I recently switched to emacs)

- [x] Drop CoC and \<v0.5 support
- [x] Use Lazy.nvim package manager
- [x] Replace lightline with lualine
- [x] Set up snippets
- [x] Replace NERDTree with ~neo-tree~ nvim-tree
- [x] Make use of Lazy loading
- [x] Fix ftplugin + lazy ft handle
- [x] Use Tree sitter


### Vanilla Emacs

I use chemacs2 for switching emacs profiles (emacs < v29) and it allows me to
use both vanilla emacs and doom emacs at the same time. See
[`.emacs-profiles.el`](.emacs-profiles.el).

**Setup**: `dotscripts/setup/emacs`. This script is pretty heavily tested and
it should set up everything you need to start using both emacs and doom
normally. It installs chemacs and doom if not already installed.

My main emacs is vanilla emacs, the configuration lives at `~/.config/emacs`. My
primary emacs version is Emacs 29, because I need better SVG support. I use
Emacs 28 sometimes for testing.

Configuration for both Vanilla and Doom emacs are Literate (org files that get
exported "tangled" to indicated Elisp destinations).

### Doom emacs

Doom itself requires at least emacs 27+ and git 2.23+ among other things. More
info on the [github repo](https://github.com/hlissner/doom-emacs).

2023: I started to use Doom a lot more, before switching to primarily vanilla
emacs in 2023-09. The configuration there is somewhat up to date, but not
guaranteed as I don't really use doom anymore.

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

- [x] add LSP and completion to emacs
- [x] (n)vim mapping for gg=<backtick><backtick> (not needed anymore since we had `g@` mapping for LSP format)
- [x] `.addpath` file
- [x] Wait for nvim 0.5 to because more widely adopted in system packages, then
  do the things in [nvim TODO](#todo-for-neovim)
- [ ] Flesh out emacs config to support *most* of my needs for programming and writing
- [ ] (planning) Drop Doom support
