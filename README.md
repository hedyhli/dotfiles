# dotfiles

- [Source (SourceHut)](https://git.sr.ht/~hedy/dotfiles)
- [Patches](https://lists.sr.ht/~hedy/inbox)
- [Questions/Feedback](https://lists.sr.ht/~hedy/inbox)
- Chat ([Libera](https://web.libera.chat/#hedy) / [Tilde](https://tilde.chat/kiwi/##hedy))

These are the setup I have on almost all machines I work in, and because I work
on many [tildes](https://tildeverse.org), I make this set of configuration as
cross-platform as possible.

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

---

Repository: [SourceHut](https://sr.ht/~hedy/dotfiles)

Mirrors: [tildegit (gitea)](https://tildegit.org/hedy/dotfiles) |
[GitHub](https://github.com/hedyhli/dotfiles)

**Table of Contents**

<!-- mtoc-start -->

* [Overview](#overview)
* [Installation](#installation)
* [Shell](#shell)
  * [Oh My Fish](#oh-my-fish)
  * [Fish functions](#fish-functions)
  * [Local](#local)
  * [Nushell](#nushell)
* [Editor](#editor)
  * [Vim and nvim](#vim-and-nvim)
    * [Neovim Lua Setup](#neovim-lua-setup)
    * [TODO for neovim](#todo-for-neovim)
  * [Vanilla Emacs](#vanilla-emacs)
  * [Doom emacs](#doom-emacs)
* [Terminal](#terminal)
* [Gemini and Spartan client](#gemini-and-spartan-client)
* [TODO](#todo)

<!-- mtoc-end -->

## Overview

- Manager: [yadm](https://yadm.io) ([`.config/yadm`](.config/yadm))
- Shell & tools:
  - Fish ([`.config/fish`](.config/fish))
  - Git ([`.gitconfig`](.gitconfig), [`.gitignore_global`](.gitignore_global))
  - Jujutsu ([`.config/jutjutsu`](.config/jujutsu))
  - Tmux ([`.tmux.conf`](.tmux.conf))
- Theme: Catppuccin Mocha (Kitty, Fish and Helix), Tundra (Neovim), Modus Vivendi (emacs)
  - Note I use a modified Catppuccin Mocha variant that swaps around some colors
    (without modifying the palette itself) for Kitty ([`.config/kitty/mocha.conf`]
    (.config/kitty/mocha.conf)) and Fish ([`.config/fish/themes/mocha.theme`]
    (.config/fish/themes/mocha.theme))
- Font:
  - Monospace: IBM Plex Mono and Fira Code, without ligatures (Kitty and Emacs)
  - Variable width: Inter (Emacs, with ligatures in Logseq), IBM Plex Sans and
    iA Writer Quattro S (in some contexts)
- Email: [aerc](https://aerc-mail.org) ([`.config/aerc`](.config/aerc))
- Terminal: [Kitty](https://sw.kovidgoyal.net/kitty/) ([`.config/kitty`](.config/kitty))
- Editor:
  - Vim ([`.vimrc`](.vimrc)) (not used)
  - Neovim ([`.config/nvim`](.config/nvim))
  - Emacs ([`.config/emacs`](.config/emacs)) (with chemacs: [.emacs-profiles.el](.emacs-profiles.el))
  - Helix ([`.config/helix`](.config/helix))
  - Doom ([`.config/doom`](.config/doom))
  - [Acme](http://acme.cat-v.org/): Does not have config files, but I do have some scripts (`bin/gotagacme`, `bin/gotagclean.py`)
- Web client (browser):
  - [Elinks fork](https://github.com/rkd77/elinks) with local modifications ([`.elinks/elinks.conf`](.elinks/elinks.conf))
  - [Qutebrowser](https://qutebrowser.org) ([`.qutebrowser/config.py`](.qutebrowser/config.py))
  - And others irrelevant to dotfiles
- Gemini client:
  - [gelim (also spartan)](https://sr.ht/~hedy/gelim) ([`.config/gelim`](.config/gelim))
  - [amfora](https://github.com/makeworld-the-better-one/amfora) ([`.config/amfora`](.config/amfora))

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
POSIX-compatible shells. see `dotscripts/setup/bash`.

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
- `pj`: quickly access projects (NOTE: as of 2024, I've switched to my own
  implementation for `pj` because this does not support `pj
  project-name/sub/dir/here`, and I do not use multiple project paths, which it
  supports.)

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

### Nushell

I don't (yet?) use Nushell as my primary shell because:
- it hasn't received enough support and adoption from external CLI programs
  (completions, plugins etc.)
- There are often breaking changes
- It does not yet support a lot of configuration I need for it to become my
  default shell, such as more nuanced and intelligent syntax highlighting at the
  prompt
- Its completion and history is not as good as Fish's

However, the language is absolutely amazing, and sometimes I might drop into
this shell to do some complex pipelining or advanced scripting (without
wanting to install external packages). Hence, my Nushell environment is setup up
to be satisfactorily usable, ie, same aliases, similar readline keybindings,
environment variables and PATH. The latter too is inherited from the parent
process, ie, fish.

**Config**: `.config/nushell`

**Scripts**: `dotscripts/*/nu`

## Editor

- Vim: for systems that don't have neovim installed
- Neovim (>= 0.8): primary editor, for now
- Emacs (29 - I need emoji): continuously improving my setup, until one day,
  *one day*, I'll have it replace neovim.
- Doom: looking for IDE features and inspiration for my nvim and vanilla emacs
  setup. I might drop this soon - takes so much time and resources to set it up
  and maintain the millions of packages only to leave it lying there and never get
  used.
- ~~VScode: I only use this when I'm too stressed to remember vim/emacs's key
  binds, or sometimes when I'm remote-editting.~~ I no longer have any need for
  waiting for an editor an eternity to start up and maintaining... JSON configs??
  that runs on Electron?? yeah... no thanks, sorry.

### Vim and nvim

**README**: `.config/nvim/README.norg`

Minimum supported version: NVIM 0.5.0

Recommended version: NVIM 0.9.0+

The [bin/nv](./bin/nv) script is an alias to neovim, and runs vim if neovim is
not installed.

The vim config is usable, but largely unmaintained. It can be used on systems
without neovim or with an unsupported version of neovim.

<details>
<summary> Legacy neovim setup </summary>

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
available for quick editing. No auto-complete, no fancy themes, just some
must-have utilities.
</details>

#### Neovim Lua Setup

- Plugin manager: Lazy.nvim
- Completion: nvim-cmp
- File explorer: nvim-tree and mini.files
- Icons: nvim-web-devicons
- Status line: Disabled (using rulerfmt + incline.nvim)

Ever since then, my startup time has improved significantly and this switch came
with many other advantages including opening up a world of many modern, speedy
plugins with very useful (not exactly due to being trendy) functionalities.

I like to choose plugins that are fast, customizable, and generally does not
have feature creep.

**LSP and completion**

I use the official lsp-config plus nvim-cmp.

**Linting**

Linting is triggered on save. I use nvim-lint.

**Treesitter**

Treesitter plugin is enabled conditionally if neovim version >= 0.9.

Note that a proper C/C++ compiler toolchain is required for building the
parsers.

**Telescope**

Telescope plugin is enabled conditionally if neovim version >= 0.9.

**Setup**:
- `dotscripts/setup/nvim` (no longer needed as I no longer use vim-plug)
- `dotscripts/install/misc` - Includes Vundle setup together with other
software.

**Additional information**: `.config/nvim/README.norg` including brief notes about neovim vs emacs.

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
- [x] Ensure conditionally loaded plugins (from nvim version) work as expected


### Vanilla Emacs

I use chemacs2 for switching emacs profiles (for emacs < v29) and it allows me
to use both vanilla emacs and doom emacs at the same time. See
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
chemacs). Relevant environment variables are set in `.exportenvs`, also see
`.emacs-profiles.el`.

## Terminal

The shell theme depends on the terminal theme. I use Kitty Terminal, where its
config at `.config/kitty`. It uses customized Catppuccin Mocha theme. Font
configuration varies depending on the system hence it should be in a separate
file `font.conf`. A `local.conf` file is recognized. Both of which are not
tracked in this repo as they are platform-dependent.

## Gemini and Spartan client

My primary client is Gelim on the terminal and Lagrange otherwise.

Gelim config is at `.config/gelim`. gelim is my own gemini and spartan client
for the terminal that isn't a full-blown TUI and tries to stay simple whilst
offering many features and extensibility. It's still WIP so there won't be much
configuration in there yet.

## TODO

- [x] add LSP and completion to emacs
- [x] `.addpath` file
- [x] Wait for nvim 0.5 to because more widely adopted in system packages, then
  use full lua config + lsp
- [ ] Flesh out emacs config to support *most* of my needs for programming and writing
- [ ] (planning) Drop Doom support
