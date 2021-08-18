source ~/.aliases

# Environment variables
for line in (cat ~/.exportenvs)
	bass $line
end

if test -f ~/.config/fish/config_local.fish
	source ~/.config/fish/config_local.fish
end

set -gx HOMEBREW_PREFIX "/home/hedy/.linuxbrew";
set -gx HOMEBREW_CELLAR "/home/hedy/.linuxbrew/Cellar";
set -gx HOMEBREW_REPOSITORY "/home/hedy/.linuxbrew/Homebrew";
set -gx HOMEBREW_SHELLENV_PREFIX "/home/hedy/.linuxbrew";
set -q PATH; or set PATH ''; set -gx PATH "/home/hedy/.linuxbrew/bin" "/home/hedy/.linuxbrew/sbin" $PATH;
set -q MANPATH; or set MANPATH ''; set -gx MANPATH "/home/hedy/.linuxbrew/share/man" $MANPATH;
set -q INFOPATH; or set INFOPATH ''; set -gx INFOPATH "/home/hedy/.linuxbrew/share/info" $INFOPATH;
