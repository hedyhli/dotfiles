set -gx fish_cursor_default line

source ~/.aliases

# Environment variables
# ~/.exportenvs.fish is generated by ~/dotscripts/gen/fish-exportenvs
if test -f ~/.exportenvs.fish
    source ~/.exportenvs.fish
end

if test -f ~/.config/fish/config_local.fish
	source ~/.config/fish/config_local.fish
end

abbr -a --set-cursor pj cd $PROJECTS_ROOT/%
abbr -a --set-cursor ccd ranger --choosedir=/tmp/rangerdir % \&\& cd \(cat /tmp/rangerdir\)
abbr -a --set-cursor cdd ranger --choosedir=/tmp/rangerdir % \&\& cd \(cat /tmp/rangerdir\)

# TODO: migrate .addpath and .exportenvs to direnv :D
if command -sq direnv > /dev/null
    direnv hook fish | source
end

if command -sq pyenv > /dev/null
    status is-login; and pyenv init --path | source
    status is-interactive; and pyenv init - | source
end

function ls_on_cd --on-variable PWD;
    ls --color=auto
end
