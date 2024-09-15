# Based on top of fish's built-in "informative" shell style (see fish_config)
# I actually forgot what it's called exactly
#
# Most is the same. Only thing different is that it:
# - prints leading space because my head is tired from moving to the left in
#   internet articles, terminal programs, and prompt
# - changes the suffix to ">"
# - fixes git clean state char
# - makes base name blue
# - print user@hostname in right prompt (see fish_right_prompt)
function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus

    if not set -q __fish_git_prompt_show_informative_status
        set -g __fish_git_prompt_show_informative_status 1
    end
    if not set -q __fish_git_prompt_hide_untrackedfiles
        set -g __fish_git_prompt_hide_untrackedfiles 1
    end
    set -g __fish_git_prompt_color_branch magenta
    if not set -q __fish_git_prompt_showupstream
        set -g __fish_git_prompt_showupstream "informative"
    end
    if not set -q __fish_git_prompt_char_upstream_ahead
        set -g __fish_git_prompt_char_upstream_ahead "↑"
    end
    if not set -q __fish_git_prompt_char_upstream_behind
        set -g __fish_git_prompt_char_upstream_behind "↓"
    end
    if not set -q __fish_git_prompt_char_upstream_prefix
        set -g __fish_git_prompt_char_upstream_prefix ""
    end
    if not set -q __fish_git_prompt_char_stagedstate
        set -g __fish_git_prompt_char_stagedstate "●"
    end
    if not set -q __fish_git_prompt_char_dirtystate
        set -g __fish_git_prompt_char_dirtystate "✚"
    end
    if not set -q __fish_git_prompt_char_untrackedfiles
        set -g __fish_git_prompt_char_untrackedfiles "…"
    end
    if not set -q __fish_git_prompt_char_invalidstate
        set -g __fish_git_prompt_char_invalidstate "✖"
    end
    if not set -q __fish_git_prompt_char_cleanstate
        # Switch from the check symbol to = because this fixes the width
        # calculation problem where where the last char at the right prompt
        # will be wrapped to the next line.
        # Solution from stack overflow obviously, lost link though
        set -g __fish_git_prompt_char_cleanstate "="
    end
    if not set -q __fish_git_prompt_color_dirtystate
        set -g __fish_git_prompt_color_dirtystate blue
    end
    if not set -q __fish_git_prompt_color_stagedstate
        set -g __fish_git_prompt_color_stagedstate yellow
    end
    if not set -q __fish_git_prompt_color_invalidstate
        set -g __fish_git_prompt_color_invalidstate red
    end
    if not set -q __fish_git_prompt_color_untrackedfiles
        set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
    end
    if not set -q __fish_git_prompt_color_cleanstate
        set -g __fish_git_prompt_color_cleanstate green --bold
    end

    set -l color_cwd
    set -l prefix
    set -l suffix
    switch "$USER"
        case root toor
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
                set color_cwd_base $fish_color_cwd_root
            else
                set color_cwd $fish_color_cwd
                set color_cwd_base blue
            end
            set suffix ' #'
        case '*'
            set color_cwd $fish_color_cwd
            set color_cwd_base blue
            set suffix '>'
    end

    # NOW we print the prompt :p

    # PWD
    set wd (prompt_pwd)
    set_color $color_cwd
    if not [ $wd = '~' ]
        # Make basename blue just because
        echo -n (dirname $wd)'/'
        set_color $color_cwd_base
        echo -n (basename $wd)
    else
        echo -n ' ~'
    end
    set_color normal

    printf '%s' (fish_vcs_prompt)

    set -l pipestatus_string (__fish_print_pipestatus "[" "] " "|" (set_color $fish_color_status) (set_color --bold $fish_color_status) $last_pipestatus)
    echo -n $pipestatus_string
    set_color normal

    echo -n "$suffix "
end
