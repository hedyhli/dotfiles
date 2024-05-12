# Nu #############################################
$env.config = {
    show_banner: false
    history: {
        max_size: 100_000
        sync_on_enter: true
        file_format: "sqlite"
        # Use current shell session when up/down'ing history
        isolation: true
    }
    edit_mode: vi
    cursor_shape: {
        emacs: blink_line
        vi_insert: line
        vi_normal: block
    }
    color_config: {
        shape_filepath: cyan_underline
        shape_directory: green_underline
        shape_string: yellow
        shape_globpattern: blue
        shape_flag: green

        shape_externalarg: cyan
        shape_external: blue

        shape_internalcall: light_purple
        shape_keyword: purple_bold
    }
    menus: [
        # Adjust prompt marker and colors for some menus
        {
            name: completion_menu
            only_buffer_difference: false
            marker: "> "
            type: {
                layout: columnar
                columns: 4
                col_padding: 2
            }
            style: {
                text: white
                selected_text: { attr: r }
                description_text: cyan
                match_text: { attr: b }
                selected_match_text: { attr: br }
            }
        }
        {
            name: history_menu
            only_buffer_difference: true
            marker: "> "
            type: {
                layout: list
                page_size: 10
            }
            style: {
                text: cyan
                selected_text: cyan_reverse
                description_text: white
            }
        }
    ]
    keybindings: [
        # Add alt-* bindings, it's easier to reach on some keyboards than ctrl
        {
            name: alt_backspace
            modifier: alt
            keycode: backspace
            mode: [emacs, vi_insert, vi_normal]
            event: { edit: backspaceword }
        }
        {
            name: alt_left
            modifier: alt
            keycode: left
            mode: [emacs, vi_insert, vi_normal]
            event: { edit: movewordleft }
        }
        {
            name: alt_right
            modifier: alt
            keycode: right
            mode: [emacs, vi_insert, vi_normal]
            event: { edit: movewordright }
        }
    ]
}

# Basics #########################################
# Nu does not support sourcing inside blocks without creating closures.
# https://github.com/nushell/nushell/issues/8668
#
# If ~/.aliases.nu does not exist, I'll just have to remember to `dot gen nu`
source ~/.aliases.nu

# Functions ######################################
module functions {
    # Change to a project directory
    export def --env pj [
        path?: string@pj_complete  # /$PROJECTS_ROOT/[project/path/here]
    ]: nothing -> nothing {
        cd $"(pj_root)/($path)"
    }

    def pj_root [] {
        if $env.PROJECTS_ROOT == null {
            ~/projects
        } else {
            $env.PROJECTS_ROOT | str trim --right --char '/'
        }
    }

    def pj_complete [ctx: string] {
        # Nushell doesn't seem to support passing in a parsed arg list instead,
        # but I'll hope args are checked against command signature before
        # calling the completion function.
        mut prefix = ($ctx | str trim | str substring 2.. | split row -r '\s+' | last)
        if ($prefix | str starts-with '/') {
            return []
        }

        if ($prefix != '') and not ($prefix | str ends-with '/') {
            if ($"(pj_root)/($prefix)" | path exists) {
                return [$"($prefix)/"]
            } else {
                $prefix = ($prefix | path dirname)
            }
        }
        let search = $"(pj_root)/($prefix)"
        ls $search | where type == dir | get name | str replace $search $prefix
    }
}

use functions *
