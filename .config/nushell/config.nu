# Nu #############################################
$env.config = {
    show_banner: false
    edit_mode: vi
    cursor_shape: {
        # block, underscore, line, blink_block, blink_underscore, blink_line, inherit
        emacs: blink_line
        vi_insert: blink_line
        vi_normal: block
    }
    # color_config: {
    #     shape_filepath: cyan_underline
    # }
}

# Basics #########################################
# Nu does not support sourcing inside blocks without creating closures.
# https://github.com/nushell/nushell/issues/8668
#
# ~/.aliases.nu does not exist, I'll just have to remember to `dot gen nu`
source ~/.aliases.nu

# Functions ######################################
# Change to a project directory
def --env pj [
    path: string  # /project-dir/<project/path/here>
]: nothing -> nothing {
    if $env.PROJECTS_ROOT == null { ~/projects/ } else { $env.PROJECTS_ROOT } | cd $"($in)/($path)"
}

