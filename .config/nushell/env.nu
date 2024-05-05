# Nu #############################################
def create_left_prompt [] {
    let path_color = (if (is-admin) { ansi red } else { ansi green })
    let separator_color = (ansi white)

    let dir = (
        if ($env.PWD != $env.HOME) {
            ($env.PWD | str replace $env.HOME '~')
        } else {' ~'}
    )
    let path_segment = (
        $"($path_color)($dir)" |
        str replace -a (char path_sep) $"($separator_color)(char path_sep)($path_color)"
    )
    let last_exit_code = (
        if ($env.LAST_EXIT_CODE != 0) {
            $"(ansi rb)[($env.LAST_EXIT_CODE)](ansi reset)"
        } else { "" }
    )

    ([($path_segment) $last_exit_code (ansi reset)] | str join)
}

def create_right_prompt [] {
    let time = (date now | format date '%H:%M:%S')
    $"(ansi xterm_grey35) ($env.USER)@(hostname) ($time)"
}

$env.PROMPT_COMMAND = { create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = { create_right_prompt }

$env.PROMPT_INDICATOR = {|| "> " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| "> " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| "= " }
$env.PROMPT_MULTILINE_INDICATOR = {|| "::: " }

# Env ############################################
# TODO: Path and envs, if/when nu is no longer run from fish
# dotscripts/gen/nu
# - Loop through `dotscripts/convert/addpath ' '`
#   use path add from std module to append
# - Loop through ~/.exportenvs
#   set $env.<name>
#
# here
# - source the generated files
