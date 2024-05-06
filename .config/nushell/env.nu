# Nu #############################################
# Prompt with shortened path and exit code
def create_left_prompt [] {
    let path_color      = (if (is-admin) { ansi red } else { ansi green })
    let last_path_color = (if (is-admin) { ansi red_bold } else { ansi blue })
    let separator_color = (ansi white)
    let path_segment = do {
        # "~/path/to/dir>" OR " ~>"
        let dirs = (
            if ($env.PWD != $env.HOME) {
                ($env.PWD | str replace $env.HOME '~')
            } else {' ~'}
        )
        # Shorten some parts and apply highlights
        let parts = ($dirs | path split)
        let last = ($parts | length) - 1
        # For each part of the path except last
        $parts | drop 1 | enumerate | each { |p|
            let this = (
                if ($last in 4..) and ($p.index == $last - 1) {
                    $p.item  # ~/1/2/[33]/44, ~/1/2/3/[44]/55
                } else if ($last == 1) {
                    $p.item  # ~/[11]
                } else {
                    $p.item | str substring 0..1
                }
            )
            ([$path_color $this $separator_color (char path_sep)] | str join)
        } | append [$last_path_color ($parts | get $last)] | str join
    }
    let last_exit_code = (
        if ($env.LAST_EXIT_CODE != 0) {
            $"(ansi rb)[($env.LAST_EXIT_CODE)](ansi reset)"
        } else { "" }
    )

    ([$path_segment $last_exit_code (ansi reset)] | str join)
}

$env.PROMPT_COMMAND = { create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = {
    $"(ansi xterm_grey35)($env.USER)@(hostname) (^date '+%H:%M:%S')"
}

$env.PROMPT_INDICATOR = {|| $"(ansi reset)> " }
$env.PROMPT_INDICATOR_VI_INSERT = $env.PROMPT_INDICATOR
$env.PROMPT_INDICATOR_VI_NORMAL = {|| $"(ansi yellow_reverse)>(ansi reset) " }
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
