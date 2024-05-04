# Change to a project directory
def --env pj [
    path: string  # /project-dir/<project/path/here>
]: nothing -> nothing {
    if $env.PROJECTS_ROOT == null { ~/projects/ } else { $env.PROJECTS_ROOT } | cd $"($in)/($path)"
}
