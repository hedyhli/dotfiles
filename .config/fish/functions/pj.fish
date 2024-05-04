function pj --description "Go to project"
    set root "$PROJECTS_ROOT"
    if test -n root
        set root ~/projects/
    end
    if test $argv[1] = "--help" -o $argv[1] = "-h"
        echo Usage: pj project-name'[/extra/path]'
        echo
        echo Using PROJECTS_ROOT="$root"
        return
    end
    set path $root"$argv"
    if not test -d $path
        echo No such directory: $path
        return 1
    end
    cd $path
end
