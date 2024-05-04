function condasetup
    if test -f /Users/hedy/miniconda3/bin/conda
        eval /Users/hedy/miniconda3/bin/conda "shell.fish" "hook" $argv | source
    end
end
