function ranger --description 'alias ranger=TERM=xterm-kitty env ranger'
    set path (which ranger)
    if test -z "$TMUX"
        TERM=xterm-kitty $path $argv
    else
        $path $argv
    end
end
