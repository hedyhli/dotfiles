function cat --description 'alias cat to ccat'
    # if command -sq bat
    #     bat --theme Dracula $argv;
    # else if command -sq ccat
    #     ccat $argv;
    if command -sq ccat
        ccat $argv;
    else
        set catpath (which cat)
        $catpath $argv;
    end
end
