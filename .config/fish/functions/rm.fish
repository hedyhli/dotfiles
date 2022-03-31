function rm --wraps=trash --description 'alias rm=trash'
    if command -sq trash
        trash $argv;
    else
        set rmpath (which rm)
        $rmpath $argv;
    end
end
