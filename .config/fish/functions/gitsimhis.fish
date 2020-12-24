function gitsimhis
    cd $argv[1]
    if read_confirm 'Everything in this directory will be replaced. Do you want to continue? '
        rm -rf *
    else
        echo 'Aborting...'
    return 1
    end
    
    echo '1' > a
    git init && git add . && git commit -m '1'
    echo '2' >> a
    git add . && git commit -m '2'
    echo '3' >> a
    git add . && git commit -m '3'
end

