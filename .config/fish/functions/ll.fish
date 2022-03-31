function ll
    if command -sq exa
        exa -lahg --git -t modified $argv
    else
        ls -Al $argv
    end
end
