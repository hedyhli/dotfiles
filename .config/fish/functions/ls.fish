function ls
	set lspath (which ls)
    if [ (uname) = "NetBSD" ]
        set __fish_ls_color_opt ""
    end
	command $lspath $__fish_ls_color_opt $argv
end
