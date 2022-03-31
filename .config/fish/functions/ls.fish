function ls
	set lspath (which ls)
    if [ (uname) = "NetBSD" ]
        command $lspath $argv
        return
    end
	command $lspath $__fish_ls_color_opt $argv
end
