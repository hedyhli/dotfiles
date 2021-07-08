function ls
	set lspath (which ls)
	command $lspath $__fish_ls_color_opt $argv
end
