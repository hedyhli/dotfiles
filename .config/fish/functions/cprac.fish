function cprac
	if test -e /mnt/c/Users/hedyh/codeprac/
		set -gx cprac_path /mnt/c/Users/hedyh/codeprac/
	else
		echo "codeprac path not found, please enter path: "
		read -gx cprac_path
	end
	if string length -q -- $argv
		if test -e $cprac_path$argv[1]
			cd  $cprac_path$argv[1]
		else
			cd $cprac_path/*/$argv[1]/
		end
	else
		cd $cprac_path
	end
end

