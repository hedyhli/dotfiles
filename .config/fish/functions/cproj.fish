function cproj
	if test -e /mnt/c/Users/hedyh/codeproj/
		set -gx cproj_path /mnt/c/Users/hedyh/codeproj/
	else
		echo "codeproj path not found, please enter path"
		read -gx cproj_path
	end
	if string length -q -- $argv
			cd  $cproj_path$argv[1]
	else
		cd $cproj_path
	end
end

