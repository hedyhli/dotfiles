# Defined in - @ line 1
function x 
    if test -e ~/.git_creds		
		if read_confirm 'gitlock?'
			gitlock
		end
	end
	exit  $argv;
end
