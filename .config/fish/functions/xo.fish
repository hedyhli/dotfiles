# Defined in - @ line 1
function xo 
  if test -e ~/.git_creds && read_confirm 'gitlock?'
	  gitlock
  end
  exit 0 $argv;
end
