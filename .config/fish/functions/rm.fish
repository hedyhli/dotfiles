function rm --wraps=trash --description 'alias rm=trash'
  if which trash > /dev/null
	  trash  $argv;
  else
	  /usr/bin/rm $argv;
  end
end
