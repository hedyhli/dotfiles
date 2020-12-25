function rm --wraps=trash --description 'alias rm=trash'
  if which trash
	  trash  $argv;
  else
	  /usr/bin/rm $argv;
  end
end
