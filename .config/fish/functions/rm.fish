function rm --wraps=trash --description 'alias rm=trash'
  if command -sq trash
	  trash $argv;
  else
	  /usr/bin/rm $argv;
  end
end
