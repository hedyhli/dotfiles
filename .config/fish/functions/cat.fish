function cat --wraps=ccat --description 'alias cat=ccat'
	if which ccat > /dev/null
		ccat  $argv;
  else
	  /usr/bin/cat $argv;
	end
end
