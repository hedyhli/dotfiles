function cat --wraps=ccat --description 'alias cat=ccat'
  if command -sq ccat
		ccat $argv;
  else
	  /usr/bin/cat $argv;
  end
end
