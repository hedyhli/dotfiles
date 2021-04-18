function cat --wraps=ccat --description 'alias cat=ccat'
  if command -sq ccat
		ccat $argv;
  else
	  set catpath (which cat)
	  $catpath $argv;
  end
end
