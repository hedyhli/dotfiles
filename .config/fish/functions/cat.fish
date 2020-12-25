function cat --wraps=ccat --description 'alias cat=ccat'
  if which ccat > /dev/null
    ccat  $argv;
  else
		cat $argv;
end
