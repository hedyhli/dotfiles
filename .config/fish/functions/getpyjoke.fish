# Defined in - @ line 1
function getpyjoke --wraps=python3\ -c\ \'import\ pyjokes\;\ print\(pyjokes.get_joke\(\)\)\' --description alias\ getpyjoke=python3\ -c\ \'import\ pyjokes\;\ print\(pyjokes.get_joke\(\)\)\'
  python3 -c 'import pyjokes; print(pyjokes.get_joke())' $argv;
end
