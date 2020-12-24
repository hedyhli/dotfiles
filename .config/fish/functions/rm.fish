# Defined in - @ line 1
function rm --wraps=trash --description 'alias rm=trash'
  trash  $argv;
end
