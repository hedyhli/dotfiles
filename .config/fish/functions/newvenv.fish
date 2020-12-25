# Defined in - @ line 1
function newvenv --wraps='python3 -m virtualenv venv' --wraps='python3 -m virtualenv venv && source ./venv/activate.fish' --wraps='python3 -m virtualenv venv && source ./venv/bin/activate.fish' --description 'alias newvenv=python3 -m virtualenv venv && source ./venv/bin/activate.fish'
  python3 -m virtualenv venv && source ./venv/bin/activate.fish $argv;
end
