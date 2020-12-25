# Defined in - @ line 1
function codespell --wraps='codespell --skip="venv,.git,*.pyc,*.png,*.PNG,*.gif"' --wraps='codespell --skip="venv,.git,*.pyc,*.png,*.PNG,*.gif",.vim' --description 'alias codespell=codespell --skip="venv,.git,*.pyc,*.png,*.PNG,*.gif",.vim'
 command codespell --skip="venv,.git,*.pyc,*.png,*.PNG,*.gif",.vim $argv;
end
