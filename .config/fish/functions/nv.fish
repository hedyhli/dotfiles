# Defined via `source`
function nv --description 'alias nv=nvim'
	if command -sq nvim
		nvim $argv
	else
		vim $argv
	end
end
