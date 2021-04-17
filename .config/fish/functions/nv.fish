# Defined via `source`
function --description 'alias nv=nvim'
	if command -sq nvim
		nvim $argv
	else
		vim $argv
	end
end
