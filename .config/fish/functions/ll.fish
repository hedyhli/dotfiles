function ll
	if which exa > /dev/null
		exa -lahg --git -t modified $argv
	else
		ls -Al $argv
	end
end
