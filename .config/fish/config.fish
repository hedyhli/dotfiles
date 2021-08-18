source ~/.aliases

# Environment variables
for line in (cat ~/.exportenvs)
	bass $line
end

if test -f ~/.config/fish/config_local.fish
	source ~/.config/fish/config_local.fish
end

