bass 'export GEM_HOME=$HOME/gems'
if command -sq links
	bass 'export BROWSER=links'
else
	bass 'export BROWSER=w3m'
end
