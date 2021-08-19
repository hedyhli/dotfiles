local lspconfig = require('lspconfig')

-- from https://github.com/neovim/nvim-lspconfig/wiki/User-contributed-tips#peek-definition

local function preview_location_callback(_, _, result)
	if result == nil or vim.tbl_isempty(result) then
		return nil
	end
	vim.lsp.util.preview_location(result[1])
end

function PeekDefinition()
	local params = vim.lsp.util.make_position_params()
	return vim.lsp.buf_request(0, 'textDocument/definition', params, preview_location_callback)
end


-- from https://github.com/neovim/nvim-lspconfig#Keybindings-and-completion

local on_attach = function(client, bufnr)
	local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
	local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

	--Enable completion triggered by <c-x><c-o>
	buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

	-- Mappings.
	local opts = { noremap=true, silent=true }

	-- See `:help vim.lsp.*` for documentation on any of the below functions
	buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
	buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
	buf_set_keymap('n', 'gp', '<Cmd>lua PeekDefinition()<CR>', opts)
	buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
	-- buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
	buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
	-- buf_set_keymap('n', '<localleader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
	-- buf_set_keymap('n', '<localleader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
	-- buf_set_keymap('n', '<localleader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
	buf_set_keymap('n', '<localleader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
	buf_set_keymap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
	buf_set_keymap('n', '<localleader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
	buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
	buf_set_keymap('n', '<localleader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
	buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
	buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
	buf_set_keymap("n", "g@", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
	--buf_set_keymap('n', '<localleader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

-- Call setup on list of wanted servers and map local keybinds
local servers = { "gopls", "pylsp", "bashls", "vimls" }
for _, lsp in ipairs(servers) do
	lspconfig[lsp].setup {
		on_attach = on_attach,
		flags = {
			debounce_text_changes = 150,
		}
	}
end

