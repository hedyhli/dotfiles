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
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gp', '<cmd>lua PeekDefinition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  -- buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<localleader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<localleader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  if vim.fn.has('nvim-0.6') == 1 then
    buf_set_keymap('n', '<localleader>e', '<cmd>lua vim.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  else
    buf_set_keymap('n', '<localleader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  end
  buf_set_keymap("n", "g@", "<cmd>lua vim.lsp.buf.format{async=true}<CR>", opts)
  --buf_set_keymap('n', '<localleader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
-- Call setup on list of wanted servers and map local keybinds
local servers = {
  "bashls",  -- sudo npm i -g bash-language-server
  "vimls",   -- sudo npm i -g vim-language-server
  "marksman", -- https://github.com/artempyanykh/marksman/releases
  "ccls", -- https://github.com/MaskRay/ccls/wiki
}
for _, lsp in pairs(servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    }
  }
end
-- pipx install python-lsp-server
-- ~/.local/pipx/venvs/python-lsp-server/bin/python3 -m pip install python-lsp-black pylsp-mypy python-lsp-ruff
-- Add additional symlinks!
lspconfig.pylsp.setup{
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    pylsp = {
      plugins = {
        pylint = { enabled = true },
        black = { enabled = true },
        pylsp_mypy = {
          enabled = true,
          live_mode = true,
        },
        ruff = {
          enabled = true,
          extendSelect = { "I" },
        },
      },
    }
  }
}
-- go install golang.org/x/tools/gopls@latest
-- https://github.com/hrsh7th/nvim-cmp/wiki/Language-Server-Specific-Samples#golang-gopls
lspconfig.gopls.setup{
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    gopls = {
      experimentalPostfixCompletions = true,
      analyses = {
        unusedparams = true,
        shadow = true,
      },
      staticcheck = true,
    },
  },
  init_options = {
    usePlaceholders = true,
  }
}
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#lua_ls
lspconfig.lua_ls.setup {
 settings = {
   Lua = {
     -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
     runtime = { version = 'LuaJIT' },
     -- Get the language server to recognize the `vim` global
     diagnostics = {
       globals = {'vim'},
     },
     -- Make the server aware of Neovim runtime files
     workspace = {
       library = vim.api.nvim_get_runtime_file("", true),
       checkThirdParty = false,
     },
     -- Do not send telemetry data containing a randomized but unique identifier
     telemetry = { enable = false },
   },
 },
}
