local function config()
-- IMPORTANT: make sure to setup neodev BEFORE lspconfig
require("neodev").setup({
  setup_jsonls = false,
})
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
  -- Let's not override C-k for window nav thanks
  buf_set_keymap('n', '<C-S-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<localleader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<localleader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<localleader>e', '<cmd>lua vim.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap("n", "g@", "<cmd>lua vim.lsp.buf.format{async=true}<CR>", opts)
  --buf_set_keymap('n', '<localleader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

local handlers =  {
  ["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, {border = 'rounded'}),
  ["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, {border = 'rounded' }),
}

local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
-- Call setup on list of wanted servers and map local keybinds
local servers = {
  "bashls",  -- node bash-language-server
  "vimls",   -- node vim-language-server
  "marksman", -- https://github.com/artempyanykh/marksman/releases
  -- "ccls", -- https://github.com/MaskRay/ccls/wiki
  "nim_langserver", -- nimble install
  "rust_analyzer",
  "zls", -- https://github.com/zigtools/zls/wiki/Installation
  "hls", -- brew
  "v_analyzer", -- https://github.com/vlang/v-analyzer
}
for _, lsp in pairs(servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    handlers = handlers,
    flags = {
      debounce_text_changes = 150,
    }
  }
end
-- comes with deno
lspconfig.denols.setup {
  on_attach = on_attach,
  root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
  capabilities = capabilities,
  handlers = handlers,
  flags = {
    debounce_text_changes = 150,
  }
}
-- node typescript-language-server
lspconfig.tsserver.setup {
  on_attach = on_attach,
  root_dir = lspconfig.util.root_pattern("package.json"),
  single_file_support = false,
  capabilities = capabilities,
  handlers = handlers,
  flags = {
    debounce_text_changes = 150,
  }
}
-- pipx install python-lsp-server
-- ~/.local/pipx/venvs/python-lsp-server/bin/python3 -m pip install python-lsp-black pylsp-mypy python-lsp-ruff
-- Add additional symlinks!
lspconfig.pylsp.setup{
  on_attach = on_attach,
  capabilities = capabilities,
  handlers = handlers,
  settings = {
    pylsp = {
      plugins = {
        pylint = { enabled = false },
        black = { enabled = true },
        pylsp_mypy = {
          enabled = false,
          live_mode = false,
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
  handlers = handlers,
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
    -- hoverKind = "SynopsisDocumentation",
  }
}
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#lua_ls
lspconfig.lua_ls.setup {
  on_attach = on_attach,
  capabilities = capabilities,
  handlers = handlers,
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
      completion = {
        callSnippets = "Replace"
      },
    },
  },
}
end

return {
  { "neovim/nvim-lspconfig",
    config = config,
    ft = {"python", "go", "markdown", "lua", "vim", "bash", "javascript", 'javascriptreact', 'sh', "typescript", "typescriptreact", "nim", "rust", "zig", "haskell", "v"},
  },
  { "ray-x/lsp_signature.nvim",
    event = "VeryLazy",
    opts = {
      doc_lines = 10,
      -- set to 0 if you DO NOT want any API comments be shown
      -- This setting only take effect in insert mode, it does not affect signature help in normal
      -- mode, 10 by default
      max_height = 12,
      max_width = 80,
      noice = false,
      wrap = true,
      floating_window = false,
      floating_window_above_cur_line = true,
      floating_window_off_x = 1,
      floating_window_off_y = 0, -- -2 move window up 2 lines; 2 move down 2 lines
      -- can be either number or function, see examples
      close_timeout = 4000,
      fix_pos = false,  -- don't auto-close the floating window all parameters finished
      hint_enable = true, -- virtual hint
      hint_prefix = " ",
      hint_scheme = "String",
      hint_inline = function() return vim.fn.has('nvim-0.10') == 1 end,
      hi_parameter = "LspSignatureActiveParameter",
      handler_opts = { border = "rounded" },
      always_trigger = false,
      auto_close_after = nil,
      extra_trigger_chars = {","},
      zindex = 200,
      padding = '',
      transparency = nil, -- 1~100
      timer_interval = 200, -- lower to reduce latency
      toggle_key = '<M-s>', -- toggle floating window key (must set below to true)
      toggle_key_flip_floatwin_setting = true,
      select_signature_key = '<M-n>', -- next signature for (eg) overloads
    },
  },
  { "onsails/lspkind.nvim" },
  { "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    config = function()
      require("lsp_lines").setup()

      vim.diagnostic.config({ virtual_lines = false })
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern = "*",
        callback = function()
          vim.b.lsp_lines_enabled = false
        end
      })

      vim.keymap.set("n", "<Leader>ll", function()
        require("lsp_lines").toggle()
        -- Disable virtual_text since it's redundant due to lsp_lines.
        if vim.b.lsp_lines_enabled then
          -- IT was enabled, now it's disabled.
          vim.diagnostic.config({ virtual_text = true })
          vim.b.lsp_lines_enabled = false
        else
          vim.diagnostic.config({ virtual_text = false })
          vim.b.lsp_lines_enabled = true
        end
      end , {desc = "Toggle Lsp Lines"})
    end,
  },
}
