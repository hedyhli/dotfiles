-- https://github.com/hrsh7th/nvim-cmp
local cmp = require'cmp'

cmp.setup({
  completion = {
    autocomplete = false, -- trigger with <C-space>
  },
  snippet = {
    expand = function(args)
      require('snippy').expand_snippet(args.body)
    end,
  },
  view = {
    entries = {
      name = "custom",
    },
  },
  window = {
    completion = {
      border = "single",
      winhighlight = 'Normal:Pmenu,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None',
      max_width = 40,
    },
    documentation = {
      border = "single",
      winhighlight = 'Normal:Pmenu,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None',
    },
  },
  formatting = {
    format = function(entry, vim_item)
      if vim.tbl_contains({ 'path' }, entry.source.name) then
        local icon, hl_group = require('nvim-web-devicons').get_icon(entry:get_completion_item().label)
        if icon then
          vim_item.kind = icon
          vim_item.kind_hl_group = hl_group
          return vim_item
        end
      end
      return require('lspkind').cmp_format({
        with_text = false,
        mode = 'symbol',
        maxwidth = 50,
        ellipsis_char = "⋯",
      })(entry, vim_item)
    end
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    ['<C-d>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<C-g>'] = cmp.mapping.abort(),
    -- Accept currently selected item. Set `select` to `false` to only
    -- confirm explicitly selected items.
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'snippy' },
  }, {
    -- { name = 'buffer' },
    { name = 'emoji' },
    { name = "latex_symbols" },
    { name = 'path' },
    { name = 'calc' },
  }),
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'cmp_git' },
  }, {
    { name = 'emoji' },
    { name = "latex_symbols" },
    -- { name = 'buffer' },
    { name = 'path' },
  })
})
cmp.setup.filetype('fish', {
  sources = cmp.config.sources({
    { name = 'cmp_fish' },
  }, {
    -- { name = 'buffer' },
    { name = 'calc' },
    { name = 'emoji' },
    { name = "latex_symbols" },
    { name = 'path' },
  })
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't
-- work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'path' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't
-- work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- autopairs + cmp confirm
local cmp_autopairs = require('nvim-autopairs.completion.cmp')
cmp.event:on(
  'confirm_done',
  cmp_autopairs.on_confirm_done()
)

-- Set the completion item color of unmatched portion to white
-- Somehow it is set to black by default ever since I switched my config to lua.
-- I don't need this anymore when using tundra (theme)
-- vim.api.nvim_set_hl(0, "CmpItemAbbr", { fg=vim.g['dracula#palette.fg'] })

function _G.CmpDisable()
  cmp.setup.buffer { enabled = false }
end
