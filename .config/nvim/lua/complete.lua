-- https://github.com/hrsh7th/nvim-cmp
local cmp = require'cmp'

cmp.setup({
    snippet = {
        expand = function(args)
            require('snippy').expand_snippet(args.body)
        end,
    },
    window = {
        -- completion = cmp.config.window.bordered(),
        -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-u>'] = cmp.mapping.scroll_docs(-4),
        ['<C-d>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        -- Accept currently selected item. Set `select` to `false` to only
        -- confirm explicitly selected items.
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
    }),
    sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'snippy' },
    }, {
        -- { name = 'buffer' },
        { name = 'calc' },
        { name = 'emoji' },
        { name = "latex_symbols" },
        { name = 'path' },
    }),
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
        { name = 'cmp_git' },
    }, {
        { name = 'buffer' },
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
        { name = 'buffer' }
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
