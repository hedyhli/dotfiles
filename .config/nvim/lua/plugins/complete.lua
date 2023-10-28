-- https://github.com/hrsh7th/nvim-cmp
local has_words_before = function()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local snippy = require("snippy")
local cmp = require'cmp'

cmp.setup({
  completion = {
    autocomplete = false, -- trigger with <C-space> or <TAB>
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
      border = "rounded",
      winhighlight = 'Normal:Pmenu,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None',
    },
    documentation = {
      border = "rounded",
      winhighlight = 'Normal:Pmenu,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None',
    },
  },
  formatting = {
    fields = { 'kind', 'abbr', 'menu' },
    format = function(entry, vim_item)
      if vim.tbl_contains({ 'path' }, entry.source.name) then
        local icon, hl_group = require('nvim-web-devicons').get_icon(entry:get_completion_item().label)
        if icon then
          vim_item.kind = icon..' '
          vim_item.kind_hl_group = hl_group
          return vim_item
        end
      end
      vim_item = require('lspkind').cmp_format({
        with_text = false,
        mode = 'symbol',
        maxwidth = 40,
        ellipsis_char = "⋯",
      })(entry, vim_item)
      vim_item.kind = vim_item.kind..' '
      return vim_item
    end
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    ['<C-d>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        -- Complete immediately if there's only one candidate remaining
        if #cmp.get_entries() == 1 then
          cmp.confirm({ select = true })
        else
          cmp.select_next_item()
        end
      elseif snippy.can_expand_or_advance() then
        snippy.expand_or_advance()
      elseif has_words_before() then
        cmp.complete()
        -- Complete immediately if there's only one candidate
        if #cmp.get_entries() == 1 then
          cmp.confirm({ select = true })
        end
      else
        fallback()
      end
    end, { "i", "s" }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif snippy.can_jump(-1) then
        snippy.previous()
      else
        fallback()
      end
    end, { "i", "s" }),
    ['<C-g>'] = cmp.mapping.abort(),
    -- Accept currently selected item. Set `select` to `false` to only
    -- confirm explicitly selected items.
    ['<CR>'] = cmp.mapping({
      i = function(fallback)
        if cmp.visible() and cmp.get_active_entry() then
          cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
        else
          fallback()
        end
      end,
      s = cmp.mapping.confirm({ select = true }),
      -- Super useful for path completion in the command line!
      --    :e ~/partial_path<Tab><CR>
      -- Typing the above would confirm the selected entry and continue to
      -- complete entries from ~/partial_path/*
      c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
     }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'snippy' },
  }, {
    -- { name = 'buffer' },
    { name = 'emoji' },
    { name = "latex_symbols" },
    { name = 'path' },
    -- { name = 'calc' },
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
    -- { name = 'buffer' }
  }
})

-- if you enabled `native_menu` this won't work anymore
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline({
    ['<Tab>'] = {
      c = function(_)
        if cmp.visible() then
          -- Complete immediately if there's only one candidate remaining.
          -- Avoids an extra <CR> confirm trigger
          if #cmp.get_entries() == 1 then
            cmp.confirm({ select = true })
          else
            cmp.select_next_item()
          end
        else
          cmp.complete()
          -- Complete immediately if there's only one candidate
          if #cmp.get_entries() == 1 then
            cmp.confirm({ select = true })
          end
        end
      end,
    }
  }),
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
