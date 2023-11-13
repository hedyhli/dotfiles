require('outline').setup {
  preview_window = {
    border = 'rounded',
    open_hover_on_preview = false,
  },
  symbol_folding = {
    autofold_depth = 1,
    auto_unfold_hover = true,
  },
  guides = {
    enabled = true,
  },
  outline_window = {
    show_cursorline = true,
    hide_cursor = true,
    winhl = "OutlineDetails:LineNr,OutlineLineno:LineNr,OutlineGuides:Comment",
  },
  keymaps = {
    close = "q",
  },
  symbols = {
    icon_source = "lspkind",
  },
}
