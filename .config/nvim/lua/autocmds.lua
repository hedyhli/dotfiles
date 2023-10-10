vim.api.nvim_create_autocmd(
   { "BufNewFile" },
   { pattern = "*.*", command = "silent! execute '0r '.$VIMHOME.'/templates/skeleton.'.expand(\"<afile>:e\")" }
)
