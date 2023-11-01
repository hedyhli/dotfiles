vim.api.nvim_create_autocmd(
   { "FileType" },
   { pattern = "CompetiTest",
     command = "setl signcolumn=no" }
)

function CPConfig()
  -- Allows opening of terminal in directory of current file (to avoid
  -- submitting code to the wrong problem!)
  -- This has since been moved to global config
  -- vim.o.autochdir = true
  -- <C-R> prefix is also used by fugitive...
  local function d(s) return { desc = s } end
  local function map(...) vim.keymap.set(...) end
  map('c', "<C-R><C-R>", "CompetiTest run<cr>", d"CompetiTest run")
  map('c', "<C-R>c", "CompetiTest receive contest<cr>", d"CompetiTest receive contest")
end

vim.api.nvim_create_autocmd(
   { "BufRead" },
   { pattern = "*/projects/cp/*",
     callback = CPConfig }
)

local function get_pieces(task)
  local judge, contest, problem, problem_char, div, round
  round = "" div = "" problem = "" problem_char = ""
  local hyphen = string.find(task.group, " - ", 1, true)
  if not hyphen then
    judge = task.group
    contest = "contest"
  else
    judge = string.sub(task.group, 1, hyphen - 1)
    if judge == "Codeforces" then
      judge = "cf"
    else
      judge = judge:lower()
    end
    -- Educational Codeforces Round 000 Div. 3
    contest = string.sub(task.group, hyphen + 3)
    div = contest:match("Div. (%d+)")  -- 3
    round = contest:match("Round (%d+)")  -- 000
    local edu = contest:match("Educational")
    if edu ~= nil then
      round = "edu"..round  -- edu000
    end
    div = "div"..div  -- div3
  end
  problem = task.name  -- A. Name of Task
  problem_char = problem:match(".") -- A
  -- A_name-of-task
  problem = problem_char .. "_" .. problem:match(" (.+)$"):gsub("%s", "-"):lower()

  return {
    round = round,   -- 000
    div = div,       -- div3
    problem = problem,
    problem_char = problem_char,
    judge = judge,
  }
end

require('competitest').setup {
  local_config_file_name = ".competitest.lua",

  floating_border = "rounded",
  floating_border_highlight = "FloatBorder",
  picker_ui = {
    width = 0.2,
    height = 0.3,
    mappings = {
      focus_next = { "j", "<down>", "<Tab>" },
      focus_prev = { "k", "<up>", "<S-Tab>" },
      close = { "<esc>", "<C-c>", "q", "Q" },
      submit = { "<cr>" },
    },
  },
  editor_ui = {
    popup_width = 0.4,
    popup_height = 0.6,
    show_nu = true,
    show_rnu = false,
    normal_mode_mappings = {
      switch_window = { "<C-h>", "<C-l>", "<C-i>" },
      save_and_close = "<C-s>",
      cancel = { "q", "Q" },
    },
    insert_mode_mappings = {
      switch_window = { "<C-h>", "<C-l>", "<C-i>" },
      save_and_close = "<C-s>",
      cancel = "<C-q>",
    },
  },
  runner_ui = {
    interface = "split",
    selector_show_nu = false,
    selector_show_rnu = false,
    show_nu = false,
    show_rnu = false,
    mappings = {
      run_again = "R",
      run_all_again = "<C-r>",
      kill = "K",
      kill_all = "<C-k>",
      view_input = { "i", "I" },
      view_output = { "a", "A" },
      view_stdout = { "o", "O" },
      view_stderr = { "e", "E" },
      toggle_diff = { "d", "D" },
      close = { "q", "Q" },
    },
    viewer = {
      width = 0.5,
      height = 0.5,
      show_nu = true,
      show_rnu = false,
      close_mappings = { "q", "Q" },
    },
  },
  popup_ui = {
    total_width = 0.8,
    total_height = 0.8,
    layout = {
      { 4, "tc" },
      { 5, { { 1, "so" }, { 1, "si" } } },
      { 5, { { 1, "eo" }, { 1, "se" } } },
    },
  },
  split_ui = {
    position = "right",
    relative_to_editor = true,
    total_width = 0.3,
    vertical_layout = {
      { 1, "tc" },
      { 1, { { 1, "so" }, { 1, "eo" } } },
      { 1, { { 1, "si" }, { 1, "se" } } },
    },
    total_height = 0.4,
    horizontal_layout = {
      { 2, "tc" },
      { 3, { { 1, "so" }, { 1, "si" } } },
      { 3, { { 1, "eo" }, { 1, "se" } } },
    },
  },

  save_current_file = true,
  save_all_files = false,
  compile_directory = ".",
  compile_command = {
    c = { exec = "gcc", args = { "-Wall", "$(FNAME)", "-o", "$(FNOEXT).out" } },
    -- cpp = { exec = "clang++", args = { "-Wall", "$(FNAME)", "-o", "$(FNOEXT).out", "--include-directory=/opt/homebrew/Cellar/gcc/13.2.0/include/c++/13/aarch64-apple-darwin22" } },
    cpp = { exec = "g++", args = { "-Wall", "$(FNAME)", "-o", "$(FNOEXT).out" } },
    rust = { exec = "rustc", args = { "$(FNAME)" } },
    java = { exec = "javac", args = { "$(FNAME)" } },
  },
  running_directory = ".",
  run_command = {
    c = { exec = "./$(FNOEXT).out" },
    cpp = { exec = "./$(FNOEXT).out" },
    rust = { exec = "./$(FNOEXT)" },
    python = { exec = "python", args = { "$(FNAME)" } },
    java = { exec = "java", args = { "$(FNOEXT)" } },
  },
  multiple_testing = -1,
  maximum_time = 5000,
  output_compare_method = "squish",
  view_output_diff = false,

  testcases_directory = ".",
  testcases_use_single_file = false,
  testcases_auto_detect_storage = true,
  testcases_single_file_format = "$(FNOEXT)_testcases.txt",
  testcases_input_file_format = "input$(TCNUM).txt",
  testcases_output_file_format = "output$(TCNUM).txt",

  companion_port = 27121,
  receive_print_message = true,
  template_file = "~/projects/cp/t.cpp",
  evaluate_template_modifiers = true,
  -- Group = "Codeforces - Educational Codeforces Round 156 (Rated for Div. 2)"
  -- Judge = "Codeforces"
  -- Contest = after '-'
  -- JAVA_TASK_CLASS = class friendly problem name
  date_format = "%Y-%m-%d %H:%M",
  received_files_extension = "cpp",
  -- ~/projects/cp/contest/div0/[edu]000/A_problem-name/a.cpp
  received_problems_path = function(task, fext)
    local info = get_pieces(task)
    return vim.loop.os_homedir()..string.format("/projects/cp/%s/%s/%s/%s/%s.%s",
    info.judge, info.div, info.round, info.problem, info.problem_char:lower(), fext)
  end,
  received_problems_prompt_path = true,
  -- ~/projects/cp/contest/div0/[edu]000/
  received_contests_directory = function (task, fext)
    local info = get_pieces(task)
    return vim.loop.os_homedir()..string.format("/projects/cp/%s/%s/%s/",
    info.judge, info.div, info.round)
  end,
  -- A_problem-name/a.cpp
  received_contests_problems_path = function (task, fext)
    local info = get_pieces(task)
    return string.format("%s/%s.%s",
    info.problem, info.problem_char:lower(), fext)
  end,
  received_contests_prompt_directory = true,
  received_contests_prompt_extension = true,
  open_received_problems = true,
  open_received_contests = true,
  replace_received_testcases = false,
}
