local Runners = { }

-- Check if the extension is "txt"
function Runners:compile_c()
  local filename = vim.fn.expand('%:t')  -- Get the name of the file in the current buffer
  local extension = vim.fn.fnamemodify(filename, ':e')  -- Extract the extension

  if extension == "c" or extension == "cpp" then
    print("This is a c file.")
    vim.api.nvim_command('w')
    vim.api.nvim_command("make -k && time ./%:p:h:t")
  else
    print("This is not a c file.")
  end
end

return Runners
