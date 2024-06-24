local Runners = { }
local switch = function(n)
  local switch_table = {
    c = function()
      -- case 1 actions
      print("This is a c file.")
      vim.api.nvim_command('w')
      vim.api.nvim_command("make -k && time ./%:p:h:t")
    end,
    cpp = function()
      -- case 2 actions
      vim.api.nvim_command('w')
      vim.api.nvim_command("make -k && time ./%:p:h:t")
      print("This is a cpp file.")
    end,
    rs = function()
      -- case 3 actions
      print("this is a rust file.")
      vim.api.nvim_command('w')
      vim.api.nvim_command("RustRun")
    end,
    default = function()
      -- default case actions
      print("No configuration for this type of file")
    end,
  }

  -- Execute the case, if it exists, otherwise execute the default case
  if switch_table[n] then
    switch_table[n]()
  else
    switch_table['default']()
  end
end

function Runners:compile_c()
  local filename = vim.fn.expand('%:t')  -- Get the name of the file in the current buffer
  local extension = vim.fn.fnamemodify(filename, ':e')  -- Extract the extension
  switch(extension)
end

return Runners

-- -- Check if the extension is "txt"
-- function Runners:compile_c1()
--   local filename = vim.fn.expand('%:t')  -- Get the name of the file in the current buffer
--   local extension = vim.fn.fnamemodify(filename, ':e')  -- Extract the extension
--
--   if extension == "c" or extension == "cpp" then
--     print("This is a c file.")
--     vim.api.nvim_command('w')
--     vim.api.nvim_command("make -k && time ./%:p:h:t")
--   elseif extension == "rs" then
--     print("This is a rust file.")
--     vim.api.nvim_command('w')
--     vim.api.nvim_command('RustRun')
--   else
--     print("This is not a c file.")
--   end
-- end

