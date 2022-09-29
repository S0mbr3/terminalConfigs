--little plugin to help nvim-terminal one to have better workflow automatic insert when toggled automatic escape when toggled back
local M = {}
M.switchTerm = function()
  local mode = vim.api.nvim_get_mode()["mode"]
  if mode == "n" then
    vim.cmd([[lua NTGlobal["terminal"]:toggle() 
    startinsert!]])
  elseif mode == 'i'then
    vim.cmd([[stopinsert
    lua NTGlobal["terminal"]:toggle()]])
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("i", true, false, true), 'x', true)
  elseif mode == 'v' then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<esc>", true, false, true), 'x', true)
    vim.cmd([[
    lua NTGlobal["terminal"]:toggle() 
    startinsert]])
  elseif mode == 'V' then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<esc>", true, false, true), 'x', true)
    vim.cmd([[
    lua NTGlobal["terminal"]:toggle() 
    startinsert]])
  elseif mode == "t" then
    vim.cmd('lua NTGlobal["terminal"]:toggle()')
  elseif mode == "nt" then
    vim.cmd('lua NTGlobal["terminal"]:toggle()')
  end
end

M.defined = false
M.expanded = false
M.screenHeight = nil
M.termHeight = nil
M.size = nil
M.termSizer = function()
  local mode = vim.api.nvim_get_mode()["mode"]
  if mode == "t" or mode == "nt" then
    if M.defined == false then
      M.screenHeight = tonumber(vim.o.lines)
      M.termHeight = tonumber(vim.fn.winheight(0))
      M.defined=true
    end
    if M.expanded == false then
      M.size = math.floor(M.screenHeight / 2) - M.termHeight
      M.expanded = true
    elseif M.expanded == true then
      M.size = -M.size
      M.expanded = false
    end
    if M.size ~= nil then
      vim.api.nvim_command('lua NTGlobal["window"]:change_height(' .. M.size ..')')
    end
  end
end
return M
