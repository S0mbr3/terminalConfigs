local awful = require("awful")
local M = {}

local statusbar_visibility = {}

function updateStatusBarVisibility()
  --local tag = awful.tag.selected() DEPRECATED
  local tag = awful.screen.focused().selected_tag
  if not tag then return end
  awful.screen.connect_for_each_screen(function(s)
    if s.mywibox then s.mywibox.visible = not statusbar_visibility[tag]end
  end)
end

M.toggleStatusBarVisibility = function()
  --local tag = awful.tag.selected() DEPRECATED
  local tag = awful.screen.focused().selected_tag
  if not tag then return end
  statusbar_visibility[tag] = not statusbar_visibility[tag]
  updateStatusBarVisibility()
end
tag.connect_signal("property::selected", updateStatusBarVisibility)
return M
