local wezterm = require 'wezterm'
local act = wezterm.action

return {
  color_scheme = "tokyonight-storm",
  tab_bar_at_bottom = true,
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  keys = {
    { key = 'PageUp', mods = 'CTRL|SHIFT', action = act.ScrollToPrompt(-1) },
    { key = 'PageDown', mods = 'CTRL|SHIFT', action = act.ScrollToPrompt(1) },
    { key = 'Enter', mods = 'CTRL|SHIFT', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
  },
}
