local awful = require("awful")
local wibox = require("wibox")
 local M = { }

 M.systray = function ()

 awful.screen.connect_for_each_screen(function(s)
   -- Create the wibox
   s.mywibox = awful.wibar({ position = "top", screen = s })

   -- Add widgets to the wibox
   s.mywibox:setup {
     layout = wibox.layout.align.horizontal,
     -- left widgets
     {},
     -- Middle Widget
     {},
     { -- Right widgets
     layout = wibox.layout.fixed.horizontal,
     wibox.widget.systray(),
   },
 }
 end)
 end
 return  M


