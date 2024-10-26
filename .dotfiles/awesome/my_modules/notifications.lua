local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local lgi = require('lgi')
local Gio = lgi.require('Gio')
--local dbus = require("dbus")

local M = {}

local unseen_notifications = {}
M.notification_icon = wibox.widget.textbox()
M.notification_icon.text=""
M.notification_icon.font = "FiraCode Nerd 20"
--🔔
local update_notification_icon = function()
  if #unseen_notifications > 0 then
    M.notification_icon.text = ""  -- This is your notification icon
  else
    M.notification_icon.text = ""  -- This is your empty notification icon
  end
end

local notification_widget = wibox.widget {
  layout = wibox.layout.fixed.vertical,
  -- Add widgets here
}

local populate_notification_widget = function()
  --naughty.notification { title = "Claimed fullscreen ?", message="clicked"}
  notification_widget:reset()
  for _, notif in ipairs(unseen_notifications) do
    notification_widget:add(wibox.widget.textbox(notif.title))
    notification_widget:add(wibox.widget.textbox(notif.text))
    notification_widget:add(wibox.widget.textbox("\n\n"))
  end
end

local show_notification_popup = function()
  populate_notification_widget()
  M.notification_popup.visible = true
end
M.notification_icon:buttons(


  gears.table.join(
    awful.button({}, 1, function()
      if M.notification_popup.visible then
        M.notification_popup.visible = false
      else
        show_notification_popup()
      end
    end)
  )
)

local add_notification = function(title, text)
  table.insert(unseen_notifications, {title=title, text=text})
  update_notification_icon()
end

local mark_notifications_as_seen = function()
  unseen_notifications = {}
  M.notification_popup.visible = false
  update_notification_icon()
end


M.notification_popup = awful.popup {
  widget = notification_widget,
  -- Other properties
}



-- Bind show_notification_popup() and mark_notifications_as_seen() to keys or mouse events.

-- Listen for custom D-Bus signal
awesome.connect_signal("dbus_notification", function(summary, body)
  naughty.notification { title = summary, message = body }
end)

naughty.connect_signal("added", function(notification)
  add_notification(notification.title, notification.text)  -- or M.add_notification(summary, body) if in a module
  print(notification.text)
end)

return M
