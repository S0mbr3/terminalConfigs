local net_widgets = require("net_widgets")
local M = {}
M.sys = function()
net_wired = net_widgets.indicator({
    interfaces  = {"enp6s0"},
    timeout     = 5
})
net_internet = net_widgets.internet({indent = 0, timeout = 5})

end

return M
