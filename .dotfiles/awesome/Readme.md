# I had to monkey patch the awesome library file client.lua at /usr/share/awesome/lib/awful/client.lua to allow a fake fullscreen mode (usefull to watch yt while coding)
## the monkey patch concern the addition of a rule for firefox, requiring awful.rules using a metatable to prevent circle dependency loop and the modification of the client.tiled function
## for an unknown reason I had to monkey patch also the same way the function screen.object.get_tiled_clients in the screen.lua library file to have the borders active at the master client

### the required awful.rules in firefox_rules

 local firefox_rules
do
    firefox_rules = setmetatable({}, {
        __index = function(_, k)
            firefox_rules = require("awful.rules")
            return firefox_rules[k]
        end,
        __newindex = error -- Just to be sure in case anything ever does this
    })
end
### the rule for firefox in firefox_rule
local firefox_rule = {class= "firefox"}  

### the add of the or in the if condition to allow firefox to be tiled while in fullscreen or maximized mode (/usr/share/awesome/lib/awful/screen.lua)

--- Get visible and tiled clients
--
-- @deprecated awful.client.tiled
-- @see screen.tiled_clients
-- @tparam nil|integer|screen s The screen, or nil for all screens.
-- @tparam[opt=false] boolean stacked Use stacking order? (top to bottom)
-- @treturn table A table with all visible and tiled clients.
function client.tiled(s, stacked)
    local clients = client.visible(s, stacked)
    local tclients = {}
    -- Remove floating clients
    for _, c in pairs(clients) do
        if not client.object.get_floating(c)
            and not c.fullscreen
            and not c.maximized
            and not c.maximized_vertical
            and not c.maximized_horizontal
	    or firefox_rules.match(c, firefox_rule) then
            table.insert(tclients, c)
        end
    end
    return tclients
end


### to have the colored borders around the master client (/usr/share/awesome/lib/awful/screen.lua)
--- Tiled clients for the screen.
-- @property tiled_clients
-- @tparam[opt={}] table tiled_clients The clients list, ordered from top to bottom.
-- @tablerowtype A list of `client` objects.

--- Get tiled clients for the screen.
--
-- This is used by `tiles_clients` internally (with `stacked=true`).
--
  -- @method get_tiled_clients
-- @tparam[opt=true] boolean stacked Use stacking order? (top to bottom)
  -- @treturn table The clients list.
function screen.object.get_tiled_clients(s, stacked)
  local clients = s:get_clients(stacked)
  local tclients = {}
  -- Remove floating clients
  for _, c in pairs(clients) do
    if not c.floating
      and not c.immobilized_horizontal
      and not c.immobilized_vertical 
    or firefox_rules.match(c, firefox_rule) then
    table.insert(tclients, c)
    end
  end
  return tclients
end
