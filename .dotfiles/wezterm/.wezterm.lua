local wezterm = require 'wezterm'
local act = wezterm.action
local layouts = 'right'
--local l = require('layouts')

function getCommand(commandVar, ifnil)
      local handle = io.popen(commandVar)
      local output = nil
      if handle  then
      output = handle:read("*a")
      handle:close()
    else
      output = ifnil
    end
    return output
end


  local split = function(position)
    local _,_,_ = wezterm.run_child_process {'wezterm', 'cli', 'split-pane', position}
  end

local right = function(pane)
  if #pane:tab():panes() <= 1 then
    --pane:split {direction = 'Right'}
    split('--right')
  else
    --pane:split {direction = 'Bottom'}
    split('--bottom')
  end
end

local bottom = function (pane)
  if #pane:tab():panes() <= 1 then
    split('--bottom')
    --pane:split {direction = 'Bottom'}
  else
    split('--right')
    --pane:split {direction = 'Right'}
  end
end

wezterm.on ("flayouts",
function(window, pane)
  if layouts == 'right' then
    layouts = 'top'
  elseif layouts == 'top' then
    layouts = 'right'
    end
  --act.MoveTabRelative()
  window:perform_action(wezterm.action.MoveTabRelative(-1), pane)
  --local _,tabs,_ = wezterm.run_child_process {"jq", "'.[]", "|", "select(.window_id==0)", "|", ".pane_id'"}
  print(pane:tab():tab_id())
  local pane_ids = {}
  print ( pane:tab():panes() )
  local panes = pane:tab():panes()
  for _, v in ipairs(panes) do
  local v_string = tostring(v)
    for pane_id in string.gmatch(v_string, "pane_id:(%d+)") do
        table.insert(pane_ids, pane_id)
    end
  end
  print(pane_ids)
  --local _,tabs,_ = wezterm.run_child_process {'/bin/bash $HOME/wez.sh', pane:tab():tab_id()}
  --local tabs = getCommand('/bin/bash $HOME/wez.sh', "deso")
  --print("tabs lists: " ..tabs)
  end);

wezterm.on("make-layouts",
function(window, pane)
  --local panes = pane:tab():panes()
  print("layouts tests")
  print(#pane:tab():panes())
  print(pane:tab():panes())
  print(pane:tab():panes_with_info())
  if layouts == "right" then
    right(pane)
  elseif layouts == "top" then
    bottom(pane)
  end
end);

wezterm.on("update-status", function(window, pane)
      -- demonstrates shelling out to get some external status.
      -- wezterm will parse escape sequences output by the
      -- child process and include them in the status area, too.
      --local success, stdout, stderr = wezterm.run_child_process { 'jq', '-r', '.name', '/home/oxhart/scripts/fete/day.json' }
      --local success, user, stderr = wezterm.run_child_process {'echo',os.getenv('USER')}
      -- user = string.gsub(user, "\n", "")
      -- print(string.format("%s %s %s", stdout, success, stderr))
      local command= "jq -r '.name' $HOME/scripts/fete/day.json"
      --local output = getCommand(command, "dulamour")
      --local  success1, uname1, stderr = wezterm.run_child_process { 'uname', '-a', '|', 'cut', '-d', "' '", '-f2,3' }
      --print("the uname1 is: "..uname1.. "and success is: "..tostring(success1))
      local command1 = "uname -a | cut -d' ' -f2,3"
      --local command2= "echo $USER"
      local uname = getCommand(command1, "Oxidow")
      uname = string.gsub(uname, "\n", "")
      --local user = "oxhart"
      local fete = getCommand(command, "lamour")
      fete = string.gsub(fete, "\n", "")

      local success_date, date, stderr_date = wezterm.run_child_process({"date"});
      if success_date then
       date = wezterm.strftime("%A %H:%M:%S %d-%m-%Y");
      else
        print(stderr_date)
      end

     -- However, if all you need is to format the date/time, then:

      -- Make it italic and underlined
      window:set_right_status(wezterm.format({
        {Attribute={Underline="Curly"}},
        {Attribute={Italic=true}},
        {Text = layouts .." " ..os.getenv('USER').. " " ..uname.. " "},
        {Foreground = {AnsiColor = 'Purple' }},
        {Text = date.. " "},
        {Foreground = {AnsiColor = 'Lime' }},
        {Text=string.format("bonne fete aux %s", fete)},
        -- {Text=date},
        -- {Text=user},
        -- {Text=fete},
       --{Text=date.. " bonne fete aux " ..fete.. " ".. user},
    }));
end);
return {
  ssh_domains = {
    {
      name = 'ledeb',
      remote_address = '192.168.0.50:5045',
      username = 'nebj',
    },
    {
      name = 'learc',
      remote_address = '82.66.43.46:5046',
      username = 'oxhart',
    },
  },
  --color_scheme = "tokyonight-storm",
  color_scheme = "ChallengerDeep",
  window_background_opacity = 0.8,
  font_size=12,
  tab_bar_at_bottom = true,
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  --leader = { key = 'a', mods = 'CTRL', timeout_milliseconds=1000},
  keys = {
    { key = 'k', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Prev' },
    { key = 'j', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection 'Next' },
    { key = '[', mods = 'CTRL|SUPER', action = act.ActivatePaneDirection 'Prev' },
    { key = ']', mods = 'CTRL|SUPER', action = act.ActivatePaneDirection 'Next' },
    { key = 'w', mods = 'SHIFT|CTRL', action = act.CloseCurrentPane {confirm = true }},
    { key = 'Enter', mods = 'CTRL|SHIFT', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
    { key = '|', mods = 'CTRL|SHIFT', action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },
    { key = 'DownArrow', mods = 'SHIFT', action = act.ScrollToPrompt(1) },
    { key = 'UpArrow', mods = 'SHIFT', action = act.ScrollToPrompt(-1) },
    --{ key = 'm', mods = 'SHIFT|CTRL', action =  l.layout()},
    { key = 'm', mods = 'SHIFT|CTRL', action =  wezterm.action {EmitEvent = "make-layouts"}},
    { key = 'b', mods = 'SHIFT|CTRL', action =  wezterm.action {EmitEvent = "flayouts"}},
  },
}

