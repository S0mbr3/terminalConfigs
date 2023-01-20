local wezterm = require 'wezterm'
local act = wezterm.action

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
        date = wezterm.strftime("%Y-%m-%d %H:%M:%S");
      else
        print(stderr_date)
      end

     -- However, if all you need is to format the date/time, then:

      -- Make it italic and underlined
      window:set_right_status(wezterm.format({
        {Attribute={Underline="Curly"}},
        {Attribute={Italic=true}},
        {Text = os.getenv('USER').. " " ..uname.. " "},
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
  },
  --color_scheme = "tokyonight-storm",
  color_scheme = "ChallengerDeep",
  tab_bar_at_bottom = true,
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  keys = {
    { key = 'UpArrow', mods = 'SHIFT', action = act.ScrollToPrompt(-1) },
    { key = '[', mods = 'CTRL|SUPER', action = act.ActivatePaneDirection 'Prev' },
    { key = ']', mods = 'CTRL|SUPER', action = act.ActivatePaneDirection 'Next' },
    { key = 'UpArrow', mods = 'SHIFT', action = act.ScrollToPrompt(-1) },
    { key = 'w', mods = 'SHIFT|CTRL', action = act.CloseCurrentPane {confirm = true }},
    { key = 'DownArrow', mods = 'SHIFT', action = act.ScrollToPrompt(1) },
    { key = 'Enter', mods = 'CTRL|SHIFT', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
  },
}
