local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

-- Create a widget and update its content using the output of a shell
-- command every 10 seconds:
local battery = wibox.widget({
	text = "",
	align = "center",
	valign = "center",
	widget = wibox.widget.textbox,
})


battery:connect_signal("mouse::enter", function(w)
		awful.spawn.easy_async({ "/home/user/.local/bin/batrem" }, function(out)
			battery.text = out
		end)
end)

battery:connect_signal("mouse::leave", function(w)
		awful.spawn.easy_async({ "/home/user/.local/bin/bat" }, function(out)
			battery.text = out
		end)
end)

gears.timer({
	timeout = 10,
	call_now = true,
	autostart = true,
	callback = function()
		awful.spawn.easy_async({ "/home/user/.local/bin/bat" }, function(out)
			battery.text = out
		end)
	end,
})

return battery
