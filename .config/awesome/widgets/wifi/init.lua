local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")
local awful = require("awful")

local connected = gears.filesystem.get_dir("config") .. "widgets/wifi/wifi-connected.png"
local notconnected = gears.filesystem.get_dir("config") .. "widgets/wifi/wifi-not-connected.png"

local wifi = wibox.widget({
		image = notconnected,
		resize = true,
		widget = wibox.widget.imagebox
})

gears.timer({
	timeout = 10,
	call_now = true,
	autostart = true,
	callback = function()
		awful.spawn.easy_async_with_shell("nmcli d | grep wifi | awk '{print $3}'", function(out)
				if out == "connected\n" then
					wifi.image = connected
				else
				    wifi.image = notconnected
				end
		end)
	end,
})

return wifi
