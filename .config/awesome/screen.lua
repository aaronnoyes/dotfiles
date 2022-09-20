local gears = require("gears")
local beautiful = require("beautiful")
local awful = require("awful")
local wibox = require("wibox")

beautiful.init(gears.filesystem.get_dir("config") .. "/themes/default/theme.lua")

local function set_wallpaper(s)
	-- Wallpaper
	if beautiful.wallpaper then
		local wallpaper = beautiful.wallpaper
		-- If wallpaper is a function, call it with the screen
		if type(wallpaper) == "function" then
			wallpaper = wallpaper(s)
		end
		gears.wallpaper.maximized(wallpaper, s, true)
	end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
--screen.connect_signal("property::geometry", set_wallpaper)
-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = default_layouts
-- }}}

-- {{{ Wibar
-- Create a textclock widget
mytextclock = awful.widget.textclock(" %a %b %d, %l:%M%P", 15)

-- Create a widget and update its content using the output of a shell
-- command every 10 seconds:
local mybatterybar = wibox.widget({
	text = "100%",
	align = "center",
	valign = "center",
	widget = wibox.widget.textbox,
})

gears.timer({
	timeout = 10,
	call_now = true,
	autostart = true,
	callback = function()
		-- You should read it from `/sys/class/power_supply/` (on Linux)
		-- instead of spawning a shell. This is only an example.
		awful.spawn.easy_async({ "sh", "-c", "acpi -b | awk -F ',' '{print $2}'" }, function(out)
			mybatterybar.text = out
		end)
	end,
})

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
	awful.button({}, 1, function(t)
		t:view_only()
	end),
	awful.button({ modkey }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
		end
	end),
	awful.button({}, 3, awful.tag.viewtoggle),
	awful.button({ modkey }, 3, function(t)
		if client.focus then
			client.focus:toggle_tag(t)
		end
	end),
	awful.button({}, 4, function(t)
		awful.tag.viewnext(t.screen)
	end),
	awful.button({}, 5, function(t)
		awful.tag.viewprev(t.screen)
	end)
)

local tasklist_buttons = gears.table.join(
	awful.button({}, 1, function(c)
		if c == client.focus then
			c.minimized = true
		else
			c:emit_signal("request::activate", "tasklist", { raise = true })
		end
	end),
	awful.button({}, 3, function()
		awful.menu.client_list({ theme = { width = 250 } })
	end),
	awful.button({}, 4, function()
		awful.client.focus.byidx(1)
	end),
	awful.button({}, 5, function()
		awful.client.focus.byidx(-1)
	end)
)

awful.screen.connect_for_each_screen(function(s)
	-- Wallpaper
	--set_wallpaper(s)
    gears.wallpaper.set("#666A86")

	-- Each screen has its own tag table.
	awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

	-- Create a promptbox for each screen
	s.mypromptbox = awful.widget.prompt()
	-- Create an imagebox widget which will contain an icon indicating which layout we're using.
	-- We need one layoutbox per screen.
	s.mylayoutbox = awful.widget.layoutbox(s)
	s.mylayoutbox:buttons(gears.table.join(
		awful.button({}, 1, function()
			awful.layout.inc(1)
		end),
		awful.button({}, 3, function()
			awful.layout.inc(-1)
		end),
		awful.button({}, 4, function()
			awful.layout.inc(1)
		end),
		awful.button({}, 5, function()
			awful.layout.inc(-1)
		end)
	))
	-- Create a taglist widget
	s.mytaglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = taglist_buttons,
	})

	s.mytasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = tasklist_buttons,
		widget_template = {
            {
                {
                    id     = 'clienticon',
                    widget = awful.widget.clienticon,
                },
                margins = 4,
                widget  = wibox.container.margin,
            },
            id              = 'background_role',
            widget          = wibox.container.background,
            create_callback = function(self, c, index, objects) --luacheck: no unused
                self:get_children_by_id('clienticon')[1].client = c
            end,
        },
		style = {
				disable_task_name = true,
		}
	})

	-- Create the wibox
	s.mywibox = awful.wibar({
		position = "top",
		screen = s,
		height = 30,
	})

	-- Add widgets to the wibox
	s.mywibox:setup({
		layout = wibox.layout.align.horizontal,
		expand = "none",
		{
			layout = wibox.layout.fixed.horizontal,
			s.mytasklist,
			s.mypromptbox,
		},
		{ -- Left widgets
			layout = wibox.layout.fixed.horizontal,
			--mylauncher,
			s.mytaglist,
		},
		{ -- Right widgets
			layout = wibox.layout.fixed.horizontal,
			mybatterybar,
			wibox.widget.systray(),
			mytextclock,
			s.mylayoutbox,
			spacing = 12
		},
	})
end)
-- }}}
