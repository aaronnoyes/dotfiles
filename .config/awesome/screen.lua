local gears = require("gears")
local beautiful = require("beautiful")
local awful = require("awful")
local wibox = require("wibox")
local naughty = require("naughty")

beautiful.init(gears.filesystem.get_dir("config") .. "/themes/default/theme.lua")

-- local battery = require("widgets.battery")
local wifi = require("widgets.wifi")
local battery_widget = require("awesome-wm-widgets.battery-widget.battery")

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
mytextclock = awful.widget.textclock(" %b %d %l:%M%P", 15)


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

	local function update_taglist(widget, tag, _, _)
		if tag == s.selected_tag then
			-- color for currently active tag
			widget:get_children_by_id("circle_tag")[1].bg = "#e1e1e1"
		else
			-- this can also be s.clients if you only want visible client included
			for _, c in ipairs(s.all_clients) do
				for _, t in ipairs(c:tags()) do
					if tag == t then
						-- color for other tags with at least one client
						widget:get_children_by_id("circle_tag")[1].bg = "#919191"
						return
					end
				end
			end
			-- color for other tags with no clients
			widget:get_children_by_id("circle_tag")[1].bg = "#303030"
		end
	end

	s.mytaglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = taglist_buttons,
		layout = wibox.layout.fixed.horizontal,
		widget_template = {
			{
				id = "circle_tag",
				forced_width = 10,
				shape = gears.shape.circle,
				widget = wibox.container.background,
			},
			left = 6,
			right = 6,
			widget = wibox.container.margin,
			create_callback = update_taglist,
			update_callback = update_taglist,
		},
	})

	s.mytasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = tasklist_buttons,
		layout = {
			spacing_widget = {
				{
					forced_width = 5,
					forced_height = 24,
					thickness = 0,
					widget = wibox.widget.separator,
				},
				valign = "center",
				halign = "center",
				widget = wibox.container.place,
			},
			spacing = 5,
			layout = wibox.layout.fixed.horizontal,
		},
		widget_template = {
				{
					{
						{
							id     = 'text',
							widget = wibox.widget.textbox,
						},
						layout = wibox.layout.fixed.horizontal,
					},
					left  = 10,
					right = 10,
					widget = wibox.container.margin
				},
				id     = 'background_role',
				widget = wibox.container.background,
				create_callback = function(self, c, index, objects)
						if client.focus == c then
								self:get_children_by_id('text')[1].markup = '<span foreground="black">' .. c.class .. '</span>'
						else
								self:get_children_by_id('text')[1].markup = '<span foreground="white">' .. c.class .. '</span>'
						end
				end,
				update_callback = function(self, c, index, objects)
						if client.focus == c then
								self:get_children_by_id('text')[1].markup = '<span foreground="black">' .. c.class .. '</span>'
						else
								self:get_children_by_id('text')[1].markup = '<span foreground="white">' .. c.class .. '</span>'
						end
				end
		},
	})
	-- Create the wibox
	s.mywibox = awful.wibar({
		position = "top",
		screen = s,
		height = 40,
		margins = {
			top = 10,
			right = 20,
			bottom = 0,
			left = 20,
		},
	})

	-- Add widgets to the wibox
	s.mywibox:setup({
		{
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
				volume_widget {
						widget_type = "icon"
				},
				battery_widget,
				wifi,
				wibox.widget.systray(),
				mytextclock,
				s.mylayoutbox,
				spacing = 12,
			},
		},
		widget = wibox.container.margin,
		margins = 8,
	})
end)
-- }}}
