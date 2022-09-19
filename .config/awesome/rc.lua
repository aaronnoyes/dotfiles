-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")
local awful = require("awful")
local gears = require("gears")

terminal = "kitty"
modkey = "Mod4"

default_layouts = {
	awful.layout.suit.max,
	awful.layout.suit.spiral,
}

require("awful.autofocus")
require("errors")
require("screen")
require("keys")
require("rules")
require("client")

awesome.spawn("picom")
