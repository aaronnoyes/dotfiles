-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

terminal = "kitty"
modkey = "Mod4"

require("awful.autofocus")
require("errors")
require("screen")
require("keys")
require("rules")
require("client")
