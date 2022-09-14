local options = {
	number = true,
	cursorline = true,
	termguicolors = true,
	tabstop = 4,
	signcolumn = "yes"
}

for k, v in pairs(options) do
	vim.opt[k] = v
end
