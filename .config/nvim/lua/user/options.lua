local options = {
	number = true,
	cursorline = true,
	termguicolors = true
}

for k, v in pairs(options) do
	vim.opt[k] = v
end
