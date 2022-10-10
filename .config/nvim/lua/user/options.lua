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

vim.cmd([[
  if system('uname -r') =~ "microsoft"
		  augroup Yank
		  autocmd!
		  autocmd TextYankPost * :call system('/mnt/c/windows/system32/clip.exe ',@")
		  augroup END
  endif
]])
