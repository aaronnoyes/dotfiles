local gheight = vim.api.nvim_list_uis()[1].height
local gwidth = vim.api.nvim_list_uis()[1].width
local width = 70
local height = 30

require("nvim-tree").setup{
		view = {
				side = "left",
				-- float = {
				--   enable = true,
				--   open_win_config = {
				-- 	relative = "editor",
				-- 	border = "rounded",
				-- 	width = width,
				-- 	height = heigh,
				-- 	row = (gheight - height) * 0.5,
				-- 	col = (gwidth - width) * 0.5,
				--   },
				-- }
        },
		actions = {
            open_file = {
                quit_on_open = true,
            },
        },
        hijack_cursor = true,
}
-- nvim-tree is also there in modified buffers so this function filter it out
local modifiedBufs = function(bufs)
    local t = 0
    for k,v in pairs(bufs) do
        if v.name:match("NvimTree_") == nil then
            t = t + 1
        end
    end
    return t
end

vim.api.nvim_create_autocmd("BufEnter", {
    nested = true,
    callback = function()
        if #vim.api.nvim_list_wins() == 1 and
        vim.api.nvim_buf_get_name(0):match("NvimTree_") ~= nil and
        modifiedBufs(vim.fn.getbufinfo({bufmodified = 1})) == 0 then
            vim.cmd "quit"
        end
    end
})
