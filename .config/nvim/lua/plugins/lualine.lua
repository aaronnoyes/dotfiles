return {
	{
		"nvim-lualine/lualine.nvim",
		opts = function(_, opts)
			opts.sections.lualine_b = {
				{
					"branch",
					fmt = function(str)
						local max = 15
						local len = string.len(str)
						local indicator = ""
						if len >= max then
							indicator = "..."
						end
						return string.sub(str, 1, max) .. indicator
					end,
				},
			}
		end,
	},
}
