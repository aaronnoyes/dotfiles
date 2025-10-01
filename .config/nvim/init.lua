vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.have_nerd_font = true

--opts
vim.opt.number = true
vim.opt.mouse = "a"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.inccommand = "split"
vim.opt.scrolloff = 10
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.bo.softtabstop = 2
vim.opt.signcolumn = "yes"
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.relativenumber = true

vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"

--keymaps
vim.keymap.set("i", "jk", "<Esc>")
vim.opt.hlsearch = true
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.api.nvim_set_keymap("n", "<leader>w", "<cmd>w<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>W", "<cmd>noautocmd w<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>q", "<cmd>q<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "H", "<cmd>bprevious<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "L", "<cmd>bnext<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>bd", "<cmd>bdelete<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>bd", "<cmd>bdelete<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>o", "<c-w>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>bo", function()
  local curr = vim.fn.bufnr("%")
  for _, bufnr in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
    if bufnr.bufnr ~= curr then
      vim.cmd("bd " .. bufnr.bufnr)
    end
  end
end, { desc = "Close all other buffers" })

--install lazy
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    init = function()
      vim.cmd.colorscheme("catppuccin-mocha")
    end,
    options = {
      flavor = "mocha",
    },
  },
  { "rafamadriz/friendly-snippets" },
  {
    "L3MON4D3/LuaSnip",
    version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
    build = "make install_jsregexp",
    dependencies = { "rafamadriz/friendly-snippets" },
    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
    end,
  },
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "saadparwaiz1/cmp_luasnip",
      "L3MON4D3/LuaSnip",
      "mlaursen/vim-react-snippets",
    },
    config = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      require("vim-react-snippets").lazy_load()
      vim.opt.completeopt = { "menu", "menuone", "noselect" }

      cmp.setup({
        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
          end,
        },
        window = {
          -- completion = cmp.config.window.bordered(),
          -- documentation = cmp.config.window.bordered(),
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expandable() then
              luasnip.expand()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "nvim_lua" },
          { name = "luasnip" },
        }, {
          { name = "buffer" },
          { name = "path" },
        }),
      })
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      local treesitter = require("nvim-treesitter.configs")
      treesitter.setup({
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
      })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      { "folke/neodev.nvim", opts = {} },
      "WhoIsSethDaniel/mason-tool-installer.nvim",
    },
    config = function()
      -- Use LspAttach autocommand to only map the following keys
      -- after the language server attaches to the current buffer
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", {}),
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

          -- Buffer local mappings.
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = { buffer = ev.buf }
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
          vim.keymap.set("n", "gd", function()
            require("telescope.builtin").lsp_definitions({ reuse_win = true })
          end, opts)
          vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
          vim.keymap.set("n", "<space>cd", vim.diagnostic.open_float, opts)
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
          vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
          vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, opts)
          vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, opts)
          vim.keymap.set("n", "<space>wl", function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          end, opts)
          vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, opts)
          vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, opts)
          vim.keymap.set({ "n", "v" }, "<space>ca", vim.lsp.buf.code_action, opts)
          vim.keymap.set("n", "gr", function()
            require("telescope.builtin").lsp_references({ reuse_win = true })
          end, opts)
          vim.keymap.set("n", "<space>f", function()
            vim.lsp.buf.format({ async = true })
          end, opts)
        end,
      })

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

      require("mason").setup()

      require("mason-lspconfig").setup({
        handlers = {
          function(server_name)
            local server = servers[server_name] or {}
            -- This handles overriding only values explicitly passed
            -- by the server configuration above. Useful when disabling
            -- certain features of an LSP (for example, turning off formatting for tsserver)
            server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
            require("lspconfig")[server_name].setup(server)
          end,
        },
      })
    end,
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    keys = {
      { "<leader>?", "<cmd>WhichKey<cr>", desc = "which-key" },
    },
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
  },
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "benfowler/telescope-luasnip.nvim",
    },
    keys = {
      { "<leader><space>", "<cmd>Telescope find_files<cr>", desc = "find files" },
      {
        "<leader>h",
        function()
          require("telescope.builtin").find_files({ hidden = true, no_ignore = true })
        end,
        desc = "find hidden files",
      },
      { "<leader>sg",      "<cmd>Telescope live_grep<cr>",  desc = "find files" },
      { "<leader>sb",      "<cmd>Telescope buffers<cr>",    desc = "find buffers" },
      { "<leader>ss",      "<cmd>Telescope luasnip<cr>",    desc = "find snippets" },
      { "<leader>sR",      "<cmd>Telescope resume<cr>",     desc = "resume" },
    },
    config = function()
      local telescope = require("telescope")
      local actions = require("telescope.actions")
      telescope.load_extension("luasnip")
      telescope.setup({
        defaults = {
          -- Default configuration for telescope goes here:
          -- config_key = value,
          mappings = {
            i = {
              -- map actions.which_key to <C-h> (default: <C-/>)
              -- actions.which_key shows the mappings for your picker,
              -- e.g. git_{create, delete, ...}_branch for the git_branches picker
              ["<C-h>"] = "which_key",
            },
            n = {
              ["q"] = actions.close,
              ["<c-d>"] = require("telescope.actions").delete_buffer,
            },
          },
        },
        pickers = {
          -- Default configuration for builtin pickers goes here:
          -- picker_name = {
          --   picker_config_key = value,
          --   ...
          -- }
          -- Now the picker_config_key will be applied every time you call this
          -- builtin picker
        },
        extensions = {
          -- Your extension configuration goes here:
          -- extension_name = {
          --   extension_config_key = value,
          -- }
          -- please take a look at the readme of the extension you want to configure
        },
      })
    end,
  },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    lazy = true,
    opts = {
      enable_autocmd = false,
    },
  },
  {
    "echasnovski/mini.comment",
    event = "VeryLazy",
    opts = {
      options = {
        custom_commentstring = function()
          return require("ts_context_commentstring.internal").calculate_commentstring() or vim.bo.commentstring
        end,
      },
    },
  },
  {
    "stevearc/conform.nvim",
    config = function()
      require("conform").setup({
        formatters_by_ft = {
          lua = { "stylua" },
          -- Conform will run multiple formatters sequentially
          python = { "isort", "black" },
        },
        -- format_on_save = {
        --   timeout_ms = 500,
        --   lsp_fallback = true,
        -- },
      })
    end,
  },
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup()
    end,
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      -- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
    },
    keys = {
      { "<leader>e", "<cmd>Neotree float reveal<cr>", desc = "file tree" },
    },
    config = function()
      require("neo-tree").setup({
        close_if_last_window = true,
        filesystem = {
          filtered_items = {
            hide_gitignored = true,
          },
          follow_current_file = {
            enabled = true,          -- This will find and focus the file in the active buffer every time
            --               -- the current file is changed while the tree is open.
            leave_dirs_open = false, -- `false` closes auto expanded dirs, such as with `:Neotree reveal`
          },
        },
      })
      -- vim.cmd("hi NeoTreeNormal guibg=black")
    end,
  },
  {
    "FabijanZulj/blame.nvim",
    config = function()
      require("blame").setup()
    end,
    keys = {
      { "<leader>gb", "<cmd>BlameToggle<cr>", desc = "toggle git blame" },
    },
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("lualine").setup({
        options = { section_separators = "", component_separators = "" },
      })
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts = {},
  },
  {
    "HiPhish/rainbow-delimiters.nvim",
  },
})
