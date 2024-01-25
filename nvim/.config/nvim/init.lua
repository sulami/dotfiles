-- Keep visual selection when shifting.
vim.keymap.set({'v', 'n'}, '<', '<gv')
vim.keymap.set({'v', 'n'}, '>', '>gv')

-- Use the system clipboard.
vim.opt.clipboard = 'unnamedplus'

-- Make underscores word boundaries.
vim.api.nvim_exec([[
  set iskeyword-=_
]], false)

-- Hard-wrap at column 80.
vim.api.nvim_exec([[
  set textwidth=80
]], false)

-- Setup smartcase for searching.
vim.api.nvim_exec([[
  set ignorecase
  set smartcase
]], false)

-- Tabs are two spaces.
vim.api.nvim_exec([[
  set expandtab
  set tabstop=2
  set shiftwidth=2
]], false)

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local lazy = require("lazy")
local plugins = {
  "b0o/mapx.nvim",
  {
    "kylechui/nvim-surround",
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
		    -- Configuration here, or leave empty to use defaults
		  })
	  end
  },
}

if vim.g.vscode then
  -- VS Code config.
  lazy.setup(plugins)
else
  -- Plain neovim config, load more plugins.
  table.insert(plugins, {
    'numToStr/Comment.nvim',
    opts = {
      -- add any options here
    },
    lazy = false,
  })
  table.insert(plugins, {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim'
    }
  })
  lazy.setup(plugins)
end
