-- Keep visual selection when shifting.
vim.keymap.set({'v', 'n'}, '<', '<gv')
vim.keymap.set({'v', 'n'}, '>', '>gv')

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

require("lazy").setup({
	{ "kylechui/nvim-surround",
		event = "VeryLazy",
      		config = function()
		require("nvim-surround").setup({
		  -- Configuration here, or leave empty to use defaults
		})
	      end
      },
    "b0o/mapx.nvim",
    }
)

