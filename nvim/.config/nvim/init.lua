-- Keep visual selection when shifting.
vim.keymap.set({'v', 'n'}, '<', '<gv')
vim.keymap.set({'v', 'n'}, '>', '>gv')

-- Return in normal mode removes search highlights.
vim.keymap.set({'n'}, '<return>', ':noh<return>')

-- Use the system clipboard.
vim.opt.clipboard = 'unnamedplus'

-- Completion
vim.opt.completeopt = {'menu', 'menuone', 'noselect'}

-- Make underscores word boundaries.
vim.opt.iskeyword:remove({'_'})

-- Hard-wrap at column 80.
vim.opt.textwidth = 80

-- Setup smartcase for searching.
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Tabs are two spaces.
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2

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
      require("nvim-surround").setup({})
	  end
  },
  "neovim/nvim-lspconfig",
  "hrsh7th/nvim-cmp",
}

if vim.g.vscode then
  -- VS Code config.
  lazy.setup(plugins)
else
  -- Plain neovim config, load more plugins.
  table.insert(plugins, 'nvim-treesitter/nvim-treesitter')
  table.insert(plugins, {
    'numToStr/Comment.nvim',
    opts = {},
    lazy = false,
  })
  table.insert(plugins, {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim'
    }
  })
  table.insert(plugins, 'neovim/nvim-lspconfig')
  table.insert(plugins, 'hrsh7th/nvim-cmp')
  table.insert(plugins, 'hrsh7th/cmp-nvim-lsp')
  lazy.setup(plugins)

  vim.g.mapleader = ' ';

  local telescope_builtin = require('telescope.builtin');
  vim.keymap.set('n', '<leader>ff', telescope_builtin.find_files, {})
  vim.keymap.set('n', '<leader>ps', telescope_builtin.live_grep, {})
  vim.keymap.set('n', '<leader>bb', telescope_builtin.buffers, {})
  vim.keymap.set('n', '<leader>hh', telescope_builtin.help_tags, {})
  vim.keymap.set('n', '<leader>fr', telescope_builtin.oldfiles, {})
  vim.keymap.set('n', '<leader>ll', telescope_builtin.lsp_references, {})
  vim.keymap.set('n', '<leader>jj', telescope_builtin.lsp_document_symbols, {})
  vim.keymap.set('n', '<leader>jJ', telescope_builtin.lsp_workspace_symbols, {})

  local treesitter = require('nvim-treesitter.configs');
  treesitter.setup({
    ensure_installed = { 'lua', 'rust', 'ruby' },
    highlight = {
      enable = true,
      disable = { "vimhelp" }
    },
  });

  local lspconfig = require('lspconfig')
  lspconfig.rust_analyzer.setup({})

  local cmp = require('cmp');
  cmp.setup({
    mapping = cmp.mapping.preset.insert({
      ['<TAB>'] = cmp.mapping.confirm(),
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
    })
  })
end
