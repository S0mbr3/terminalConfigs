return {
  --{'Vaisakhkm2625/hologram-math-preview.nvim'},
  {"jbyuki/nabla.nvim"},
  {'S0mbr3/hologram-math-preview',
    dir = '~/dev/neovim-plugins/hologram-math-preview.nvim',
    --enabled = not vim.g.started_by_firenvim,
    enabled = false,
    branch= 'show-all-eq-fix',
    dependencies = "vhyrro/hologram.nvim"
  }
}
