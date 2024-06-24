return {
  --{'Vaisakhkm2625/hologram-math-preview.nvim'},
  {"jbyuki/nabla.nvim",
    keys ={
      {'<leader>po', function() require'nabla'.popup() end},
      {'<leader>pv', function() require'nabla'.toggle_virt({autogen=true}) end}
    },
  },
  {'S0mbr3/hologram-math-preview',
    dir = '~/dev/neovim-plugins/hologram-math-preview.nvim',
    branch= 'show-all-eq-fix',
    dependencies = "vhyrro/hologram.nvim",
    keys ={
      {"<leader>pm", function() require('hologram-math-preview').show_all_eq() end}
    }
  }
}
