
return {
  --use {'edluffy/hologram.nvim'} -- to see images inside neovim
  {
    'vhyrro/hologram.nvim',
    enabled = not vim.g.started_by_firenvim,
    dependencies = {'leafo/magick'},
    config = function()
      require("hologram").setup()
    end,

    --rocks = {"magick"},
  },
  -- to see images inside neovim
  {'3rd/image.nvim',
    enabled = not vim.g.started_by_firenvim,
    dependencies = {'leafo/magick'},
    config = function()
      -- default config
      require("image").setup({
        backend = "kitty",
        -- backend = "ueberzug",
        integrations = {
          markdown = {
            enabled = true,
            clear_in_insert_mode = false,
            download_remote_images = true,
            only_render_image_at_cursor = false,
            filetypes = { "markdown", "vimwiki" }, -- markdown extensions (ie. quarto) can go here
          },
          neorg = {
            enabled = true,
            clear_in_insert_mode = false,
            download_remote_images = true,
            only_render_image_at_cursor = false,
            filetypes = { "norg" },
          },
        },
        max_width = nil,
        max_height = nil,
        max_width_window_percentage = nil,
        max_height_window_percentage = 50,
        window_overlap_clear_enabled = false, -- toggles images when windows are overlapped
        window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },
      })

    end
  } -- next level of images in neovim
}
