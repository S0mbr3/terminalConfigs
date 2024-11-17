return {
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    keys = {
      {
        -- Customize or remove this keymap to your liking
        "<leader>ft",
        function()
          require("conform").format({ async = true })
        end,
        mode = "",
        desc = "Format buffer",
      },
    },
    config = function ()
      require("conform").setup({
        formatters_by_ft = {
          caddyfile = { "caddyfile" },
        },
        formatters = {
          caddyfile = {
            command = "caddy_wrap_conform_nvim",
            args={},
            --command = "caddy",
            --args = { "fmt"},
            stdin = true,
            cwd = require("conform.util").root_file({ "Caddyfile" }),
            require_cwd = true,
          }
        }
      })
    end
  }
}
