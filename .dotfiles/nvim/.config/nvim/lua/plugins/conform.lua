return {
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        ["_"] = { "caddy_fmt" },
      },

      formatters = {
        caddy_fmt = {
          command = "caddy",
          args = { "fmt" },
          stdin = true,
          condition = function(ctx)
            return vim.fs.basename(ctx.filename) ~= "Caddyfile"
          end,
        },
      },
      --[[ format_on_save = {
        -- These options will be passed to conform.format()
        timeout_ms = 500,
        lsp_format = "fallback",
      }, ]]
    },
  },
}
