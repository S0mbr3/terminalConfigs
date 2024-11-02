local home = vim.fn.expand("$HOME");
return {
  "jackMort/ChatGPT.nvim",
  event = "VeryLazy",
  enabled = true,
  config = function()
    require("chatgpt").setup({
      api_key_cmd = 'cat ' .. home .. '/openai.env'
      --[[ api_key_cmd = "gpg --quiet --batch --decrypt " .. home .. "/groq_api.txt.gpg",
      -- api_host_cmd = "gpg --quiet --batch --decrypt " .. home .. "/groq_host.txt.gpg",
      openai_params = {
        model = "llama3-70b-8192"
      },
      openai_edit_params = {
        model = "llama3-70b-8192"
      }, ]]
    })
  end,
  dependencies = {
    "MunifTanjim/nui.nvim",
    "nvim-lua/plenary.nvim",
    "folke/trouble.nvim",
    "nvim-telescope/telescope.nvim"
  },
  {
  'Exafunction/codeium.vim',
  }
}
