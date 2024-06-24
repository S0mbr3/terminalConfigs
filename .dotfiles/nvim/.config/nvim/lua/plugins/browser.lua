
return {
  'glacambre/firenvim',
  enabled = true,
  lazy = not vim.g.started_by_firenvim,
  build = function() vim.fn['firenvim#install'](0) end,
  config = function()
    vim.cmd([[
    au BufEnter codingame.com_*.txt set filetype=c
    au BufEnter leetcode.com_*.txt set filetype=cpp
    au BufEnter www.typescriptlang.org_*.txt set filetype=typescript
    let g:firenvim_config = { 
      \ 'globalSettings': {
        \ 'alt': 'all',
        \  },
        \ 'localSettings': {
          \ '.*': {
            \ 'cmdline': 'neovim',
            \ 'content': 'text',
            \ 'priority': 0,
            \ 'selector': 'textarea',
            \ 'takeover': 'never',
            \ 'filename': '/tmp/{hostname}_{pathname%10}.{extension}',
            \ },
            \ }
            \ }
            let fc = g:firenvim_config['localSettings']
            let fc['.*'] = { 'takeover': 'never' }
            ]])

          end
        }
