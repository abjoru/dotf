local M = {}
local fn = vim.fn
local icons = require('nvim-web-devicons')
local palette = require('gruvbox/palette')

function M.test(node)
  --return node['flagSet']['_flags']['git']
  --return vim.call('a:node.getChildCount')
  return #node.children
end

function M.get_file_icon(path)
  if path == nil then
    fileNode = fn.expand('%:t')
    fileExt  = fn.expand('%:e')
  else
    fileNode = fn.fnamemodify(path, ':t')
    fileExt  = fn.fnamemodify(path, ':e')
  end

  return icons.get_icon(fileNode, fileExt, {default = true})
end

function M.get_dir_icon(isOpen, isSymlink)
  local iconName = ''

  if isSymlink then 
    iconName = 'dirsymlink'
  else
    if isOpen then 
      iconName = 'diropen'
    else 
      iconName = 'dirclosed'
    end
  end

  return icons.get_icon(iconName, '', {default = true})
end

  --autocmd FileType nerdtree syntax match DevIconDirectoryOpen /\v./ containedin=ALL
function M.setup(additionalIcons)
  icons.setup(additionalIcons)

  if not additionalIcons then
    M['allIcons'] = icons.all_icons
  else
    M['allIcons'] = vim.tbl_extend('force', icons.all_icons, additionalIcons.override or {})
  end
end

return M
--{
  --get_file_icon = get_file_icon,
  --get_dir_icon = get_dir_icon,
  --mkColors = setup,
  --allIcons = icons.all_icons
--}
