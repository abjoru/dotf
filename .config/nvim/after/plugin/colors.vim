"let s:icons = ['', '', '']

let s:filetypes = join(['nerdtree'], ',')
let s:icons = luaeval('require("dotf/ui/devicons").allIcons')

augroup ColorIcons
  autocmd!
  for [n, iconData] in items(s:icons)
    let s:ico = iconData.icon
    let s:grp = 'DevIcon' . iconData.name
    execute 'autocmd FileType ' . s:filetypes . ' syntax match ' . s:grp . ' /\v' . s:ico . './ containedin=ALL'
  endfor
augroup END
