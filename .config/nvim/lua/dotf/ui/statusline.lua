local M = {}
local gl = require('galaxyline')
local gls = gl.section
local palette = require('gruvbox.palette')
local line_bg = palette.dark1

gl.short_line_list = {'LuaTree', 'NvimTree', 'vista', 'startify', 'term', 'fugitive', 'fugitiveblame'}

local buffer_not_empty = function()
  if vim.fn.empty(vim.fn.expand('%:t')) ~= 1 then
    return true
  end
  return false
end

local checkwidth = function()
  local squeeze_width = vim.fn.winwidth(0) / 2
  if squeeze_width > 40 then
    return true
  end
  return false
end

local trailing_whitespace = function()
  local trail = vim.fn.search("\\s$", "nw")
  if trail ~= 0 then
    return ' '
  else 
    return nil
  end
end

TrailingWhiteSpace = trailing_whitespace
  

M.setup = function()
  gls.left[1] = {
    FirstElement = {
      provider = function() return ' ' end,
      highlight = {palette.neutral_blue, line_bg}
    }
  }
  gls.left[2] = {
    ViMode = {
      provider = function()
        local alias = {
          n = 'NORMAL',
          i = 'INSERT',
          c= 'COMMAND',
          V= 'VISUAL',
          [''] = 'VISUAL',
          v ='VISUAL',
          c  = 'COMMAND-LINE',
          ['r?'] = ':CONFIRM',
          rm = '--MORE',
          R  = 'REPLACE',
          Rv = 'VIRTUAL',
          s  = 'SELECT',
          S  = 'SELECT',
          ['r']  = 'HIT-ENTER',
          [''] = 'SELECT',
          t  = 'TERMINAL',
          ['!']  = 'SHELL',
        }
        local mode_color = {
          n = palette.neutral_green, 
          i = palette.neutral_blue, 
          v = palette.neutral_aqua, 
          [''] = palette.neutral_blue, 
          V = palette.neutral_blue, 
          c = palette.neutral_red, 
          no = palette.neutral_aqua, 
          s = palette.neutral_orange, 
          S = palette.neutral_orange, 
          ic = palette.neutral_yellow, 
          R = palette.neutral_purple, 
          Rv = palette.neutral_purple, 
          cv = palette.neutral_red, 
          ce = palette.neutral_red, 
          r = palette.neutral_aqua, 
          rm = palette.netural_aqua, 
          ['r?'] = palette.neutral_aqua, 
          ['!'] = palette.neutral_green, 
          [''] = palette.neutral_yellow, 
          [''] = palette.neutral_orange, 
          t = palette.neutral_red, 
        }

        local vim_mode = vim.fn.mode()
        vim.api.nvim_command('hi GalaxyViMode guifg=' .. mode_color[vim_mode])
        return alias[vim_mode] .. '   '
      end,
      highlight = {palette.neutral_red, line_bg, 'bold'} 
    }
  }
  gls.left[3] = {
    FileIcon = {
      provider = 'FileIcon',
      condition = buffer_not_empty,
      highlight = {require('galaxyline.provider_fileinfo').get_file_icon_color, line_bg}
    }
  }
  gls.left[4] = {
    FileName = {
      provider = {'FileName', 'FileSize'},
      condition = buffer_not_empty,
      highlight = {palette.neutral_blue, line_bg, 'bold'}
    }
  }
  gls.left[5] = {
    GitIcon = {
      provider = function() return '  ' end,
      condition = require('galaxyline.provider_vcs').check_git_workspace,
      highlight = {palette.neutral_orange, line_bg}
    }
  }
  gls.left[6] = {
    GitBranch = {
      provider = 'GitBranch',
      condition = require('galaxyline.provider_vcs').check_git_workspace,
      highlight = {palette.light0, line_bg, 'bold'}
    }
  }

  gls.left[7] = {
    DiffAdd = {
      provider = 'DiffAdd',
      condition = checkwidth,
      icon = ' ',
      highlight = {palette.neutral_green, line_bg}
    }
  }
  gls.left[8] = {
    DiffModified = {
      provider = 'DiffModified',
      condition = checkwidth,
      icon = ' ',
      highlight = {palette.neutral_orange, line_bg}
    }
  }
  gls.left[9] = {
    DiffRemove = {
      provider = 'DiffRemove',
      condition = checkwidth,
      icon = ' ',
      highlight = {palette.neutral_red, line_bg}
    }
  }
  gls.left[10] = {
    LeftEnd = {
      provider = function() return ' ' end,
      separator = ' ',
      separator_highlight = {palette.dark2, line_bg},
      highlight = {line_bg, line_bg}
    }
  }
  gls.left[11] = {
    TrailingWhiteSpace = {
      provider = TrailingWhiteSpace,
      icon = '  ',
      highlight = {palette.neutral_yellow, palette.dark2}
    }
  }
  gls.left[12] = {
    DiagnosticError = {
      provider = 'DiagnosticError',
      icon = '  ',
      highlight = {palette.neutral_red, palette.dark2}
    }
  }
  gls.left[13] = {
    Space = {
      provider = function() return ' ' end,
      highlight = {palette.neutral_orange, palette.dark2}
    }
  }
  gls.left[14] = {
    DiagnosticWarn = {
      provider = 'DiagnosticWarn',
      icon = '  ',
      highlight = {palette.neutral_yellow, palette.dark2}
    }
  }
  gls.left[15] = {
    MetalsStatus = {
      provider = function() return '  ' .. (vim.g['metals_status'] or '') end,
      icon = '  λ ',
      highlight = {palette.neutral_green, palette.dark2}
    }
  }


  gls.right[1] = {
    FileFormat = {
      provider = 'FileFormat',
      separator = ' ',
      separator_highlight = {palette.dark2, line_bg},
      highlight = {palette.light0, line_bg, 'bold'}
    }
  }
  gls.right[2] = {
    LineInfo = {
      provider = 'LineColumn',
      separator = ' | ',
      separator_highlight = {palette.dark2, line_bg},
      highlight = {palette.light0, line_bg}
    }
  }
  -- gls.right[3] = {
    -- ScrollBar = {
      -- provider = 'ScrollBar',
      -- separator = ' | ',
      -- separator_highlight = {palette.neutral_aqua, line_bg},
      -- highlight = {palette.neutral_aqua, palette.faded_blue}
    -- }
  -- }
  gls.right[4] = {
    PerCent = {
      provider = 'LinePercent',
      separator = ' ',
      separator_highlight = {line_bg, line_bg},
      highlight = {palette.neutral_aqua, line_bg, 'bold'}
    }
  }

  gls.short_line_left[1] = {
    BufferType = {
      provider = 'FileTypeName',
      --separator = '',
      separator = ' ',
      condition = has_file_type,
      separator_highlight = {palette.gray, palette.dark2},
      highlight = {palette.dark2, palette.gray}
    }
  }

  gls.short_line_right[1] = {
    BufferIcon = {
      provider = 'BufferIcon',
      -- separator = '',
      separator = ' ',
      condition = has_file_type,
      separator_highlight = {palette.gray, palette.dark2},
      highlight = {palette.dark2, palette.gray}
    }
  }
end

return M
