local M = {}
local gl = require('galaxyline')
local gls = gl.section
--local palette = require('gruvbox.palette')

gl.short_line_list = {'LuaTree', 'NvimTree', 'nerdtree', 'vista', 'startify', 'term', 'fugitive', 'fugitiveblame'}

local colors = require('dotf/palette').oneDarkStatusline

local mode_name = function()
  local aliases = {
    n     = 'NORMAL',
    i     = 'INSERT',
    c     = 'COMMAND',
    v     = 'VISUAL',
    V     = 'V-LINE',
    ['']  = 'VISUAL',
    R     = 'REPLACE',
    t     = 'TERMINAL',
    s     = 'SELECT',
    S     = 'S-LINE'
  }

  if aliases[vim.fn.mode()] ~= nil then
    return aliases[vim.fn.mode()]
  else
    return 'V-BLOCK'
  end
end

local mode_color = function()
  local mode_colors = {
    n     = colors.green,
    i     = colors.blue,
    c     = colors.green,
    V     = colors.purple,
    ['']  = colors.purple,
    v     = colors.purple,
    R     = colors.red1,
    t     = colors.blue
  }

  if mode_colors[vim.fn.mode()] ~= nil then
    return mode_colors[vim.fn.mode()]
  else
    print(vim.fn.mode())
    return colors.purple
  end
end

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

local function file_readonly()
  if vim.bo.filetype == 'help' then return '' end
  if vim.bo.readonly == true then return '  ' end
  return ''
end

local function get_current_file_name()
  local file = vim.fn.expand('%:t')
  if vim.fn.empty(file) == 1 then return '' end
  if string.len(file_readonly()) ~= 0 then return file .. file_readonly() end
  if vim.bo.modifiable then
    if vim.bo.modified then return file .. '  ' end
  end
  return file .. ' '
end

M.setup = function()
  gls.left[1] = {
    ViMode = {
      provider = function()
        vim.api.nvim_command('hi GalaxyViMode guibg=' .. mode_color())
        return '  ' .. mode_name() .. '  '
      end,
      highlight = {colors.bg, colors.bg, 'bold'} 
    }
  }
  gls.left[2] = {
    FileIcon = {
      provider = {function() return '  ' end, 'FileIcon'},
      condition = buffer_not_empty,
      highlight = {require('galaxyline.provider_fileinfo').get_file_icon_color, colors.section_bg}
    }
  }
  gls.left[3] = {
    FileName = {
      provider = get_current_file_name,
      condition = buffer_not_empty,
      highlight = {colors.fg, colors.section_bg},
      --separator = "",
      separator = ' ',
      separator_highlight = {colors.section_bg, colors.bg}
    }
  }
  gls.left[4] = {
    DiagnosticError = {
      provider = 'DiagnosticError',
      icon = '  ',
      highlight = {colors.red1, colors.bg}
    }
  }
  gls.left[5] = {
    Space = {
      provider = function() return ' ' end,
      highlight = {colors.section_bg, colors.bg}
    }
  }
  gls.left[6] = {
    DiagnosticWarn = {
      provider = 'DiagnosticWarn',
      icon = '  ',
      highlight = {colors.orange, colors.bg}
    }
  }
  gls.left[7] = {
    Space = {
      provider = function() return ' ' end,
      highlight = {colors.section_bg, colors.bg}
    }
  }
  gls.left[8] = {
    DiagnosticInfo = {
      provider = 'DiagnosticInfo',
      icon = '  ',
      highlight = {colors.blue, colors.section_bg},
      separator = ' ',
      separator_highlight = {colors.section_bg, colors.bg}
    }
  }
  gls.left[9] = {
    Space = {
      provider = function() return ' ' end,
      highlight = {colors.section_bg, colors.bg}
    }
  }
  -- TODO condition scala project?
  gls.left[10] = {
    MetalsStatus = {
      provider = function() return '  ' .. (vim.g['metals_status'] or '') end,
      icon = ' λ ',
      highlight = {colors.orange, colors.bg}
    }
  }

  ----------------
  -- Right Side --
  ----------------



  gls.right[1] = {
    DiffAdd = {
      provider = 'DiffAdd',
      condition = checkwidth,
      icon = ' ',
      highlight = {colors.green, colors.bg}
    }
  }
  gls.right[2] = {
    DiffModified = {
      provider = 'DiffModified',
      condition = checkwidth,
      icon = ' ',
      highlight = {colors.orange, colors.bg}
    }
  }
  gls.right[3] = {
    DiffRemove = {
      provider = 'DiffRemove',
      condition = checkwidth,
      icon = ' ',
      highlight = {colors.red1, colors.bg}
    }
  }
  gls.right[4] = {
    GitIcon = {
      provider = function() return '  ' end,
      condition = buffer_not_empty and require('galaxyline.provider_vcs').check_git_workspace,
      highlight = {colors.gray2, colors.bg}
    }
  }
  gls.right[5] = {
    GitBranch = {
      provider = 'GitBranch',
      condition = buffer_not_empty and require('galaxyline.provider_vcs').check_git_workspace,
      highlight = {colors.gray2, colors.bg}
    }
  }
  gls.right[6] = {
    FileFormat = {
      provider = 'FileFormat',
      separator = ' | ',
      separator_highlight = {colors.section_bg, colors.bg},
      highlight = {colors.fg, colors.bg, 'bold'}
    }
  }
  gls.right[7] = {
    LineInfo = {
      provider = 'LineColumn',
      separator = ' | ',
      separator_highlight = {colors.section_bg, colors.bg},
      highlight = {colors.fg, colors.bg}
    }
  }
  gls.right[8] = {
    PerCent = {
      provider = 'LinePercent',
      separator = ' ',
      separator_highlight = {colors.blue, colors.bg},
      highlight = {colors.bg, colors.blue}
    }
  }

  --gls.left[10] = {
    --LeftEnd = {
      --provider = function() return ' ' end,
      --separator = ' ',
      --separator_highlight = {palette.dark2, line_bg},
      --highlight = {line_bg, line_bg}
    --}
  --}
  --gls.left[11] = {
    --TrailingWhiteSpace = {
      --provider = TrailingWhiteSpace,
      --icon = '  ',
      --highlight = {palette.neutral_yellow, palette.dark2}
    --}
  --}

  -- gls.right[3] = {
    -- ScrollBar = {
      -- provider = 'ScrollBar',
      -- separator = ' | ',
      -- separator_highlight = {palette.neutral_aqua, line_bg},
      -- highlight = {palette.neutral_aqua, palette.faded_blue}
    -- }
  -- }

  ----------------
  -- Short line --
  ----------------

  gls.short_line_left[1] = {
    BufferType = {
      provider = {function() return ' ' end, 'FileTypeName', function() return ' ' end},
      separator = ' ',
      condition = has_file_type,
      separator_highlight = {colors.section_bg, colors.bg},
      highlight = {colors.fg, colors.section_bg}
    }
  }
  gls.short_line_right[1] = {
    BufferIcon = {
      provider = 'BufferIcon',
      condition = has_file_type,
      separator = '',
      separator_highlight = {colors.section_bg, colors.bg},
      highlight = {colors.yellow, colors.section_bg}
    }
  }
end

return M
