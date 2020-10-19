command! -nargs=0 -bar DfTerminalOpen rightbelow split | terminal
command! -nargs=0 -bar DfTerminalCmd call feedkeys(":! ")
