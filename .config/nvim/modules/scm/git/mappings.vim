let g:lmap.g = get(g:lmap, 'g', { 'name': 'Git' })

DfNMap 'gc', 'Commit', 'Gcommit'
DfNMap 'gp', 'Push current branch', 'PushToCurrentBranch'
DfNMap 'gs', 'Status', 'Gstatus'
DfNMap 'gl', 'Log', 'Commits!'
DfNMap 'gL', 'Log current file', 'BCommits!'
DfNMap 'gd', 'Git diff-tool', 'Gdiff'
