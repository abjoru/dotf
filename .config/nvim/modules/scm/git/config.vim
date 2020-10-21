let g:lmap.g = get(g:lmap, 'g', { 'name': 'Git' })

DfNMap 'gc', 'commit', 'Gcommit'
DfNMap 'gp', 'push-current-branch', 'PushToCurrentBranch'
DfNMap 'gs', 'status', 'Gstatus'
DfNMap 'gl', 'log', 'Commits!'
DfNMap 'gL', 'log-current-file', 'BCommits!'
DfNMap 'gd', 'git-diff-tool', 'Gdiff'

let g:gitgutter_map_keys = 0
