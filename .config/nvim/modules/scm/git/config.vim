let g:lmap.g = get(g:lmap, 'g', { 'name': 'git' })

DfNMap 'gc', 'commit', 'Gcommit'
DfNMap 'gp', 'push-current-branch', 'PushToCurrentBranch'
DfNMap 'gs', 'status', 'Gstatus'
DfNMap 'gl', 'log', 'Commits!'
DfNMap 'gL', 'log-current-file', 'BCommits!'

let g:gitgutter_map_keys = 0
