import glob
import os.path

c.aliases = {
    'q': 'quit',
    'w': 'session-save',
    'wq': 'quit --save',
    'x': 'quit',
    'h': 'help',
    'r': 'reload',
    'h': 'home'
}

c.downloads.location.directory = '~/Downloads'
c.statusbar.show = 'in-mode'
c.tabs.show = 'multiple'
c.confirm_quit = ['downloads']

c.url.searchengines = {
    'DEFAULT': 'https://www.google.com/search?hl=en&q={}',
    'aw': 'https://wiki.archlinux.org/?search={}',
    'yt': 'https://www.youtube.com/results?search_query={}',
    'gt': 'https://github.com/search?q={}'
}

c.url.start_pages = '~/.cache/dotf/homepage.html'

config.bind('xb', 'config-cycle statusbar.show always in-mode')
config.bind('xt', 'config-cycle tabs.show multiple switching')
config.bind('xx', 'config-cycle statusbar.show always in-mode ;; config-cycle tabs.show multiple switching')
config.bind('zl', 'spawn --userscript qute-pass')
config.bind('zpl', 'spawn --userscript qute-pass --password-only')
config.bind('zul', 'spawn --userscript qute-pass --username-only')

config.load_autoconfig()
