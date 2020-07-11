import glob
import os.path

c.aliases = {
    'q': 'quit',
    'w': 'session-save',
    'wq': 'quit --save',
    'x': 'quit',
    'h': 'help'
}

c.downloads.location.directory = '~/Downloads'
c.statusbar.hide = True
c.tabs.show = 'multiple'
c.confirm_quit = ['downloads']

c.url.searchengines = {
    'DEFAULT': 'https://www.google.com/search?hl=en&q={}',
    'aw': 'https://wiki.archlinux.org/?search={}',
    'yt': 'https://www.youtube.com/results?search_query={}',
    'gt': 'https://github.com/search?q={}'
}

c.url.start_pages = '~/.cache/dotf/homepage.html'

config.bind('xb', 'config-cycle statusbar.hide')
config.bind('xt', 'config-cycle tabs.show multiple switching')
config.bind('xx', 'config-cycle statusbar.hide ;; config-cycle tabs.show multiple switching')

config.load_autoconfig()
