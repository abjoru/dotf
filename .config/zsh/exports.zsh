# Make sure Weechat uses XDG folders
export WEECHAT_HOME=${XDG_CONFIG_HOME:-$HOME/.config}/weechat

# Force SBT to use XDG folders.
export SBT_OPTS="-Dsbt.ivy.home=${XDG_CACHE_HOME:-$HOME/.cache}/ivy -Dsbt.boot.directory=${XDG_CACHE_HOME:-$HOME/.cache}/sbt/boot -Dsbt.preloaded=${XDG_CACHE_HOME:-$HOME/.cache}/sbt/preloaded -Dsbt.global.base=${XDG_CACHE_HOME:-$HOME/.cache}/sbt -Dsbt.global.staging=${XDG_CACHE_HOME:-$HOME/.cache}/sbt/staging -Dsbt.global.zinc=${XDG_CACHE_HOME:-$HOME/.cache}/sbt/zinc -Dsbt.dependency.base=${XDG_CACHE_HOME:-$HOME/.cache}/sbt/dependency -Dsbt.repository.config=${XDG_CONFIG_HOME:-$HOME/.config}/sbt/repositories -Dsbt.global.settings=${XDG_CONFIG_HOME:-$HOME/.config}/sbt/global -Dsbt.global.plugins=${XDG_CONFIG_HOME:-$HOME/.config}/sbt/plugins"
