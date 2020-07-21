if [[ "$OSTYPE" == "darwin"* ]]; then
  export CONSCRIPT_HOME="$HOME/.conscript"
  export CONSCRIPT_OPTS="-XX:MaxPermSize=512M -Dfile.encoding=UTF-8"
  export PATH="$HOME/.jenv/bin:/Applications/Visual Studio Code.app/Contents/Resources/app/bin:$CONSCRIPT_HOME/bin:$PATH"
fi

# Global java options
export JAVA_OPTS="-Divy.home=${XDG_CACHE_HOME:=$HOME/.cache}/ivy"
