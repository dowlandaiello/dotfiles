export ZSH="/home/dowlandaiello/.oh-my-zsh"
plugins=(git)
ZSH_THEME="powerlevel9k/powerlevel9k"
source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8
export PYENV_ROOT=$HOME/.pyenv
export NODENV_ROOT=$HOME/.nodenv
export PATH=$HOME/bin:/usr/local/bin:$PYENV_ROOT/bin:$NODENV_ROOT/bin:$HOME/.yarn/bin:/home/dowlandaiello/.cargo/bin:/usr/lib/jvm/java-13-openjdk/jre/bin/:$HOME/google-cloud-sdk/bin:$PATH

# Runtime shell configuration
eval "$(pyenv init -)"
eval "$(nodenv init -)"
eval $(dircolors ~/.dir_colors)

# Haskell stuff
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

# Googel cloud completion
if [ -f '/home/dowlandaiello/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/home/dowlandaiello/Downloads/google-cloud-sdk/path.zsh.inc'; fi
if [ -f '/home/dowlandaiello/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/dowlandaiello/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
