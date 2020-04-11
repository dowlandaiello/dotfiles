# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="$HOME/.oh-my-zsh"
plugins=(git zsh-syntax-highlighting zsh-vim-mode)
ZSH_THEME="powerlevel10k/powerlevel10k"
source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8
export PYENV_ROOT=$HOME/.pyenv
export NODENV_ROOT=$HOME/.nodenv
export PATH=$HOME/bin:/usr/local/bin:$PYENV_ROOT/bin:$NODENV_ROOT/bin:$HOME/.yarn/bin:$HOME/.cargo/bin:/usr/lib/jvm/java-13-openjdk/jre/bin/:$HOME/google-cloud-sdk/bin:$PATH

# Runtime shell configuration
eval "$(pyenv init -)"
eval "$(nodenv init -)"

# Haskell stuff
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

# Google cloud completion
if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then . "$HOME/google-cloud-sdk/path.zsh.inc"; fi
if [ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]; then . "$HOME/google-cloud-sdk/completion.zsh.inc"; fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Match the current vim mode with the cursor
export MODE_CURSOR_VICMD="block"
export MODE_CURSOR_VIINS="blinking bar"
export MODE_CURSOR_SEARCH="steady underline"

# I use mupdf to vieew my PDF files, but on macOS, I use preview
if [ "$(uname 2> /dev/null)" = "Darwin" ]; then
    alias mupdf=mupdf-gl
fi
