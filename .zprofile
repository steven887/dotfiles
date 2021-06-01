emulate sh -c 'source /etc/profile'

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec startx
fi

PATH="$HOME/.local/bin:$PATH"
export npm_config_prefix="$HOME/.local"
