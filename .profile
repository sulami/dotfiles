# These are OpenBSD-specific exports that need to stay here to be independent
# of zsh's config.
PATH=$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:.
PKG_PATH=ftp://openbsd.cs.fau.de/pub/OpenBSD/snapshots/packages/amd64
CVSROOT=anoncvs@openbsd.cs.fau.de:/cvs
LC_CTYTPE=en_US.UTF-8
LANG=en_US.UTF-8
LESSCHARSET=utf-8
export PATH HOME TERM PKG_PATH CVSROOT LC_CTYTPE LANG LESSCHARSET

# Map Capslock to Control
wsconsctl keyboard.map+="keysym Caps_Lock = Control_L"

clear

echo "Welcome to $(hostname)"
echo "Running $(uname) $(uname -r) on $(uname -m)"
echo
echo "Uptime:"
echo "$(uptime)"
echo
echo "Disks:"
echo "$(df -h)"
echo
echo "Users:"
echo "$(w -h)"
echo
echo "Last logins:"
echo "$(last -n 3)"
echo

if [[ -s /var/mail/$(whoami) ]]; then
    echo "You've got mail!"
    echo
fi

