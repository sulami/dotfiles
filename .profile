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

