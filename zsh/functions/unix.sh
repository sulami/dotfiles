# Make a new dir and cd to it
mkdircd()
{
    mkdir -p "$1" && cd "$1"
}

# Disown a process and delete its output
ds()
{
    if test -t 1
    then
        exec 1>/dev/null
    fi
    if test -t 2;
    then
        exec 2>/dev/null
    fi

    "$@" &
}

# Cd back to a dir in pwd
bd ()
{
    (($#<1)) && {
        print -- "usage: $0 <name-of-any-parent-directory>"
        return 1
    } >&2
    # Get parents (in reverse order)
    local parents
    local num=${#${(ps:/:)${PWD}}}
    local i
    for i in {$((num+1))..2}
    do
        parents=($parents "`echo $PWD | cut -d'/' -f$i`")
    done
    parents=($parents "/")
    # Build dest and 'cd' to it
    local dest="./"
    local parent
    foreach parent (${parents})
    do
        if [[ $1 == $parent ]]
        then
        cd $dest
        return 0
        fi
        dest+="../"
    done
    print -- "bd: Error: No parent directory named '$1'"
    return 1
}
_bd ()
{
    # Get parents (in reverse order)
    local num=${#${(ps:/:)${PWD}}}
    local i
    for i in {$((num+1))..2}
    do
        reply=($reply "`echo $PWD | cut -d'/' -f$i`")
    done
    reply=($reply "/")
}
compctl -V directories -K _bd bd

