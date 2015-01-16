#!/usr/bin/env python

from subprocess import check_output, CalledProcessError

def passwd(acc):
    """
    Get a password from a gpg file, expects something like 'password = "bla"'
    """
    path = '/home/sulami/dotfiles/mutt/mutt-{}.gpg'.format(acc)
    args = ['gpg', '--use-agent', '-q', '-d', path]
    try:
        return check_output(args).strip().split('"')[1]
    except CalledProcessError:
        return 'FAIL'
