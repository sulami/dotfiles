#!/bin/sh

DATE=$(date --rfc-822)
DIR=/home/sulami/mail/
GIT=/usr/bin/git

cd $DIR
$GIT add -A
$GIT add -u
$GIT commit -am "$DATE - autocommit"
$GIT push origin master

