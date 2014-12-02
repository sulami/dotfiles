#!/bin/zsh

cv()
{
    curl "$1" | pv | vim -
}

