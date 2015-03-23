cv()
{
    curl "$1" | pv | vim -
}

