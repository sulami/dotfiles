#!/bin/sh

PARENT="/home/sulami/"

DIRS=("Documents"
      "git"
      "Music"
      "Pictures")

TARGET="/home/sulami/mnt/raspi/"

mount $TARGET

for DIR in ${DIRS[*]}
do
    echo "$PARENT$DIR -> $TARGET$DIR"
    echo "[$(date --rfc-3339=date)] $PARENT$DIR" >> $TARGET"backup.log"
    rsync -aPq --delete $PARENT$DIR $TARGET | tee -a $TARGET"backup.log"
done

umount $TARGET

