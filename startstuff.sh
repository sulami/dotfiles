sudo ifup br0
sudo ifup em1
mount ~/mnt/data
cd ~/mnt/data/Bilder/Wallpapers/Space-HQ-Set/
sh rotate_wallpaper.sh > /dev/null &
cd ~/build/Disk-Indicator/
sudo ./disk_indicator &

