#!/bin/sh

# Install a freshly made Linux Kernel on Arch w/ Syslinux
# Use with root privileges

# Syslinux entry to boot:
#
# LABEL next
#     MENU LABEL Linux-Next
#     LINUX ../vmlinuz-next
#     APPEND root=/dev/sda1 rw
#     INITRD ../initramfs-next.img

ARCH=$(uname -m)
KERNELRELEASE=$(make kernelrelease)

cp arch/$ARCH/boot/bzImage /boot/vmlinuz-next
mkinitcpio -k $KERNELRELEASE -c /etc/mkinitcpio.conf -g /boot/initramfs-next.img

