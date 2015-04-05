#!/bin/sh

# Use this after partitioning and mounting everything on /mnt.

pacstrap /mnt base base-devel vim gptfdisk syslinux

genfstab -p /mnt > /mnt/etc/fstab

# Set the hosname
vi /mnt/etc/hostname
vi /mnt/etc/hosts

ln -sf /mnt/usr/share/zoneinfo/Europe/Berlin /mnt/etc/localtime

echo "LANG=en_US.UTF-8" > /mnt/etc/locale.conf
echo "LC_COLLATE=C" >> /mnt/etc/locale.conf
echo "KEYMAP=us" > /mnt/etc/vconsole.conf

echo "en_US.UTF-8 UTF-8" > /mnt/etc/locale.gen
arch-chroot /mnt locale-gen

# Configure syslinux
vi /mnt/boot/syslinux/syslinux.cfg
arch-chroot /mnt syslinux-install_update -iam

# Enable Mulitlib if needed, and colors.
vi /mnt/etc/pacman.conf

# Change initramfs modules and hooks if needed.
vi /mnt/etc/mkinitcpio.conf
arch-chroot /mnt mkinitcpio -p linux

arch-chroot /mnt passwd

