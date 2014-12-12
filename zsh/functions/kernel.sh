#!/bin/sh

# Install a freshly made Linux Kernel on Arch w/ Syslinux
# First argument decides the name (mainline or next)

# Syslinux entry to boot:
#
# LABEL mainline / next
#     MENU LABEL Linux-Next/Mainline
#     LINUX ../vmlinuz-next/mainline
#     APPEND root=/dev/sda1 rw
#     INITRD ../initramfs-next/mainline.img

# UPDATE:
# Syslinux entry now for encrypted EFI-installation:
#
# LABEL mainline / next
#     MENU LABEL Linux-Next/Mainline
#     LINUX ../../vmlinuz-next/mainline
#     APPEND root=/dev/mapper/cryptroot cryptdevice=/dev/sda2:cryptroot crypto=:::: rw
#     INITRD ../../initramfs-next/mainline.img

kernel_install()
{
    if [ "$#" -ne 1 ]
    then
        echo "Usage: $0 <name>" >&2
        return 1
    fi

    ARCH=$(uname -m)
    KERNELRELEASE=$(make kernelrelease)

    sudo make modules_install
    sudo cp arch/$ARCH/boot/bzImage /boot/vmlinuz-$1
    sudo mkinitcpio -k $KERNELRELEASE -c /etc/mkinitcpio.conf \
        -g /boot/initramfs-$1.img
}

