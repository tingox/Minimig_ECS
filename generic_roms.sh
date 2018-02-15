#!/bin/sh
rtl_emard/tools/jbboot_bin2vhdl.py Minimig_ECS/JBBoot/amigaboot.bin jbboot.vhd
rtl_emard/tools/osdbootstrap_bin2vhdl.py Minimig_ECS/BOOTROM1/osdload.bin osd_bootstrap.vhd