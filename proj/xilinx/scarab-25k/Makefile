project = miniSpartan6-plus
vendor = xilinx
family = spartan6
MODEL ?= xc6slx25
part = ${MODEL}-ftg256-3 # last number -3 is speed grade
flashsize = 8192 # KB (kilobytes) - SPI flash device is M25P40
top_module = amiga_scarab
isedir = /opt/Xilinx/ISE/14.7/ISE_DS
xil_env = . $(isedir)/settings64.sh
SHELL = /bin/bash
# openocd_interface = interface/altera-usb-blaster.cfg
# openocd_interface = ../include/ft2232-fpu1.ocd
openocd_interface = programmer/ft2232-generic.ocd
xc3sprog_interface = ftdi

include ../include/xilinx.mk

vfiles = \
../../../Minimig_ECS/impl1/source/Agnus.v \
../../../Minimig_ECS/impl1/source/akiko.v \
../../../Minimig_ECS/impl1/source/Amber.v \
../../../Minimig_ECS/impl1/source/Audio.v \
../../../Minimig_ECS/impl1/source/Beamcounter.v \
../../../Minimig_ECS/impl1/source/Bitplanes.v \
../../../Minimig_ECS/impl1/source/Blitter.v \
../../../Minimig_ECS/impl1/source/CIA8520.v \
../../../Minimig_ECS/impl1/source/Clock.v \
../../../Minimig_ECS/impl1/source/Copper.v \
../../../Minimig_ECS/impl1/source/Denise.v \
../../../Minimig_ECS/impl1/source/Floppy.v \
../../../Minimig_ECS/impl1/source/Gary.v \
../../../Minimig_ECS/impl1/source/Gayle.v \
../../../source_emard/Minimig1.v \
../../../Minimig_ECS/impl1/source/Paula.v \
../../../source_emard/PS2Keyboard.v \
../../../source_emard/twowaycache.v \
../../../Minimig_ECS/impl1/source/Sprites.v \
../../../Minimig_ECS/impl1/source/Userio.v \

vhdfiles = \
top/amiga_scarab.vhd \
clocks/clk_50_28_140_7_116.vhd \
../../../Minimig_ECS/impl1/source/Fampiga.vhd \
../../../source_emard/cfide.vhd \
../../../Minimig_ECS/impl1/source/Debounce.vhd \
../../../Minimig_ECS/RTL/sdram_cache.vhd \
../../../Minimig_ECS/impl1/source/TG68K.vhd \
../../../Minimig_ECS/impl1/source/TG68K_ALU.vhd \
../../../Minimig_ECS/impl1/source/TG68K_Pack.vhd \
../../../Minimig_ECS/impl1/source/TG68KdotC_Kernel.vhd \
../../../Minimig_ECS/impl1/source/poweronreset.vhd \
../../../osd_bootstrap.vhd \
../../../Minimig_ECS/impl1/source/HDMI/aux_ecc1.vhd \
../../../Minimig_ECS/impl1/source/HDMI/aux_ecc2.vhd \
../../../Minimig_ECS/impl1/source/HDMI/aux_encoder.vhd \
../../../source_emard/HDMI/dvid.vhd \
../../../Minimig_ECS/impl1/source/HDMI/dvienc_defs.vhd \
../../../Minimig_ECS/impl1/source/HDMI/edvi_ucode.vhd \
../../../Minimig_ECS/impl1/source/HDMI/infoframe_rom_800x600_60hz_40M_48k.vhd \
../../../Minimig_ECS/impl1/source/HDMI/TMDS_encoder.vhd \
../../../jbboot.vhd \
../../../rtl_emard/osd/osd.vhd \
../../../rtl_emard/osd/char_rom.vhd \
../../../rtl_emard/generic/bram_true2p_1clk.vhd \
../../../rtl_emard/vga/vga.vhd \
../../../rtl_emard/vga/hdmi/vga2dvid.vhd \
../../../rtl_emard/vga/hdmi/tmds_encoder.vhd \
../../../rtl_emard/vga/hdmi/ddr_dvid_out_se.vhd \
../../../rtl_emard/vga/xilinx/xc6/ddr_out.vhd \
../../../rtl_emard/vga/xilinx/xc6/hdmi_out_xc6.vhd \
../../../rtl_emard/spdif/spdif_tx.vhd \

# ../../../rtl_emard/vga/hdmi_out.vhd \
