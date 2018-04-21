###################################################################
# Project Configuration: 
# 
# Specify the name of the design (project) and the Quartus II
# Settings File (.qsf)
###################################################################

PROJECT = project
TOP_LEVEL_ENTITY = amiga_ffm_c5a4_sd
ASSIGNMENT_FILES = $(PROJECT).qpf $(PROJECT).qsf

###################################################################
# Part, Family, Boardfile
FAMILY = "Cyclone V"
PART = 5CEBA4F23C7
#BOARDFILE = ffm-c5a4-sd-v2r0-0.board
BOARDFILE = FFM-C5A4-SD-V2r0_mit_FFC-CA7-V2r0.board
CONFIG_DEVICE = EPCS64 # fixme
SERIAL_FLASH_LOADER_DEVICE = EP4CE22 # fixme
OPENOCD_BOARD=ffm-fpga-c5a4.ocd
# OPENOCD_INTERFACE=altera-usb-blaster.ocd
OPENOCD_INTERFACE=ft4232.ocd
# OPENOCD_INTERFACE=remote.ocd
OPENOCD_SVF_CLOCK=33MHz

###################################################################
#
# Quartus shell environment vars
#
###################################################################

quartus_env ?= . ../include/quartus_env.sh

# include makefile which does it all
include ../include/altera.mk

###################################################################
# Setup your sources here
SRCS = \
top/amiga_ffm_c5a4_sd.vhd \
clocks/clk_28_280_112_112s_7.vhd \
clocks/clk_28_280_112_112s_7_0002.v \
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
../../../rtl_emard/generic/bram_true2p_1clk.vhd \
../../../rtl_emard/vga/vga.vhd \
../../../rtl_emard/vga/hdmi/vga2dvid.vhd \
../../../rtl_emard/vga/hdmi/tmds_encoder.vhd \
../../../rtl_emard/vga/hdmi/ddr_dvid_out_se.vhd \
../../../rtl_emard/vga/hdmi_out.vhd \
../../../rtl_emard/vga/generic/ddr_out_emard.vhd \
../../../rtl_emard/vga/adv/i2c_sender.vhd \
../../../rtl_emard/spdif/spdif_tx.vhd \
../../../Minimig_ECS/impl1/source/Agnus.v \
../../../Minimig_ECS/impl1/source/akiko.v \
../../../Minimig_ECS/impl1/source/Amber.v \
../../../Minimig_ECS/impl1/source/Audio.v \
../../../Minimig_ECS/impl1/source/Beamcounter.v \
../../../Minimig_ECS/impl1/source/Bitplanes.v \
../../../Minimig_ECS/impl1/source/Blitter.v \
../../../Minimig_ECS/impl1/source/BootRom.v \
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
../../../source_emard/Userio.v \
