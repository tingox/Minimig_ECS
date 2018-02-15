#!/bin/sh
rm -rf Minimig_ECS
unzip -o Minimig_ECS_Diamond_Project.zip
rm -f Minimig_ECS_Flash_Era_Prgm.vme
cd Minimig_ECS
rm -rf sin_table hdmi_upscale_buffer Cache_BlockRAM JBBoot OSDBootstrap Cache_DataRAM ddr_out flash_config.xcf Minimig_tcl.html Minimig_tcr.dir promote.xml reportview.xml sram_config-SPI_Flash_Era_Prgm.vme test.tpf ttt.txt Untitled.tpf
cd impl1
rm -rf ._Real_._Math_.vhd .build_status backup coreip dm hdla_gen_hierarchy.html Minimig_impl1.bit Minimig_impl1.dir Minimig_impl1_summary.html synlog syntmp synwork
rm -rf source/TG68_old
cd ../..
rtl_emard/tools/jbboot_bin2vhdl.py Minimig_ECS/BOOTROM1/amigaboot.bin jbboot.vhd
rtl_emard/tools/osdbootstrap_bin2vhdl.py Minimig_ECS/BOOTROM1/osdload.bin osd_bootstrap.vhd
