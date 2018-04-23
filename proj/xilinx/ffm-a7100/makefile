#vivado=/opt/Xilinx/Vivado/2015.3/bin/vivado
project=amiga_ffm_a7100
xc3sprog_interface = ft4232h_fast
#xc3sprog_interface = ft4232h
# name of resulting bitstream file (*.bit)
bitfile=$(project).runs/impl_1/_xram_axiram_ddr3.bit

junk=*~
junk+=.Xil vivado.log vivado.jou ip_upgrade.log
junk+=$(project).ip_user_files
junk+=$(project).sim
junk+=$(project).cache

junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.upgrade_log
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.v
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.vh
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.vhdl
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.dcp
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.xml
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.xdc
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.veo
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.vho
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.tcl
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.ncf
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/*.log
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/doc
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/sim
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/synth
junk+=$(project).srcs/sources_1/ip/clk_d100_7_28_140_280_120MHz/clk_wiz_v5_3_1

include ../include/vivado.mk
