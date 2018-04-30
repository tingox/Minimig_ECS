#vivado=/opt/Xilinx/Vivado/2015.3/bin/vivado
project=amiga_ffm_a7100
xc3sprog_interface = ft4232h_fast
#xc3sprog_interface = ft4232h
# name of resulting bitstream file (*.bit)
bitfile=$(project).runs/impl_1/amiga_ffm_a7100.bit

junk=*~
junk+=.Xil vivado.log vivado.jou ip_upgrade.log
junk+=$(project).ip_user_files
junk+=$(project).sim
junk+=$(project).cache

junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.upgrade_log
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.v
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.vh
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.vhdl
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.dcp
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.xml
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.xdc
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.veo
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.vho
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.tcl
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.ncf
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/*.log
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/doc
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/sim
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/synth
junk+=$(project).srcs/sources_1/ip/clk_d100_112_7_28_140_280MHz/clk_wiz_v5_3_1

junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.upgrade_log
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.v
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.vh
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.vhdl
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.dcp
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.xml
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.xdc
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.veo
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.vho
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.tcl
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.ncf
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/*.log
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/doc
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/sim
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/synth
junk+=$(project).srcs/sources_1/ip/clk_d100_112sMHz/clk_wiz_v5_3_1

include ../include/vivado.mk
