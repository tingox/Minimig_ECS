----------------------------
-- ULX3S Top level for MINIMIG
-- http://github.com/emard
----------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

--library unisim;
--use unisim.vcomponents.all;

entity amiga_ffm_c5a4_sd is
generic
(
  C_flea_av: boolean := false; -- use original flea's hdmi audio/video, false: use emard's audio/video
  C_flea_hdmi_audio: boolean := false -- great for digital TV's but incompatible for most PC monitors
);
port
(
  clock_50a: in std_logic;
  -- RS232
  uart3_txd: out std_logic; -- rs232 txd
  uart3_rxd: in std_logic; -- rs232 rxd
  -- LED
  led: out std_logic;
  -- SD card (SPI)
  sd_m_clk, sd_m_cmd: out std_logic;
  sd_m_d: inout std_logic_vector(3 downto 0); 
  sd_m_cdet: in std_logic;
  -- SDRAM
  dr_clk: out std_logic;
  dr_cke: out std_logic;
  dr_cs_n: out std_logic;
  dr_a: out std_logic_vector(12 downto 0);
  dr_ba: out std_logic_vector(1 downto 0);
  dr_ras_n, dr_cas_n: out std_logic;
  dr_dqm: out std_logic_vector(3 downto 0);
  dr_d: inout std_logic_vector(31 downto 0);
  dr_we_n: out std_logic;
  -- FFM Module IO
  fio: inout std_logic_vector(23 downto 0);
  -- ADV7513 video chip
  dv_clk: inout std_logic;
  dv_sda: inout std_logic;
  dv_scl: inout std_logic;
  dv_int: inout std_logic;
  dv_de: inout std_logic;
  dv_hsync: inout std_logic;
  dv_vsync: inout std_logic;
  dv_spdif: inout std_logic;
  dv_mclk: inout std_logic;
  dv_i2s: inout std_logic_vector(3 downto 0);
  dv_sclk: inout std_logic;
  dv_lrclk: inout std_logic;
  dv_d: inout std_logic_vector(23 downto 0);
  -- Low-Cost HDMI video out
  vid_d_p, vid_d_n: out std_logic_vector(2 downto 0);
  vid_clk_p, vid_clk_n: out std_logic
);
end;

architecture struct of amiga_ffm_c5a4_sd is
	-- keyboard
	alias ps2_clk1 : std_logic is fio(0);
	alias ps2_data1 : std_logic is fio(1);
	signal PS_enable: std_logic; -- dummy on ulx3s v1.7.x
        -- mouse
	alias ps2_clk2 : std_logic is fio(2);
	alias ps2_data2 : std_logic is fio(3);

	alias sys_clock: std_logic is clock_50a;
	
	signal LVDS_Red: std_logic_vector(0 downto 0);
	signal LVDS_Green: std_logic_vector(0 downto 0);
	signal LVDS_Blue: std_logic_vector(0 downto 0);
	signal LVDS_ck: std_logic_vector(0 downto 0);
	signal tmds_out_clk: std_logic;
	signal tmds_out_rgb: std_logic_vector(2 downto 0);

	signal sys_reset: std_logic;

	alias mmc_dat1: std_logic is sd_m_d(1);
	alias mmc_dat2: std_logic is sd_m_d(2);
	alias mmc_n_cs: std_logic is sd_m_d(3);
	alias mmc_clk: std_logic is sd_m_clk;
	alias mmc_mosi: std_logic is sd_m_cmd;
	alias mmc_miso: std_logic is sd_m_d(0);
        
	-- END ALIASING

	signal clk  : std_logic := '0';	
	signal clk7m  : std_logic := '0';
	signal clk28m  : std_logic := '0';   
 
	signal aud_l  : std_logic;
	signal aud_r  : std_logic;  
	signal dma_1  : std_logic := '1'; 
 
	signal n_joy1   : std_logic_vector(5 downto 0);
	signal n_joy2   : std_logic_vector(5 downto 0);
 
	signal ps2k_clk_in : std_logic;
	signal ps2k_clk_out : std_logic;
	signal ps2k_dat_in : std_logic;
	signal ps2k_dat_out : std_logic;	
	signal ps2m_clk_in : std_logic;
	signal ps2m_clk_out : std_logic;
	signal ps2m_dat_in : std_logic;
	signal ps2m_dat_out : std_logic;	
 
   signal red_u     : std_logic_vector(3 downto 0);
   signal green_u   : std_logic_vector(3 downto 0);
   signal blue_u    : std_logic_vector(3 downto 0); 
 
   signal red     : std_logic_vector(7 downto 0) := (others => '0');
   signal green   : std_logic_vector(7 downto 0) := (others => '0');
   signal blue    : std_logic_vector(7 downto 0) := (others => '0');
   signal hsync   : std_logic := '0';
   signal vsync   : std_logic := '0';
   signal dvi_hsync   : std_logic := '0';
   signal dvi_vsync   : std_logic := '0';
   signal blank   : std_logic := '0';
   signal videoblank: std_logic;  
  
   signal clk_sdram, clkn_sdram: std_logic;
   signal clk_dvi  : std_logic := '0';
   signal clk_dvin : std_logic := '0'; 
 
   signal temp_we : std_logic := '0';
   signal diskoff : std_logic;
	
   signal pwm_accumulator : std_logic_vector(8 downto 0);
	
    -- signal clk_vga   : std_logic := '0';
   signal PLL_lock  : std_logic := '0';
   signal n_15khz   : std_logic := '1';

	signal VTEMP_DAC		:std_logic_vector(4 downto 0);
	signal audio_data : std_logic_vector(17 downto 0);
	signal convert_audio_data : std_logic_vector(17 downto 0);

	signal DAC_R : std_logic;
	signal DAC_L : std_logic;

	signal l_audio_ena    : boolean; 
	signal r_audio_ena    : boolean;
	
	constant cnt_div: integer:=617;                  -- Countervalue for 48khz Audio Enable,  567 for 25MHz PCLK
    signal   cnt:     integer range 0 to cnt_div-1; 
    signal   ce:      std_logic;

    signal   rightdatasum:	std_logic_vector(14 downto 0);
    signal   leftdatasum:	std_logic_vector(14 downto 0);
    signal   left_sampled:	std_logic_vector(15 downto 0);
    signal   right_sampled:	std_logic_vector(15 downto 0);
	
	 
    signal   pll_locked 	: std_logic;
    signal   reset 	: std_logic;
    signal   reset_n 	: std_logic;
    signal   reset_combo1 	: std_logic;


        -- emard audio-video and aliasing
	signal S_audio: std_logic_vector(23 downto 0) := (others => '0');
	signal S_spdif_out: std_logic;
	signal ddr_d: std_logic_vector(2 downto 0);
	signal ddr_clk: std_logic;
	signal dvid_red, dvid_green, dvid_blue, dvid_clock: std_logic_vector(1 downto 0);
	alias clk_pixel: std_logic is clk28m;
	alias clk_pixel_shift: std_logic is clk_dvi;
	alias clkn_pixel_shift: std_logic is clk_dvin;
	-- end emard AV
  signal sw: std_logic_vector(3 downto 0) := (others => '1');

begin
  -- btn(0) used as reset has inverted logic
  sys_reset <= '1'; -- '1' is not reset, '0' is reset
  mmc_dat1 <= '1';
  mmc_dat2 <= '1';

	-- Housekeeping logic for unwanted peripherals on FleaFPGA Ohm board goes here..
	-- (Note: comment out any of the following code lines if peripheral is required)

  -- Joystick bits(5-0) = fire2,fire,right,left,down,up mapped to GPIO header
  n_joy1(3)<= fio(10) ; -- up
  n_joy1(2)<= fio(11) ; -- down
  n_joy1(1)<= fio(12) ; -- left
  n_joy1(0)<= fio(13) ; -- right
  n_joy1(4)<= fio(14) ; -- fire
  n_joy1(5)<= fio(15) ; -- fire2

  n_joy2(3)<= fio(16) ; -- up
  n_joy2(2)<= fio(17) ; -- down
  n_joy2(1)<= fio(18) ; -- left 
  n_joy2(0)<= fio(19) ; -- right  
  n_joy2(4)<= fio(20) ; -- fire
  n_joy2(5)<= fio(21) ; -- fire2 

  -- Video output horizontal scanrate select 15/30kHz select via GP[BIO header
  -- n_15khz <= GP(21) ; -- Default is 30kHz video out if pin left unconnected. Connect to GND for 15kHz video.
  n_15khz <= sw(1) ; -- Default is '1' for 30kHz video out. set to '0' for 15kHz video.

  -- PS/2 Keyboard and Mouse definitions
  ps2k_dat_in<=PS2_data1;
  PS2_data1 <= '0' when ps2k_dat_out='0' else 'Z';
  ps2k_clk_in<=PS2_clk1;
  PS2_clk1 <= '0' when ps2k_clk_out='0' else 'Z';	
 
  ps2m_dat_in<=PS2_data2;
  PS2_data2 <= '0' when ps2m_dat_out='0' else 'Z';
  ps2m_clk_in<=PS2_clk2;
  PS2_clk2 <= '0' when ps2m_clk_out='0' else 'Z';	 
  
  -- User HDL project modules and port mappings go here..
  clk_pll: entity work.clk_28_280_112_112s_7
  port map
  (
    refclk     => sys_clock, -- 50 MHz
    rst        => '0',
    outclk_0   => clk28m,
    outclk_1   => clk_dvi,
    outclk_2   => clk_sdram,
    outclk_3   => open,
    outclk_4   => clk7m,
    LOCKED     => pll_locked
  );
  clk <= clk_sdram;

  reset_combo1 <= sys_reset and pll_locked;
		
  u10 : entity work.poweronreset
  port map
  ( 
    clk => clk,
    reset_button => reset_combo1,
    reset_out => reset_n
    --power_button => power_button,
    --power_hold => power_hold		
  );
  reset <= not reset_n;
		
  led <= not diskoff;

  myFampiga: entity work.Fampiga
  port map
  (
    clk => clk,
    clk7m => clk7m,
    clk28m => clk28m,
    reset_n => reset_n,
    --powerled_out=>power_leds(5 downto 4),
    diskled_out => diskoff,
    --oddled_out=>odd_leds(5), 

    -- SDRAM.  A separate shifted clock is provided by the toplevel
    sdr_addr => dr_a,
    sdr_data => dr_d(15 downto 0),
    sdr_ba => dr_ba,
    sdr_cke => dr_cke,
    sdr_dqm => dr_dqm(1 downto 0),
    sdr_cs => dr_cs_n,
    sdr_we => dr_we_n,
    sdr_cas => dr_cas_n, 
    sdr_ras => dr_ras_n,

    -- VGA 
    vga_r => red_u,
    vga_g => green_u,
    vga_b => blue_u,
    vid_blank => videoblank,
    vga_hsync => hsync,
    vga_vsync => vsync,
    n_15khz => n_15khz,

    -- PS/2
    ps2k_clk_in => ps2k_clk_in,
    ps2k_clk_out => ps2k_clk_out,
    ps2k_dat_in => ps2k_dat_in,
    ps2k_dat_out => ps2k_dat_out,
    ps2m_clk_in => ps2m_clk_in,
    ps2m_clk_out => ps2m_clk_out,
    ps2m_dat_in => ps2m_dat_in,
    ps2m_dat_out => ps2m_dat_out,

    -- Audio
    sigmaL => DAC_L,
    sigmaR => DAC_R,
    leftdatasum => leftdatasum,
    rightdatasum => rightdatasum,
		
    -- Game ports
    n_joy1 => n_joy1,
    n_joy2 => n_joy2,		
		
    -- RS232
    rs232_rxd => fio(19),
    rs232_txd => fio(18),
		
    -- ESP8266 wifi modem
    amiga_rs232_rxd => fio(22),
    amiga_rs232_txd => fio(23),
		
    -- SD card interface
    sd_cs => mmc_n_cs,
    sd_miso => mmc_miso,
    sd_mosi => mmc_mosi,
    sd_clk => mmc_clk
  );

  dr_clk <= not clk_sdram;
  dr_d(31 downto 16) <= (others => 'Z');
  dr_dqm(3 downto 2) <= (others => '1');

  flea_video: if C_flea_av generate
    -- Audio output mapped to 3.5mm jack
    --audio_r <= DAC_R;
    --audio_l <= DAC_L;
    process(clk28m)
    begin
      if rising_edge(clk28m) then
	red <= std_logic_vector(red_u) & "0000";
	green <= std_logic_vector(green_u) & "0000";
	blue <= std_logic_vector(blue_u) & "0000";  
	--blank <= hsync AND vsync;
	blank <= videoblank;	
	dvi_hsync <= hsync;
	dvi_vsync <= vsync;
      end if;
    end process;    

    left_sampled <= leftdatasum(14 downto 0) & '0';
    right_sampled <= rightdatasum(14 downto 0) & '0';

    Inst_DVI: entity work.dvid 
    --GENERIC MAP (
    --  Invert_Red => true,
    --  Invert_Green => true,
    --  Invert_Blue => true,
    --  Invert_Clock => true
    --)
    PORT MAP
    (
    clk		  => clk_dvi,
    clk_n         => clk_dvin,	 
    clk_pixel     => clk28m,
    clk_pixel_en  => true, 

    red_p         => red,
    green_p       => green,
    blue_p        => blue,
    blank         => blank,
    hsync         => dvi_hsync, 
    vsync         => dvi_vsync,
    EnhancedMode  => C_flea_hdmi_audio,
    IsProgressive => true, 
    IsPAL  	  => true, 
    Is30kHz  	  => true,
    Limited_Range => false,
    Widescreen    => true,
    HDMI_audio_L  => left_sampled,
    HDMI_audio_R  => right_sampled,
    HDMI_LeftEnable  => l_audio_ena,
    HDMI_RightEnable => l_audio_ena,
    dvid_red      => dvid_red,
    dvid_green    => dvid_green,
    dvid_blue     => dvid_blue,
    dvid_clock    => dvid_clock,
    red_s         => LVDS_Red,
    green_s       => LVDS_Green, 
    blue_s        => LVDS_Blue,
    clock_s       => LVDS_ck
    ); 
  
    process(clk28m)
    begin
    if rising_edge(clk28m) then
      if cnt=cnt_div-1 then
        ce  <= '1';
        cnt <= 0; 
      else
        ce  <= '0';
        cnt <= cnt +1 ;
      end if;
    end if;
    end process;

    process(clk28m)
    begin
    if rising_edge(clk28m) then
      if ce='1' then
        l_audio_ena <= true;
      else
        l_audio_ena <= false;
      end if;
    end if;
    end process;

    -- this module instantiates vendor specific modules ddr_out to
    -- convert SDR 2-bit input to DDR clocked 1-bit output (single-ended)
    G_vgatext_ddrout_flea: entity work.ddr_dvid_out_se
    port map
    (
    clk       => clk_pixel_shift,
    clk_n     => clkn_pixel_shift,
    in_red    => dvid_red,
    in_green  => dvid_green,
    in_blue   => dvid_blue,
    in_clock  => dvid_clock,
    out_red   => ddr_d(2),
    out_green => ddr_d(1),
    out_blue  => ddr_d(0),
    out_clock => ddr_clk
    );
  end generate;

  emard_video: if not C_flea_av generate
  no_audio: if false generate -- disable audio generation, doesn't fit on device
  S_audio(23 downto 9) <= leftdatasum(14 downto 0);
  G_spdif_out: entity work.spdif_tx
  generic map
  (
    C_clk_freq => 28000000,  -- Hz
    C_sample_freq => 48000   -- Hz
  )
  port map
  (
    clk => clk_pixel,
    data_in => S_audio,
    spdif_out => S_spdif_out
  );
  --audio_r <= DAC_R;
  --audio_l <= DAC_L;
  -- audio_v(1 downto 0) <= (others => S_spdif_out);
  end generate;

  vga2dvi_converter: entity work.vga2dvid
  generic map
  (
      C_ddr     => false,
      C_depth   => 4 -- 4bpp (4 bit per pixel)
  )
  port map
  (
      clk_pixel => clk_pixel, -- 28 MHz
      clk_shift => clk_pixel_shift, -- 5*28 MHz

      in_red   => red_u,
      in_green => green_u,
      in_blue  => blue_u,

      in_hsync => hsync,
      in_vsync => vsync,
      in_blank => videoblank,

      -- single-ended output ready for differential buffers
      out_red   => dvid_red,
      out_green => dvid_green,
      out_blue  => dvid_blue,
      out_clock => dvid_clock
  );

    -- single ended outputs simulating differential buffering for DVI clock and video
    G_sdr_dvi_out: if true generate
    dvi_output: entity work.hdmi_out
      port map
      (
        tmds_in_rgb    => dvid_red(0) & dvid_green(0) & dvid_blue(0),
        tmds_out_rgb_p => vid_d_p,   -- D2+ red  D1+ green  D0+ blue
        tmds_out_rgb_n => vid_d_n,   -- D2- red  D1- green  D0- blue
        tmds_in_clk    => dvid_clock(0),
        tmds_out_clk_p => vid_clk_p, -- CLK+ clock
        tmds_out_clk_n => vid_clk_n  -- CLK- clock
      );
    end generate;

--    dvi_out_buf: entity work.hdmi_out
--      port map
--      (
--        tmds_in_rgb    => ddr_dvid(3 downto 1),
--        tmds_out_rgb_p => vid_d_p,   -- D2+ red  D1+ green  D0+ blue
--        tmds_out_rgb_n => vid_d_n,   -- D2- red  D1- green  D0- blue
--        tmds_in_clk    => ddr_dvid(0),
--        tmds_out_clk_p => vid_clk_p, -- CLK+ clock
--        tmds_out_clk_n => vid_clk_n  -- CLK- clock
--      );

end generate; -- if not C_flea_av

    -- adv7513 routing
    dv_clk <= clk_pixel;
    dv_de <= not videoblank;
    dv_hsync <= hsync;
    dv_vsync <= vsync;
    dv_d(23 downto 20) <= red_u;
    dv_d(19 downto 16) <= (others => red_u(0));
    dv_d(15 downto 12) <= green_u;
    dv_d(11 downto 8) <= (others => green_u(0));
    dv_d(7 downto 4) <= blue_u;
    dv_d(3 downto 0) <= (others => blue_u(0));

    i2c_send: entity work.i2c_sender
      port map
      (
        clk => clk_pixel,
        resend => reset,
        sioc => dv_scl,
        siod => dv_sda
      );

end struct;
