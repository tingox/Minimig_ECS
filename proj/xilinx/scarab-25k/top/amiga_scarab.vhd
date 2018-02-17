----------------------------
-- ULX3S Top level for MINIMIG
-- http://github.com/emard
----------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

entity amiga_scarab is
generic
(
  C_flea_av: boolean := false; -- use original flea's hdmi audio/video, false: use emard's audio/video
  C_flea_hdmi_audio: boolean := false -- great for digital TV's but incompatible for most PC monitors
);
port (
  clk_50MHz: in std_logic;
  sdram_clk   : out std_logic;
  sdram_cke   : out std_logic;
  sdram_csn   : out std_logic;
  sdram_rasn  : out std_logic;
  sdram_casn  : out std_logic;
  sdram_wen   : out std_logic;
  sdram_a     : out std_logic_vector (12 downto 0);
  sdram_ba    : out std_logic_vector(1 downto 0);
  sdram_dqm   : out std_logic_vector(1 downto 0);
  sdram_d     : inout std_logic_vector (15 downto 0);
  rs232_tx: out std_logic;
  rs232_rx: in std_logic;
  --flash_cs, flash_cclk, flash_mosi: out std_logic;
  --flash_miso: in std_logic;
  sd_clk, sd_cd_dat3, sd_cmd: out std_logic;
  sd_dat0: in std_logic;
  sd_dat1, sd_dat2: inout std_logic := 'Z'; -- not used is SPI mode
  leds: out std_logic_vector(7 downto 0);
  porta, portb: inout std_logic_vector(11 downto 0);
  portc: inout std_logic_vector(11 downto 0);
  portd: inout std_logic_vector(3 downto 0); -- fm and cw antennas are here
  porte, portf: inout std_logic_vector(11 downto 0);
  audio_l, audio_r: out std_logic := '0'; -- 3.5mm audio jack
  --TMDS_in_P, TMDS_in_N: out std_logic_vector(2 downto 0);
  --TMDS_in_CLK_P, TMDS_in_CLK_N: out std_logic;
  --FPGA_SDA, FPGA_SCL: inout std_logic; -- i2c on TMDS_in
  TMDS_out_P, TMDS_out_N: out std_logic_vector(2 downto 0);
  TMDS_out_CLK_P, TMDS_out_CLK_N: out std_logic;
  sw: in std_logic_vector(4 downto 1)
);
end;

architecture struct of amiga_scarab is
	-- FLEA OHM aliasing
	-- keyboard
	alias ps2_clk1 : std_logic is porta(0);
	alias ps2_data1 : std_logic is porta(1);
	signal PS_enable: std_logic; -- dummy on ulx3s v1.7.x
        -- mouse
	alias ps2_clk2 : std_logic is porta(2);
	alias ps2_data2 : std_logic is porta(3);

	alias sys_clock: std_logic is clk_50MHz;
	
	signal LVDS_Red: std_logic_vector(0 downto 0);
	signal LVDS_Green: std_logic_vector(0 downto 0);
	signal LVDS_Blue: std_logic_vector(0 downto 0);
	signal LVDS_ck: std_logic_vector(0 downto 0);
	signal tmds_out_clk: std_logic;
	signal tmds_out_rgb: std_logic_vector(2 downto 0);

	signal sys_reset: std_logic;

	alias mmc_dat1: std_logic is sd_dat1;
	alias mmc_dat2: std_logic is sd_dat2;
	alias mmc_n_cs: std_logic is sd_cd_dat3;
	alias mmc_clk: std_logic is sd_clk;
	alias mmc_mosi: std_logic is sd_cmd;
	alias mmc_miso: std_logic is sd_dat0;
        
	-- END FLEA OHM ALIASING


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

begin
  -- btn(0) used as reset has inverted logic
  sys_reset <= '1'; -- '1' is not reset, '0' is reset
  leds(0) <= sys_reset;
  sd_dat1 <= '1';
  sd_dat2 <= '1';
  leds(6 downto 1) <= (others => '0');

	-- Housekeeping logic for unwanted peripherals on FleaFPGA Ohm board goes here..
	-- (Note: comment out any of the following code lines if peripheral is required)

  -- Joystick bits(5-0) = fire2,fire,right,left,down,up mapped to GPIO header
  n_joy1(3)<= portb(0) ; -- up
  n_joy1(2)<= portb(1) ; -- down
  n_joy1(1)<= portb(2) ; -- left
  n_joy1(0)<= portb(3) ; -- right
  n_joy1(4)<= portb(4) ; -- fire
  n_joy1(5)<= portb(5) ; -- fire2

  n_joy2(3)<= portb(6) ; -- up
  n_joy2(2)<= portb(7) ; -- down
  n_joy2(1)<= portb(8) ; -- left 
  n_joy2(0)<= portb(9) ; -- right  
  n_joy2(4)<= portb(10) ; -- fire
  n_joy2(5)<= portb(11) ; -- fire2 

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
  u0 : entity work.clk_50_28_140_7_116
  port map
  (
    in_50M     => clk_50MHz,
    reset      => '0',
    out_7M     => clk7m,
    out_28M    => clk28m,
    out_116M66 => clk_sdram,
    out_140Mp  => clk_dvi,
    out_140Mn  => clk_dvin,
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
		
  leds(7) <= not diskoff;

  myFampiga: entity work.Fampiga
  port map(
    clk=> clk,
    clk7m=> clk7m,
    clk28m=> clk28m,
    reset_n=>reset_n,
    --powerled_out=>power_leds(5 downto 4),
    diskled_out=>diskoff,
    --oddled_out=>odd_leds(5), 

		-- SDRAM.  A separate shifted clock is provided by the toplevel
		sdr_addr => sdram_a,
		sdr_data => sdram_d,
		sdr_ba => sdram_ba,
                sdr_cke => sdram_cke,
		sdr_dqm => sdram_dqm,
		sdr_cs => sdram_csn,
		sdr_we => sdram_wen,
		sdr_cas => sdram_casn, 
		sdr_ras => sdram_rasn,
	 
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
		rs232_rxd => rs232_rx,
		rs232_txd => rs232_tx,
		
		-- ESP8266 wifi modem
		amiga_rs232_rxd => portf(0),
		amiga_rs232_txd => portf(1),
		
		-- SD card interface
		sd_cs => mmc_n_cs,
		sd_miso => mmc_miso,
		sd_mosi => mmc_mosi,
		sd_clk => mmc_clk
  );
  -- SDRAM clock output needs special routing on Spartan-6
  clkn_sdram <= not clk_sdram;
  sdram_clk_forward : ODDR2
      generic map
      (
        DDR_ALIGNMENT => "NONE", INIT => '0', SRTYPE => "SYNC"
      )
      port map
      (
        Q => sdram_clk, C0 => clk_sdram, C1 => clkn_sdram, CE => '1',
        R => '0', S => '0', D0 => '0', D1 => '1'
      );


  flea_video: if C_flea_av generate
    -- Audio output mapped to 3.5mm jack
    audio_r <= DAC_R;
    audio_l <= DAC_L;
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
      C_ddr     => true,
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

  -- this module instantiates vendor specific modules ddr_out to
  -- convert SDR 2-bit input to DDR clocked 1-bit output (single-ended)
  G_vgatext_ddrout: entity work.ddr_dvid_out_se
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

    -- differential output buffering for HDMI clock and video
    hdmi_output0: entity work.hdmi_out
      port map
      (
        tmds_in_clk    => tmds_out_clk,
        tmds_out_clk_p => tmds_out_clk_p,
        tmds_out_clk_n => tmds_out_clk_n,
        tmds_in_rgb    => tmds_out_rgb,
        tmds_out_rgb_p => tmds_out_p,
        tmds_out_rgb_n => tmds_out_n
      );
end generate;

end struct;
