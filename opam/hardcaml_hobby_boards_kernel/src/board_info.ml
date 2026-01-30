let nexys_a7_100t_dot_sexp =
  "((part xc7a100tcsg324-1)\n\
  \ (pins (\n\
  \   ; clocking\n\
  \   ((name clock_100)          (loc E3)  (iostandard LVCMOS33))\n\
  \   ((name reset_n)            (loc C12) (iostandard LVCMOS33))\n\
  \   ; accelerometer\n\
  \   ((name acl_miso)           (loc E15) (iostandard LVCMOS33))\n\
  \   ((name acl_mosi)           (loc F14) (iostandard LVCMOS33))\n\
  \   ((name acl_sclk)           (loc F15) (iostandard LVCMOS33))\n\
  \   ((name acl_csn)            (loc D15) (iostandard LVCMOS33))\n\
  \   ((name acl_int[0])         (loc B13) (iostandard LVCMOS33))\n\
  \   ((name acl_int[1])         (loc C16) (iostandard LVCMOS33))\n\
  \   ; switches\n\
  \   ((name switches[0])        (loc J15) (iostandard LVCMOS33))\n\
  \   ((name switches[1])        (loc L16) (iostandard LVCMOS33))\n\
  \   ((name switches[2])        (loc M13) (iostandard LVCMOS33))\n\
  \   ((name switches[3])        (loc R15) (iostandard LVCMOS33))\n\
  \   ((name switches[4])        (loc R17) (iostandard LVCMOS33))\n\
  \   ((name switches[5])        (loc T18) (iostandard LVCMOS33))\n\
  \   ((name switches[6])        (loc U18) (iostandard LVCMOS33))\n\
  \   ((name switches[7])        (loc R13) (iostandard LVCMOS33))\n\
  \   ((name switches[8])        (loc T8)  (iostandard LVCMOS18))\n\
  \   ((name switches[9])        (loc U8)  (iostandard LVCMOS18))\n\
  \   ((name switches[10])       (loc R16) (iostandard LVCMOS33))\n\
  \   ((name switches[11])       (loc T13) (iostandard LVCMOS33))\n\
  \   ((name switches[12])       (loc H6)  (iostandard LVCMOS33))\n\
  \   ((name switches[13])       (loc U12) (iostandard LVCMOS33))\n\
  \   ((name switches[14])       (loc U11) (iostandard LVCMOS33))\n\
  \   ((name switches[15])       (loc V10) (iostandard LVCMOS33))\n\
  \   ; microphone\n\
  \   ((name mic_clock)          (loc J5)  (iostandard LVCMOS33))\n\
  \   ((name mic_data)           (loc H5)  (iostandard LVCMOS33))\n\
  \   ((name mic_lrsel)          (loc F5)  (iostandard LVCMOS33))\n\
  \   ; audio amplifier\n\
  \   ((name aud_pwm)            (loc A11) (iostandard LVCMOS33))\n\
  \   ((name aud_sd)             (loc D12) (iostandard LVCMOS33))\n\
  \   ; seven segment display\n\
  \   ((name seven_seg_set[0])   (loc T10) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_set[1])   (loc R10) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_set[2])   (loc K16) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_set[3])   (loc K13) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_set[4])   (loc P15) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_set[5])   (loc T11) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_set[6])   (loc L18) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_set[7])   (loc H15) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_sel_n[0]) (loc J17) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_sel_n[1]) (loc J18) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_sel_n[2]) (loc T9)  (iostandard LVCMOS33))\n\
  \   ((name seven_seg_sel_n[3]) (loc J14) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_sel_n[4]) (loc P14) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_sel_n[5]) (loc T14) (iostandard LVCMOS33))\n\
  \   ((name seven_seg_sel_n[6]) (loc K2)  (iostandard LVCMOS33))\n\
  \   ((name seven_seg_sel_n[7]) (loc U13) (iostandard LVCMOS33))\n\
  \   ; 16 leds\n\
  \   ((name leds[0])            (loc H17) (iostandard LVCMOS33))\n\
  \   ((name leds[1])            (loc K15) (iostandard LVCMOS33))\n\
  \   ((name leds[2])            (loc J13) (iostandard LVCMOS33))\n\
  \   ((name leds[3])            (loc N14) (iostandard LVCMOS33))\n\
  \   ((name leds[4])            (loc R18) (iostandard LVCMOS33))\n\
  \   ((name leds[5])            (loc V17) (iostandard LVCMOS33))\n\
  \   ((name leds[6])            (loc U17) (iostandard LVCMOS33))\n\
  \   ((name leds[7])            (loc U16) (iostandard LVCMOS33))\n\
  \   ((name leds[8])            (loc V16) (iostandard LVCMOS33))\n\
  \   ((name leds[9])            (loc T15) (iostandard LVCMOS33))\n\
  \   ((name leds[10])           (loc U14) (iostandard LVCMOS33))\n\
  \   ((name leds[11])           (loc T16) (iostandard LVCMOS33))\n\
  \   ((name leds[12])           (loc V15) (iostandard LVCMOS33))\n\
  \   ((name leds[13])           (loc V14) (iostandard LVCMOS33))\n\
  \   ((name leds[14])           (loc V12) (iostandard LVCMOS33))\n\
  \   ((name leds[15])           (loc V11) (iostandard LVCMOS33))\n\
  \   ; buttons\n\
  \   ((name button_c)           (loc N17) (iostandard LVCMOS33))\n\
  \   ((name button_u)           (loc M18) (iostandard LVCMOS33))\n\
  \   ((name button_l)           (loc P17) (iostandard LVCMOS33))\n\
  \   ((name button_r)           (loc M17) (iostandard LVCMOS33))\n\
  \   ((name button_d)           (loc P18) (iostandard LVCMOS33))\n\
  \   ; qspi\n\
  \   ((name qspi_csn)           (loc L13) (iostandard LVCMOS33))\n\
  \   ((name qspi_dq[0])         (loc K17) (iostandard LVCMOS33))\n\
  \   ((name qspi_dq[1])         (loc K18) (iostandard LVCMOS33))\n\
  \   ((name qspi_dq[2])         (loc L14) (iostandard LVCMOS33))\n\
  \   ((name qspi_dq[3])         (loc M14) (iostandard LVCMOS33))\n\
  \   ; rgb leds\n\
  \   ((name rgb_led_0_r)        (loc N15) (iostandard LVCMOS33))\n\
  \   ((name rgb_led_0_g)        (loc M16) (iostandard LVCMOS33))\n\
  \   ((name rgb_led_0_b)        (loc R12) (iostandard LVCMOS33))\n\
  \   ((name rgb_led_1_r)        (loc N16) (iostandard LVCMOS33))\n\
  \   ((name rgb_led_1_g)        (loc R11) (iostandard LVCMOS33))\n\
  \   ((name rgb_led_1_b)        (loc G14) (iostandard LVCMOS33))\n\
  \   ; temperature sensor\n\
  \   ((name temp_scl)           (loc C14) (iostandard LVCMOS33))\n\
  \   ((name temp_sda)           (loc C15) (iostandard LVCMOS33))\n\
  \   ; pmods\n\
  \   ((name JA1)                (loc C17) (iostandard LVCMOS33))\n\
  \   ((name JA2)                (loc D18) (iostandard LVCMOS33))\n\
  \   ((name JA3)                (loc E18) (iostandard LVCMOS33))\n\
  \   ((name JA4)                (loc G17) (iostandard LVCMOS33))\n\
  \   ((name JA7)                (loc D17) (iostandard LVCMOS33))\n\
  \   ((name JA8)                (loc E17) (iostandard LVCMOS33))\n\
  \   ((name JA9)                (loc F18) (iostandard LVCMOS33))\n\
  \   ((name JA10)               (loc G18) (iostandard LVCMOS33))\n\
  \   ((name JB1)                (loc D14) (iostandard LVCMOS33))\n\
  \   ((name JB2)                (loc F16) (iostandard LVCMOS33))\n\
  \   ((name JB3)                (loc G16) (iostandard LVCMOS33))\n\
  \   ((name JB4)                (loc H14) (iostandard LVCMOS33))\n\
  \   ((name JB7)                (loc E16) (iostandard LVCMOS33))\n\
  \   ((name JB8)                (loc F13) (iostandard LVCMOS33))\n\
  \   ((name JB9)                (loc G13) (iostandard LVCMOS33))\n\
  \   ((name JB10)               (loc H16) (iostandard LVCMOS33))\n\
  \   ((name JC1)                (loc K1)  (iostandard LVCMOS33))\n\
  \   ((name JC2)                (loc F6)  (iostandard LVCMOS33))\n\
  \   ((name JC3)                (loc J2)  (iostandard LVCMOS33))\n\
  \   ((name JC4)                (loc G6)  (iostandard LVCMOS33))\n\
  \   ((name JC7)                (loc E7)  (iostandard LVCMOS33))\n\
  \   ((name JC8)                (loc J3)  (iostandard LVCMOS33))\n\
  \   ((name JC9)                (loc J4)  (iostandard LVCMOS33))\n\
  \   ((name JC10)               (loc E6)  (iostandard LVCMOS33))\n\
  \   ((name JD1)                (loc H4)  (iostandard LVCMOS33))\n\
  \   ((name JD2)                (loc H1)  (iostandard LVCMOS33))\n\
  \   ((name JD3)                (loc G1)  (iostandard LVCMOS33))\n\
  \   ((name JD4)                (loc G3)  (iostandard LVCMOS33))\n\
  \   ((name JD7)                (loc H2)  (iostandard LVCMOS33))\n\
  \   ((name JD8)                (loc G4)  (iostandard LVCMOS33))\n\
  \   ((name JD9)                (loc G2)  (iostandard LVCMOS33))\n\
  \   ((name JD10)               (loc F3)  (iostandard LVCMOS33))\n\
  \   ((name JXADC1)             (loc A13) (iostandard LVCMOS33))\n\
  \   ((name JXADC2)             (loc A15) (iostandard LVCMOS33))\n\
  \   ((name JXADC3)             (loc B16) (iostandard LVCMOS33))\n\
  \   ((name JXADC4)             (loc B18) (iostandard LVCMOS33))\n\
  \   ((name JXADC7)             (loc A14) (iostandard LVCMOS33))\n\
  \   ((name JXADC8)             (loc A16) (iostandard LVCMOS33))\n\
  \   ((name JXADC9)             (loc B17) (iostandard LVCMOS33))\n\
  \   ((name JXADC10)            (loc A18) (iostandard LVCMOS33))\n\
  \   ; sdcard\n\
  \   ((name SD1)                (loc D2)  (iostandard LVCMOS33))\n\
  \   ((name SD2)                (loc C1)  (iostandard LVCMOS33))\n\
  \   ((name SD3)                (loc C2)  (iostandard LVCMOS33))\n\
  \   ((name SD4)                (loc B1)  (iostandard LVCMOS33))\n\
  \   ((name SD7)                (loc E1)  (iostandard LVCMOS33))\n\
  \   ((name SD8)                (loc F1)  (iostandard LVCMOS33))\n\
  \   ((name SD9)                (loc A1)  (iostandard LVCMOS33))\n\
  \   ((name SD10)               (loc E2)  (iostandard LVCMOS33))\n\
  \   ; ps2\n\
  \   ((name ps2_clk)            (loc F4)  (iostandard LVCMOS33))\n\
  \   ((name ps2_data)           (loc B2)  (iostandard LVCMOS33))\n\
  \   ; usb uart\n\
  \   ((name usb_uart_rxd)       (loc C4)  (iostandard LVCMOS33))\n\
  \   ((name usb_uart_txd)       (loc D4)  (iostandard LVCMOS33))\n\
  \   ((name usb_uart_cts)       (loc E5)  (iostandard LVCMOS33))\n\
  \   ((name usb_uart_rts)       (loc D3)  (iostandard LVCMOS33))\n\
  \   ((name vga_r[0])           (loc A3)  (iostandard LVCMOS33))\n\
  \   ((name vga_r[1])           (loc B4)  (iostandard LVCMOS33))\n\
  \   ((name vga_r[2])           (loc C5)  (iostandard LVCMOS33))\n\
  \   ((name vga_r[3])           (loc A4)  (iostandard LVCMOS33))\n\
  \   ((name vga_g[0])           (loc C6)  (iostandard LVCMOS33))\n\
  \   ((name vga_g[1])           (loc A5)  (iostandard LVCMOS33))\n\
  \   ((name vga_g[2])           (loc B6)  (iostandard LVCMOS33))\n\
  \   ((name vga_g[3])           (loc A6)  (iostandard LVCMOS33))\n\
  \   ((name vga_b[0])           (loc B7)  (iostandard LVCMOS33))\n\
  \   ((name vga_b[1])           (loc C7)  (iostandard LVCMOS33))\n\
  \   ((name vga_b[2])           (loc D7)  (iostandard LVCMOS33))\n\
  \   ((name vga_b[3])           (loc D8)  (iostandard LVCMOS33))\n\
  \   ((name vga_hs)             (loc B11) (iostandard LVCMOS33))\n\
  \   ((name vga_vs)             (loc B12) (iostandard LVCMOS33))\n\
  \   ; ethernet\n\
  \   ((name eth_mdc)            (loc C9)  (iostandard LVCMOS33))\n\
  \   ((name eth_mdio)           (loc A9)  (iostandard LVCMOS33))\n\
  \   ((name eth_rstn)           (loc B3)  (iostandard LVCMOS33))\n\
  \   ((name eth_crsdv)          (loc D9)  (iostandard LVCMOS33))\n\
  \   ((name eth_rxerr)          (loc C10) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[0])         (loc C11) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[1])         (loc D10) (iostandard LVCMOS33))\n\
  \   ((name eth_txen)           (loc B9)  (iostandard LVCMOS33))\n\
  \   ((name eth_txd[0])         (loc A10) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[1])         (loc A8)  (iostandard LVCMOS33))\n\
  \   ((name eth_refclk)         (loc D5)  (iostandard LVCMOS33))\n\
  \   ((name eth_intn)           (loc B8)  (iostandard LVCMOS33)))))\n"
;;

let arty_a7_35_dot_sexp =
  "((part xc7a35ticsg324-1L)\n\
  \ (pins (\n\
  \   ((name       clock_100)\n\
  \    (loc        E3)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       dip_switches_4bits_tri_i[0])\n\
  \    (loc        A8)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       dip_switches_4bits_tri_i[1])\n\
  \    (loc        C11)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       dip_switches_4bits_tri_i[2])\n\
  \    (loc        C10)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       dip_switches_4bits_tri_i[3])\n\
  \    (loc        A10)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name eth_col)            (loc D17) (iostandard LVCMOS33))\n\
  \   ((name eth_crs)            (loc G14) (iostandard LVCMOS33))\n\
  \   ((name eth_mdc)            (loc F16) (iostandard LVCMOS33))\n\
  \   ((name eth_mdio_i)         (loc K13) (iostandard LVCMOS33))\n\
  \   ((name eth_rstn)           (loc C16) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[0])         (loc D18) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[1])         (loc E17) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[2])         (loc E18) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[3])         (loc G17) (iostandard LVCMOS33))\n\
  \   ((name eth_rx_clk)         (loc F15) (iostandard LVCMOS33))\n\
  \   ((name eth_rx_dv)          (loc G16) (iostandard LVCMOS33))\n\
  \   ((name eth_rx_er)          (loc C17) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[0])         (loc H14) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[1])         (loc J14) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[2])         (loc J13) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[3])         (loc H17) (iostandard LVCMOS33))\n\
  \   ((name eth_tx_clk)         (loc H16) (iostandard LVCMOS33))\n\
  \   ((name eth_tx_en)          (loc H15) (iostandard LVCMOS33))\n\
  \   ((name i2c_pullup[0])      (loc A14) (iostandard LVCMOS33))\n\
  \   ((name i2c_pullup[1])      (loc A13) (iostandard LVCMOS33))\n\
  \   ((name i2c_scl_i)          (loc L18) (iostandard LVCMOS33))\n\
  \   ((name i2c_sda_i)          (loc M18) (iostandard LVCMOS33))\n\
  \   ((name led_4bits_tri_o[0]) (loc H5)  (iostandard LVCMOS33))\n\
  \   ((name led_4bits_tri_o[1]) (loc J5)  (iostandard LVCMOS33))\n\
  \   ((name led_4bits_tri_o[2]) (loc T9)  (iostandard LVCMOS33))\n\
  \   ((name led_4bits_tri_o[3]) (loc T10) (iostandard LVCMOS33))\n\
  \   ((name       push_buttons_4bits_tri_i[0])\n\
  \    (loc        D9)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       push_buttons_4bits_tri_i[1])\n\
  \    (loc        C9)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       push_buttons_4bits_tri_i[2])\n\
  \    (loc        B9)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       push_buttons_4bits_tri_i[3])\n\
  \    (loc        B8)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name qspi_csn_i)        (loc L13) (iostandard LVCMOS33))\n\
  \   ((name qspi_db0_i)        (loc K17) (iostandard LVCMOS33))\n\
  \   ((name qspi_db1_i)        (loc K18) (iostandard LVCMOS33))\n\
  \   ((name qspi_db2_i)        (loc L14) (iostandard LVCMOS33))\n\
  \   ((name qspi_db3_i)        (loc M14) (iostandard LVCMOS33))\n\
  \   ((name qspi_sclk_i)       (loc L16) (iostandard LVCMOS33))\n\
  \   ((name reset_n)           (loc C2)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[0])  (loc E1)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[1])  (loc F6)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[2])  (loc G6)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[3])  (loc G4)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[4])  (loc J4)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[5])  (loc G3)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[6])  (loc H4)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[7])  (loc J2)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[8])  (loc J3)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[9])  (loc K2)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[10]) (loc H6)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[11]) (loc K1)  (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[0])\n\
  \    (loc        V15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[1])\n\
  \    (loc        U16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[2])\n\
  \    (loc        P14)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[3])\n\
  \    (loc        T11)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[4])\n\
  \    (loc        R12)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[5])\n\
  \    (loc        T14)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[6])\n\
  \    (loc        T15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[7])\n\
  \    (loc        T16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[8])\n\
  \    (loc        N15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[9])\n\
  \    (loc        M16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[10])\n\
  \    (loc        C1)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[11])\n\
  \    (loc        U18)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[12])\n\
  \    (loc        R17)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[13])\n\
  \    (loc        P17)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[14])\n\
  \    (loc        F5)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[15])\n\
  \    (loc        D8)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[16])\n\
  \    (loc        C7)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[17])\n\
  \    (loc        E7)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[18])\n\
  \    (loc        D7)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[19])\n\
  \    (loc        D5)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[0])\n\
  \    (loc        U11)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[1])\n\
  \    (loc        V16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[2])\n\
  \    (loc        M13)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[3])\n\
  \    (loc        R10)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[4])\n\
  \    (loc        R11)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[5])\n\
  \    (loc        R13)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[6])\n\
  \    (loc        R15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[7])\n\
  \    (loc        P15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[8])\n\
  \    (loc        R16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[9])\n\
  \    (loc        N16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[10])\n\
  \    (loc        N14)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[11])\n\
  \    (loc        U17)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[12])\n\
  \    (loc        T18)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[13])\n\
  \    (loc        R18)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[14])\n\
  \    (loc        P18)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[15])\n\
  \    (loc        N17)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name spi_miso_i)   (loc G1)  (iostandard LVCMOS33))\n\
  \   ((name spi_mosi_i)   (loc H1)  (iostandard LVCMOS33))\n\
  \   ((name spi_sclk_i)   (loc F1)  (iostandard LVCMOS33))\n\
  \   ((name spi_ss_i)     (loc V17) (iostandard LVCMOS33))\n\
  \   ((name usb_uart_rxd) (loc A9)  (iostandard LVCMOS33))\n\
  \   ((name usb_uart_txd) (loc D10) (iostandard LVCMOS33))\n\
  \   ((name JA1)          (loc G13) (iostandard LVCMOS33))\n\
  \   ((name JA2)          (loc B11) (iostandard LVCMOS33))\n\
  \   ((name JA3)          (loc A11) (iostandard LVCMOS33))\n\
  \   ((name JA4)          (loc D12) (iostandard LVCMOS33))\n\
  \   ((name JA7)          (loc D13) (iostandard LVCMOS33))\n\
  \   ((name JA8)          (loc B18) (iostandard LVCMOS33))\n\
  \   ((name JA9)          (loc A18) (iostandard LVCMOS33))\n\
  \   ((name JA10)         (loc K16) (iostandard LVCMOS33))\n\
  \   ((name JB1)          (loc E15) (iostandard LVCMOS33))\n\
  \   ((name JB2)          (loc E16) (iostandard LVCMOS33))\n\
  \   ((name JB3)          (loc D15) (iostandard LVCMOS33))\n\
  \   ((name JB4)          (loc C15) (iostandard LVCMOS33))\n\
  \   ((name JB7)          (loc J17) (iostandard LVCMOS33))\n\
  \   ((name JB8)          (loc J18) (iostandard LVCMOS33))\n\
  \   ((name JB9)          (loc K15) (iostandard LVCMOS33))\n\
  \   ((name JB10)         (loc J15) (iostandard LVCMOS33))\n\
  \   ((name JC1)          (loc U12) (iostandard LVCMOS33))\n\
  \   ((name JC2)          (loc V12) (iostandard LVCMOS33))\n\
  \   ((name JC3)          (loc V10) (iostandard LVCMOS33))\n\
  \   ((name JC4)          (loc V11) (iostandard LVCMOS33))\n\
  \   ((name JC7)          (loc U14) (iostandard LVCMOS33))\n\
  \   ((name JC8)          (loc V14) (iostandard LVCMOS33))\n\
  \   ((name JC9)          (loc T13) (iostandard LVCMOS33))\n\
  \   ((name JC10)         (loc U13) (iostandard LVCMOS33))\n\
  \   ((name JD1)          (loc D4)  (iostandard LVCMOS33))\n\
  \   ((name JD2)          (loc D3)  (iostandard LVCMOS33))\n\
  \   ((name JD3)          (loc F4)  (iostandard LVCMOS33))\n\
  \   ((name JD4)          (loc F3)  (iostandard LVCMOS33))\n\
  \   ((name JD7)          (loc E2)  (iostandard LVCMOS33))\n\
  \   ((name JD8)          (loc D2)  (iostandard LVCMOS33))\n\
  \   ((name JD9)          (loc H2)  (iostandard LVCMOS33))\n\
  \   ((name JD10)         (loc G2)  (iostandard LVCMOS33)))))\n"
;;

let arty_a7_100_dot_sexp =
  "((part xc7a100tcsg324-1L)\n\
  \ (pins (\n\
  \   ((name       clock_100)\n\
  \    (loc        E3)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       dip_switches_4bits_tri_i[0])\n\
  \    (loc        A8)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       dip_switches_4bits_tri_i[1])\n\
  \    (loc        C11)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       dip_switches_4bits_tri_i[2])\n\
  \    (loc        C10)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       dip_switches_4bits_tri_i[3])\n\
  \    (loc        A10)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name eth_col)            (loc D17) (iostandard LVCMOS33))\n\
  \   ((name eth_crs)            (loc G14) (iostandard LVCMOS33))\n\
  \   ((name eth_mdc)            (loc F16) (iostandard LVCMOS33))\n\
  \   ((name eth_mdio_i)         (loc K13) (iostandard LVCMOS33))\n\
  \   ((name eth_rstn)           (loc C16) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[0])         (loc D18) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[1])         (loc E17) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[2])         (loc E18) (iostandard LVCMOS33))\n\
  \   ((name eth_rxd[3])         (loc G17) (iostandard LVCMOS33))\n\
  \   ((name eth_rx_clk)         (loc F15) (iostandard LVCMOS33))\n\
  \   ((name eth_rx_dv)          (loc G16) (iostandard LVCMOS33))\n\
  \   ((name eth_rx_er)          (loc C17) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[0])         (loc H14) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[1])         (loc J14) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[2])         (loc J13) (iostandard LVCMOS33))\n\
  \   ((name eth_txd[3])         (loc H17) (iostandard LVCMOS33))\n\
  \   ((name eth_tx_clk)         (loc H16) (iostandard LVCMOS33))\n\
  \   ((name eth_tx_en)          (loc H15) (iostandard LVCMOS33))\n\
  \   ((name i2c_pullup[0])      (loc A14) (iostandard LVCMOS33))\n\
  \   ((name i2c_pullup[1])      (loc A13) (iostandard LVCMOS33))\n\
  \   ((name i2c_scl_i)          (loc L18) (iostandard LVCMOS33))\n\
  \   ((name i2c_sda_i)          (loc M18) (iostandard LVCMOS33))\n\
  \   ((name led_4bits_tri_o[0]) (loc H5)  (iostandard LVCMOS33))\n\
  \   ((name led_4bits_tri_o[1]) (loc J5)  (iostandard LVCMOS33))\n\
  \   ((name led_4bits_tri_o[2]) (loc T9)  (iostandard LVCMOS33))\n\
  \   ((name led_4bits_tri_o[3]) (loc T10) (iostandard LVCMOS33))\n\
  \   ((name       push_buttons_4bits_tri_i[0])\n\
  \    (loc        D9)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       push_buttons_4bits_tri_i[1])\n\
  \    (loc        C9)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       push_buttons_4bits_tri_i[2])\n\
  \    (loc        B9)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       push_buttons_4bits_tri_i[3])\n\
  \    (loc        B8)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name qspi_csn_i)        (loc L13) (iostandard LVCMOS33))\n\
  \   ((name qspi_db0_i)        (loc K17) (iostandard LVCMOS33))\n\
  \   ((name qspi_db1_i)        (loc K18) (iostandard LVCMOS33))\n\
  \   ((name qspi_db2_i)        (loc L14) (iostandard LVCMOS33))\n\
  \   ((name qspi_db3_i)        (loc M14) (iostandard LVCMOS33))\n\
  \   ((name qspi_sclk_i)       (loc L16) (iostandard LVCMOS33))\n\
  \   ((name reset_n)           (loc C2)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[0])  (loc E1)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[1])  (loc F6)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[2])  (loc G6)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[3])  (loc G4)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[4])  (loc J4)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[5])  (loc G3)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[6])  (loc H4)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[7])  (loc J2)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[8])  (loc J3)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[9])  (loc K2)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[10]) (loc H6)  (iostandard LVCMOS33))\n\
  \   ((name rgb_led_tri_o[11]) (loc K1)  (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[0])\n\
  \    (loc        V15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[1])\n\
  \    (loc        U16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[2])\n\
  \    (loc        P14)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[3])\n\
  \    (loc        T11)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[4])\n\
  \    (loc        R12)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[5])\n\
  \    (loc        T14)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[6])\n\
  \    (loc        T15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[7])\n\
  \    (loc        T16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[8])\n\
  \    (loc        N15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[9])\n\
  \    (loc        M16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[10])\n\
  \    (loc        C1)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[11])\n\
  \    (loc        U18)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[12])\n\
  \    (loc        R17)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[13])\n\
  \    (loc        P17)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[14])\n\
  \    (loc        F5)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[15])\n\
  \    (loc        D8)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[16])\n\
  \    (loc        C7)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[17])\n\
  \    (loc        E7)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[18])\n\
  \    (loc        D7)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp0_dp19_tri_i[19])\n\
  \    (loc        D5)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[0])\n\
  \    (loc        U11)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[1])\n\
  \    (loc        V16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[2])\n\
  \    (loc        M13)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[3])\n\
  \    (loc        R10)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[4])\n\
  \    (loc        R11)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[5])\n\
  \    (loc        R13)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[6])\n\
  \    (loc        R15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[7])\n\
  \    (loc        P15)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[8])\n\
  \    (loc        R16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[9])\n\
  \    (loc        N16)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[10])\n\
  \    (loc        N14)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[11])\n\
  \    (loc        U17)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[12])\n\
  \    (loc        T18)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[13])\n\
  \    (loc        R18)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[14])\n\
  \    (loc        P18)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name       shield_dp26_dp41_tri_i[15])\n\
  \    (loc        N17)\n\
  \    (iostandard LVCMOS33))\n\
  \   ((name spi_miso_i)   (loc G1)  (iostandard LVCMOS33))\n\
  \   ((name spi_mosi_i)   (loc H1)  (iostandard LVCMOS33))\n\
  \   ((name spi_sclk_i)   (loc F1)  (iostandard LVCMOS33))\n\
  \   ((name spi_ss_i)     (loc V17) (iostandard LVCMOS33))\n\
  \   ((name usb_uart_rxd) (loc A9)  (iostandard LVCMOS33))\n\
  \   ((name usb_uart_txd) (loc D10) (iostandard LVCMOS33))\n\
  \   ((name JA1)          (loc G13) (iostandard LVCMOS33))\n\
  \   ((name JA2)          (loc B11) (iostandard LVCMOS33))\n\
  \   ((name JA3)          (loc A11) (iostandard LVCMOS33))\n\
  \   ((name JA4)          (loc D12) (iostandard LVCMOS33))\n\
  \   ((name JA7)          (loc D13) (iostandard LVCMOS33))\n\
  \   ((name JA8)          (loc B18) (iostandard LVCMOS33))\n\
  \   ((name JA9)          (loc A18) (iostandard LVCMOS33))\n\
  \   ((name JA10)         (loc K16) (iostandard LVCMOS33))\n\
  \   ((name JB1)          (loc E15) (iostandard LVCMOS33))\n\
  \   ((name JB2)          (loc E16) (iostandard LVCMOS33))\n\
  \   ((name JB3)          (loc D15) (iostandard LVCMOS33))\n\
  \   ((name JB4)          (loc C15) (iostandard LVCMOS33))\n\
  \   ((name JB7)          (loc J17) (iostandard LVCMOS33))\n\
  \   ((name JB8)          (loc J18) (iostandard LVCMOS33))\n\
  \   ((name JB9)          (loc K15) (iostandard LVCMOS33))\n\
  \   ((name JB10)         (loc J15) (iostandard LVCMOS33))\n\
  \   ((name JC1)          (loc U12) (iostandard LVCMOS33))\n\
  \   ((name JC2)          (loc V12) (iostandard LVCMOS33))\n\
  \   ((name JC3)          (loc V10) (iostandard LVCMOS33))\n\
  \   ((name JC4)          (loc V11) (iostandard LVCMOS33))\n\
  \   ((name JC7)          (loc U14) (iostandard LVCMOS33))\n\
  \   ((name JC8)          (loc V14) (iostandard LVCMOS33))\n\
  \   ((name JC9)          (loc T13) (iostandard LVCMOS33))\n\
  \   ((name JC10)         (loc U13) (iostandard LVCMOS33))\n\
  \   ((name JD1)          (loc D4)  (iostandard LVCMOS33))\n\
  \   ((name JD2)          (loc D3)  (iostandard LVCMOS33))\n\
  \   ((name JD3)          (loc F4)  (iostandard LVCMOS33))\n\
  \   ((name JD4)          (loc F3)  (iostandard LVCMOS33))\n\
  \   ((name JD7)          (loc E2)  (iostandard LVCMOS33))\n\
  \   ((name JD8)          (loc D2)  (iostandard LVCMOS33))\n\
  \   ((name JD9)          (loc H2)  (iostandard LVCMOS33))\n\
  \   ((name JD10)         (loc G2)  (iostandard LVCMOS33)))))\n"
;;

let by_filename =
  [ "arty-a7-100.sexp", arty_a7_100_dot_sexp
  ; "arty-a7-35.sexp", arty_a7_35_dot_sexp
  ; "nexys-a7-100t.sexp", nexys_a7_100t_dot_sexp
  ]
;;
