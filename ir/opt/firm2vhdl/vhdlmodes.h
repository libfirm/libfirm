#ifndef VHDLMODES_H
#define VHDLMODES_H

void init_vhdl_modes(void);

#define vhdl_decl_mode(bits)						\
  extern ir_mode *mode_V ## bits ## u;					\
  ir_mode *get_modeV ## bits ## u (void);				\
  extern ir_mode *mode_V ## bits ## s;					\
  ir_mode *get_modeV ## bits ## s (void);

vhdl_decl_mode(1)
vhdl_decl_mode(2)
vhdl_decl_mode(3)
vhdl_decl_mode(4)
vhdl_decl_mode(5)
vhdl_decl_mode(6)
vhdl_decl_mode(7)
vhdl_decl_mode(8)
vhdl_decl_mode(9)
vhdl_decl_mode(10)
vhdl_decl_mode(11)
vhdl_decl_mode(12)
vhdl_decl_mode(13)
vhdl_decl_mode(14)
vhdl_decl_mode(15)
vhdl_decl_mode(16)
vhdl_decl_mode(17)
vhdl_decl_mode(18)
vhdl_decl_mode(19)
vhdl_decl_mode(20)
vhdl_decl_mode(21)
vhdl_decl_mode(22)
vhdl_decl_mode(23)
vhdl_decl_mode(24)
vhdl_decl_mode(25)
vhdl_decl_mode(26)
vhdl_decl_mode(27)
vhdl_decl_mode(28)
vhdl_decl_mode(29)
vhdl_decl_mode(30)
vhdl_decl_mode(31)
vhdl_decl_mode(32)
vhdl_decl_mode(33)
vhdl_decl_mode(34)
vhdl_decl_mode(35)
vhdl_decl_mode(36)
vhdl_decl_mode(37)
vhdl_decl_mode(38)
vhdl_decl_mode(39)
vhdl_decl_mode(40)
vhdl_decl_mode(41)
vhdl_decl_mode(42)
vhdl_decl_mode(43)
vhdl_decl_mode(44)
vhdl_decl_mode(45)
vhdl_decl_mode(46)
vhdl_decl_mode(47)
vhdl_decl_mode(48)
vhdl_decl_mode(49)
vhdl_decl_mode(50)
vhdl_decl_mode(51)
vhdl_decl_mode(52)
vhdl_decl_mode(53)
vhdl_decl_mode(54)
vhdl_decl_mode(55)
vhdl_decl_mode(56)
vhdl_decl_mode(57)
vhdl_decl_mode(58)
vhdl_decl_mode(59)
vhdl_decl_mode(60)
vhdl_decl_mode(61)
vhdl_decl_mode(62)
vhdl_decl_mode(63)
vhdl_decl_mode(64)

#undef vhdl_decl_mode

#endif
