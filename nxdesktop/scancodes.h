/**************************************************************************/
/*                                                                        */
/* Copyright (c) 2001,2006 NoMachine, http://www.nomachine.com.           */
/*                                                                        */
/* NXDESKTOP, NX protocol compression and NX extensions to this software  */
/* are copyright of NoMachine. Redistribution and use of the present      */
/* software is allowed according to terms specified in the file LICENSE   */
/* which comes in the source distribution.                                */
/*                                                                        */
/* Check http://www.nomachine.com/licensing.html for applicability.       */
/*                                                                        */
/* NX and NoMachine are trademarks of Medialogic S.p.A.                   */
/*                                                                        */
/* All rights reserved.                                                   */
/*                                                                        */
/**************************************************************************/

/* Two defines for every scancode:
   One called SCANCODE_KEY_<num>, where <num> is the key location number. 
   One called SCANCODE_CHAR_<char-on-us-kbd>, where <char-on-us-kbd> is the 
   unshifted character on a US 101/102 keyboard. See the Microsoft 
   document "Keyboard Scan Code Specification" for more information. 

   Up keys normally have scancode values +0x80. 

   0x0: Avoid 
   0x1- 0x59: Normal
   0x60, 0x61: Avoid, since the up key would be 0xe1, 0xe1
   0x61 - 0x79: Normal
   0xfa - : keyboard drivers interpret these as responses from the 8042 chip

   We use bit 7 to indicate 0xe0 prefix instead of two-byte sequence (0xe0, something). Eq.,
   0xe 0x38 is defined as (0x80 | 0x38)
*/

/* FIXME: 
   Special keys like Numeric / are very complicated and emits many scancodes. 
*/

#define SCANCODE_EXTENDED 0x80

#define SCANCODE_KEY_1 0x29
#define SCANCODE_CHAR_GRAVE 0x29

#define SCANCODE_KEY_2 0x2
#define SCANCODE_CHAR_1 SCANCODE_KEY_2

#define SCANCODE_KEY_3 0x3
#define SCANCODE_CHAR_2 SCANCODE_KEY_3

#define SCANCODE_KEY_4 0x4
#define SCANCODE_CHAR_3 SCANCODE_KEY_4

#define SCANCODE_KEY_5 0x5
#define SCANCODE_CHAR_4 SCANCODE_KEY_5

#define SCANCODE_KEY_6 0x6
#define SCANCODE_CHAR_5 SCANCODE_KEY_6

#define SCANCODE_KEY_7 0x7
#define SCANCODE_CHAR_6 SCANCODE_KEY_7

#define SCANCODE_KEY_8 0x8
#define SCANCODE_CHAR_7 SCANCODE_KEY_8

#define SCANCODE_KEY_9 0x9
#define SCANCODE_CHAR_8 SCANCODE_KEY_9

#define SCANCODE_KEY_10 0xa
#define SCANCODE_CHAR_9 SCANCODE_KEY_10

#define SCANCODE_KEY_11 0xb
#define SCANCODE_CHAR_0 SCANCODE_KEY_11

#define SCANCODE_KEY_12 0xc
#define SCANCODE_CHAR_MINUS SCANCODE_KEY_12

#define SCANCODE_KEY_13 0xd
#define SCANCODE_CHAR_EQUAL SCANCODE_KEY_13

/* Key 14 does not exist */

#define SCANCODE_KEY_15 0xe
#define SCANCODE_CHAR_BACKSPACE SCANCODE_KEY_15

#define SCANCODE_KEY_16 0xf
#define SCANCODE_CHAR_TAB SCANCODE_KEY_16

#define SCANCODE_KEY_17 0x10
#define SCANCODE_CHAR_Q SCANCODE_KEY_17

#define SCANCODE_KEY_18 0x11
#define SCANCODE_CHAR_W SCANCODE_KEY_18

#define SCANCODE_KEY_19 0x12
#define SCANCODE_CHAR_E SCANCODE_KEY_19

#define SCANCODE_KEY_20 0x13
#define SCANCODE_CHAR_R SCANCODE_KEY_20

#define SCANCODE_KEY_21 0x14
#define SCANCODE_CHAR_T SCANCODE_KEY_21

#define SCANCODE_KEY_22 0x15
#define SCANCODE_CHAR_Y SCANCODE_KEY_22

#define SCANCODE_KEY_23 0x16
#define SCANCODE_CHAR_U SCANCODE_KEY_23

#define SCANCODE_KEY_24 0x17
#define SCANCODE_CHAR_I SCANCODE_KEY_24

#define SCANCODE_KEY_25 0x18
#define SCANCODE_CHAR_O SCANCODE_KEY_25

#define SCANCODE_KEY_26 0x19
#define SCANCODE_CHAR_P SCANCODE_KEY_26

#define SCANCODE_KEY_27 0x1a
#define SCANCODE_CHAR_BRACKETLEFT SCANCODE_KEY_27

#define SCANCODE_KEY_28 0x1b
#define SCANCODE_CHAR_BRACKETRIGHT SCANCODE_KEY_28

/* Only on US keyboard */
#define SCANCODE_KEY_29 0x2b
#define SCANCODE_CHAR_BACKSLASH SCANCODE_KEY_29

#define SCANCODE_KEY_30 0x3a
#define SCANCODE_CHAR_CAPSLOCK SCANCODE_KEY_30

#define SCANCODE_KEY_31 0x1e
#define SCANCODE_CHAR_A SCANCODE_KEY_31

#define SCANCODE_KEY_32 0x1f
#define SCANCODE_CHAR_S SCANCODE_KEY_32

#define SCANCODE_KEY_33 0x20
#define SCANCODE_CHAR_D SCANCODE_KEY_33

#define SCANCODE_KEY_34 0x21
#define SCANCODE_CHAR_F SCANCODE_KEY_34

#define SCANCODE_KEY_35 0x22
#define SCANCODE_CHAR_G SCANCODE_KEY_35

#define SCANCODE_KEY_36 0x23
#define SCANCODE_CHAR_H SCANCODE_KEY_36

#define SCANCODE_KEY_37 0x24
#define SCANCODE_CHAR_J SCANCODE_KEY_37

#define SCANCODE_KEY_38 0x25
#define SCANCODE_CHAR_K SCANCODE_KEY_38

#define SCANCODE_KEY_39 0x26
#define SCANCODE_CHAR_L SCANCODE_KEY_39

#define SCANCODE_KEY_40 0x27
#define SCANCODE_CHAR_SEMICOLON SCANCODE_KEY_40

#define SCANCODE_KEY_41 0x28
#define SCANCODE_CHAR_APOSTROPHE SCANCODE_KEY_41

/* Only on international keyboard */
#define SCANCODE_KEY_42 0x2b

#define SCANCODE_KEY_43 0x1c
#define SCANCODE_CHAR_ENTER SCANCODE_KEY_43

#define SCANCODE_KEY_44 0x2a
#define SCANCODE_CHAR_LSHIFT SCANCODE_KEY_44

/* Only on international keyboard */
#define SCANCODE_KEY_45 0x56

#define SCANCODE_KEY_46 0x2c
#define SCANCODE_CHAR_Z SCANCODE_KEY_46

#define SCANCODE_KEY_47 0x2d
#define SCANCODE_CHAR_X SCANCODE_KEY_47

#define SCANCODE_KEY_48 0x2e
#define SCANCODE_CHAR_C SCANCODE_KEY_48

#define SCANCODE_KEY_49 0x2f
#define SCANCODE_CHAR_V SCANCODE_KEY_49

#define SCANCODE_KEY_50 0x30
#define SCANCODE_CHAR_B SCANCODE_KEY_50

#define SCANCODE_KEY_51 0x31
#define SCANCODE_CHAR_N SCANCODE_KEY_51

#define SCANCODE_KEY_52 0x32
#define SCANCODE_CHAR_M SCANCODE_KEY_52

#define SCANCODE_KEY_53 0x33
#define SCANCODE_CHAR_COMMA SCANCODE_KEY_53

#define SCANCODE_KEY_54 0x34
#define SCANCODE_CHAR_DOT SCANCODE_KEY_54

#define SCANCODE_KEY_55 0x35
#define SCANCODE_CHAR_SLASH SCANCODE_KEY_55

/* Only on Brazilian and some Far East keyboards */
#define SCANCODE_KEY_56 0x73

#define SCANCODE_KEY_57 0x36
#define SCANCODE_CHAR_RSHIFT SCANCODE_KEY_57

#define SCANCODE_KEY_58 0x1d
#define SCANCODE_CHAR_LCTRL SCANCODE_KEY_58

/* Key 59 does not exist */

#define SCANCODE_KEY_60 0x38
#define SCANCODE_CHAR_LALT SCANCODE_KEY_60

#define SCANCODE_KEY_61 0x39
#define SCANCODE_CHAR_SPACE SCANCODE_KEY_61

#define SCANCODE_KEY_62 (SCANCODE_EXTENDED | 0x38)
#define SCANCODE_CHAR_RALT SCANCODE_KEY_62

/* Key 63 does not exist */

#define SCANCODE_KEY_64 (SCANCODE_EXTENDED | 0x1d)
#define SCANCODE_CHAR_RCTRL SCANCODE_KEY_64

/* Key 65 - 74 does not exist */

#define SCANCODE_KEY_75 (SCANCODE_EXTENDED | 0x52)
#define SCANCODE_CHAR_INSERT SCANCODE_KEY_75

#define SCANCODE_KEY_76 (SCANCODE_EXTENDED | 0x53)
#define SCANCODE_CHAR_DELETE SCANCODE_KEY_76

/* Key 77 - 78 does not exist */

#define SCANCODE_KEY_79 (SCANCODE_EXTENDED | 0x4b)
#define SCANCODE_CHAR_LARROW SCANCODE_KEY_79

#define SCANCODE_KEY_80 (SCANCODE_EXTENDED | 0x47)
#define SCANCODE_CHAR_HOME SCANCODE_KEY_80

#define SCANCODE_KEY_81 (SCANCODE_EXTENDED | 0x4f)
#define SCANCODE_CHAR_END SCANCODE_KEY_81

/* Key 82 does not exist */

#define SCANCODE_KEY_83 (SCANCODE_EXTENDED | 0x48)
#define SCANCODE_CHAR_UPARROW SCANCODE_KEY_83

#define SCANCODE_KEY_84 (SCANCODE_EXTENDED | 0x50)
#define SCANCODE_CHAR_DNARROW SCANCODE_KEY_84

#define SCANCODE_KEY_85 (SCANCODE_EXTENDED | 0x49)
#define SCANCODE_CHAR_PAGEUP SCANCODE_KEY_85

#define SCANCODE_KEY_86 (SCANCODE_EXTENDED | 0x51)
#define SCANCODE_CHAR_PAGEDOWN SCANCODE_KEY_86

/* Key 87 - 88 does not exist */

#define SCANCODE_KEY_89 (SCANCODE_EXTENDED | 0x4d)
#define SCANCODE_CHAR_RARROW SCANCODE_KEY_89

#define SCANCODE_KEY_90 0x45
#define SCANCODE_CHAR_NUMLOCK SCANCODE_KEY_90

#define SCANCODE_KEY_91 0x47
#define SCANCODE_CHAR_NUMERIC7 SCANCODE_KEY_91

#define SCANCODE_KEY_92 0x4b
#define SCANCODE_CHAR_NUMERIC4 SCANCODE_KEY_92

#define SCANCODE_KEY_93 0x4f
#define SCANCODE_CHAR_NUMERIC1 SCANCODE_KEY_93

/* Key 94 does not exist */

#define SCANCODE_KEY_95 (SCANCODE_EXTENDED | 0x35)
#define SCANCODE_CHAR_NUMERICSLASH SCANCODE_KEY_95

#define SCANCODE_KEY_96 0x48
#define SCANCODE_CHAR_NUMERIC8 SCANCODE_KEY_96

#define SCANCODE_KEY_97 0x4c
#define SCANCODE_CHAR_NUMERIC5 SCANCODE_KEY_97

#define SCANCODE_KEY_98 0x50
#define SCANCODE_CHAR_NUMERIC2 SCANCODE_KEY_98

#define SCANCODE_KEY_99 0x52
#define SCANCODE_CHAR_NUMERIC0 SCANCODE_KEY_99

#define SCANCODE_KEY_100 0x37
#define SCANCODE_CHAR_NUMERICSTAR SCANCODE_KEY_100

#define SCANCODE_KEY_101 0x49
#define SCANCODE_CHAR_NUMERIC9 SCANCODE_KEY_101

#define SCANCODE_KEY_102 0x4d
#define SCANCODE_CHAR_NUMERIC6 SCANCODE_KEY_102

#define SCANCODE_KEY_103 0x51
#define SCANCODE_CHAR_NUMERIC3 SCANCODE_KEY_103

#define SCANCODE_KEY_104 0x53
#define SCANCODE_CHAR_NUMERICDOT SCANCODE_KEY_104

#define SCANCODE_KEY_105 0x4a
#define SCANCODE_CHAR_NUMERICMINUS SCANCODE_KEY_105

#define SCANCODE_KEY_106 0x4e
#define SCANCODE_CHAR_NUMERICPLUS SCANCODE_KEY_106

/* Only on Brazilian and some Far East keyboards */
#define SCANCODE_KEY_107 0x

#define SCANCODE_KEY_108 (SCANCODE_EXTENDED | 0x1c)
#define SCANCODE_CHAR_NUMERICENTER SCANCODE_KEY_108

/* Key 109 does not exist */

#define SCANCODE_KEY_110 0x1
#define SCANCODE_CHAR_ESC SCANCODE_KEY_110

/* Key 111 does not exist */

#define SCANCODE_KEY_112 0x3b
#define SCANCODE_CHAR_F1 SCANCODE_KEY_112

#define SCANCODE_KEY_113 0x3c
#define SCANCODE_CHAR_F2 SCANCODE_KEY_113

#define SCANCODE_KEY_114 0x3d
#define SCANCODE_CHAR_F3 SCANCODE_KEY_114

#define SCANCODE_KEY_115 0x3e
#define SCANCODE_CHAR_F4 SCANCODE_KEY_115

#define SCANCODE_KEY_116 0x3f
#define SCANCODE_CHAR_F5 SCANCODE_KEY_116

#define SCANCODE_KEY_117 0x40
#define SCANCODE_CHAR_F6 SCANCODE_KEY_117

#define SCANCODE_KEY_118 0x41
#define SCANCODE_CHAR_F7 SCANCODE_KEY_118

#define SCANCODE_KEY_119 0x42
#define SCANCODE_CHAR_F8 SCANCODE_KEY_119

#define SCANCODE_KEY_120 0x43
#define SCANCODE_CHAR_F9 SCANCODE_KEY_120

#define SCANCODE_KEY_121 0x44
#define SCANCODE_CHAR_F10 SCANCODE_KEY_121

#define SCANCODE_KEY_122 0x57
#define SCANCODE_CHAR_F11 SCANCODE_KEY_122

#define SCANCODE_KEY_123 0x58
#define SCANCODE_CHAR_F12 SCANCODE_KEY_123

/* Key 124: The Print Screen sequence is complicated, and
   hardcoded in xkeymap.c */

#define SCANCODE_KEY_125 0x46
#define SCANCODE_CHAR_SCROLLLOCK SCANCODE_KEY_125

/* Key 126: The Pause and Break sequences is complicated, and
   hardcoded in xkeymap.c */

/* 
   The keys below does not have any key location number
*/

#define SCANCODE_CHAR_LWIN (SCANCODE_EXTENDED | 0x5b)

#define SCANCODE_CHAR_RWIN (SCANCODE_EXTENDED | 0x5c)

#define SCANCODE_CHAR_APPLICATION (SCANCODE_EXTENDED | 0x5d)

#define SCANCODE_CHAR_ACPIPOWER (SCANCODE_EXTENDED | 0x5e)

#define SCANCODE_CHAR_ACPISLEEP (SCANCODE_EXTENDED | 0x5f)

#define SCANCODE_CHAR_ACPIWAKE (SCANCODE_EXTENDED | 0x63)
