--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Normalization             Luebeck            --
--  Implementation                                 Autumn, 2025       --
--                                                                    --
--                                Last revision :  11:03 04 Dec 2025  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with Strings_Edit.UTF8.Categorization;
use  Strings_Edit.UTF8.Categorization;

package body Strings_Edit.UTF8.Normalization is

   Compatibility : constant := 16#10000000#;
   Mask          : constant := Compatibility - 1;
   Buffer_Size   : constant := 128;

   type Code_Point_Array is array (Positive range <>) of Code_Point;
   type Code_Point_Array_Ptr is access constant Code_Point_Array;
   type Normalization_Map_Size is range 1..5857;
   type Normalization_Map is
      array (Normalization_Map_Size) of Code_Point_Array_Ptr;

   SA0 : aliased constant Code_Point_Array := (16#A0#,16#10000020#);
   SA8 : aliased constant Code_Point_Array := (16#A8#,16#10000020#,16#308#);
   SAA : aliased constant Code_Point_Array := (16#AA#,16#10000061#);
   SAF : aliased constant Code_Point_Array := (16#AF#,16#10000020#,16#304#);
   SB2 : aliased constant Code_Point_Array := (16#B2#,16#10000032#);
   SB3 : aliased constant Code_Point_Array := (16#B3#,16#10000033#);
   SB4 : aliased constant Code_Point_Array := (16#B4#,16#10000020#,16#301#);
   SB5 : aliased constant Code_Point_Array := (16#B5#,16#100003BC#);
   SB8 : aliased constant Code_Point_Array := (16#B8#,16#10000020#,16#327#);
   SB9 : aliased constant Code_Point_Array := (16#B9#,16#10000031#);
   SBA : aliased constant Code_Point_Array := (16#BA#,16#1000006F#);
   SBC : aliased constant Code_Point_Array := (16#BC#,16#10000031#,16#2044#,16#34#);
   SBD : aliased constant Code_Point_Array := (16#BD#,16#10000031#,16#2044#,16#32#);
   SBE : aliased constant Code_Point_Array := (16#BE#,16#10000033#,16#2044#,16#34#);
   SC0 : aliased constant Code_Point_Array := (16#C0#,16#41#,16#300#);
   SC1 : aliased constant Code_Point_Array := (16#C1#,16#41#,16#301#);
   SC2 : aliased constant Code_Point_Array := (16#C2#,16#41#,16#302#);
   SC3 : aliased constant Code_Point_Array := (16#C3#,16#41#,16#303#);
   SC4 : aliased constant Code_Point_Array := (16#C4#,16#41#,16#308#);
   SC5 : aliased constant Code_Point_Array := (16#C5#,16#41#,16#30A#);
   SC7 : aliased constant Code_Point_Array := (16#C7#,16#43#,16#327#);
   SC8 : aliased constant Code_Point_Array := (16#C8#,16#45#,16#300#);
   SC9 : aliased constant Code_Point_Array := (16#C9#,16#45#,16#301#);
   SCA : aliased constant Code_Point_Array := (16#CA#,16#45#,16#302#);
   SCB : aliased constant Code_Point_Array := (16#CB#,16#45#,16#308#);
   SCC : aliased constant Code_Point_Array := (16#CC#,16#49#,16#300#);
   SCD : aliased constant Code_Point_Array := (16#CD#,16#49#,16#301#);
   SCE : aliased constant Code_Point_Array := (16#CE#,16#49#,16#302#);
   SCF : aliased constant Code_Point_Array := (16#CF#,16#49#,16#308#);
   SD1 : aliased constant Code_Point_Array := (16#D1#,16#4E#,16#303#);
   SD2 : aliased constant Code_Point_Array := (16#D2#,16#4F#,16#300#);
   SD3 : aliased constant Code_Point_Array := (16#D3#,16#4F#,16#301#);
   SD4 : aliased constant Code_Point_Array := (16#D4#,16#4F#,16#302#);
   SD5 : aliased constant Code_Point_Array := (16#D5#,16#4F#,16#303#);
   SD6 : aliased constant Code_Point_Array := (16#D6#,16#4F#,16#308#);
   SD9 : aliased constant Code_Point_Array := (16#D9#,16#55#,16#300#);
   SDA : aliased constant Code_Point_Array := (16#DA#,16#55#,16#301#);
   SDB : aliased constant Code_Point_Array := (16#DB#,16#55#,16#302#);
   SDC : aliased constant Code_Point_Array := (16#DC#,16#55#,16#308#);
   SDD : aliased constant Code_Point_Array := (16#DD#,16#59#,16#301#);
   SE0 : aliased constant Code_Point_Array := (16#E0#,16#61#,16#300#);
   SE1 : aliased constant Code_Point_Array := (16#E1#,16#61#,16#301#);
   SE2 : aliased constant Code_Point_Array := (16#E2#,16#61#,16#302#);
   SE3 : aliased constant Code_Point_Array := (16#E3#,16#61#,16#303#);
   SE4 : aliased constant Code_Point_Array := (16#E4#,16#61#,16#308#);
   SE5 : aliased constant Code_Point_Array := (16#E5#,16#61#,16#30A#);
   SE7 : aliased constant Code_Point_Array := (16#E7#,16#63#,16#327#);
   SE8 : aliased constant Code_Point_Array := (16#E8#,16#65#,16#300#);
   SE9 : aliased constant Code_Point_Array := (16#E9#,16#65#,16#301#);
   SEA : aliased constant Code_Point_Array := (16#EA#,16#65#,16#302#);
   SEB : aliased constant Code_Point_Array := (16#EB#,16#65#,16#308#);
   SEC : aliased constant Code_Point_Array := (16#EC#,16#69#,16#300#);
   SED : aliased constant Code_Point_Array := (16#ED#,16#69#,16#301#);
   SEE : aliased constant Code_Point_Array := (16#EE#,16#69#,16#302#);
   SEF : aliased constant Code_Point_Array := (16#EF#,16#69#,16#308#);
   SF1 : aliased constant Code_Point_Array := (16#F1#,16#6E#,16#303#);
   SF2 : aliased constant Code_Point_Array := (16#F2#,16#6F#,16#300#);
   SF3 : aliased constant Code_Point_Array := (16#F3#,16#6F#,16#301#);
   SF4 : aliased constant Code_Point_Array := (16#F4#,16#6F#,16#302#);
   SF5 : aliased constant Code_Point_Array := (16#F5#,16#6F#,16#303#);
   SF6 : aliased constant Code_Point_Array := (16#F6#,16#6F#,16#308#);
   SF9 : aliased constant Code_Point_Array := (16#F9#,16#75#,16#300#);
   SFA : aliased constant Code_Point_Array := (16#FA#,16#75#,16#301#);
   SFB : aliased constant Code_Point_Array := (16#FB#,16#75#,16#302#);
   SFC : aliased constant Code_Point_Array := (16#FC#,16#75#,16#308#);
   SFD : aliased constant Code_Point_Array := (16#FD#,16#79#,16#301#);
   SFF : aliased constant Code_Point_Array := (16#FF#,16#79#,16#308#);
   S100 : aliased constant Code_Point_Array := (16#100#,16#41#,16#304#);
   S101 : aliased constant Code_Point_Array := (16#101#,16#61#,16#304#);
   S102 : aliased constant Code_Point_Array := (16#102#,16#41#,16#306#);
   S103 : aliased constant Code_Point_Array := (16#103#,16#61#,16#306#);
   S104 : aliased constant Code_Point_Array := (16#104#,16#41#,16#328#);
   S105 : aliased constant Code_Point_Array := (16#105#,16#61#,16#328#);
   S106 : aliased constant Code_Point_Array := (16#106#,16#43#,16#301#);
   S107 : aliased constant Code_Point_Array := (16#107#,16#63#,16#301#);
   S108 : aliased constant Code_Point_Array := (16#108#,16#43#,16#302#);
   S109 : aliased constant Code_Point_Array := (16#109#,16#63#,16#302#);
   S10A : aliased constant Code_Point_Array := (16#10A#,16#43#,16#307#);
   S10B : aliased constant Code_Point_Array := (16#10B#,16#63#,16#307#);
   S10C : aliased constant Code_Point_Array := (16#10C#,16#43#,16#30C#);
   S10D : aliased constant Code_Point_Array := (16#10D#,16#63#,16#30C#);
   S10E : aliased constant Code_Point_Array := (16#10E#,16#44#,16#30C#);
   S10F : aliased constant Code_Point_Array := (16#10F#,16#64#,16#30C#);
   S112 : aliased constant Code_Point_Array := (16#112#,16#45#,16#304#);
   S113 : aliased constant Code_Point_Array := (16#113#,16#65#,16#304#);
   S114 : aliased constant Code_Point_Array := (16#114#,16#45#,16#306#);
   S115 : aliased constant Code_Point_Array := (16#115#,16#65#,16#306#);
   S116 : aliased constant Code_Point_Array := (16#116#,16#45#,16#307#);
   S117 : aliased constant Code_Point_Array := (16#117#,16#65#,16#307#);
   S118 : aliased constant Code_Point_Array := (16#118#,16#45#,16#328#);
   S119 : aliased constant Code_Point_Array := (16#119#,16#65#,16#328#);
   S11A : aliased constant Code_Point_Array := (16#11A#,16#45#,16#30C#);
   S11B : aliased constant Code_Point_Array := (16#11B#,16#65#,16#30C#);
   S11C : aliased constant Code_Point_Array := (16#11C#,16#47#,16#302#);
   S11D : aliased constant Code_Point_Array := (16#11D#,16#67#,16#302#);
   S11E : aliased constant Code_Point_Array := (16#11E#,16#47#,16#306#);
   S11F : aliased constant Code_Point_Array := (16#11F#,16#67#,16#306#);
   S120 : aliased constant Code_Point_Array := (16#120#,16#47#,16#307#);
   S121 : aliased constant Code_Point_Array := (16#121#,16#67#,16#307#);
   S122 : aliased constant Code_Point_Array := (16#122#,16#47#,16#327#);
   S123 : aliased constant Code_Point_Array := (16#123#,16#67#,16#327#);
   S124 : aliased constant Code_Point_Array := (16#124#,16#48#,16#302#);
   S125 : aliased constant Code_Point_Array := (16#125#,16#68#,16#302#);
   S128 : aliased constant Code_Point_Array := (16#128#,16#49#,16#303#);
   S129 : aliased constant Code_Point_Array := (16#129#,16#69#,16#303#);
   S12A : aliased constant Code_Point_Array := (16#12A#,16#49#,16#304#);
   S12B : aliased constant Code_Point_Array := (16#12B#,16#69#,16#304#);
   S12C : aliased constant Code_Point_Array := (16#12C#,16#49#,16#306#);
   S12D : aliased constant Code_Point_Array := (16#12D#,16#69#,16#306#);
   S12E : aliased constant Code_Point_Array := (16#12E#,16#49#,16#328#);
   S12F : aliased constant Code_Point_Array := (16#12F#,16#69#,16#328#);
   S130 : aliased constant Code_Point_Array := (16#130#,16#49#,16#307#);
   S132 : aliased constant Code_Point_Array := (16#132#,16#10000049#,16#4A#);
   S133 : aliased constant Code_Point_Array := (16#133#,16#10000069#,16#6A#);
   S134 : aliased constant Code_Point_Array := (16#134#,16#4A#,16#302#);
   S135 : aliased constant Code_Point_Array := (16#135#,16#6A#,16#302#);
   S136 : aliased constant Code_Point_Array := (16#136#,16#4B#,16#327#);
   S137 : aliased constant Code_Point_Array := (16#137#,16#6B#,16#327#);
   S139 : aliased constant Code_Point_Array := (16#139#,16#4C#,16#301#);
   S13A : aliased constant Code_Point_Array := (16#13A#,16#6C#,16#301#);
   S13B : aliased constant Code_Point_Array := (16#13B#,16#4C#,16#327#);
   S13C : aliased constant Code_Point_Array := (16#13C#,16#6C#,16#327#);
   S13D : aliased constant Code_Point_Array := (16#13D#,16#4C#,16#30C#);
   S13E : aliased constant Code_Point_Array := (16#13E#,16#6C#,16#30C#);
   S13F : aliased constant Code_Point_Array := (16#13F#,16#1000004C#,16#B7#);
   S140 : aliased constant Code_Point_Array := (16#140#,16#1000006C#,16#B7#);
   S143 : aliased constant Code_Point_Array := (16#143#,16#4E#,16#301#);
   S144 : aliased constant Code_Point_Array := (16#144#,16#6E#,16#301#);
   S145 : aliased constant Code_Point_Array := (16#145#,16#4E#,16#327#);
   S146 : aliased constant Code_Point_Array := (16#146#,16#6E#,16#327#);
   S147 : aliased constant Code_Point_Array := (16#147#,16#4E#,16#30C#);
   S148 : aliased constant Code_Point_Array := (16#148#,16#6E#,16#30C#);
   S149 : aliased constant Code_Point_Array := (16#149#,16#100002BC#,16#6E#);
   S14C : aliased constant Code_Point_Array := (16#14C#,16#4F#,16#304#);
   S14D : aliased constant Code_Point_Array := (16#14D#,16#6F#,16#304#);
   S14E : aliased constant Code_Point_Array := (16#14E#,16#4F#,16#306#);
   S14F : aliased constant Code_Point_Array := (16#14F#,16#6F#,16#306#);
   S150 : aliased constant Code_Point_Array := (16#150#,16#4F#,16#30B#);
   S151 : aliased constant Code_Point_Array := (16#151#,16#6F#,16#30B#);
   S154 : aliased constant Code_Point_Array := (16#154#,16#52#,16#301#);
   S155 : aliased constant Code_Point_Array := (16#155#,16#72#,16#301#);
   S156 : aliased constant Code_Point_Array := (16#156#,16#52#,16#327#);
   S157 : aliased constant Code_Point_Array := (16#157#,16#72#,16#327#);
   S158 : aliased constant Code_Point_Array := (16#158#,16#52#,16#30C#);
   S159 : aliased constant Code_Point_Array := (16#159#,16#72#,16#30C#);
   S15A : aliased constant Code_Point_Array := (16#15A#,16#53#,16#301#);
   S15B : aliased constant Code_Point_Array := (16#15B#,16#73#,16#301#);
   S15C : aliased constant Code_Point_Array := (16#15C#,16#53#,16#302#);
   S15D : aliased constant Code_Point_Array := (16#15D#,16#73#,16#302#);
   S15E : aliased constant Code_Point_Array := (16#15E#,16#53#,16#327#);
   S15F : aliased constant Code_Point_Array := (16#15F#,16#73#,16#327#);
   S160 : aliased constant Code_Point_Array := (16#160#,16#53#,16#30C#);
   S161 : aliased constant Code_Point_Array := (16#161#,16#73#,16#30C#);
   S162 : aliased constant Code_Point_Array := (16#162#,16#54#,16#327#);
   S163 : aliased constant Code_Point_Array := (16#163#,16#74#,16#327#);
   S164 : aliased constant Code_Point_Array := (16#164#,16#54#,16#30C#);
   S165 : aliased constant Code_Point_Array := (16#165#,16#74#,16#30C#);
   S168 : aliased constant Code_Point_Array := (16#168#,16#55#,16#303#);
   S169 : aliased constant Code_Point_Array := (16#169#,16#75#,16#303#);
   S16A : aliased constant Code_Point_Array := (16#16A#,16#55#,16#304#);
   S16B : aliased constant Code_Point_Array := (16#16B#,16#75#,16#304#);
   S16C : aliased constant Code_Point_Array := (16#16C#,16#55#,16#306#);
   S16D : aliased constant Code_Point_Array := (16#16D#,16#75#,16#306#);
   S16E : aliased constant Code_Point_Array := (16#16E#,16#55#,16#30A#);
   S16F : aliased constant Code_Point_Array := (16#16F#,16#75#,16#30A#);
   S170 : aliased constant Code_Point_Array := (16#170#,16#55#,16#30B#);
   S171 : aliased constant Code_Point_Array := (16#171#,16#75#,16#30B#);
   S172 : aliased constant Code_Point_Array := (16#172#,16#55#,16#328#);
   S173 : aliased constant Code_Point_Array := (16#173#,16#75#,16#328#);
   S174 : aliased constant Code_Point_Array := (16#174#,16#57#,16#302#);
   S175 : aliased constant Code_Point_Array := (16#175#,16#77#,16#302#);
   S176 : aliased constant Code_Point_Array := (16#176#,16#59#,16#302#);
   S177 : aliased constant Code_Point_Array := (16#177#,16#79#,16#302#);
   S178 : aliased constant Code_Point_Array := (16#178#,16#59#,16#308#);
   S179 : aliased constant Code_Point_Array := (16#179#,16#5A#,16#301#);
   S17A : aliased constant Code_Point_Array := (16#17A#,16#7A#,16#301#);
   S17B : aliased constant Code_Point_Array := (16#17B#,16#5A#,16#307#);
   S17C : aliased constant Code_Point_Array := (16#17C#,16#7A#,16#307#);
   S17D : aliased constant Code_Point_Array := (16#17D#,16#5A#,16#30C#);
   S17E : aliased constant Code_Point_Array := (16#17E#,16#7A#,16#30C#);
   S17F : aliased constant Code_Point_Array := (16#17F#,16#10000073#);
   S1A0 : aliased constant Code_Point_Array := (16#1A0#,16#4F#,16#31B#);
   S1A1 : aliased constant Code_Point_Array := (16#1A1#,16#6F#,16#31B#);
   S1AF : aliased constant Code_Point_Array := (16#1AF#,16#55#,16#31B#);
   S1B0 : aliased constant Code_Point_Array := (16#1B0#,16#75#,16#31B#);
   S1C4 : aliased constant Code_Point_Array := (16#1C4#,16#10000044#,16#17D#);
   S1C5 : aliased constant Code_Point_Array := (16#1C5#,16#10000044#,16#17E#);
   S1C6 : aliased constant Code_Point_Array := (16#1C6#,16#10000064#,16#17E#);
   S1C7 : aliased constant Code_Point_Array := (16#1C7#,16#1000004C#,16#4A#);
   S1C8 : aliased constant Code_Point_Array := (16#1C8#,16#1000004C#,16#6A#);
   S1C9 : aliased constant Code_Point_Array := (16#1C9#,16#1000006C#,16#6A#);
   S1CA : aliased constant Code_Point_Array := (16#1CA#,16#1000004E#,16#4A#);
   S1CB : aliased constant Code_Point_Array := (16#1CB#,16#1000004E#,16#6A#);
   S1CC : aliased constant Code_Point_Array := (16#1CC#,16#1000006E#,16#6A#);
   S1CD : aliased constant Code_Point_Array := (16#1CD#,16#41#,16#30C#);
   S1CE : aliased constant Code_Point_Array := (16#1CE#,16#61#,16#30C#);
   S1CF : aliased constant Code_Point_Array := (16#1CF#,16#49#,16#30C#);
   S1D0 : aliased constant Code_Point_Array := (16#1D0#,16#69#,16#30C#);
   S1D1 : aliased constant Code_Point_Array := (16#1D1#,16#4F#,16#30C#);
   S1D2 : aliased constant Code_Point_Array := (16#1D2#,16#6F#,16#30C#);
   S1D3 : aliased constant Code_Point_Array := (16#1D3#,16#55#,16#30C#);
   S1D4 : aliased constant Code_Point_Array := (16#1D4#,16#75#,16#30C#);
   S1D5 : aliased constant Code_Point_Array := (16#1D5#,16#DC#,16#304#);
   S1D6 : aliased constant Code_Point_Array := (16#1D6#,16#FC#,16#304#);
   S1D7 : aliased constant Code_Point_Array := (16#1D7#,16#DC#,16#301#);
   S1D8 : aliased constant Code_Point_Array := (16#1D8#,16#FC#,16#301#);
   S1D9 : aliased constant Code_Point_Array := (16#1D9#,16#DC#,16#30C#);
   S1DA : aliased constant Code_Point_Array := (16#1DA#,16#FC#,16#30C#);
   S1DB : aliased constant Code_Point_Array := (16#1DB#,16#DC#,16#300#);
   S1DC : aliased constant Code_Point_Array := (16#1DC#,16#FC#,16#300#);
   S1DE : aliased constant Code_Point_Array := (16#1DE#,16#C4#,16#304#);
   S1DF : aliased constant Code_Point_Array := (16#1DF#,16#E4#,16#304#);
   S1E0 : aliased constant Code_Point_Array := (16#1E0#,16#226#,16#304#);
   S1E1 : aliased constant Code_Point_Array := (16#1E1#,16#227#,16#304#);
   S1E2 : aliased constant Code_Point_Array := (16#1E2#,16#C6#,16#304#);
   S1E3 : aliased constant Code_Point_Array := (16#1E3#,16#E6#,16#304#);
   S1E6 : aliased constant Code_Point_Array := (16#1E6#,16#47#,16#30C#);
   S1E7 : aliased constant Code_Point_Array := (16#1E7#,16#67#,16#30C#);
   S1E8 : aliased constant Code_Point_Array := (16#1E8#,16#4B#,16#30C#);
   S1E9 : aliased constant Code_Point_Array := (16#1E9#,16#6B#,16#30C#);
   S1EA : aliased constant Code_Point_Array := (16#1EA#,16#4F#,16#328#);
   S1EB : aliased constant Code_Point_Array := (16#1EB#,16#6F#,16#328#);
   S1EC : aliased constant Code_Point_Array := (16#1EC#,16#1EA#,16#304#);
   S1ED : aliased constant Code_Point_Array := (16#1ED#,16#1EB#,16#304#);
   S1EE : aliased constant Code_Point_Array := (16#1EE#,16#1B7#,16#30C#);
   S1EF : aliased constant Code_Point_Array := (16#1EF#,16#292#,16#30C#);
   S1F0 : aliased constant Code_Point_Array := (16#1F0#,16#6A#,16#30C#);
   S1F1 : aliased constant Code_Point_Array := (16#1F1#,16#10000044#,16#5A#);
   S1F2 : aliased constant Code_Point_Array := (16#1F2#,16#10000044#,16#7A#);
   S1F3 : aliased constant Code_Point_Array := (16#1F3#,16#10000064#,16#7A#);
   S1F4 : aliased constant Code_Point_Array := (16#1F4#,16#47#,16#301#);
   S1F5 : aliased constant Code_Point_Array := (16#1F5#,16#67#,16#301#);
   S1F8 : aliased constant Code_Point_Array := (16#1F8#,16#4E#,16#300#);
   S1F9 : aliased constant Code_Point_Array := (16#1F9#,16#6E#,16#300#);
   S1FA : aliased constant Code_Point_Array := (16#1FA#,16#C5#,16#301#);
   S1FB : aliased constant Code_Point_Array := (16#1FB#,16#E5#,16#301#);
   S1FC : aliased constant Code_Point_Array := (16#1FC#,16#C6#,16#301#);
   S1FD : aliased constant Code_Point_Array := (16#1FD#,16#E6#,16#301#);
   S1FE : aliased constant Code_Point_Array := (16#1FE#,16#D8#,16#301#);
   S1FF : aliased constant Code_Point_Array := (16#1FF#,16#F8#,16#301#);
   S200 : aliased constant Code_Point_Array := (16#200#,16#41#,16#30F#);
   S201 : aliased constant Code_Point_Array := (16#201#,16#61#,16#30F#);
   S202 : aliased constant Code_Point_Array := (16#202#,16#41#,16#311#);
   S203 : aliased constant Code_Point_Array := (16#203#,16#61#,16#311#);
   S204 : aliased constant Code_Point_Array := (16#204#,16#45#,16#30F#);
   S205 : aliased constant Code_Point_Array := (16#205#,16#65#,16#30F#);
   S206 : aliased constant Code_Point_Array := (16#206#,16#45#,16#311#);
   S207 : aliased constant Code_Point_Array := (16#207#,16#65#,16#311#);
   S208 : aliased constant Code_Point_Array := (16#208#,16#49#,16#30F#);
   S209 : aliased constant Code_Point_Array := (16#209#,16#69#,16#30F#);
   S20A : aliased constant Code_Point_Array := (16#20A#,16#49#,16#311#);
   S20B : aliased constant Code_Point_Array := (16#20B#,16#69#,16#311#);
   S20C : aliased constant Code_Point_Array := (16#20C#,16#4F#,16#30F#);
   S20D : aliased constant Code_Point_Array := (16#20D#,16#6F#,16#30F#);
   S20E : aliased constant Code_Point_Array := (16#20E#,16#4F#,16#311#);
   S20F : aliased constant Code_Point_Array := (16#20F#,16#6F#,16#311#);
   S210 : aliased constant Code_Point_Array := (16#210#,16#52#,16#30F#);
   S211 : aliased constant Code_Point_Array := (16#211#,16#72#,16#30F#);
   S212 : aliased constant Code_Point_Array := (16#212#,16#52#,16#311#);
   S213 : aliased constant Code_Point_Array := (16#213#,16#72#,16#311#);
   S214 : aliased constant Code_Point_Array := (16#214#,16#55#,16#30F#);
   S215 : aliased constant Code_Point_Array := (16#215#,16#75#,16#30F#);
   S216 : aliased constant Code_Point_Array := (16#216#,16#55#,16#311#);
   S217 : aliased constant Code_Point_Array := (16#217#,16#75#,16#311#);
   S218 : aliased constant Code_Point_Array := (16#218#,16#53#,16#326#);
   S219 : aliased constant Code_Point_Array := (16#219#,16#73#,16#326#);
   S21A : aliased constant Code_Point_Array := (16#21A#,16#54#,16#326#);
   S21B : aliased constant Code_Point_Array := (16#21B#,16#74#,16#326#);
   S21E : aliased constant Code_Point_Array := (16#21E#,16#48#,16#30C#);
   S21F : aliased constant Code_Point_Array := (16#21F#,16#68#,16#30C#);
   S226 : aliased constant Code_Point_Array := (16#226#,16#41#,16#307#);
   S227 : aliased constant Code_Point_Array := (16#227#,16#61#,16#307#);
   S228 : aliased constant Code_Point_Array := (16#228#,16#45#,16#327#);
   S229 : aliased constant Code_Point_Array := (16#229#,16#65#,16#327#);
   S22A : aliased constant Code_Point_Array := (16#22A#,16#D6#,16#304#);
   S22B : aliased constant Code_Point_Array := (16#22B#,16#F6#,16#304#);
   S22C : aliased constant Code_Point_Array := (16#22C#,16#D5#,16#304#);
   S22D : aliased constant Code_Point_Array := (16#22D#,16#F5#,16#304#);
   S22E : aliased constant Code_Point_Array := (16#22E#,16#4F#,16#307#);
   S22F : aliased constant Code_Point_Array := (16#22F#,16#6F#,16#307#);
   S230 : aliased constant Code_Point_Array := (16#230#,16#22E#,16#304#);
   S231 : aliased constant Code_Point_Array := (16#231#,16#22F#,16#304#);
   S232 : aliased constant Code_Point_Array := (16#232#,16#59#,16#304#);
   S233 : aliased constant Code_Point_Array := (16#233#,16#79#,16#304#);
   S2B0 : aliased constant Code_Point_Array := (16#2B0#,16#10000068#);
   S2B1 : aliased constant Code_Point_Array := (16#2B1#,16#10000266#);
   S2B2 : aliased constant Code_Point_Array := (16#2B2#,16#1000006A#);
   S2B3 : aliased constant Code_Point_Array := (16#2B3#,16#10000072#);
   S2B4 : aliased constant Code_Point_Array := (16#2B4#,16#10000279#);
   S2B5 : aliased constant Code_Point_Array := (16#2B5#,16#1000027B#);
   S2B6 : aliased constant Code_Point_Array := (16#2B6#,16#10000281#);
   S2B7 : aliased constant Code_Point_Array := (16#2B7#,16#10000077#);
   S2B8 : aliased constant Code_Point_Array := (16#2B8#,16#10000079#);
   S2D8 : aliased constant Code_Point_Array := (16#2D8#,16#10000020#,16#306#);
   S2D9 : aliased constant Code_Point_Array := (16#2D9#,16#10000020#,16#307#);
   S2DA : aliased constant Code_Point_Array := (16#2DA#,16#10000020#,16#30A#);
   S2DB : aliased constant Code_Point_Array := (16#2DB#,16#10000020#,16#328#);
   S2DC : aliased constant Code_Point_Array := (16#2DC#,16#10000020#,16#303#);
   S2DD : aliased constant Code_Point_Array := (16#2DD#,16#10000020#,16#30B#);
   S2E0 : aliased constant Code_Point_Array := (16#2E0#,16#10000263#);
   S2E1 : aliased constant Code_Point_Array := (16#2E1#,16#1000006C#);
   S2E2 : aliased constant Code_Point_Array := (16#2E2#,16#10000073#);
   S2E3 : aliased constant Code_Point_Array := (16#2E3#,16#10000078#);
   S2E4 : aliased constant Code_Point_Array := (16#2E4#,16#10000295#);
   S340 : aliased constant Code_Point_Array := (16#340#,16#300#);
   S341 : aliased constant Code_Point_Array := (16#341#,16#301#);
   S343 : aliased constant Code_Point_Array := (16#343#,16#313#);
   S344 : aliased constant Code_Point_Array := (16#344#,16#308#,16#301#);
   S374 : aliased constant Code_Point_Array := (16#374#,16#2B9#);
   S37A : aliased constant Code_Point_Array := (16#37A#,16#10000020#,16#345#);
   S37E : aliased constant Code_Point_Array := (16#37E#,16#3B#);
   S384 : aliased constant Code_Point_Array := (16#384#,16#10000020#,16#301#);
   S385 : aliased constant Code_Point_Array := (16#385#,16#A8#,16#301#);
   S386 : aliased constant Code_Point_Array := (16#386#,16#391#,16#301#);
   S387 : aliased constant Code_Point_Array := (16#387#,16#B7#);
   S388 : aliased constant Code_Point_Array := (16#388#,16#395#,16#301#);
   S389 : aliased constant Code_Point_Array := (16#389#,16#397#,16#301#);
   S38A : aliased constant Code_Point_Array := (16#38A#,16#399#,16#301#);
   S38C : aliased constant Code_Point_Array := (16#38C#,16#39F#,16#301#);
   S38E : aliased constant Code_Point_Array := (16#38E#,16#3A5#,16#301#);
   S38F : aliased constant Code_Point_Array := (16#38F#,16#3A9#,16#301#);
   S390 : aliased constant Code_Point_Array := (16#390#,16#3CA#,16#301#);
   S3AA : aliased constant Code_Point_Array := (16#3AA#,16#399#,16#308#);
   S3AB : aliased constant Code_Point_Array := (16#3AB#,16#3A5#,16#308#);
   S3AC : aliased constant Code_Point_Array := (16#3AC#,16#3B1#,16#301#);
   S3AD : aliased constant Code_Point_Array := (16#3AD#,16#3B5#,16#301#);
   S3AE : aliased constant Code_Point_Array := (16#3AE#,16#3B7#,16#301#);
   S3AF : aliased constant Code_Point_Array := (16#3AF#,16#3B9#,16#301#);
   S3B0 : aliased constant Code_Point_Array := (16#3B0#,16#3CB#,16#301#);
   S3CA : aliased constant Code_Point_Array := (16#3CA#,16#3B9#,16#308#);
   S3CB : aliased constant Code_Point_Array := (16#3CB#,16#3C5#,16#308#);
   S3CC : aliased constant Code_Point_Array := (16#3CC#,16#3BF#,16#301#);
   S3CD : aliased constant Code_Point_Array := (16#3CD#,16#3C5#,16#301#);
   S3CE : aliased constant Code_Point_Array := (16#3CE#,16#3C9#,16#301#);
   S3D0 : aliased constant Code_Point_Array := (16#3D0#,16#100003B2#);
   S3D1 : aliased constant Code_Point_Array := (16#3D1#,16#100003B8#);
   S3D2 : aliased constant Code_Point_Array := (16#3D2#,16#100003A5#);
   S3D3 : aliased constant Code_Point_Array := (16#3D3#,16#3D2#,16#301#);
   S3D4 : aliased constant Code_Point_Array := (16#3D4#,16#3D2#,16#308#);
   S3D5 : aliased constant Code_Point_Array := (16#3D5#,16#100003C6#);
   S3D6 : aliased constant Code_Point_Array := (16#3D6#,16#100003C0#);
   S3F0 : aliased constant Code_Point_Array := (16#3F0#,16#100003BA#);
   S3F1 : aliased constant Code_Point_Array := (16#3F1#,16#100003C1#);
   S3F2 : aliased constant Code_Point_Array := (16#3F2#,16#100003C2#);
   S3F4 : aliased constant Code_Point_Array := (16#3F4#,16#10000398#);
   S3F5 : aliased constant Code_Point_Array := (16#3F5#,16#100003B5#);
   S3F9 : aliased constant Code_Point_Array := (16#3F9#,16#100003A3#);
   S400 : aliased constant Code_Point_Array := (16#400#,16#415#,16#300#);
   S401 : aliased constant Code_Point_Array := (16#401#,16#415#,16#308#);
   S403 : aliased constant Code_Point_Array := (16#403#,16#413#,16#301#);
   S407 : aliased constant Code_Point_Array := (16#407#,16#406#,16#308#);
   S40C : aliased constant Code_Point_Array := (16#40C#,16#41A#,16#301#);
   S40D : aliased constant Code_Point_Array := (16#40D#,16#418#,16#300#);
   S40E : aliased constant Code_Point_Array := (16#40E#,16#423#,16#306#);
   S419 : aliased constant Code_Point_Array := (16#419#,16#418#,16#306#);
   S439 : aliased constant Code_Point_Array := (16#439#,16#438#,16#306#);
   S450 : aliased constant Code_Point_Array := (16#450#,16#435#,16#300#);
   S451 : aliased constant Code_Point_Array := (16#451#,16#435#,16#308#);
   S453 : aliased constant Code_Point_Array := (16#453#,16#433#,16#301#);
   S457 : aliased constant Code_Point_Array := (16#457#,16#456#,16#308#);
   S45C : aliased constant Code_Point_Array := (16#45C#,16#43A#,16#301#);
   S45D : aliased constant Code_Point_Array := (16#45D#,16#438#,16#300#);
   S45E : aliased constant Code_Point_Array := (16#45E#,16#443#,16#306#);
   S476 : aliased constant Code_Point_Array := (16#476#,16#474#,16#30F#);
   S477 : aliased constant Code_Point_Array := (16#477#,16#475#,16#30F#);
   S4C1 : aliased constant Code_Point_Array := (16#4C1#,16#416#,16#306#);
   S4C2 : aliased constant Code_Point_Array := (16#4C2#,16#436#,16#306#);
   S4D0 : aliased constant Code_Point_Array := (16#4D0#,16#410#,16#306#);
   S4D1 : aliased constant Code_Point_Array := (16#4D1#,16#430#,16#306#);
   S4D2 : aliased constant Code_Point_Array := (16#4D2#,16#410#,16#308#);
   S4D3 : aliased constant Code_Point_Array := (16#4D3#,16#430#,16#308#);
   S4D6 : aliased constant Code_Point_Array := (16#4D6#,16#415#,16#306#);
   S4D7 : aliased constant Code_Point_Array := (16#4D7#,16#435#,16#306#);
   S4DA : aliased constant Code_Point_Array := (16#4DA#,16#4D8#,16#308#);
   S4DB : aliased constant Code_Point_Array := (16#4DB#,16#4D9#,16#308#);
   S4DC : aliased constant Code_Point_Array := (16#4DC#,16#416#,16#308#);
   S4DD : aliased constant Code_Point_Array := (16#4DD#,16#436#,16#308#);
   S4DE : aliased constant Code_Point_Array := (16#4DE#,16#417#,16#308#);
   S4DF : aliased constant Code_Point_Array := (16#4DF#,16#437#,16#308#);
   S4E2 : aliased constant Code_Point_Array := (16#4E2#,16#418#,16#304#);
   S4E3 : aliased constant Code_Point_Array := (16#4E3#,16#438#,16#304#);
   S4E4 : aliased constant Code_Point_Array := (16#4E4#,16#418#,16#308#);
   S4E5 : aliased constant Code_Point_Array := (16#4E5#,16#438#,16#308#);
   S4E6 : aliased constant Code_Point_Array := (16#4E6#,16#41E#,16#308#);
   S4E7 : aliased constant Code_Point_Array := (16#4E7#,16#43E#,16#308#);
   S4EA : aliased constant Code_Point_Array := (16#4EA#,16#4E8#,16#308#);
   S4EB : aliased constant Code_Point_Array := (16#4EB#,16#4E9#,16#308#);
   S4EC : aliased constant Code_Point_Array := (16#4EC#,16#42D#,16#308#);
   S4ED : aliased constant Code_Point_Array := (16#4ED#,16#44D#,16#308#);
   S4EE : aliased constant Code_Point_Array := (16#4EE#,16#423#,16#304#);
   S4EF : aliased constant Code_Point_Array := (16#4EF#,16#443#,16#304#);
   S4F0 : aliased constant Code_Point_Array := (16#4F0#,16#423#,16#308#);
   S4F1 : aliased constant Code_Point_Array := (16#4F1#,16#443#,16#308#);
   S4F2 : aliased constant Code_Point_Array := (16#4F2#,16#423#,16#30B#);
   S4F3 : aliased constant Code_Point_Array := (16#4F3#,16#443#,16#30B#);
   S4F4 : aliased constant Code_Point_Array := (16#4F4#,16#427#,16#308#);
   S4F5 : aliased constant Code_Point_Array := (16#4F5#,16#447#,16#308#);
   S4F8 : aliased constant Code_Point_Array := (16#4F8#,16#42B#,16#308#);
   S4F9 : aliased constant Code_Point_Array := (16#4F9#,16#44B#,16#308#);
   S587 : aliased constant Code_Point_Array := (16#587#,16#10000565#,16#582#);
   S622 : aliased constant Code_Point_Array := (16#622#,16#627#,16#653#);
   S623 : aliased constant Code_Point_Array := (16#623#,16#627#,16#654#);
   S624 : aliased constant Code_Point_Array := (16#624#,16#648#,16#654#);
   S625 : aliased constant Code_Point_Array := (16#625#,16#627#,16#655#);
   S626 : aliased constant Code_Point_Array := (16#626#,16#64A#,16#654#);
   S675 : aliased constant Code_Point_Array := (16#675#,16#10000627#,16#674#);
   S676 : aliased constant Code_Point_Array := (16#676#,16#10000648#,16#674#);
   S677 : aliased constant Code_Point_Array := (16#677#,16#100006C7#,16#674#);
   S678 : aliased constant Code_Point_Array := (16#678#,16#1000064A#,16#674#);
   S6C0 : aliased constant Code_Point_Array := (16#6C0#,16#6D5#,16#654#);
   S6C2 : aliased constant Code_Point_Array := (16#6C2#,16#6C1#,16#654#);
   S6D3 : aliased constant Code_Point_Array := (16#6D3#,16#6D2#,16#654#);
   S929 : aliased constant Code_Point_Array := (16#929#,16#928#,16#93C#);
   S931 : aliased constant Code_Point_Array := (16#931#,16#930#,16#93C#);
   S934 : aliased constant Code_Point_Array := (16#934#,16#933#,16#93C#);
   S958 : aliased constant Code_Point_Array := (16#958#,16#915#,16#93C#);
   S959 : aliased constant Code_Point_Array := (16#959#,16#916#,16#93C#);
   S95A : aliased constant Code_Point_Array := (16#95A#,16#917#,16#93C#);
   S95B : aliased constant Code_Point_Array := (16#95B#,16#91C#,16#93C#);
   S95C : aliased constant Code_Point_Array := (16#95C#,16#921#,16#93C#);
   S95D : aliased constant Code_Point_Array := (16#95D#,16#922#,16#93C#);
   S95E : aliased constant Code_Point_Array := (16#95E#,16#92B#,16#93C#);
   S95F : aliased constant Code_Point_Array := (16#95F#,16#92F#,16#93C#);
   S9CB : aliased constant Code_Point_Array := (16#9CB#,16#9C7#,16#9BE#);
   S9CC : aliased constant Code_Point_Array := (16#9CC#,16#9C7#,16#9D7#);
   S9DC : aliased constant Code_Point_Array := (16#9DC#,16#9A1#,16#9BC#);
   S9DD : aliased constant Code_Point_Array := (16#9DD#,16#9A2#,16#9BC#);
   S9DF : aliased constant Code_Point_Array := (16#9DF#,16#9AF#,16#9BC#);
   SA33 : aliased constant Code_Point_Array := (16#A33#,16#A32#,16#A3C#);
   SA36 : aliased constant Code_Point_Array := (16#A36#,16#A38#,16#A3C#);
   SA59 : aliased constant Code_Point_Array := (16#A59#,16#A16#,16#A3C#);
   SA5A : aliased constant Code_Point_Array := (16#A5A#,16#A17#,16#A3C#);
   SA5B : aliased constant Code_Point_Array := (16#A5B#,16#A1C#,16#A3C#);
   SA5E : aliased constant Code_Point_Array := (16#A5E#,16#A2B#,16#A3C#);
   SB48 : aliased constant Code_Point_Array := (16#B48#,16#B47#,16#B56#);
   SB4B : aliased constant Code_Point_Array := (16#B4B#,16#B47#,16#B3E#);
   SB4C : aliased constant Code_Point_Array := (16#B4C#,16#B47#,16#B57#);
   SB5C : aliased constant Code_Point_Array := (16#B5C#,16#B21#,16#B3C#);
   SB5D : aliased constant Code_Point_Array := (16#B5D#,16#B22#,16#B3C#);
   SB94 : aliased constant Code_Point_Array := (16#B94#,16#B92#,16#BD7#);
   SBCA : aliased constant Code_Point_Array := (16#BCA#,16#BC6#,16#BBE#);
   SBCB : aliased constant Code_Point_Array := (16#BCB#,16#BC7#,16#BBE#);
   SBCC : aliased constant Code_Point_Array := (16#BCC#,16#BC6#,16#BD7#);
   SC48 : aliased constant Code_Point_Array := (16#C48#,16#C46#,16#C56#);
   SCC0 : aliased constant Code_Point_Array := (16#CC0#,16#CBF#,16#CD5#);
   SCC7 : aliased constant Code_Point_Array := (16#CC7#,16#CC6#,16#CD5#);
   SCC8 : aliased constant Code_Point_Array := (16#CC8#,16#CC6#,16#CD6#);
   SCCA : aliased constant Code_Point_Array := (16#CCA#,16#CC6#,16#CC2#);
   SCCB : aliased constant Code_Point_Array := (16#CCB#,16#CCA#,16#CD5#);
   SD4A : aliased constant Code_Point_Array := (16#D4A#,16#D46#,16#D3E#);
   SD4B : aliased constant Code_Point_Array := (16#D4B#,16#D47#,16#D3E#);
   SD4C : aliased constant Code_Point_Array := (16#D4C#,16#D46#,16#D57#);
   SDDA : aliased constant Code_Point_Array := (16#DDA#,16#DD9#,16#DCA#);
   SDDC : aliased constant Code_Point_Array := (16#DDC#,16#DD9#,16#DCF#);
   SDDD : aliased constant Code_Point_Array := (16#DDD#,16#DDC#,16#DCA#);
   SDDE : aliased constant Code_Point_Array := (16#DDE#,16#DD9#,16#DDF#);
   SE33 : aliased constant Code_Point_Array := (16#E33#,16#10000E4D#,16#E32#);
   SEB3 : aliased constant Code_Point_Array := (16#EB3#,16#10000ECD#,16#EB2#);
   SEDC : aliased constant Code_Point_Array := (16#EDC#,16#10000EAB#,16#E99#);
   SEDD : aliased constant Code_Point_Array := (16#EDD#,16#10000EAB#,16#EA1#);
   SF0C : aliased constant Code_Point_Array := (16#F0C#,16#10000F0B#);
   SF43 : aliased constant Code_Point_Array := (16#F43#,16#F42#,16#FB7#);
   SF4D : aliased constant Code_Point_Array := (16#F4D#,16#F4C#,16#FB7#);
   SF52 : aliased constant Code_Point_Array := (16#F52#,16#F51#,16#FB7#);
   SF57 : aliased constant Code_Point_Array := (16#F57#,16#F56#,16#FB7#);
   SF5C : aliased constant Code_Point_Array := (16#F5C#,16#F5B#,16#FB7#);
   SF69 : aliased constant Code_Point_Array := (16#F69#,16#F40#,16#FB5#);
   SF73 : aliased constant Code_Point_Array := (16#F73#,16#F71#,16#F72#);
   SF75 : aliased constant Code_Point_Array := (16#F75#,16#F71#,16#F74#);
   SF76 : aliased constant Code_Point_Array := (16#F76#,16#FB2#,16#F80#);
   SF77 : aliased constant Code_Point_Array := (16#F77#,16#10000FB2#,16#F81#);
   SF78 : aliased constant Code_Point_Array := (16#F78#,16#FB3#,16#F80#);
   SF79 : aliased constant Code_Point_Array := (16#F79#,16#10000FB3#,16#F81#);
   SF81 : aliased constant Code_Point_Array := (16#F81#,16#F71#,16#F80#);
   SF93 : aliased constant Code_Point_Array := (16#F93#,16#F92#,16#FB7#);
   SF9D : aliased constant Code_Point_Array := (16#F9D#,16#F9C#,16#FB7#);
   SFA2 : aliased constant Code_Point_Array := (16#FA2#,16#FA1#,16#FB7#);
   SFA7 : aliased constant Code_Point_Array := (16#FA7#,16#FA6#,16#FB7#);
   SFAC : aliased constant Code_Point_Array := (16#FAC#,16#FAB#,16#FB7#);
   SFB9 : aliased constant Code_Point_Array := (16#FB9#,16#F90#,16#FB5#);
   S1026 : aliased constant Code_Point_Array := (16#1026#,16#1025#,16#102E#);
   S10FC : aliased constant Code_Point_Array := (16#10FC#,16#100010DC#);
   S1B06 : aliased constant Code_Point_Array := (16#1B06#,16#1B05#,16#1B35#);
   S1B08 : aliased constant Code_Point_Array := (16#1B08#,16#1B07#,16#1B35#);
   S1B0A : aliased constant Code_Point_Array := (16#1B0A#,16#1B09#,16#1B35#);
   S1B0C : aliased constant Code_Point_Array := (16#1B0C#,16#1B0B#,16#1B35#);
   S1B0E : aliased constant Code_Point_Array := (16#1B0E#,16#1B0D#,16#1B35#);
   S1B12 : aliased constant Code_Point_Array := (16#1B12#,16#1B11#,16#1B35#);
   S1B3B : aliased constant Code_Point_Array := (16#1B3B#,16#1B3A#,16#1B35#);
   S1B3D : aliased constant Code_Point_Array := (16#1B3D#,16#1B3C#,16#1B35#);
   S1B40 : aliased constant Code_Point_Array := (16#1B40#,16#1B3E#,16#1B35#);
   S1B41 : aliased constant Code_Point_Array := (16#1B41#,16#1B3F#,16#1B35#);
   S1B43 : aliased constant Code_Point_Array := (16#1B43#,16#1B42#,16#1B35#);
   S1D2C : aliased constant Code_Point_Array := (16#1D2C#,16#10000041#);
   S1D2D : aliased constant Code_Point_Array := (16#1D2D#,16#100000C6#);
   S1D2E : aliased constant Code_Point_Array := (16#1D2E#,16#10000042#);
   S1D30 : aliased constant Code_Point_Array := (16#1D30#,16#10000044#);
   S1D31 : aliased constant Code_Point_Array := (16#1D31#,16#10000045#);
   S1D32 : aliased constant Code_Point_Array := (16#1D32#,16#1000018E#);
   S1D33 : aliased constant Code_Point_Array := (16#1D33#,16#10000047#);
   S1D34 : aliased constant Code_Point_Array := (16#1D34#,16#10000048#);
   S1D35 : aliased constant Code_Point_Array := (16#1D35#,16#10000049#);
   S1D36 : aliased constant Code_Point_Array := (16#1D36#,16#1000004A#);
   S1D37 : aliased constant Code_Point_Array := (16#1D37#,16#1000004B#);
   S1D38 : aliased constant Code_Point_Array := (16#1D38#,16#1000004C#);
   S1D39 : aliased constant Code_Point_Array := (16#1D39#,16#1000004D#);
   S1D3A : aliased constant Code_Point_Array := (16#1D3A#,16#1000004E#);
   S1D3C : aliased constant Code_Point_Array := (16#1D3C#,16#1000004F#);
   S1D3D : aliased constant Code_Point_Array := (16#1D3D#,16#10000222#);
   S1D3E : aliased constant Code_Point_Array := (16#1D3E#,16#10000050#);
   S1D3F : aliased constant Code_Point_Array := (16#1D3F#,16#10000052#);
   S1D40 : aliased constant Code_Point_Array := (16#1D40#,16#10000054#);
   S1D41 : aliased constant Code_Point_Array := (16#1D41#,16#10000055#);
   S1D42 : aliased constant Code_Point_Array := (16#1D42#,16#10000057#);
   S1D43 : aliased constant Code_Point_Array := (16#1D43#,16#10000061#);
   S1D44 : aliased constant Code_Point_Array := (16#1D44#,16#10000250#);
   S1D45 : aliased constant Code_Point_Array := (16#1D45#,16#10000251#);
   S1D46 : aliased constant Code_Point_Array := (16#1D46#,16#10001D02#);
   S1D47 : aliased constant Code_Point_Array := (16#1D47#,16#10000062#);
   S1D48 : aliased constant Code_Point_Array := (16#1D48#,16#10000064#);
   S1D49 : aliased constant Code_Point_Array := (16#1D49#,16#10000065#);
   S1D4A : aliased constant Code_Point_Array := (16#1D4A#,16#10000259#);
   S1D4B : aliased constant Code_Point_Array := (16#1D4B#,16#1000025B#);
   S1D4C : aliased constant Code_Point_Array := (16#1D4C#,16#1000025C#);
   S1D4D : aliased constant Code_Point_Array := (16#1D4D#,16#10000067#);
   S1D4F : aliased constant Code_Point_Array := (16#1D4F#,16#1000006B#);
   S1D50 : aliased constant Code_Point_Array := (16#1D50#,16#1000006D#);
   S1D51 : aliased constant Code_Point_Array := (16#1D51#,16#1000014B#);
   S1D52 : aliased constant Code_Point_Array := (16#1D52#,16#1000006F#);
   S1D53 : aliased constant Code_Point_Array := (16#1D53#,16#10000254#);
   S1D54 : aliased constant Code_Point_Array := (16#1D54#,16#10001D16#);
   S1D55 : aliased constant Code_Point_Array := (16#1D55#,16#10001D17#);
   S1D56 : aliased constant Code_Point_Array := (16#1D56#,16#10000070#);
   S1D57 : aliased constant Code_Point_Array := (16#1D57#,16#10000074#);
   S1D58 : aliased constant Code_Point_Array := (16#1D58#,16#10000075#);
   S1D59 : aliased constant Code_Point_Array := (16#1D59#,16#10001D1D#);
   S1D5A : aliased constant Code_Point_Array := (16#1D5A#,16#1000026F#);
   S1D5B : aliased constant Code_Point_Array := (16#1D5B#,16#10000076#);
   S1D5C : aliased constant Code_Point_Array := (16#1D5C#,16#10001D25#);
   S1D5D : aliased constant Code_Point_Array := (16#1D5D#,16#100003B2#);
   S1D5E : aliased constant Code_Point_Array := (16#1D5E#,16#100003B3#);
   S1D5F : aliased constant Code_Point_Array := (16#1D5F#,16#100003B4#);
   S1D60 : aliased constant Code_Point_Array := (16#1D60#,16#100003C6#);
   S1D61 : aliased constant Code_Point_Array := (16#1D61#,16#100003C7#);
   S1D62 : aliased constant Code_Point_Array := (16#1D62#,16#10000069#);
   S1D63 : aliased constant Code_Point_Array := (16#1D63#,16#10000072#);
   S1D64 : aliased constant Code_Point_Array := (16#1D64#,16#10000075#);
   S1D65 : aliased constant Code_Point_Array := (16#1D65#,16#10000076#);
   S1D66 : aliased constant Code_Point_Array := (16#1D66#,16#100003B2#);
   S1D67 : aliased constant Code_Point_Array := (16#1D67#,16#100003B3#);
   S1D68 : aliased constant Code_Point_Array := (16#1D68#,16#100003C1#);
   S1D69 : aliased constant Code_Point_Array := (16#1D69#,16#100003C6#);
   S1D6A : aliased constant Code_Point_Array := (16#1D6A#,16#100003C7#);
   S1D78 : aliased constant Code_Point_Array := (16#1D78#,16#1000043D#);
   S1D9B : aliased constant Code_Point_Array := (16#1D9B#,16#10000252#);
   S1D9C : aliased constant Code_Point_Array := (16#1D9C#,16#10000063#);
   S1D9D : aliased constant Code_Point_Array := (16#1D9D#,16#10000255#);
   S1D9E : aliased constant Code_Point_Array := (16#1D9E#,16#100000F0#);
   S1D9F : aliased constant Code_Point_Array := (16#1D9F#,16#1000025C#);
   S1DA0 : aliased constant Code_Point_Array := (16#1DA0#,16#10000066#);
   S1DA1 : aliased constant Code_Point_Array := (16#1DA1#,16#1000025F#);
   S1DA2 : aliased constant Code_Point_Array := (16#1DA2#,16#10000261#);
   S1DA3 : aliased constant Code_Point_Array := (16#1DA3#,16#10000265#);
   S1DA4 : aliased constant Code_Point_Array := (16#1DA4#,16#10000268#);
   S1DA5 : aliased constant Code_Point_Array := (16#1DA5#,16#10000269#);
   S1DA6 : aliased constant Code_Point_Array := (16#1DA6#,16#1000026A#);
   S1DA7 : aliased constant Code_Point_Array := (16#1DA7#,16#10001D7B#);
   S1DA8 : aliased constant Code_Point_Array := (16#1DA8#,16#1000029D#);
   S1DA9 : aliased constant Code_Point_Array := (16#1DA9#,16#1000026D#);
   S1DAA : aliased constant Code_Point_Array := (16#1DAA#,16#10001D85#);
   S1DAB : aliased constant Code_Point_Array := (16#1DAB#,16#1000029F#);
   S1DAC : aliased constant Code_Point_Array := (16#1DAC#,16#10000271#);
   S1DAD : aliased constant Code_Point_Array := (16#1DAD#,16#10000270#);
   S1DAE : aliased constant Code_Point_Array := (16#1DAE#,16#10000272#);
   S1DAF : aliased constant Code_Point_Array := (16#1DAF#,16#10000273#);
   S1DB0 : aliased constant Code_Point_Array := (16#1DB0#,16#10000274#);
   S1DB1 : aliased constant Code_Point_Array := (16#1DB1#,16#10000275#);
   S1DB2 : aliased constant Code_Point_Array := (16#1DB2#,16#10000278#);
   S1DB3 : aliased constant Code_Point_Array := (16#1DB3#,16#10000282#);
   S1DB4 : aliased constant Code_Point_Array := (16#1DB4#,16#10000283#);
   S1DB5 : aliased constant Code_Point_Array := (16#1DB5#,16#100001AB#);
   S1DB6 : aliased constant Code_Point_Array := (16#1DB6#,16#10000289#);
   S1DB7 : aliased constant Code_Point_Array := (16#1DB7#,16#1000028A#);
   S1DB8 : aliased constant Code_Point_Array := (16#1DB8#,16#10001D1C#);
   S1DB9 : aliased constant Code_Point_Array := (16#1DB9#,16#1000028B#);
   S1DBA : aliased constant Code_Point_Array := (16#1DBA#,16#1000028C#);
   S1DBB : aliased constant Code_Point_Array := (16#1DBB#,16#1000007A#);
   S1DBC : aliased constant Code_Point_Array := (16#1DBC#,16#10000290#);
   S1DBD : aliased constant Code_Point_Array := (16#1DBD#,16#10000291#);
   S1DBE : aliased constant Code_Point_Array := (16#1DBE#,16#10000292#);
   S1DBF : aliased constant Code_Point_Array := (16#1DBF#,16#100003B8#);
   S1E00 : aliased constant Code_Point_Array := (16#1E00#,16#41#,16#325#);
   S1E01 : aliased constant Code_Point_Array := (16#1E01#,16#61#,16#325#);
   S1E02 : aliased constant Code_Point_Array := (16#1E02#,16#42#,16#307#);
   S1E03 : aliased constant Code_Point_Array := (16#1E03#,16#62#,16#307#);
   S1E04 : aliased constant Code_Point_Array := (16#1E04#,16#42#,16#323#);
   S1E05 : aliased constant Code_Point_Array := (16#1E05#,16#62#,16#323#);
   S1E06 : aliased constant Code_Point_Array := (16#1E06#,16#42#,16#331#);
   S1E07 : aliased constant Code_Point_Array := (16#1E07#,16#62#,16#331#);
   S1E08 : aliased constant Code_Point_Array := (16#1E08#,16#C7#,16#301#);
   S1E09 : aliased constant Code_Point_Array := (16#1E09#,16#E7#,16#301#);
   S1E0A : aliased constant Code_Point_Array := (16#1E0A#,16#44#,16#307#);
   S1E0B : aliased constant Code_Point_Array := (16#1E0B#,16#64#,16#307#);
   S1E0C : aliased constant Code_Point_Array := (16#1E0C#,16#44#,16#323#);
   S1E0D : aliased constant Code_Point_Array := (16#1E0D#,16#64#,16#323#);
   S1E0E : aliased constant Code_Point_Array := (16#1E0E#,16#44#,16#331#);
   S1E0F : aliased constant Code_Point_Array := (16#1E0F#,16#64#,16#331#);
   S1E10 : aliased constant Code_Point_Array := (16#1E10#,16#44#,16#327#);
   S1E11 : aliased constant Code_Point_Array := (16#1E11#,16#64#,16#327#);
   S1E12 : aliased constant Code_Point_Array := (16#1E12#,16#44#,16#32D#);
   S1E13 : aliased constant Code_Point_Array := (16#1E13#,16#64#,16#32D#);
   S1E14 : aliased constant Code_Point_Array := (16#1E14#,16#112#,16#300#);
   S1E15 : aliased constant Code_Point_Array := (16#1E15#,16#113#,16#300#);
   S1E16 : aliased constant Code_Point_Array := (16#1E16#,16#112#,16#301#);
   S1E17 : aliased constant Code_Point_Array := (16#1E17#,16#113#,16#301#);
   S1E18 : aliased constant Code_Point_Array := (16#1E18#,16#45#,16#32D#);
   S1E19 : aliased constant Code_Point_Array := (16#1E19#,16#65#,16#32D#);
   S1E1A : aliased constant Code_Point_Array := (16#1E1A#,16#45#,16#330#);
   S1E1B : aliased constant Code_Point_Array := (16#1E1B#,16#65#,16#330#);
   S1E1C : aliased constant Code_Point_Array := (16#1E1C#,16#228#,16#306#);
   S1E1D : aliased constant Code_Point_Array := (16#1E1D#,16#229#,16#306#);
   S1E1E : aliased constant Code_Point_Array := (16#1E1E#,16#46#,16#307#);
   S1E1F : aliased constant Code_Point_Array := (16#1E1F#,16#66#,16#307#);
   S1E20 : aliased constant Code_Point_Array := (16#1E20#,16#47#,16#304#);
   S1E21 : aliased constant Code_Point_Array := (16#1E21#,16#67#,16#304#);
   S1E22 : aliased constant Code_Point_Array := (16#1E22#,16#48#,16#307#);
   S1E23 : aliased constant Code_Point_Array := (16#1E23#,16#68#,16#307#);
   S1E24 : aliased constant Code_Point_Array := (16#1E24#,16#48#,16#323#);
   S1E25 : aliased constant Code_Point_Array := (16#1E25#,16#68#,16#323#);
   S1E26 : aliased constant Code_Point_Array := (16#1E26#,16#48#,16#308#);
   S1E27 : aliased constant Code_Point_Array := (16#1E27#,16#68#,16#308#);
   S1E28 : aliased constant Code_Point_Array := (16#1E28#,16#48#,16#327#);
   S1E29 : aliased constant Code_Point_Array := (16#1E29#,16#68#,16#327#);
   S1E2A : aliased constant Code_Point_Array := (16#1E2A#,16#48#,16#32E#);
   S1E2B : aliased constant Code_Point_Array := (16#1E2B#,16#68#,16#32E#);
   S1E2C : aliased constant Code_Point_Array := (16#1E2C#,16#49#,16#330#);
   S1E2D : aliased constant Code_Point_Array := (16#1E2D#,16#69#,16#330#);
   S1E2E : aliased constant Code_Point_Array := (16#1E2E#,16#CF#,16#301#);
   S1E2F : aliased constant Code_Point_Array := (16#1E2F#,16#EF#,16#301#);
   S1E30 : aliased constant Code_Point_Array := (16#1E30#,16#4B#,16#301#);
   S1E31 : aliased constant Code_Point_Array := (16#1E31#,16#6B#,16#301#);
   S1E32 : aliased constant Code_Point_Array := (16#1E32#,16#4B#,16#323#);
   S1E33 : aliased constant Code_Point_Array := (16#1E33#,16#6B#,16#323#);
   S1E34 : aliased constant Code_Point_Array := (16#1E34#,16#4B#,16#331#);
   S1E35 : aliased constant Code_Point_Array := (16#1E35#,16#6B#,16#331#);
   S1E36 : aliased constant Code_Point_Array := (16#1E36#,16#4C#,16#323#);
   S1E37 : aliased constant Code_Point_Array := (16#1E37#,16#6C#,16#323#);
   S1E38 : aliased constant Code_Point_Array := (16#1E38#,16#1E36#,16#304#);
   S1E39 : aliased constant Code_Point_Array := (16#1E39#,16#1E37#,16#304#);
   S1E3A : aliased constant Code_Point_Array := (16#1E3A#,16#4C#,16#331#);
   S1E3B : aliased constant Code_Point_Array := (16#1E3B#,16#6C#,16#331#);
   S1E3C : aliased constant Code_Point_Array := (16#1E3C#,16#4C#,16#32D#);
   S1E3D : aliased constant Code_Point_Array := (16#1E3D#,16#6C#,16#32D#);
   S1E3E : aliased constant Code_Point_Array := (16#1E3E#,16#4D#,16#301#);
   S1E3F : aliased constant Code_Point_Array := (16#1E3F#,16#6D#,16#301#);
   S1E40 : aliased constant Code_Point_Array := (16#1E40#,16#4D#,16#307#);
   S1E41 : aliased constant Code_Point_Array := (16#1E41#,16#6D#,16#307#);
   S1E42 : aliased constant Code_Point_Array := (16#1E42#,16#4D#,16#323#);
   S1E43 : aliased constant Code_Point_Array := (16#1E43#,16#6D#,16#323#);
   S1E44 : aliased constant Code_Point_Array := (16#1E44#,16#4E#,16#307#);
   S1E45 : aliased constant Code_Point_Array := (16#1E45#,16#6E#,16#307#);
   S1E46 : aliased constant Code_Point_Array := (16#1E46#,16#4E#,16#323#);
   S1E47 : aliased constant Code_Point_Array := (16#1E47#,16#6E#,16#323#);
   S1E48 : aliased constant Code_Point_Array := (16#1E48#,16#4E#,16#331#);
   S1E49 : aliased constant Code_Point_Array := (16#1E49#,16#6E#,16#331#);
   S1E4A : aliased constant Code_Point_Array := (16#1E4A#,16#4E#,16#32D#);
   S1E4B : aliased constant Code_Point_Array := (16#1E4B#,16#6E#,16#32D#);
   S1E4C : aliased constant Code_Point_Array := (16#1E4C#,16#D5#,16#301#);
   S1E4D : aliased constant Code_Point_Array := (16#1E4D#,16#F5#,16#301#);
   S1E4E : aliased constant Code_Point_Array := (16#1E4E#,16#D5#,16#308#);
   S1E4F : aliased constant Code_Point_Array := (16#1E4F#,16#F5#,16#308#);
   S1E50 : aliased constant Code_Point_Array := (16#1E50#,16#14C#,16#300#);
   S1E51 : aliased constant Code_Point_Array := (16#1E51#,16#14D#,16#300#);
   S1E52 : aliased constant Code_Point_Array := (16#1E52#,16#14C#,16#301#);
   S1E53 : aliased constant Code_Point_Array := (16#1E53#,16#14D#,16#301#);
   S1E54 : aliased constant Code_Point_Array := (16#1E54#,16#50#,16#301#);
   S1E55 : aliased constant Code_Point_Array := (16#1E55#,16#70#,16#301#);
   S1E56 : aliased constant Code_Point_Array := (16#1E56#,16#50#,16#307#);
   S1E57 : aliased constant Code_Point_Array := (16#1E57#,16#70#,16#307#);
   S1E58 : aliased constant Code_Point_Array := (16#1E58#,16#52#,16#307#);
   S1E59 : aliased constant Code_Point_Array := (16#1E59#,16#72#,16#307#);
   S1E5A : aliased constant Code_Point_Array := (16#1E5A#,16#52#,16#323#);
   S1E5B : aliased constant Code_Point_Array := (16#1E5B#,16#72#,16#323#);
   S1E5C : aliased constant Code_Point_Array := (16#1E5C#,16#1E5A#,16#304#);
   S1E5D : aliased constant Code_Point_Array := (16#1E5D#,16#1E5B#,16#304#);
   S1E5E : aliased constant Code_Point_Array := (16#1E5E#,16#52#,16#331#);
   S1E5F : aliased constant Code_Point_Array := (16#1E5F#,16#72#,16#331#);
   S1E60 : aliased constant Code_Point_Array := (16#1E60#,16#53#,16#307#);
   S1E61 : aliased constant Code_Point_Array := (16#1E61#,16#73#,16#307#);
   S1E62 : aliased constant Code_Point_Array := (16#1E62#,16#53#,16#323#);
   S1E63 : aliased constant Code_Point_Array := (16#1E63#,16#73#,16#323#);
   S1E64 : aliased constant Code_Point_Array := (16#1E64#,16#15A#,16#307#);
   S1E65 : aliased constant Code_Point_Array := (16#1E65#,16#15B#,16#307#);
   S1E66 : aliased constant Code_Point_Array := (16#1E66#,16#160#,16#307#);
   S1E67 : aliased constant Code_Point_Array := (16#1E67#,16#161#,16#307#);
   S1E68 : aliased constant Code_Point_Array := (16#1E68#,16#1E62#,16#307#);
   S1E69 : aliased constant Code_Point_Array := (16#1E69#,16#1E63#,16#307#);
   S1E6A : aliased constant Code_Point_Array := (16#1E6A#,16#54#,16#307#);
   S1E6B : aliased constant Code_Point_Array := (16#1E6B#,16#74#,16#307#);
   S1E6C : aliased constant Code_Point_Array := (16#1E6C#,16#54#,16#323#);
   S1E6D : aliased constant Code_Point_Array := (16#1E6D#,16#74#,16#323#);
   S1E6E : aliased constant Code_Point_Array := (16#1E6E#,16#54#,16#331#);
   S1E6F : aliased constant Code_Point_Array := (16#1E6F#,16#74#,16#331#);
   S1E70 : aliased constant Code_Point_Array := (16#1E70#,16#54#,16#32D#);
   S1E71 : aliased constant Code_Point_Array := (16#1E71#,16#74#,16#32D#);
   S1E72 : aliased constant Code_Point_Array := (16#1E72#,16#55#,16#324#);
   S1E73 : aliased constant Code_Point_Array := (16#1E73#,16#75#,16#324#);
   S1E74 : aliased constant Code_Point_Array := (16#1E74#,16#55#,16#330#);
   S1E75 : aliased constant Code_Point_Array := (16#1E75#,16#75#,16#330#);
   S1E76 : aliased constant Code_Point_Array := (16#1E76#,16#55#,16#32D#);
   S1E77 : aliased constant Code_Point_Array := (16#1E77#,16#75#,16#32D#);
   S1E78 : aliased constant Code_Point_Array := (16#1E78#,16#168#,16#301#);
   S1E79 : aliased constant Code_Point_Array := (16#1E79#,16#169#,16#301#);
   S1E7A : aliased constant Code_Point_Array := (16#1E7A#,16#16A#,16#308#);
   S1E7B : aliased constant Code_Point_Array := (16#1E7B#,16#16B#,16#308#);
   S1E7C : aliased constant Code_Point_Array := (16#1E7C#,16#56#,16#303#);
   S1E7D : aliased constant Code_Point_Array := (16#1E7D#,16#76#,16#303#);
   S1E7E : aliased constant Code_Point_Array := (16#1E7E#,16#56#,16#323#);
   S1E7F : aliased constant Code_Point_Array := (16#1E7F#,16#76#,16#323#);
   S1E80 : aliased constant Code_Point_Array := (16#1E80#,16#57#,16#300#);
   S1E81 : aliased constant Code_Point_Array := (16#1E81#,16#77#,16#300#);
   S1E82 : aliased constant Code_Point_Array := (16#1E82#,16#57#,16#301#);
   S1E83 : aliased constant Code_Point_Array := (16#1E83#,16#77#,16#301#);
   S1E84 : aliased constant Code_Point_Array := (16#1E84#,16#57#,16#308#);
   S1E85 : aliased constant Code_Point_Array := (16#1E85#,16#77#,16#308#);
   S1E86 : aliased constant Code_Point_Array := (16#1E86#,16#57#,16#307#);
   S1E87 : aliased constant Code_Point_Array := (16#1E87#,16#77#,16#307#);
   S1E88 : aliased constant Code_Point_Array := (16#1E88#,16#57#,16#323#);
   S1E89 : aliased constant Code_Point_Array := (16#1E89#,16#77#,16#323#);
   S1E8A : aliased constant Code_Point_Array := (16#1E8A#,16#58#,16#307#);
   S1E8B : aliased constant Code_Point_Array := (16#1E8B#,16#78#,16#307#);
   S1E8C : aliased constant Code_Point_Array := (16#1E8C#,16#58#,16#308#);
   S1E8D : aliased constant Code_Point_Array := (16#1E8D#,16#78#,16#308#);
   S1E8E : aliased constant Code_Point_Array := (16#1E8E#,16#59#,16#307#);
   S1E8F : aliased constant Code_Point_Array := (16#1E8F#,16#79#,16#307#);
   S1E90 : aliased constant Code_Point_Array := (16#1E90#,16#5A#,16#302#);
   S1E91 : aliased constant Code_Point_Array := (16#1E91#,16#7A#,16#302#);
   S1E92 : aliased constant Code_Point_Array := (16#1E92#,16#5A#,16#323#);
   S1E93 : aliased constant Code_Point_Array := (16#1E93#,16#7A#,16#323#);
   S1E94 : aliased constant Code_Point_Array := (16#1E94#,16#5A#,16#331#);
   S1E95 : aliased constant Code_Point_Array := (16#1E95#,16#7A#,16#331#);
   S1E96 : aliased constant Code_Point_Array := (16#1E96#,16#68#,16#331#);
   S1E97 : aliased constant Code_Point_Array := (16#1E97#,16#74#,16#308#);
   S1E98 : aliased constant Code_Point_Array := (16#1E98#,16#77#,16#30A#);
   S1E99 : aliased constant Code_Point_Array := (16#1E99#,16#79#,16#30A#);
   S1E9A : aliased constant Code_Point_Array := (16#1E9A#,16#10000061#,16#2BE#);
   S1E9B : aliased constant Code_Point_Array := (16#1E9B#,16#17F#,16#307#);
   S1EA0 : aliased constant Code_Point_Array := (16#1EA0#,16#41#,16#323#);
   S1EA1 : aliased constant Code_Point_Array := (16#1EA1#,16#61#,16#323#);
   S1EA2 : aliased constant Code_Point_Array := (16#1EA2#,16#41#,16#309#);
   S1EA3 : aliased constant Code_Point_Array := (16#1EA3#,16#61#,16#309#);
   S1EA4 : aliased constant Code_Point_Array := (16#1EA4#,16#C2#,16#301#);
   S1EA5 : aliased constant Code_Point_Array := (16#1EA5#,16#E2#,16#301#);
   S1EA6 : aliased constant Code_Point_Array := (16#1EA6#,16#C2#,16#300#);
   S1EA7 : aliased constant Code_Point_Array := (16#1EA7#,16#E2#,16#300#);
   S1EA8 : aliased constant Code_Point_Array := (16#1EA8#,16#C2#,16#309#);
   S1EA9 : aliased constant Code_Point_Array := (16#1EA9#,16#E2#,16#309#);
   S1EAA : aliased constant Code_Point_Array := (16#1EAA#,16#C2#,16#303#);
   S1EAB : aliased constant Code_Point_Array := (16#1EAB#,16#E2#,16#303#);
   S1EAC : aliased constant Code_Point_Array := (16#1EAC#,16#1EA0#,16#302#);
   S1EAD : aliased constant Code_Point_Array := (16#1EAD#,16#1EA1#,16#302#);
   S1EAE : aliased constant Code_Point_Array := (16#1EAE#,16#102#,16#301#);
   S1EAF : aliased constant Code_Point_Array := (16#1EAF#,16#103#,16#301#);
   S1EB0 : aliased constant Code_Point_Array := (16#1EB0#,16#102#,16#300#);
   S1EB1 : aliased constant Code_Point_Array := (16#1EB1#,16#103#,16#300#);
   S1EB2 : aliased constant Code_Point_Array := (16#1EB2#,16#102#,16#309#);
   S1EB3 : aliased constant Code_Point_Array := (16#1EB3#,16#103#,16#309#);
   S1EB4 : aliased constant Code_Point_Array := (16#1EB4#,16#102#,16#303#);
   S1EB5 : aliased constant Code_Point_Array := (16#1EB5#,16#103#,16#303#);
   S1EB6 : aliased constant Code_Point_Array := (16#1EB6#,16#1EA0#,16#306#);
   S1EB7 : aliased constant Code_Point_Array := (16#1EB7#,16#1EA1#,16#306#);
   S1EB8 : aliased constant Code_Point_Array := (16#1EB8#,16#45#,16#323#);
   S1EB9 : aliased constant Code_Point_Array := (16#1EB9#,16#65#,16#323#);
   S1EBA : aliased constant Code_Point_Array := (16#1EBA#,16#45#,16#309#);
   S1EBB : aliased constant Code_Point_Array := (16#1EBB#,16#65#,16#309#);
   S1EBC : aliased constant Code_Point_Array := (16#1EBC#,16#45#,16#303#);
   S1EBD : aliased constant Code_Point_Array := (16#1EBD#,16#65#,16#303#);
   S1EBE : aliased constant Code_Point_Array := (16#1EBE#,16#CA#,16#301#);
   S1EBF : aliased constant Code_Point_Array := (16#1EBF#,16#EA#,16#301#);
   S1EC0 : aliased constant Code_Point_Array := (16#1EC0#,16#CA#,16#300#);
   S1EC1 : aliased constant Code_Point_Array := (16#1EC1#,16#EA#,16#300#);
   S1EC2 : aliased constant Code_Point_Array := (16#1EC2#,16#CA#,16#309#);
   S1EC3 : aliased constant Code_Point_Array := (16#1EC3#,16#EA#,16#309#);
   S1EC4 : aliased constant Code_Point_Array := (16#1EC4#,16#CA#,16#303#);
   S1EC5 : aliased constant Code_Point_Array := (16#1EC5#,16#EA#,16#303#);
   S1EC6 : aliased constant Code_Point_Array := (16#1EC6#,16#1EB8#,16#302#);
   S1EC7 : aliased constant Code_Point_Array := (16#1EC7#,16#1EB9#,16#302#);
   S1EC8 : aliased constant Code_Point_Array := (16#1EC8#,16#49#,16#309#);
   S1EC9 : aliased constant Code_Point_Array := (16#1EC9#,16#69#,16#309#);
   S1ECA : aliased constant Code_Point_Array := (16#1ECA#,16#49#,16#323#);
   S1ECB : aliased constant Code_Point_Array := (16#1ECB#,16#69#,16#323#);
   S1ECC : aliased constant Code_Point_Array := (16#1ECC#,16#4F#,16#323#);
   S1ECD : aliased constant Code_Point_Array := (16#1ECD#,16#6F#,16#323#);
   S1ECE : aliased constant Code_Point_Array := (16#1ECE#,16#4F#,16#309#);
   S1ECF : aliased constant Code_Point_Array := (16#1ECF#,16#6F#,16#309#);
   S1ED0 : aliased constant Code_Point_Array := (16#1ED0#,16#D4#,16#301#);
   S1ED1 : aliased constant Code_Point_Array := (16#1ED1#,16#F4#,16#301#);
   S1ED2 : aliased constant Code_Point_Array := (16#1ED2#,16#D4#,16#300#);
   S1ED3 : aliased constant Code_Point_Array := (16#1ED3#,16#F4#,16#300#);
   S1ED4 : aliased constant Code_Point_Array := (16#1ED4#,16#D4#,16#309#);
   S1ED5 : aliased constant Code_Point_Array := (16#1ED5#,16#F4#,16#309#);
   S1ED6 : aliased constant Code_Point_Array := (16#1ED6#,16#D4#,16#303#);
   S1ED7 : aliased constant Code_Point_Array := (16#1ED7#,16#F4#,16#303#);
   S1ED8 : aliased constant Code_Point_Array := (16#1ED8#,16#1ECC#,16#302#);
   S1ED9 : aliased constant Code_Point_Array := (16#1ED9#,16#1ECD#,16#302#);
   S1EDA : aliased constant Code_Point_Array := (16#1EDA#,16#1A0#,16#301#);
   S1EDB : aliased constant Code_Point_Array := (16#1EDB#,16#1A1#,16#301#);
   S1EDC : aliased constant Code_Point_Array := (16#1EDC#,16#1A0#,16#300#);
   S1EDD : aliased constant Code_Point_Array := (16#1EDD#,16#1A1#,16#300#);
   S1EDE : aliased constant Code_Point_Array := (16#1EDE#,16#1A0#,16#309#);
   S1EDF : aliased constant Code_Point_Array := (16#1EDF#,16#1A1#,16#309#);
   S1EE0 : aliased constant Code_Point_Array := (16#1EE0#,16#1A0#,16#303#);
   S1EE1 : aliased constant Code_Point_Array := (16#1EE1#,16#1A1#,16#303#);
   S1EE2 : aliased constant Code_Point_Array := (16#1EE2#,16#1A0#,16#323#);
   S1EE3 : aliased constant Code_Point_Array := (16#1EE3#,16#1A1#,16#323#);
   S1EE4 : aliased constant Code_Point_Array := (16#1EE4#,16#55#,16#323#);
   S1EE5 : aliased constant Code_Point_Array := (16#1EE5#,16#75#,16#323#);
   S1EE6 : aliased constant Code_Point_Array := (16#1EE6#,16#55#,16#309#);
   S1EE7 : aliased constant Code_Point_Array := (16#1EE7#,16#75#,16#309#);
   S1EE8 : aliased constant Code_Point_Array := (16#1EE8#,16#1AF#,16#301#);
   S1EE9 : aliased constant Code_Point_Array := (16#1EE9#,16#1B0#,16#301#);
   S1EEA : aliased constant Code_Point_Array := (16#1EEA#,16#1AF#,16#300#);
   S1EEB : aliased constant Code_Point_Array := (16#1EEB#,16#1B0#,16#300#);
   S1EEC : aliased constant Code_Point_Array := (16#1EEC#,16#1AF#,16#309#);
   S1EED : aliased constant Code_Point_Array := (16#1EED#,16#1B0#,16#309#);
   S1EEE : aliased constant Code_Point_Array := (16#1EEE#,16#1AF#,16#303#);
   S1EEF : aliased constant Code_Point_Array := (16#1EEF#,16#1B0#,16#303#);
   S1EF0 : aliased constant Code_Point_Array := (16#1EF0#,16#1AF#,16#323#);
   S1EF1 : aliased constant Code_Point_Array := (16#1EF1#,16#1B0#,16#323#);
   S1EF2 : aliased constant Code_Point_Array := (16#1EF2#,16#59#,16#300#);
   S1EF3 : aliased constant Code_Point_Array := (16#1EF3#,16#79#,16#300#);
   S1EF4 : aliased constant Code_Point_Array := (16#1EF4#,16#59#,16#323#);
   S1EF5 : aliased constant Code_Point_Array := (16#1EF5#,16#79#,16#323#);
   S1EF6 : aliased constant Code_Point_Array := (16#1EF6#,16#59#,16#309#);
   S1EF7 : aliased constant Code_Point_Array := (16#1EF7#,16#79#,16#309#);
   S1EF8 : aliased constant Code_Point_Array := (16#1EF8#,16#59#,16#303#);
   S1EF9 : aliased constant Code_Point_Array := (16#1EF9#,16#79#,16#303#);
   S1F00 : aliased constant Code_Point_Array := (16#1F00#,16#3B1#,16#313#);
   S1F01 : aliased constant Code_Point_Array := (16#1F01#,16#3B1#,16#314#);
   S1F02 : aliased constant Code_Point_Array := (16#1F02#,16#1F00#,16#300#);
   S1F03 : aliased constant Code_Point_Array := (16#1F03#,16#1F01#,16#300#);
   S1F04 : aliased constant Code_Point_Array := (16#1F04#,16#1F00#,16#301#);
   S1F05 : aliased constant Code_Point_Array := (16#1F05#,16#1F01#,16#301#);
   S1F06 : aliased constant Code_Point_Array := (16#1F06#,16#1F00#,16#342#);
   S1F07 : aliased constant Code_Point_Array := (16#1F07#,16#1F01#,16#342#);
   S1F08 : aliased constant Code_Point_Array := (16#1F08#,16#391#,16#313#);
   S1F09 : aliased constant Code_Point_Array := (16#1F09#,16#391#,16#314#);
   S1F0A : aliased constant Code_Point_Array := (16#1F0A#,16#1F08#,16#300#);
   S1F0B : aliased constant Code_Point_Array := (16#1F0B#,16#1F09#,16#300#);
   S1F0C : aliased constant Code_Point_Array := (16#1F0C#,16#1F08#,16#301#);
   S1F0D : aliased constant Code_Point_Array := (16#1F0D#,16#1F09#,16#301#);
   S1F0E : aliased constant Code_Point_Array := (16#1F0E#,16#1F08#,16#342#);
   S1F0F : aliased constant Code_Point_Array := (16#1F0F#,16#1F09#,16#342#);
   S1F10 : aliased constant Code_Point_Array := (16#1F10#,16#3B5#,16#313#);
   S1F11 : aliased constant Code_Point_Array := (16#1F11#,16#3B5#,16#314#);
   S1F12 : aliased constant Code_Point_Array := (16#1F12#,16#1F10#,16#300#);
   S1F13 : aliased constant Code_Point_Array := (16#1F13#,16#1F11#,16#300#);
   S1F14 : aliased constant Code_Point_Array := (16#1F14#,16#1F10#,16#301#);
   S1F15 : aliased constant Code_Point_Array := (16#1F15#,16#1F11#,16#301#);
   S1F18 : aliased constant Code_Point_Array := (16#1F18#,16#395#,16#313#);
   S1F19 : aliased constant Code_Point_Array := (16#1F19#,16#395#,16#314#);
   S1F1A : aliased constant Code_Point_Array := (16#1F1A#,16#1F18#,16#300#);
   S1F1B : aliased constant Code_Point_Array := (16#1F1B#,16#1F19#,16#300#);
   S1F1C : aliased constant Code_Point_Array := (16#1F1C#,16#1F18#,16#301#);
   S1F1D : aliased constant Code_Point_Array := (16#1F1D#,16#1F19#,16#301#);
   S1F20 : aliased constant Code_Point_Array := (16#1F20#,16#3B7#,16#313#);
   S1F21 : aliased constant Code_Point_Array := (16#1F21#,16#3B7#,16#314#);
   S1F22 : aliased constant Code_Point_Array := (16#1F22#,16#1F20#,16#300#);
   S1F23 : aliased constant Code_Point_Array := (16#1F23#,16#1F21#,16#300#);
   S1F24 : aliased constant Code_Point_Array := (16#1F24#,16#1F20#,16#301#);
   S1F25 : aliased constant Code_Point_Array := (16#1F25#,16#1F21#,16#301#);
   S1F26 : aliased constant Code_Point_Array := (16#1F26#,16#1F20#,16#342#);
   S1F27 : aliased constant Code_Point_Array := (16#1F27#,16#1F21#,16#342#);
   S1F28 : aliased constant Code_Point_Array := (16#1F28#,16#397#,16#313#);
   S1F29 : aliased constant Code_Point_Array := (16#1F29#,16#397#,16#314#);
   S1F2A : aliased constant Code_Point_Array := (16#1F2A#,16#1F28#,16#300#);
   S1F2B : aliased constant Code_Point_Array := (16#1F2B#,16#1F29#,16#300#);
   S1F2C : aliased constant Code_Point_Array := (16#1F2C#,16#1F28#,16#301#);
   S1F2D : aliased constant Code_Point_Array := (16#1F2D#,16#1F29#,16#301#);
   S1F2E : aliased constant Code_Point_Array := (16#1F2E#,16#1F28#,16#342#);
   S1F2F : aliased constant Code_Point_Array := (16#1F2F#,16#1F29#,16#342#);
   S1F30 : aliased constant Code_Point_Array := (16#1F30#,16#3B9#,16#313#);
   S1F31 : aliased constant Code_Point_Array := (16#1F31#,16#3B9#,16#314#);
   S1F32 : aliased constant Code_Point_Array := (16#1F32#,16#1F30#,16#300#);
   S1F33 : aliased constant Code_Point_Array := (16#1F33#,16#1F31#,16#300#);
   S1F34 : aliased constant Code_Point_Array := (16#1F34#,16#1F30#,16#301#);
   S1F35 : aliased constant Code_Point_Array := (16#1F35#,16#1F31#,16#301#);
   S1F36 : aliased constant Code_Point_Array := (16#1F36#,16#1F30#,16#342#);
   S1F37 : aliased constant Code_Point_Array := (16#1F37#,16#1F31#,16#342#);
   S1F38 : aliased constant Code_Point_Array := (16#1F38#,16#399#,16#313#);
   S1F39 : aliased constant Code_Point_Array := (16#1F39#,16#399#,16#314#);
   S1F3A : aliased constant Code_Point_Array := (16#1F3A#,16#1F38#,16#300#);
   S1F3B : aliased constant Code_Point_Array := (16#1F3B#,16#1F39#,16#300#);
   S1F3C : aliased constant Code_Point_Array := (16#1F3C#,16#1F38#,16#301#);
   S1F3D : aliased constant Code_Point_Array := (16#1F3D#,16#1F39#,16#301#);
   S1F3E : aliased constant Code_Point_Array := (16#1F3E#,16#1F38#,16#342#);
   S1F3F : aliased constant Code_Point_Array := (16#1F3F#,16#1F39#,16#342#);
   S1F40 : aliased constant Code_Point_Array := (16#1F40#,16#3BF#,16#313#);
   S1F41 : aliased constant Code_Point_Array := (16#1F41#,16#3BF#,16#314#);
   S1F42 : aliased constant Code_Point_Array := (16#1F42#,16#1F40#,16#300#);
   S1F43 : aliased constant Code_Point_Array := (16#1F43#,16#1F41#,16#300#);
   S1F44 : aliased constant Code_Point_Array := (16#1F44#,16#1F40#,16#301#);
   S1F45 : aliased constant Code_Point_Array := (16#1F45#,16#1F41#,16#301#);
   S1F48 : aliased constant Code_Point_Array := (16#1F48#,16#39F#,16#313#);
   S1F49 : aliased constant Code_Point_Array := (16#1F49#,16#39F#,16#314#);
   S1F4A : aliased constant Code_Point_Array := (16#1F4A#,16#1F48#,16#300#);
   S1F4B : aliased constant Code_Point_Array := (16#1F4B#,16#1F49#,16#300#);
   S1F4C : aliased constant Code_Point_Array := (16#1F4C#,16#1F48#,16#301#);
   S1F4D : aliased constant Code_Point_Array := (16#1F4D#,16#1F49#,16#301#);
   S1F50 : aliased constant Code_Point_Array := (16#1F50#,16#3C5#,16#313#);
   S1F51 : aliased constant Code_Point_Array := (16#1F51#,16#3C5#,16#314#);
   S1F52 : aliased constant Code_Point_Array := (16#1F52#,16#1F50#,16#300#);
   S1F53 : aliased constant Code_Point_Array := (16#1F53#,16#1F51#,16#300#);
   S1F54 : aliased constant Code_Point_Array := (16#1F54#,16#1F50#,16#301#);
   S1F55 : aliased constant Code_Point_Array := (16#1F55#,16#1F51#,16#301#);
   S1F56 : aliased constant Code_Point_Array := (16#1F56#,16#1F50#,16#342#);
   S1F57 : aliased constant Code_Point_Array := (16#1F57#,16#1F51#,16#342#);
   S1F59 : aliased constant Code_Point_Array := (16#1F59#,16#3A5#,16#314#);
   S1F5B : aliased constant Code_Point_Array := (16#1F5B#,16#1F59#,16#300#);
   S1F5D : aliased constant Code_Point_Array := (16#1F5D#,16#1F59#,16#301#);
   S1F5F : aliased constant Code_Point_Array := (16#1F5F#,16#1F59#,16#342#);
   S1F60 : aliased constant Code_Point_Array := (16#1F60#,16#3C9#,16#313#);
   S1F61 : aliased constant Code_Point_Array := (16#1F61#,16#3C9#,16#314#);
   S1F62 : aliased constant Code_Point_Array := (16#1F62#,16#1F60#,16#300#);
   S1F63 : aliased constant Code_Point_Array := (16#1F63#,16#1F61#,16#300#);
   S1F64 : aliased constant Code_Point_Array := (16#1F64#,16#1F60#,16#301#);
   S1F65 : aliased constant Code_Point_Array := (16#1F65#,16#1F61#,16#301#);
   S1F66 : aliased constant Code_Point_Array := (16#1F66#,16#1F60#,16#342#);
   S1F67 : aliased constant Code_Point_Array := (16#1F67#,16#1F61#,16#342#);
   S1F68 : aliased constant Code_Point_Array := (16#1F68#,16#3A9#,16#313#);
   S1F69 : aliased constant Code_Point_Array := (16#1F69#,16#3A9#,16#314#);
   S1F6A : aliased constant Code_Point_Array := (16#1F6A#,16#1F68#,16#300#);
   S1F6B : aliased constant Code_Point_Array := (16#1F6B#,16#1F69#,16#300#);
   S1F6C : aliased constant Code_Point_Array := (16#1F6C#,16#1F68#,16#301#);
   S1F6D : aliased constant Code_Point_Array := (16#1F6D#,16#1F69#,16#301#);
   S1F6E : aliased constant Code_Point_Array := (16#1F6E#,16#1F68#,16#342#);
   S1F6F : aliased constant Code_Point_Array := (16#1F6F#,16#1F69#,16#342#);
   S1F70 : aliased constant Code_Point_Array := (16#1F70#,16#3B1#,16#300#);
   S1F71 : aliased constant Code_Point_Array := (16#1F71#,16#3AC#);
   S1F72 : aliased constant Code_Point_Array := (16#1F72#,16#3B5#,16#300#);
   S1F73 : aliased constant Code_Point_Array := (16#1F73#,16#3AD#);
   S1F74 : aliased constant Code_Point_Array := (16#1F74#,16#3B7#,16#300#);
   S1F75 : aliased constant Code_Point_Array := (16#1F75#,16#3AE#);
   S1F76 : aliased constant Code_Point_Array := (16#1F76#,16#3B9#,16#300#);
   S1F77 : aliased constant Code_Point_Array := (16#1F77#,16#3AF#);
   S1F78 : aliased constant Code_Point_Array := (16#1F78#,16#3BF#,16#300#);
   S1F79 : aliased constant Code_Point_Array := (16#1F79#,16#3CC#);
   S1F7A : aliased constant Code_Point_Array := (16#1F7A#,16#3C5#,16#300#);
   S1F7B : aliased constant Code_Point_Array := (16#1F7B#,16#3CD#);
   S1F7C : aliased constant Code_Point_Array := (16#1F7C#,16#3C9#,16#300#);
   S1F7D : aliased constant Code_Point_Array := (16#1F7D#,16#3CE#);
   S1F80 : aliased constant Code_Point_Array := (16#1F80#,16#1F00#,16#345#);
   S1F81 : aliased constant Code_Point_Array := (16#1F81#,16#1F01#,16#345#);
   S1F82 : aliased constant Code_Point_Array := (16#1F82#,16#1F02#,16#345#);
   S1F83 : aliased constant Code_Point_Array := (16#1F83#,16#1F03#,16#345#);
   S1F84 : aliased constant Code_Point_Array := (16#1F84#,16#1F04#,16#345#);
   S1F85 : aliased constant Code_Point_Array := (16#1F85#,16#1F05#,16#345#);
   S1F86 : aliased constant Code_Point_Array := (16#1F86#,16#1F06#,16#345#);
   S1F87 : aliased constant Code_Point_Array := (16#1F87#,16#1F07#,16#345#);
   S1F88 : aliased constant Code_Point_Array := (16#1F88#,16#1F08#,16#345#);
   S1F89 : aliased constant Code_Point_Array := (16#1F89#,16#1F09#,16#345#);
   S1F8A : aliased constant Code_Point_Array := (16#1F8A#,16#1F0A#,16#345#);
   S1F8B : aliased constant Code_Point_Array := (16#1F8B#,16#1F0B#,16#345#);
   S1F8C : aliased constant Code_Point_Array := (16#1F8C#,16#1F0C#,16#345#);
   S1F8D : aliased constant Code_Point_Array := (16#1F8D#,16#1F0D#,16#345#);
   S1F8E : aliased constant Code_Point_Array := (16#1F8E#,16#1F0E#,16#345#);
   S1F8F : aliased constant Code_Point_Array := (16#1F8F#,16#1F0F#,16#345#);
   S1F90 : aliased constant Code_Point_Array := (16#1F90#,16#1F20#,16#345#);
   S1F91 : aliased constant Code_Point_Array := (16#1F91#,16#1F21#,16#345#);
   S1F92 : aliased constant Code_Point_Array := (16#1F92#,16#1F22#,16#345#);
   S1F93 : aliased constant Code_Point_Array := (16#1F93#,16#1F23#,16#345#);
   S1F94 : aliased constant Code_Point_Array := (16#1F94#,16#1F24#,16#345#);
   S1F95 : aliased constant Code_Point_Array := (16#1F95#,16#1F25#,16#345#);
   S1F96 : aliased constant Code_Point_Array := (16#1F96#,16#1F26#,16#345#);
   S1F97 : aliased constant Code_Point_Array := (16#1F97#,16#1F27#,16#345#);
   S1F98 : aliased constant Code_Point_Array := (16#1F98#,16#1F28#,16#345#);
   S1F99 : aliased constant Code_Point_Array := (16#1F99#,16#1F29#,16#345#);
   S1F9A : aliased constant Code_Point_Array := (16#1F9A#,16#1F2A#,16#345#);
   S1F9B : aliased constant Code_Point_Array := (16#1F9B#,16#1F2B#,16#345#);
   S1F9C : aliased constant Code_Point_Array := (16#1F9C#,16#1F2C#,16#345#);
   S1F9D : aliased constant Code_Point_Array := (16#1F9D#,16#1F2D#,16#345#);
   S1F9E : aliased constant Code_Point_Array := (16#1F9E#,16#1F2E#,16#345#);
   S1F9F : aliased constant Code_Point_Array := (16#1F9F#,16#1F2F#,16#345#);
   S1FA0 : aliased constant Code_Point_Array := (16#1FA0#,16#1F60#,16#345#);
   S1FA1 : aliased constant Code_Point_Array := (16#1FA1#,16#1F61#,16#345#);
   S1FA2 : aliased constant Code_Point_Array := (16#1FA2#,16#1F62#,16#345#);
   S1FA3 : aliased constant Code_Point_Array := (16#1FA3#,16#1F63#,16#345#);
   S1FA4 : aliased constant Code_Point_Array := (16#1FA4#,16#1F64#,16#345#);
   S1FA5 : aliased constant Code_Point_Array := (16#1FA5#,16#1F65#,16#345#);
   S1FA6 : aliased constant Code_Point_Array := (16#1FA6#,16#1F66#,16#345#);
   S1FA7 : aliased constant Code_Point_Array := (16#1FA7#,16#1F67#,16#345#);
   S1FA8 : aliased constant Code_Point_Array := (16#1FA8#,16#1F68#,16#345#);
   S1FA9 : aliased constant Code_Point_Array := (16#1FA9#,16#1F69#,16#345#);
   S1FAA : aliased constant Code_Point_Array := (16#1FAA#,16#1F6A#,16#345#);
   S1FAB : aliased constant Code_Point_Array := (16#1FAB#,16#1F6B#,16#345#);
   S1FAC : aliased constant Code_Point_Array := (16#1FAC#,16#1F6C#,16#345#);
   S1FAD : aliased constant Code_Point_Array := (16#1FAD#,16#1F6D#,16#345#);
   S1FAE : aliased constant Code_Point_Array := (16#1FAE#,16#1F6E#,16#345#);
   S1FAF : aliased constant Code_Point_Array := (16#1FAF#,16#1F6F#,16#345#);
   S1FB0 : aliased constant Code_Point_Array := (16#1FB0#,16#3B1#,16#306#);
   S1FB1 : aliased constant Code_Point_Array := (16#1FB1#,16#3B1#,16#304#);
   S1FB2 : aliased constant Code_Point_Array := (16#1FB2#,16#1F70#,16#345#);
   S1FB3 : aliased constant Code_Point_Array := (16#1FB3#,16#3B1#,16#345#);
   S1FB4 : aliased constant Code_Point_Array := (16#1FB4#,16#3AC#,16#345#);
   S1FB6 : aliased constant Code_Point_Array := (16#1FB6#,16#3B1#,16#342#);
   S1FB7 : aliased constant Code_Point_Array := (16#1FB7#,16#1FB6#,16#345#);
   S1FB8 : aliased constant Code_Point_Array := (16#1FB8#,16#391#,16#306#);
   S1FB9 : aliased constant Code_Point_Array := (16#1FB9#,16#391#,16#304#);
   S1FBA : aliased constant Code_Point_Array := (16#1FBA#,16#391#,16#300#);
   S1FBB : aliased constant Code_Point_Array := (16#1FBB#,16#386#);
   S1FBC : aliased constant Code_Point_Array := (16#1FBC#,16#391#,16#345#);
   S1FBD : aliased constant Code_Point_Array := (16#1FBD#,16#10000020#,16#313#);
   S1FBE : aliased constant Code_Point_Array := (16#1FBE#,16#3B9#);
   S1FBF : aliased constant Code_Point_Array := (16#1FBF#,16#10000020#,16#313#);
   S1FC0 : aliased constant Code_Point_Array := (16#1FC0#,16#10000020#,16#342#);
   S1FC1 : aliased constant Code_Point_Array := (16#1FC1#,16#A8#,16#342#);
   S1FC2 : aliased constant Code_Point_Array := (16#1FC2#,16#1F74#,16#345#);
   S1FC3 : aliased constant Code_Point_Array := (16#1FC3#,16#3B7#,16#345#);
   S1FC4 : aliased constant Code_Point_Array := (16#1FC4#,16#3AE#,16#345#);
   S1FC6 : aliased constant Code_Point_Array := (16#1FC6#,16#3B7#,16#342#);
   S1FC7 : aliased constant Code_Point_Array := (16#1FC7#,16#1FC6#,16#345#);
   S1FC8 : aliased constant Code_Point_Array := (16#1FC8#,16#395#,16#300#);
   S1FC9 : aliased constant Code_Point_Array := (16#1FC9#,16#388#);
   S1FCA : aliased constant Code_Point_Array := (16#1FCA#,16#397#,16#300#);
   S1FCB : aliased constant Code_Point_Array := (16#1FCB#,16#389#);
   S1FCC : aliased constant Code_Point_Array := (16#1FCC#,16#397#,16#345#);
   S1FCD : aliased constant Code_Point_Array := (16#1FCD#,16#1FBF#,16#300#);
   S1FCE : aliased constant Code_Point_Array := (16#1FCE#,16#1FBF#,16#301#);
   S1FCF : aliased constant Code_Point_Array := (16#1FCF#,16#1FBF#,16#342#);
   S1FD0 : aliased constant Code_Point_Array := (16#1FD0#,16#3B9#,16#306#);
   S1FD1 : aliased constant Code_Point_Array := (16#1FD1#,16#3B9#,16#304#);
   S1FD2 : aliased constant Code_Point_Array := (16#1FD2#,16#3CA#,16#300#);
   S1FD3 : aliased constant Code_Point_Array := (16#1FD3#,16#390#);
   S1FD6 : aliased constant Code_Point_Array := (16#1FD6#,16#3B9#,16#342#);
   S1FD7 : aliased constant Code_Point_Array := (16#1FD7#,16#3CA#,16#342#);
   S1FD8 : aliased constant Code_Point_Array := (16#1FD8#,16#399#,16#306#);
   S1FD9 : aliased constant Code_Point_Array := (16#1FD9#,16#399#,16#304#);
   S1FDA : aliased constant Code_Point_Array := (16#1FDA#,16#399#,16#300#);
   S1FDB : aliased constant Code_Point_Array := (16#1FDB#,16#38A#);
   S1FDD : aliased constant Code_Point_Array := (16#1FDD#,16#1FFE#,16#300#);
   S1FDE : aliased constant Code_Point_Array := (16#1FDE#,16#1FFE#,16#301#);
   S1FDF : aliased constant Code_Point_Array := (16#1FDF#,16#1FFE#,16#342#);
   S1FE0 : aliased constant Code_Point_Array := (16#1FE0#,16#3C5#,16#306#);
   S1FE1 : aliased constant Code_Point_Array := (16#1FE1#,16#3C5#,16#304#);
   S1FE2 : aliased constant Code_Point_Array := (16#1FE2#,16#3CB#,16#300#);
   S1FE3 : aliased constant Code_Point_Array := (16#1FE3#,16#3B0#);
   S1FE4 : aliased constant Code_Point_Array := (16#1FE4#,16#3C1#,16#313#);
   S1FE5 : aliased constant Code_Point_Array := (16#1FE5#,16#3C1#,16#314#);
   S1FE6 : aliased constant Code_Point_Array := (16#1FE6#,16#3C5#,16#342#);
   S1FE7 : aliased constant Code_Point_Array := (16#1FE7#,16#3CB#,16#342#);
   S1FE8 : aliased constant Code_Point_Array := (16#1FE8#,16#3A5#,16#306#);
   S1FE9 : aliased constant Code_Point_Array := (16#1FE9#,16#3A5#,16#304#);
   S1FEA : aliased constant Code_Point_Array := (16#1FEA#,16#3A5#,16#300#);
   S1FEB : aliased constant Code_Point_Array := (16#1FEB#,16#38E#);
   S1FEC : aliased constant Code_Point_Array := (16#1FEC#,16#3A1#,16#314#);
   S1FED : aliased constant Code_Point_Array := (16#1FED#,16#A8#,16#300#);
   S1FEE : aliased constant Code_Point_Array := (16#1FEE#,16#385#);
   S1FEF : aliased constant Code_Point_Array := (16#1FEF#,16#60#);
   S1FF2 : aliased constant Code_Point_Array := (16#1FF2#,16#1F7C#,16#345#);
   S1FF3 : aliased constant Code_Point_Array := (16#1FF3#,16#3C9#,16#345#);
   S1FF4 : aliased constant Code_Point_Array := (16#1FF4#,16#3CE#,16#345#);
   S1FF6 : aliased constant Code_Point_Array := (16#1FF6#,16#3C9#,16#342#);
   S1FF7 : aliased constant Code_Point_Array := (16#1FF7#,16#1FF6#,16#345#);
   S1FF8 : aliased constant Code_Point_Array := (16#1FF8#,16#39F#,16#300#);
   S1FF9 : aliased constant Code_Point_Array := (16#1FF9#,16#38C#);
   S1FFA : aliased constant Code_Point_Array := (16#1FFA#,16#3A9#,16#300#);
   S1FFB : aliased constant Code_Point_Array := (16#1FFB#,16#38F#);
   S1FFC : aliased constant Code_Point_Array := (16#1FFC#,16#3A9#,16#345#);
   S1FFD : aliased constant Code_Point_Array := (16#1FFD#,16#B4#);
   S1FFE : aliased constant Code_Point_Array := (16#1FFE#,16#10000020#,16#314#);
   S2000 : aliased constant Code_Point_Array := (16#2000#,16#2002#);
   S2001 : aliased constant Code_Point_Array := (16#2001#,16#2003#);
   S2002 : aliased constant Code_Point_Array := (16#2002#,16#10000020#);
   S2003 : aliased constant Code_Point_Array := (16#2003#,16#10000020#);
   S2004 : aliased constant Code_Point_Array := (16#2004#,16#10000020#);
   S2005 : aliased constant Code_Point_Array := (16#2005#,16#10000020#);
   S2006 : aliased constant Code_Point_Array := (16#2006#,16#10000020#);
   S2007 : aliased constant Code_Point_Array := (16#2007#,16#10000020#);
   S2008 : aliased constant Code_Point_Array := (16#2008#,16#10000020#);
   S2009 : aliased constant Code_Point_Array := (16#2009#,16#10000020#);
   S200A : aliased constant Code_Point_Array := (16#200A#,16#10000020#);
   S2011 : aliased constant Code_Point_Array := (16#2011#,16#10002010#);
   S2017 : aliased constant Code_Point_Array := (16#2017#,16#10000020#,16#333#);
   S2024 : aliased constant Code_Point_Array := (16#2024#,16#1000002E#);
   S2025 : aliased constant Code_Point_Array := (16#2025#,16#1000002E#,16#2E#);
   S2026 : aliased constant Code_Point_Array := (16#2026#,16#1000002E#,16#2E#,16#2E#);
   S202F : aliased constant Code_Point_Array := (16#202F#,16#10000020#);
   S2033 : aliased constant Code_Point_Array := (16#2033#,16#10002032#,16#2032#);
   S2034 : aliased constant Code_Point_Array := (16#2034#,16#10002032#,16#2032#,16#2032#);
   S2036 : aliased constant Code_Point_Array := (16#2036#,16#10002035#,16#2035#);
   S2037 : aliased constant Code_Point_Array := (16#2037#,16#10002035#,16#2035#,16#2035#);
   S203C : aliased constant Code_Point_Array := (16#203C#,16#10000021#,16#21#);
   S203E : aliased constant Code_Point_Array := (16#203E#,16#10000020#,16#305#);
   S2047 : aliased constant Code_Point_Array := (16#2047#,16#1000003F#,16#3F#);
   S2048 : aliased constant Code_Point_Array := (16#2048#,16#1000003F#,16#21#);
   S2049 : aliased constant Code_Point_Array := (16#2049#,16#10000021#,16#3F#);
   S2057 : aliased constant Code_Point_Array := (16#2057#,16#10002032#,16#2032#,16#2032#,16#2032#);
   S205F : aliased constant Code_Point_Array := (16#205F#,16#10000020#);
   S2070 : aliased constant Code_Point_Array := (16#2070#,16#10000030#);
   S2071 : aliased constant Code_Point_Array := (16#2071#,16#10000069#);
   S2074 : aliased constant Code_Point_Array := (16#2074#,16#10000034#);
   S2075 : aliased constant Code_Point_Array := (16#2075#,16#10000035#);
   S2076 : aliased constant Code_Point_Array := (16#2076#,16#10000036#);
   S2077 : aliased constant Code_Point_Array := (16#2077#,16#10000037#);
   S2078 : aliased constant Code_Point_Array := (16#2078#,16#10000038#);
   S2079 : aliased constant Code_Point_Array := (16#2079#,16#10000039#);
   S207A : aliased constant Code_Point_Array := (16#207A#,16#1000002B#);
   S207B : aliased constant Code_Point_Array := (16#207B#,16#10002212#);
   S207C : aliased constant Code_Point_Array := (16#207C#,16#1000003D#);
   S207D : aliased constant Code_Point_Array := (16#207D#,16#10000028#);
   S207E : aliased constant Code_Point_Array := (16#207E#,16#10000029#);
   S207F : aliased constant Code_Point_Array := (16#207F#,16#1000006E#);
   S2080 : aliased constant Code_Point_Array := (16#2080#,16#10000030#);
   S2081 : aliased constant Code_Point_Array := (16#2081#,16#10000031#);
   S2082 : aliased constant Code_Point_Array := (16#2082#,16#10000032#);
   S2083 : aliased constant Code_Point_Array := (16#2083#,16#10000033#);
   S2084 : aliased constant Code_Point_Array := (16#2084#,16#10000034#);
   S2085 : aliased constant Code_Point_Array := (16#2085#,16#10000035#);
   S2086 : aliased constant Code_Point_Array := (16#2086#,16#10000036#);
   S2087 : aliased constant Code_Point_Array := (16#2087#,16#10000037#);
   S2088 : aliased constant Code_Point_Array := (16#2088#,16#10000038#);
   S2089 : aliased constant Code_Point_Array := (16#2089#,16#10000039#);
   S208A : aliased constant Code_Point_Array := (16#208A#,16#1000002B#);
   S208B : aliased constant Code_Point_Array := (16#208B#,16#10002212#);
   S208C : aliased constant Code_Point_Array := (16#208C#,16#1000003D#);
   S208D : aliased constant Code_Point_Array := (16#208D#,16#10000028#);
   S208E : aliased constant Code_Point_Array := (16#208E#,16#10000029#);
   S2090 : aliased constant Code_Point_Array := (16#2090#,16#10000061#);
   S2091 : aliased constant Code_Point_Array := (16#2091#,16#10000065#);
   S2092 : aliased constant Code_Point_Array := (16#2092#,16#1000006F#);
   S2093 : aliased constant Code_Point_Array := (16#2093#,16#10000078#);
   S2094 : aliased constant Code_Point_Array := (16#2094#,16#10000259#);
   S2095 : aliased constant Code_Point_Array := (16#2095#,16#10000068#);
   S2096 : aliased constant Code_Point_Array := (16#2096#,16#1000006B#);
   S2097 : aliased constant Code_Point_Array := (16#2097#,16#1000006C#);
   S2098 : aliased constant Code_Point_Array := (16#2098#,16#1000006D#);
   S2099 : aliased constant Code_Point_Array := (16#2099#,16#1000006E#);
   S209A : aliased constant Code_Point_Array := (16#209A#,16#10000070#);
   S209B : aliased constant Code_Point_Array := (16#209B#,16#10000073#);
   S209C : aliased constant Code_Point_Array := (16#209C#,16#10000074#);
   S20A8 : aliased constant Code_Point_Array := (16#20A8#,16#10000052#,16#73#);
   S2100 : aliased constant Code_Point_Array := (16#2100#,16#10000061#,16#2F#,16#63#);
   S2101 : aliased constant Code_Point_Array := (16#2101#,16#10000061#,16#2F#,16#73#);
   S2102 : aliased constant Code_Point_Array := (16#2102#,16#10000043#);
   S2103 : aliased constant Code_Point_Array := (16#2103#,16#100000B0#,16#43#);
   S2105 : aliased constant Code_Point_Array := (16#2105#,16#10000063#,16#2F#,16#6F#);
   S2106 : aliased constant Code_Point_Array := (16#2106#,16#10000063#,16#2F#,16#75#);
   S2107 : aliased constant Code_Point_Array := (16#2107#,16#10000190#);
   S2109 : aliased constant Code_Point_Array := (16#2109#,16#100000B0#,16#46#);
   S210A : aliased constant Code_Point_Array := (16#210A#,16#10000067#);
   S210B : aliased constant Code_Point_Array := (16#210B#,16#10000048#);
   S210C : aliased constant Code_Point_Array := (16#210C#,16#10000048#);
   S210D : aliased constant Code_Point_Array := (16#210D#,16#10000048#);
   S210E : aliased constant Code_Point_Array := (16#210E#,16#10000068#);
   S210F : aliased constant Code_Point_Array := (16#210F#,16#10000127#);
   S2110 : aliased constant Code_Point_Array := (16#2110#,16#10000049#);
   S2111 : aliased constant Code_Point_Array := (16#2111#,16#10000049#);
   S2112 : aliased constant Code_Point_Array := (16#2112#,16#1000004C#);
   S2113 : aliased constant Code_Point_Array := (16#2113#,16#1000006C#);
   S2115 : aliased constant Code_Point_Array := (16#2115#,16#1000004E#);
   S2116 : aliased constant Code_Point_Array := (16#2116#,16#1000004E#,16#6F#);
   S2119 : aliased constant Code_Point_Array := (16#2119#,16#10000050#);
   S211A : aliased constant Code_Point_Array := (16#211A#,16#10000051#);
   S211B : aliased constant Code_Point_Array := (16#211B#,16#10000052#);
   S211C : aliased constant Code_Point_Array := (16#211C#,16#10000052#);
   S211D : aliased constant Code_Point_Array := (16#211D#,16#10000052#);
   S2120 : aliased constant Code_Point_Array := (16#2120#,16#10000053#,16#4D#);
   S2121 : aliased constant Code_Point_Array := (16#2121#,16#10000054#,16#45#,16#4C#);
   S2122 : aliased constant Code_Point_Array := (16#2122#,16#10000054#,16#4D#);
   S2124 : aliased constant Code_Point_Array := (16#2124#,16#1000005A#);
   S2126 : aliased constant Code_Point_Array := (16#2126#,16#3A9#);
   S2128 : aliased constant Code_Point_Array := (16#2128#,16#1000005A#);
   S212A : aliased constant Code_Point_Array := (16#212A#,16#4B#);
   S212B : aliased constant Code_Point_Array := (16#212B#,16#C5#);
   S212C : aliased constant Code_Point_Array := (16#212C#,16#10000042#);
   S212D : aliased constant Code_Point_Array := (16#212D#,16#10000043#);
   S212F : aliased constant Code_Point_Array := (16#212F#,16#10000065#);
   S2130 : aliased constant Code_Point_Array := (16#2130#,16#10000045#);
   S2131 : aliased constant Code_Point_Array := (16#2131#,16#10000046#);
   S2133 : aliased constant Code_Point_Array := (16#2133#,16#1000004D#);
   S2134 : aliased constant Code_Point_Array := (16#2134#,16#1000006F#);
   S2135 : aliased constant Code_Point_Array := (16#2135#,16#100005D0#);
   S2136 : aliased constant Code_Point_Array := (16#2136#,16#100005D1#);
   S2137 : aliased constant Code_Point_Array := (16#2137#,16#100005D2#);
   S2138 : aliased constant Code_Point_Array := (16#2138#,16#100005D3#);
   S2139 : aliased constant Code_Point_Array := (16#2139#,16#10000069#);
   S213B : aliased constant Code_Point_Array := (16#213B#,16#10000046#,16#41#,16#58#);
   S213C : aliased constant Code_Point_Array := (16#213C#,16#100003C0#);
   S213D : aliased constant Code_Point_Array := (16#213D#,16#100003B3#);
   S213E : aliased constant Code_Point_Array := (16#213E#,16#10000393#);
   S213F : aliased constant Code_Point_Array := (16#213F#,16#100003A0#);
   S2140 : aliased constant Code_Point_Array := (16#2140#,16#10002211#);
   S2145 : aliased constant Code_Point_Array := (16#2145#,16#10000044#);
   S2146 : aliased constant Code_Point_Array := (16#2146#,16#10000064#);
   S2147 : aliased constant Code_Point_Array := (16#2147#,16#10000065#);
   S2148 : aliased constant Code_Point_Array := (16#2148#,16#10000069#);
   S2149 : aliased constant Code_Point_Array := (16#2149#,16#1000006A#);
   S2150 : aliased constant Code_Point_Array := (16#2150#,16#10000031#,16#2044#,16#37#);
   S2151 : aliased constant Code_Point_Array := (16#2151#,16#10000031#,16#2044#,16#39#);
   S2152 : aliased constant Code_Point_Array := (16#2152#,16#10000031#,16#2044#,16#31#,16#30#);
   S2153 : aliased constant Code_Point_Array := (16#2153#,16#10000031#,16#2044#,16#33#);
   S2154 : aliased constant Code_Point_Array := (16#2154#,16#10000032#,16#2044#,16#33#);
   S2155 : aliased constant Code_Point_Array := (16#2155#,16#10000031#,16#2044#,16#35#);
   S2156 : aliased constant Code_Point_Array := (16#2156#,16#10000032#,16#2044#,16#35#);
   S2157 : aliased constant Code_Point_Array := (16#2157#,16#10000033#,16#2044#,16#35#);
   S2158 : aliased constant Code_Point_Array := (16#2158#,16#10000034#,16#2044#,16#35#);
   S2159 : aliased constant Code_Point_Array := (16#2159#,16#10000031#,16#2044#,16#36#);
   S215A : aliased constant Code_Point_Array := (16#215A#,16#10000035#,16#2044#,16#36#);
   S215B : aliased constant Code_Point_Array := (16#215B#,16#10000031#,16#2044#,16#38#);
   S215C : aliased constant Code_Point_Array := (16#215C#,16#10000033#,16#2044#,16#38#);
   S215D : aliased constant Code_Point_Array := (16#215D#,16#10000035#,16#2044#,16#38#);
   S215E : aliased constant Code_Point_Array := (16#215E#,16#10000037#,16#2044#,16#38#);
   S215F : aliased constant Code_Point_Array := (16#215F#,16#10000031#,16#2044#);
   S2160 : aliased constant Code_Point_Array := (16#2160#,16#10000049#);
   S2161 : aliased constant Code_Point_Array := (16#2161#,16#10000049#,16#49#);
   S2162 : aliased constant Code_Point_Array := (16#2162#,16#10000049#,16#49#,16#49#);
   S2163 : aliased constant Code_Point_Array := (16#2163#,16#10000049#,16#56#);
   S2164 : aliased constant Code_Point_Array := (16#2164#,16#10000056#);
   S2165 : aliased constant Code_Point_Array := (16#2165#,16#10000056#,16#49#);
   S2166 : aliased constant Code_Point_Array := (16#2166#,16#10000056#,16#49#,16#49#);
   S2167 : aliased constant Code_Point_Array := (16#2167#,16#10000056#,16#49#,16#49#,16#49#);
   S2168 : aliased constant Code_Point_Array := (16#2168#,16#10000049#,16#58#);
   S2169 : aliased constant Code_Point_Array := (16#2169#,16#10000058#);
   S216A : aliased constant Code_Point_Array := (16#216A#,16#10000058#,16#49#);
   S216B : aliased constant Code_Point_Array := (16#216B#,16#10000058#,16#49#,16#49#);
   S216C : aliased constant Code_Point_Array := (16#216C#,16#1000004C#);
   S216D : aliased constant Code_Point_Array := (16#216D#,16#10000043#);
   S216E : aliased constant Code_Point_Array := (16#216E#,16#10000044#);
   S216F : aliased constant Code_Point_Array := (16#216F#,16#1000004D#);
   S2170 : aliased constant Code_Point_Array := (16#2170#,16#10000069#);
   S2171 : aliased constant Code_Point_Array := (16#2171#,16#10000069#,16#69#);
   S2172 : aliased constant Code_Point_Array := (16#2172#,16#10000069#,16#69#,16#69#);
   S2173 : aliased constant Code_Point_Array := (16#2173#,16#10000069#,16#76#);
   S2174 : aliased constant Code_Point_Array := (16#2174#,16#10000076#);
   S2175 : aliased constant Code_Point_Array := (16#2175#,16#10000076#,16#69#);
   S2176 : aliased constant Code_Point_Array := (16#2176#,16#10000076#,16#69#,16#69#);
   S2177 : aliased constant Code_Point_Array := (16#2177#,16#10000076#,16#69#,16#69#,16#69#);
   S2178 : aliased constant Code_Point_Array := (16#2178#,16#10000069#,16#78#);
   S2179 : aliased constant Code_Point_Array := (16#2179#,16#10000078#);
   S217A : aliased constant Code_Point_Array := (16#217A#,16#10000078#,16#69#);
   S217B : aliased constant Code_Point_Array := (16#217B#,16#10000078#,16#69#,16#69#);
   S217C : aliased constant Code_Point_Array := (16#217C#,16#1000006C#);
   S217D : aliased constant Code_Point_Array := (16#217D#,16#10000063#);
   S217E : aliased constant Code_Point_Array := (16#217E#,16#10000064#);
   S217F : aliased constant Code_Point_Array := (16#217F#,16#1000006D#);
   S2189 : aliased constant Code_Point_Array := (16#2189#,16#10000030#,16#2044#,16#33#);
   S219A : aliased constant Code_Point_Array := (16#219A#,16#2190#,16#338#);
   S219B : aliased constant Code_Point_Array := (16#219B#,16#2192#,16#338#);
   S21AE : aliased constant Code_Point_Array := (16#21AE#,16#2194#,16#338#);
   S21CD : aliased constant Code_Point_Array := (16#21CD#,16#21D0#,16#338#);
   S21CE : aliased constant Code_Point_Array := (16#21CE#,16#21D4#,16#338#);
   S21CF : aliased constant Code_Point_Array := (16#21CF#,16#21D2#,16#338#);
   S2204 : aliased constant Code_Point_Array := (16#2204#,16#2203#,16#338#);
   S2209 : aliased constant Code_Point_Array := (16#2209#,16#2208#,16#338#);
   S220C : aliased constant Code_Point_Array := (16#220C#,16#220B#,16#338#);
   S2224 : aliased constant Code_Point_Array := (16#2224#,16#2223#,16#338#);
   S2226 : aliased constant Code_Point_Array := (16#2226#,16#2225#,16#338#);
   S222C : aliased constant Code_Point_Array := (16#222C#,16#1000222B#,16#222B#);
   S222D : aliased constant Code_Point_Array := (16#222D#,16#1000222B#,16#222B#,16#222B#);
   S222F : aliased constant Code_Point_Array := (16#222F#,16#1000222E#,16#222E#);
   S2230 : aliased constant Code_Point_Array := (16#2230#,16#1000222E#,16#222E#,16#222E#);
   S2241 : aliased constant Code_Point_Array := (16#2241#,16#223C#,16#338#);
   S2244 : aliased constant Code_Point_Array := (16#2244#,16#2243#,16#338#);
   S2247 : aliased constant Code_Point_Array := (16#2247#,16#2245#,16#338#);
   S2249 : aliased constant Code_Point_Array := (16#2249#,16#2248#,16#338#);
   S2260 : aliased constant Code_Point_Array := (16#2260#,16#3D#,16#338#);
   S2262 : aliased constant Code_Point_Array := (16#2262#,16#2261#,16#338#);
   S226D : aliased constant Code_Point_Array := (16#226D#,16#224D#,16#338#);
   S226E : aliased constant Code_Point_Array := (16#226E#,16#3C#,16#338#);
   S226F : aliased constant Code_Point_Array := (16#226F#,16#3E#,16#338#);
   S2270 : aliased constant Code_Point_Array := (16#2270#,16#2264#,16#338#);
   S2271 : aliased constant Code_Point_Array := (16#2271#,16#2265#,16#338#);
   S2274 : aliased constant Code_Point_Array := (16#2274#,16#2272#,16#338#);
   S2275 : aliased constant Code_Point_Array := (16#2275#,16#2273#,16#338#);
   S2278 : aliased constant Code_Point_Array := (16#2278#,16#2276#,16#338#);
   S2279 : aliased constant Code_Point_Array := (16#2279#,16#2277#,16#338#);
   S2280 : aliased constant Code_Point_Array := (16#2280#,16#227A#,16#338#);
   S2281 : aliased constant Code_Point_Array := (16#2281#,16#227B#,16#338#);
   S2284 : aliased constant Code_Point_Array := (16#2284#,16#2282#,16#338#);
   S2285 : aliased constant Code_Point_Array := (16#2285#,16#2283#,16#338#);
   S2288 : aliased constant Code_Point_Array := (16#2288#,16#2286#,16#338#);
   S2289 : aliased constant Code_Point_Array := (16#2289#,16#2287#,16#338#);
   S22AC : aliased constant Code_Point_Array := (16#22AC#,16#22A2#,16#338#);
   S22AD : aliased constant Code_Point_Array := (16#22AD#,16#22A8#,16#338#);
   S22AE : aliased constant Code_Point_Array := (16#22AE#,16#22A9#,16#338#);
   S22AF : aliased constant Code_Point_Array := (16#22AF#,16#22AB#,16#338#);
   S22E0 : aliased constant Code_Point_Array := (16#22E0#,16#227C#,16#338#);
   S22E1 : aliased constant Code_Point_Array := (16#22E1#,16#227D#,16#338#);
   S22E2 : aliased constant Code_Point_Array := (16#22E2#,16#2291#,16#338#);
   S22E3 : aliased constant Code_Point_Array := (16#22E3#,16#2292#,16#338#);
   S22EA : aliased constant Code_Point_Array := (16#22EA#,16#22B2#,16#338#);
   S22EB : aliased constant Code_Point_Array := (16#22EB#,16#22B3#,16#338#);
   S22EC : aliased constant Code_Point_Array := (16#22EC#,16#22B4#,16#338#);
   S22ED : aliased constant Code_Point_Array := (16#22ED#,16#22B5#,16#338#);
   S2329 : aliased constant Code_Point_Array := (16#2329#,16#3008#);
   S232A : aliased constant Code_Point_Array := (16#232A#,16#3009#);
   S2460 : aliased constant Code_Point_Array := (16#2460#,16#10000031#);
   S2461 : aliased constant Code_Point_Array := (16#2461#,16#10000032#);
   S2462 : aliased constant Code_Point_Array := (16#2462#,16#10000033#);
   S2463 : aliased constant Code_Point_Array := (16#2463#,16#10000034#);
   S2464 : aliased constant Code_Point_Array := (16#2464#,16#10000035#);
   S2465 : aliased constant Code_Point_Array := (16#2465#,16#10000036#);
   S2466 : aliased constant Code_Point_Array := (16#2466#,16#10000037#);
   S2467 : aliased constant Code_Point_Array := (16#2467#,16#10000038#);
   S2468 : aliased constant Code_Point_Array := (16#2468#,16#10000039#);
   S2469 : aliased constant Code_Point_Array := (16#2469#,16#10000031#,16#30#);
   S246A : aliased constant Code_Point_Array := (16#246A#,16#10000031#,16#31#);
   S246B : aliased constant Code_Point_Array := (16#246B#,16#10000031#,16#32#);
   S246C : aliased constant Code_Point_Array := (16#246C#,16#10000031#,16#33#);
   S246D : aliased constant Code_Point_Array := (16#246D#,16#10000031#,16#34#);
   S246E : aliased constant Code_Point_Array := (16#246E#,16#10000031#,16#35#);
   S246F : aliased constant Code_Point_Array := (16#246F#,16#10000031#,16#36#);
   S2470 : aliased constant Code_Point_Array := (16#2470#,16#10000031#,16#37#);
   S2471 : aliased constant Code_Point_Array := (16#2471#,16#10000031#,16#38#);
   S2472 : aliased constant Code_Point_Array := (16#2472#,16#10000031#,16#39#);
   S2473 : aliased constant Code_Point_Array := (16#2473#,16#10000032#,16#30#);
   S2474 : aliased constant Code_Point_Array := (16#2474#,16#10000028#,16#31#,16#29#);
   S2475 : aliased constant Code_Point_Array := (16#2475#,16#10000028#,16#32#,16#29#);
   S2476 : aliased constant Code_Point_Array := (16#2476#,16#10000028#,16#33#,16#29#);
   S2477 : aliased constant Code_Point_Array := (16#2477#,16#10000028#,16#34#,16#29#);
   S2478 : aliased constant Code_Point_Array := (16#2478#,16#10000028#,16#35#,16#29#);
   S2479 : aliased constant Code_Point_Array := (16#2479#,16#10000028#,16#36#,16#29#);
   S247A : aliased constant Code_Point_Array := (16#247A#,16#10000028#,16#37#,16#29#);
   S247B : aliased constant Code_Point_Array := (16#247B#,16#10000028#,16#38#,16#29#);
   S247C : aliased constant Code_Point_Array := (16#247C#,16#10000028#,16#39#,16#29#);
   S247D : aliased constant Code_Point_Array := (16#247D#,16#10000028#,16#31#,16#30#,16#29#);
   S247E : aliased constant Code_Point_Array := (16#247E#,16#10000028#,16#31#,16#31#,16#29#);
   S247F : aliased constant Code_Point_Array := (16#247F#,16#10000028#,16#31#,16#32#,16#29#);
   S2480 : aliased constant Code_Point_Array := (16#2480#,16#10000028#,16#31#,16#33#,16#29#);
   S2481 : aliased constant Code_Point_Array := (16#2481#,16#10000028#,16#31#,16#34#,16#29#);
   S2482 : aliased constant Code_Point_Array := (16#2482#,16#10000028#,16#31#,16#35#,16#29#);
   S2483 : aliased constant Code_Point_Array := (16#2483#,16#10000028#,16#31#,16#36#,16#29#);
   S2484 : aliased constant Code_Point_Array := (16#2484#,16#10000028#,16#31#,16#37#,16#29#);
   S2485 : aliased constant Code_Point_Array := (16#2485#,16#10000028#,16#31#,16#38#,16#29#);
   S2486 : aliased constant Code_Point_Array := (16#2486#,16#10000028#,16#31#,16#39#,16#29#);
   S2487 : aliased constant Code_Point_Array := (16#2487#,16#10000028#,16#32#,16#30#,16#29#);
   S2488 : aliased constant Code_Point_Array := (16#2488#,16#10000031#,16#2E#);
   S2489 : aliased constant Code_Point_Array := (16#2489#,16#10000032#,16#2E#);
   S248A : aliased constant Code_Point_Array := (16#248A#,16#10000033#,16#2E#);
   S248B : aliased constant Code_Point_Array := (16#248B#,16#10000034#,16#2E#);
   S248C : aliased constant Code_Point_Array := (16#248C#,16#10000035#,16#2E#);
   S248D : aliased constant Code_Point_Array := (16#248D#,16#10000036#,16#2E#);
   S248E : aliased constant Code_Point_Array := (16#248E#,16#10000037#,16#2E#);
   S248F : aliased constant Code_Point_Array := (16#248F#,16#10000038#,16#2E#);
   S2490 : aliased constant Code_Point_Array := (16#2490#,16#10000039#,16#2E#);
   S2491 : aliased constant Code_Point_Array := (16#2491#,16#10000031#,16#30#,16#2E#);
   S2492 : aliased constant Code_Point_Array := (16#2492#,16#10000031#,16#31#,16#2E#);
   S2493 : aliased constant Code_Point_Array := (16#2493#,16#10000031#,16#32#,16#2E#);
   S2494 : aliased constant Code_Point_Array := (16#2494#,16#10000031#,16#33#,16#2E#);
   S2495 : aliased constant Code_Point_Array := (16#2495#,16#10000031#,16#34#,16#2E#);
   S2496 : aliased constant Code_Point_Array := (16#2496#,16#10000031#,16#35#,16#2E#);
   S2497 : aliased constant Code_Point_Array := (16#2497#,16#10000031#,16#36#,16#2E#);
   S2498 : aliased constant Code_Point_Array := (16#2498#,16#10000031#,16#37#,16#2E#);
   S2499 : aliased constant Code_Point_Array := (16#2499#,16#10000031#,16#38#,16#2E#);
   S249A : aliased constant Code_Point_Array := (16#249A#,16#10000031#,16#39#,16#2E#);
   S249B : aliased constant Code_Point_Array := (16#249B#,16#10000032#,16#30#,16#2E#);
   S249C : aliased constant Code_Point_Array := (16#249C#,16#10000028#,16#61#,16#29#);
   S249D : aliased constant Code_Point_Array := (16#249D#,16#10000028#,16#62#,16#29#);
   S249E : aliased constant Code_Point_Array := (16#249E#,16#10000028#,16#63#,16#29#);
   S249F : aliased constant Code_Point_Array := (16#249F#,16#10000028#,16#64#,16#29#);
   S24A0 : aliased constant Code_Point_Array := (16#24A0#,16#10000028#,16#65#,16#29#);
   S24A1 : aliased constant Code_Point_Array := (16#24A1#,16#10000028#,16#66#,16#29#);
   S24A2 : aliased constant Code_Point_Array := (16#24A2#,16#10000028#,16#67#,16#29#);
   S24A3 : aliased constant Code_Point_Array := (16#24A3#,16#10000028#,16#68#,16#29#);
   S24A4 : aliased constant Code_Point_Array := (16#24A4#,16#10000028#,16#69#,16#29#);
   S24A5 : aliased constant Code_Point_Array := (16#24A5#,16#10000028#,16#6A#,16#29#);
   S24A6 : aliased constant Code_Point_Array := (16#24A6#,16#10000028#,16#6B#,16#29#);
   S24A7 : aliased constant Code_Point_Array := (16#24A7#,16#10000028#,16#6C#,16#29#);
   S24A8 : aliased constant Code_Point_Array := (16#24A8#,16#10000028#,16#6D#,16#29#);
   S24A9 : aliased constant Code_Point_Array := (16#24A9#,16#10000028#,16#6E#,16#29#);
   S24AA : aliased constant Code_Point_Array := (16#24AA#,16#10000028#,16#6F#,16#29#);
   S24AB : aliased constant Code_Point_Array := (16#24AB#,16#10000028#,16#70#,16#29#);
   S24AC : aliased constant Code_Point_Array := (16#24AC#,16#10000028#,16#71#,16#29#);
   S24AD : aliased constant Code_Point_Array := (16#24AD#,16#10000028#,16#72#,16#29#);
   S24AE : aliased constant Code_Point_Array := (16#24AE#,16#10000028#,16#73#,16#29#);
   S24AF : aliased constant Code_Point_Array := (16#24AF#,16#10000028#,16#74#,16#29#);
   S24B0 : aliased constant Code_Point_Array := (16#24B0#,16#10000028#,16#75#,16#29#);
   S24B1 : aliased constant Code_Point_Array := (16#24B1#,16#10000028#,16#76#,16#29#);
   S24B2 : aliased constant Code_Point_Array := (16#24B2#,16#10000028#,16#77#,16#29#);
   S24B3 : aliased constant Code_Point_Array := (16#24B3#,16#10000028#,16#78#,16#29#);
   S24B4 : aliased constant Code_Point_Array := (16#24B4#,16#10000028#,16#79#,16#29#);
   S24B5 : aliased constant Code_Point_Array := (16#24B5#,16#10000028#,16#7A#,16#29#);
   S24B6 : aliased constant Code_Point_Array := (16#24B6#,16#10000041#);
   S24B7 : aliased constant Code_Point_Array := (16#24B7#,16#10000042#);
   S24B8 : aliased constant Code_Point_Array := (16#24B8#,16#10000043#);
   S24B9 : aliased constant Code_Point_Array := (16#24B9#,16#10000044#);
   S24BA : aliased constant Code_Point_Array := (16#24BA#,16#10000045#);
   S24BB : aliased constant Code_Point_Array := (16#24BB#,16#10000046#);
   S24BC : aliased constant Code_Point_Array := (16#24BC#,16#10000047#);
   S24BD : aliased constant Code_Point_Array := (16#24BD#,16#10000048#);
   S24BE : aliased constant Code_Point_Array := (16#24BE#,16#10000049#);
   S24BF : aliased constant Code_Point_Array := (16#24BF#,16#1000004A#);
   S24C0 : aliased constant Code_Point_Array := (16#24C0#,16#1000004B#);
   S24C1 : aliased constant Code_Point_Array := (16#24C1#,16#1000004C#);
   S24C2 : aliased constant Code_Point_Array := (16#24C2#,16#1000004D#);
   S24C3 : aliased constant Code_Point_Array := (16#24C3#,16#1000004E#);
   S24C4 : aliased constant Code_Point_Array := (16#24C4#,16#1000004F#);
   S24C5 : aliased constant Code_Point_Array := (16#24C5#,16#10000050#);
   S24C6 : aliased constant Code_Point_Array := (16#24C6#,16#10000051#);
   S24C7 : aliased constant Code_Point_Array := (16#24C7#,16#10000052#);
   S24C8 : aliased constant Code_Point_Array := (16#24C8#,16#10000053#);
   S24C9 : aliased constant Code_Point_Array := (16#24C9#,16#10000054#);
   S24CA : aliased constant Code_Point_Array := (16#24CA#,16#10000055#);
   S24CB : aliased constant Code_Point_Array := (16#24CB#,16#10000056#);
   S24CC : aliased constant Code_Point_Array := (16#24CC#,16#10000057#);
   S24CD : aliased constant Code_Point_Array := (16#24CD#,16#10000058#);
   S24CE : aliased constant Code_Point_Array := (16#24CE#,16#10000059#);
   S24CF : aliased constant Code_Point_Array := (16#24CF#,16#1000005A#);
   S24D0 : aliased constant Code_Point_Array := (16#24D0#,16#10000061#);
   S24D1 : aliased constant Code_Point_Array := (16#24D1#,16#10000062#);
   S24D2 : aliased constant Code_Point_Array := (16#24D2#,16#10000063#);
   S24D3 : aliased constant Code_Point_Array := (16#24D3#,16#10000064#);
   S24D4 : aliased constant Code_Point_Array := (16#24D4#,16#10000065#);
   S24D5 : aliased constant Code_Point_Array := (16#24D5#,16#10000066#);
   S24D6 : aliased constant Code_Point_Array := (16#24D6#,16#10000067#);
   S24D7 : aliased constant Code_Point_Array := (16#24D7#,16#10000068#);
   S24D8 : aliased constant Code_Point_Array := (16#24D8#,16#10000069#);
   S24D9 : aliased constant Code_Point_Array := (16#24D9#,16#1000006A#);
   S24DA : aliased constant Code_Point_Array := (16#24DA#,16#1000006B#);
   S24DB : aliased constant Code_Point_Array := (16#24DB#,16#1000006C#);
   S24DC : aliased constant Code_Point_Array := (16#24DC#,16#1000006D#);
   S24DD : aliased constant Code_Point_Array := (16#24DD#,16#1000006E#);
   S24DE : aliased constant Code_Point_Array := (16#24DE#,16#1000006F#);
   S24DF : aliased constant Code_Point_Array := (16#24DF#,16#10000070#);
   S24E0 : aliased constant Code_Point_Array := (16#24E0#,16#10000071#);
   S24E1 : aliased constant Code_Point_Array := (16#24E1#,16#10000072#);
   S24E2 : aliased constant Code_Point_Array := (16#24E2#,16#10000073#);
   S24E3 : aliased constant Code_Point_Array := (16#24E3#,16#10000074#);
   S24E4 : aliased constant Code_Point_Array := (16#24E4#,16#10000075#);
   S24E5 : aliased constant Code_Point_Array := (16#24E5#,16#10000076#);
   S24E6 : aliased constant Code_Point_Array := (16#24E6#,16#10000077#);
   S24E7 : aliased constant Code_Point_Array := (16#24E7#,16#10000078#);
   S24E8 : aliased constant Code_Point_Array := (16#24E8#,16#10000079#);
   S24E9 : aliased constant Code_Point_Array := (16#24E9#,16#1000007A#);
   S24EA : aliased constant Code_Point_Array := (16#24EA#,16#10000030#);
   S2A0C : aliased constant Code_Point_Array := (16#2A0C#,16#1000222B#,16#222B#,16#222B#,16#222B#);
   S2A74 : aliased constant Code_Point_Array := (16#2A74#,16#1000003A#,16#3A#,16#3D#);
   S2A75 : aliased constant Code_Point_Array := (16#2A75#,16#1000003D#,16#3D#);
   S2A76 : aliased constant Code_Point_Array := (16#2A76#,16#1000003D#,16#3D#,16#3D#);
   S2ADC : aliased constant Code_Point_Array := (16#2ADC#,16#2ADD#,16#338#);
   S2C7C : aliased constant Code_Point_Array := (16#2C7C#,16#1000006A#);
   S2C7D : aliased constant Code_Point_Array := (16#2C7D#,16#10000056#);
   S2D6F : aliased constant Code_Point_Array := (16#2D6F#,16#10002D61#);
   S2E9F : aliased constant Code_Point_Array := (16#2E9F#,16#10006BCD#);
   S2EF3 : aliased constant Code_Point_Array := (16#2EF3#,16#10009F9F#);
   S2F00 : aliased constant Code_Point_Array := (16#2F00#,16#10004E00#);
   S2F01 : aliased constant Code_Point_Array := (16#2F01#,16#10004E28#);
   S2F02 : aliased constant Code_Point_Array := (16#2F02#,16#10004E36#);
   S2F03 : aliased constant Code_Point_Array := (16#2F03#,16#10004E3F#);
   S2F04 : aliased constant Code_Point_Array := (16#2F04#,16#10004E59#);
   S2F05 : aliased constant Code_Point_Array := (16#2F05#,16#10004E85#);
   S2F06 : aliased constant Code_Point_Array := (16#2F06#,16#10004E8C#);
   S2F07 : aliased constant Code_Point_Array := (16#2F07#,16#10004EA0#);
   S2F08 : aliased constant Code_Point_Array := (16#2F08#,16#10004EBA#);
   S2F09 : aliased constant Code_Point_Array := (16#2F09#,16#1000513F#);
   S2F0A : aliased constant Code_Point_Array := (16#2F0A#,16#10005165#);
   S2F0B : aliased constant Code_Point_Array := (16#2F0B#,16#1000516B#);
   S2F0C : aliased constant Code_Point_Array := (16#2F0C#,16#10005182#);
   S2F0D : aliased constant Code_Point_Array := (16#2F0D#,16#10005196#);
   S2F0E : aliased constant Code_Point_Array := (16#2F0E#,16#100051AB#);
   S2F0F : aliased constant Code_Point_Array := (16#2F0F#,16#100051E0#);
   S2F10 : aliased constant Code_Point_Array := (16#2F10#,16#100051F5#);
   S2F11 : aliased constant Code_Point_Array := (16#2F11#,16#10005200#);
   S2F12 : aliased constant Code_Point_Array := (16#2F12#,16#1000529B#);
   S2F13 : aliased constant Code_Point_Array := (16#2F13#,16#100052F9#);
   S2F14 : aliased constant Code_Point_Array := (16#2F14#,16#10005315#);
   S2F15 : aliased constant Code_Point_Array := (16#2F15#,16#1000531A#);
   S2F16 : aliased constant Code_Point_Array := (16#2F16#,16#10005338#);
   S2F17 : aliased constant Code_Point_Array := (16#2F17#,16#10005341#);
   S2F18 : aliased constant Code_Point_Array := (16#2F18#,16#1000535C#);
   S2F19 : aliased constant Code_Point_Array := (16#2F19#,16#10005369#);
   S2F1A : aliased constant Code_Point_Array := (16#2F1A#,16#10005382#);
   S2F1B : aliased constant Code_Point_Array := (16#2F1B#,16#100053B6#);
   S2F1C : aliased constant Code_Point_Array := (16#2F1C#,16#100053C8#);
   S2F1D : aliased constant Code_Point_Array := (16#2F1D#,16#100053E3#);
   S2F1E : aliased constant Code_Point_Array := (16#2F1E#,16#100056D7#);
   S2F1F : aliased constant Code_Point_Array := (16#2F1F#,16#1000571F#);
   S2F20 : aliased constant Code_Point_Array := (16#2F20#,16#100058EB#);
   S2F21 : aliased constant Code_Point_Array := (16#2F21#,16#10005902#);
   S2F22 : aliased constant Code_Point_Array := (16#2F22#,16#1000590A#);
   S2F23 : aliased constant Code_Point_Array := (16#2F23#,16#10005915#);
   S2F24 : aliased constant Code_Point_Array := (16#2F24#,16#10005927#);
   S2F25 : aliased constant Code_Point_Array := (16#2F25#,16#10005973#);
   S2F26 : aliased constant Code_Point_Array := (16#2F26#,16#10005B50#);
   S2F27 : aliased constant Code_Point_Array := (16#2F27#,16#10005B80#);
   S2F28 : aliased constant Code_Point_Array := (16#2F28#,16#10005BF8#);
   S2F29 : aliased constant Code_Point_Array := (16#2F29#,16#10005C0F#);
   S2F2A : aliased constant Code_Point_Array := (16#2F2A#,16#10005C22#);
   S2F2B : aliased constant Code_Point_Array := (16#2F2B#,16#10005C38#);
   S2F2C : aliased constant Code_Point_Array := (16#2F2C#,16#10005C6E#);
   S2F2D : aliased constant Code_Point_Array := (16#2F2D#,16#10005C71#);
   S2F2E : aliased constant Code_Point_Array := (16#2F2E#,16#10005DDB#);
   S2F2F : aliased constant Code_Point_Array := (16#2F2F#,16#10005DE5#);
   S2F30 : aliased constant Code_Point_Array := (16#2F30#,16#10005DF1#);
   S2F31 : aliased constant Code_Point_Array := (16#2F31#,16#10005DFE#);
   S2F32 : aliased constant Code_Point_Array := (16#2F32#,16#10005E72#);
   S2F33 : aliased constant Code_Point_Array := (16#2F33#,16#10005E7A#);
   S2F34 : aliased constant Code_Point_Array := (16#2F34#,16#10005E7F#);
   S2F35 : aliased constant Code_Point_Array := (16#2F35#,16#10005EF4#);
   S2F36 : aliased constant Code_Point_Array := (16#2F36#,16#10005EFE#);
   S2F37 : aliased constant Code_Point_Array := (16#2F37#,16#10005F0B#);
   S2F38 : aliased constant Code_Point_Array := (16#2F38#,16#10005F13#);
   S2F39 : aliased constant Code_Point_Array := (16#2F39#,16#10005F50#);
   S2F3A : aliased constant Code_Point_Array := (16#2F3A#,16#10005F61#);
   S2F3B : aliased constant Code_Point_Array := (16#2F3B#,16#10005F73#);
   S2F3C : aliased constant Code_Point_Array := (16#2F3C#,16#10005FC3#);
   S2F3D : aliased constant Code_Point_Array := (16#2F3D#,16#10006208#);
   S2F3E : aliased constant Code_Point_Array := (16#2F3E#,16#10006236#);
   S2F3F : aliased constant Code_Point_Array := (16#2F3F#,16#1000624B#);
   S2F40 : aliased constant Code_Point_Array := (16#2F40#,16#1000652F#);
   S2F41 : aliased constant Code_Point_Array := (16#2F41#,16#10006534#);
   S2F42 : aliased constant Code_Point_Array := (16#2F42#,16#10006587#);
   S2F43 : aliased constant Code_Point_Array := (16#2F43#,16#10006597#);
   S2F44 : aliased constant Code_Point_Array := (16#2F44#,16#100065A4#);
   S2F45 : aliased constant Code_Point_Array := (16#2F45#,16#100065B9#);
   S2F46 : aliased constant Code_Point_Array := (16#2F46#,16#100065E0#);
   S2F47 : aliased constant Code_Point_Array := (16#2F47#,16#100065E5#);
   S2F48 : aliased constant Code_Point_Array := (16#2F48#,16#100066F0#);
   S2F49 : aliased constant Code_Point_Array := (16#2F49#,16#10006708#);
   S2F4A : aliased constant Code_Point_Array := (16#2F4A#,16#10006728#);
   S2F4B : aliased constant Code_Point_Array := (16#2F4B#,16#10006B20#);
   S2F4C : aliased constant Code_Point_Array := (16#2F4C#,16#10006B62#);
   S2F4D : aliased constant Code_Point_Array := (16#2F4D#,16#10006B79#);
   S2F4E : aliased constant Code_Point_Array := (16#2F4E#,16#10006BB3#);
   S2F4F : aliased constant Code_Point_Array := (16#2F4F#,16#10006BCB#);
   S2F50 : aliased constant Code_Point_Array := (16#2F50#,16#10006BD4#);
   S2F51 : aliased constant Code_Point_Array := (16#2F51#,16#10006BDB#);
   S2F52 : aliased constant Code_Point_Array := (16#2F52#,16#10006C0F#);
   S2F53 : aliased constant Code_Point_Array := (16#2F53#,16#10006C14#);
   S2F54 : aliased constant Code_Point_Array := (16#2F54#,16#10006C34#);
   S2F55 : aliased constant Code_Point_Array := (16#2F55#,16#1000706B#);
   S2F56 : aliased constant Code_Point_Array := (16#2F56#,16#1000722A#);
   S2F57 : aliased constant Code_Point_Array := (16#2F57#,16#10007236#);
   S2F58 : aliased constant Code_Point_Array := (16#2F58#,16#1000723B#);
   S2F59 : aliased constant Code_Point_Array := (16#2F59#,16#1000723F#);
   S2F5A : aliased constant Code_Point_Array := (16#2F5A#,16#10007247#);
   S2F5B : aliased constant Code_Point_Array := (16#2F5B#,16#10007259#);
   S2F5C : aliased constant Code_Point_Array := (16#2F5C#,16#1000725B#);
   S2F5D : aliased constant Code_Point_Array := (16#2F5D#,16#100072AC#);
   S2F5E : aliased constant Code_Point_Array := (16#2F5E#,16#10007384#);
   S2F5F : aliased constant Code_Point_Array := (16#2F5F#,16#10007389#);
   S2F60 : aliased constant Code_Point_Array := (16#2F60#,16#100074DC#);
   S2F61 : aliased constant Code_Point_Array := (16#2F61#,16#100074E6#);
   S2F62 : aliased constant Code_Point_Array := (16#2F62#,16#10007518#);
   S2F63 : aliased constant Code_Point_Array := (16#2F63#,16#1000751F#);
   S2F64 : aliased constant Code_Point_Array := (16#2F64#,16#10007528#);
   S2F65 : aliased constant Code_Point_Array := (16#2F65#,16#10007530#);
   S2F66 : aliased constant Code_Point_Array := (16#2F66#,16#1000758B#);
   S2F67 : aliased constant Code_Point_Array := (16#2F67#,16#10007592#);
   S2F68 : aliased constant Code_Point_Array := (16#2F68#,16#10007676#);
   S2F69 : aliased constant Code_Point_Array := (16#2F69#,16#1000767D#);
   S2F6A : aliased constant Code_Point_Array := (16#2F6A#,16#100076AE#);
   S2F6B : aliased constant Code_Point_Array := (16#2F6B#,16#100076BF#);
   S2F6C : aliased constant Code_Point_Array := (16#2F6C#,16#100076EE#);
   S2F6D : aliased constant Code_Point_Array := (16#2F6D#,16#100077DB#);
   S2F6E : aliased constant Code_Point_Array := (16#2F6E#,16#100077E2#);
   S2F6F : aliased constant Code_Point_Array := (16#2F6F#,16#100077F3#);
   S2F70 : aliased constant Code_Point_Array := (16#2F70#,16#1000793A#);
   S2F71 : aliased constant Code_Point_Array := (16#2F71#,16#100079B8#);
   S2F72 : aliased constant Code_Point_Array := (16#2F72#,16#100079BE#);
   S2F73 : aliased constant Code_Point_Array := (16#2F73#,16#10007A74#);
   S2F74 : aliased constant Code_Point_Array := (16#2F74#,16#10007ACB#);
   S2F75 : aliased constant Code_Point_Array := (16#2F75#,16#10007AF9#);
   S2F76 : aliased constant Code_Point_Array := (16#2F76#,16#10007C73#);
   S2F77 : aliased constant Code_Point_Array := (16#2F77#,16#10007CF8#);
   S2F78 : aliased constant Code_Point_Array := (16#2F78#,16#10007F36#);
   S2F79 : aliased constant Code_Point_Array := (16#2F79#,16#10007F51#);
   S2F7A : aliased constant Code_Point_Array := (16#2F7A#,16#10007F8A#);
   S2F7B : aliased constant Code_Point_Array := (16#2F7B#,16#10007FBD#);
   S2F7C : aliased constant Code_Point_Array := (16#2F7C#,16#10008001#);
   S2F7D : aliased constant Code_Point_Array := (16#2F7D#,16#1000800C#);
   S2F7E : aliased constant Code_Point_Array := (16#2F7E#,16#10008012#);
   S2F7F : aliased constant Code_Point_Array := (16#2F7F#,16#10008033#);
   S2F80 : aliased constant Code_Point_Array := (16#2F80#,16#1000807F#);
   S2F81 : aliased constant Code_Point_Array := (16#2F81#,16#10008089#);
   S2F82 : aliased constant Code_Point_Array := (16#2F82#,16#100081E3#);
   S2F83 : aliased constant Code_Point_Array := (16#2F83#,16#100081EA#);
   S2F84 : aliased constant Code_Point_Array := (16#2F84#,16#100081F3#);
   S2F85 : aliased constant Code_Point_Array := (16#2F85#,16#100081FC#);
   S2F86 : aliased constant Code_Point_Array := (16#2F86#,16#1000820C#);
   S2F87 : aliased constant Code_Point_Array := (16#2F87#,16#1000821B#);
   S2F88 : aliased constant Code_Point_Array := (16#2F88#,16#1000821F#);
   S2F89 : aliased constant Code_Point_Array := (16#2F89#,16#1000826E#);
   S2F8A : aliased constant Code_Point_Array := (16#2F8A#,16#10008272#);
   S2F8B : aliased constant Code_Point_Array := (16#2F8B#,16#10008278#);
   S2F8C : aliased constant Code_Point_Array := (16#2F8C#,16#1000864D#);
   S2F8D : aliased constant Code_Point_Array := (16#2F8D#,16#1000866B#);
   S2F8E : aliased constant Code_Point_Array := (16#2F8E#,16#10008840#);
   S2F8F : aliased constant Code_Point_Array := (16#2F8F#,16#1000884C#);
   S2F90 : aliased constant Code_Point_Array := (16#2F90#,16#10008863#);
   S2F91 : aliased constant Code_Point_Array := (16#2F91#,16#1000897E#);
   S2F92 : aliased constant Code_Point_Array := (16#2F92#,16#1000898B#);
   S2F93 : aliased constant Code_Point_Array := (16#2F93#,16#100089D2#);
   S2F94 : aliased constant Code_Point_Array := (16#2F94#,16#10008A00#);
   S2F95 : aliased constant Code_Point_Array := (16#2F95#,16#10008C37#);
   S2F96 : aliased constant Code_Point_Array := (16#2F96#,16#10008C46#);
   S2F97 : aliased constant Code_Point_Array := (16#2F97#,16#10008C55#);
   S2F98 : aliased constant Code_Point_Array := (16#2F98#,16#10008C78#);
   S2F99 : aliased constant Code_Point_Array := (16#2F99#,16#10008C9D#);
   S2F9A : aliased constant Code_Point_Array := (16#2F9A#,16#10008D64#);
   S2F9B : aliased constant Code_Point_Array := (16#2F9B#,16#10008D70#);
   S2F9C : aliased constant Code_Point_Array := (16#2F9C#,16#10008DB3#);
   S2F9D : aliased constant Code_Point_Array := (16#2F9D#,16#10008EAB#);
   S2F9E : aliased constant Code_Point_Array := (16#2F9E#,16#10008ECA#);
   S2F9F : aliased constant Code_Point_Array := (16#2F9F#,16#10008F9B#);
   S2FA0 : aliased constant Code_Point_Array := (16#2FA0#,16#10008FB0#);
   S2FA1 : aliased constant Code_Point_Array := (16#2FA1#,16#10008FB5#);
   S2FA2 : aliased constant Code_Point_Array := (16#2FA2#,16#10009091#);
   S2FA3 : aliased constant Code_Point_Array := (16#2FA3#,16#10009149#);
   S2FA4 : aliased constant Code_Point_Array := (16#2FA4#,16#100091C6#);
   S2FA5 : aliased constant Code_Point_Array := (16#2FA5#,16#100091CC#);
   S2FA6 : aliased constant Code_Point_Array := (16#2FA6#,16#100091D1#);
   S2FA7 : aliased constant Code_Point_Array := (16#2FA7#,16#10009577#);
   S2FA8 : aliased constant Code_Point_Array := (16#2FA8#,16#10009580#);
   S2FA9 : aliased constant Code_Point_Array := (16#2FA9#,16#1000961C#);
   S2FAA : aliased constant Code_Point_Array := (16#2FAA#,16#100096B6#);
   S2FAB : aliased constant Code_Point_Array := (16#2FAB#,16#100096B9#);
   S2FAC : aliased constant Code_Point_Array := (16#2FAC#,16#100096E8#);
   S2FAD : aliased constant Code_Point_Array := (16#2FAD#,16#10009751#);
   S2FAE : aliased constant Code_Point_Array := (16#2FAE#,16#1000975E#);
   S2FAF : aliased constant Code_Point_Array := (16#2FAF#,16#10009762#);
   S2FB0 : aliased constant Code_Point_Array := (16#2FB0#,16#10009769#);
   S2FB1 : aliased constant Code_Point_Array := (16#2FB1#,16#100097CB#);
   S2FB2 : aliased constant Code_Point_Array := (16#2FB2#,16#100097ED#);
   S2FB3 : aliased constant Code_Point_Array := (16#2FB3#,16#100097F3#);
   S2FB4 : aliased constant Code_Point_Array := (16#2FB4#,16#10009801#);
   S2FB5 : aliased constant Code_Point_Array := (16#2FB5#,16#100098A8#);
   S2FB6 : aliased constant Code_Point_Array := (16#2FB6#,16#100098DB#);
   S2FB7 : aliased constant Code_Point_Array := (16#2FB7#,16#100098DF#);
   S2FB8 : aliased constant Code_Point_Array := (16#2FB8#,16#10009996#);
   S2FB9 : aliased constant Code_Point_Array := (16#2FB9#,16#10009999#);
   S2FBA : aliased constant Code_Point_Array := (16#2FBA#,16#100099AC#);
   S2FBB : aliased constant Code_Point_Array := (16#2FBB#,16#10009AA8#);
   S2FBC : aliased constant Code_Point_Array := (16#2FBC#,16#10009AD8#);
   S2FBD : aliased constant Code_Point_Array := (16#2FBD#,16#10009ADF#);
   S2FBE : aliased constant Code_Point_Array := (16#2FBE#,16#10009B25#);
   S2FBF : aliased constant Code_Point_Array := (16#2FBF#,16#10009B2F#);
   S2FC0 : aliased constant Code_Point_Array := (16#2FC0#,16#10009B32#);
   S2FC1 : aliased constant Code_Point_Array := (16#2FC1#,16#10009B3C#);
   S2FC2 : aliased constant Code_Point_Array := (16#2FC2#,16#10009B5A#);
   S2FC3 : aliased constant Code_Point_Array := (16#2FC3#,16#10009CE5#);
   S2FC4 : aliased constant Code_Point_Array := (16#2FC4#,16#10009E75#);
   S2FC5 : aliased constant Code_Point_Array := (16#2FC5#,16#10009E7F#);
   S2FC6 : aliased constant Code_Point_Array := (16#2FC6#,16#10009EA5#);
   S2FC7 : aliased constant Code_Point_Array := (16#2FC7#,16#10009EBB#);
   S2FC8 : aliased constant Code_Point_Array := (16#2FC8#,16#10009EC3#);
   S2FC9 : aliased constant Code_Point_Array := (16#2FC9#,16#10009ECD#);
   S2FCA : aliased constant Code_Point_Array := (16#2FCA#,16#10009ED1#);
   S2FCB : aliased constant Code_Point_Array := (16#2FCB#,16#10009EF9#);
   S2FCC : aliased constant Code_Point_Array := (16#2FCC#,16#10009EFD#);
   S2FCD : aliased constant Code_Point_Array := (16#2FCD#,16#10009F0E#);
   S2FCE : aliased constant Code_Point_Array := (16#2FCE#,16#10009F13#);
   S2FCF : aliased constant Code_Point_Array := (16#2FCF#,16#10009F20#);
   S2FD0 : aliased constant Code_Point_Array := (16#2FD0#,16#10009F3B#);
   S2FD1 : aliased constant Code_Point_Array := (16#2FD1#,16#10009F4A#);
   S2FD2 : aliased constant Code_Point_Array := (16#2FD2#,16#10009F52#);
   S2FD3 : aliased constant Code_Point_Array := (16#2FD3#,16#10009F8D#);
   S2FD4 : aliased constant Code_Point_Array := (16#2FD4#,16#10009F9C#);
   S2FD5 : aliased constant Code_Point_Array := (16#2FD5#,16#10009FA0#);
   S3000 : aliased constant Code_Point_Array := (16#3000#,16#10000020#);
   S3036 : aliased constant Code_Point_Array := (16#3036#,16#10003012#);
   S3038 : aliased constant Code_Point_Array := (16#3038#,16#10005341#);
   S3039 : aliased constant Code_Point_Array := (16#3039#,16#10005344#);
   S303A : aliased constant Code_Point_Array := (16#303A#,16#10005345#);
   S304C : aliased constant Code_Point_Array := (16#304C#,16#304B#,16#3099#);
   S304E : aliased constant Code_Point_Array := (16#304E#,16#304D#,16#3099#);
   S3050 : aliased constant Code_Point_Array := (16#3050#,16#304F#,16#3099#);
   S3052 : aliased constant Code_Point_Array := (16#3052#,16#3051#,16#3099#);
   S3054 : aliased constant Code_Point_Array := (16#3054#,16#3053#,16#3099#);
   S3056 : aliased constant Code_Point_Array := (16#3056#,16#3055#,16#3099#);
   S3058 : aliased constant Code_Point_Array := (16#3058#,16#3057#,16#3099#);
   S305A : aliased constant Code_Point_Array := (16#305A#,16#3059#,16#3099#);
   S305C : aliased constant Code_Point_Array := (16#305C#,16#305B#,16#3099#);
   S305E : aliased constant Code_Point_Array := (16#305E#,16#305D#,16#3099#);
   S3060 : aliased constant Code_Point_Array := (16#3060#,16#305F#,16#3099#);
   S3062 : aliased constant Code_Point_Array := (16#3062#,16#3061#,16#3099#);
   S3065 : aliased constant Code_Point_Array := (16#3065#,16#3064#,16#3099#);
   S3067 : aliased constant Code_Point_Array := (16#3067#,16#3066#,16#3099#);
   S3069 : aliased constant Code_Point_Array := (16#3069#,16#3068#,16#3099#);
   S3070 : aliased constant Code_Point_Array := (16#3070#,16#306F#,16#3099#);
   S3071 : aliased constant Code_Point_Array := (16#3071#,16#306F#,16#309A#);
   S3073 : aliased constant Code_Point_Array := (16#3073#,16#3072#,16#3099#);
   S3074 : aliased constant Code_Point_Array := (16#3074#,16#3072#,16#309A#);
   S3076 : aliased constant Code_Point_Array := (16#3076#,16#3075#,16#3099#);
   S3077 : aliased constant Code_Point_Array := (16#3077#,16#3075#,16#309A#);
   S3079 : aliased constant Code_Point_Array := (16#3079#,16#3078#,16#3099#);
   S307A : aliased constant Code_Point_Array := (16#307A#,16#3078#,16#309A#);
   S307C : aliased constant Code_Point_Array := (16#307C#,16#307B#,16#3099#);
   S307D : aliased constant Code_Point_Array := (16#307D#,16#307B#,16#309A#);
   S3094 : aliased constant Code_Point_Array := (16#3094#,16#3046#,16#3099#);
   S309B : aliased constant Code_Point_Array := (16#309B#,16#10000020#,16#3099#);
   S309C : aliased constant Code_Point_Array := (16#309C#,16#10000020#,16#309A#);
   S309E : aliased constant Code_Point_Array := (16#309E#,16#309D#,16#3099#);
   S309F : aliased constant Code_Point_Array := (16#309F#,16#10003088#,16#308A#);
   S30AC : aliased constant Code_Point_Array := (16#30AC#,16#30AB#,16#3099#);
   S30AE : aliased constant Code_Point_Array := (16#30AE#,16#30AD#,16#3099#);
   S30B0 : aliased constant Code_Point_Array := (16#30B0#,16#30AF#,16#3099#);
   S30B2 : aliased constant Code_Point_Array := (16#30B2#,16#30B1#,16#3099#);
   S30B4 : aliased constant Code_Point_Array := (16#30B4#,16#30B3#,16#3099#);
   S30B6 : aliased constant Code_Point_Array := (16#30B6#,16#30B5#,16#3099#);
   S30B8 : aliased constant Code_Point_Array := (16#30B8#,16#30B7#,16#3099#);
   S30BA : aliased constant Code_Point_Array := (16#30BA#,16#30B9#,16#3099#);
   S30BC : aliased constant Code_Point_Array := (16#30BC#,16#30BB#,16#3099#);
   S30BE : aliased constant Code_Point_Array := (16#30BE#,16#30BD#,16#3099#);
   S30C0 : aliased constant Code_Point_Array := (16#30C0#,16#30BF#,16#3099#);
   S30C2 : aliased constant Code_Point_Array := (16#30C2#,16#30C1#,16#3099#);
   S30C5 : aliased constant Code_Point_Array := (16#30C5#,16#30C4#,16#3099#);
   S30C7 : aliased constant Code_Point_Array := (16#30C7#,16#30C6#,16#3099#);
   S30C9 : aliased constant Code_Point_Array := (16#30C9#,16#30C8#,16#3099#);
   S30D0 : aliased constant Code_Point_Array := (16#30D0#,16#30CF#,16#3099#);
   S30D1 : aliased constant Code_Point_Array := (16#30D1#,16#30CF#,16#309A#);
   S30D3 : aliased constant Code_Point_Array := (16#30D3#,16#30D2#,16#3099#);
   S30D4 : aliased constant Code_Point_Array := (16#30D4#,16#30D2#,16#309A#);
   S30D6 : aliased constant Code_Point_Array := (16#30D6#,16#30D5#,16#3099#);
   S30D7 : aliased constant Code_Point_Array := (16#30D7#,16#30D5#,16#309A#);
   S30D9 : aliased constant Code_Point_Array := (16#30D9#,16#30D8#,16#3099#);
   S30DA : aliased constant Code_Point_Array := (16#30DA#,16#30D8#,16#309A#);
   S30DC : aliased constant Code_Point_Array := (16#30DC#,16#30DB#,16#3099#);
   S30DD : aliased constant Code_Point_Array := (16#30DD#,16#30DB#,16#309A#);
   S30F4 : aliased constant Code_Point_Array := (16#30F4#,16#30A6#,16#3099#);
   S30F7 : aliased constant Code_Point_Array := (16#30F7#,16#30EF#,16#3099#);
   S30F8 : aliased constant Code_Point_Array := (16#30F8#,16#30F0#,16#3099#);
   S30F9 : aliased constant Code_Point_Array := (16#30F9#,16#30F1#,16#3099#);
   S30FA : aliased constant Code_Point_Array := (16#30FA#,16#30F2#,16#3099#);
   S30FE : aliased constant Code_Point_Array := (16#30FE#,16#30FD#,16#3099#);
   S30FF : aliased constant Code_Point_Array := (16#30FF#,16#100030B3#,16#30C8#);
   S3131 : aliased constant Code_Point_Array := (16#3131#,16#10001100#);
   S3132 : aliased constant Code_Point_Array := (16#3132#,16#10001101#);
   S3133 : aliased constant Code_Point_Array := (16#3133#,16#100011AA#);
   S3134 : aliased constant Code_Point_Array := (16#3134#,16#10001102#);
   S3135 : aliased constant Code_Point_Array := (16#3135#,16#100011AC#);
   S3136 : aliased constant Code_Point_Array := (16#3136#,16#100011AD#);
   S3137 : aliased constant Code_Point_Array := (16#3137#,16#10001103#);
   S3138 : aliased constant Code_Point_Array := (16#3138#,16#10001104#);
   S3139 : aliased constant Code_Point_Array := (16#3139#,16#10001105#);
   S313A : aliased constant Code_Point_Array := (16#313A#,16#100011B0#);
   S313B : aliased constant Code_Point_Array := (16#313B#,16#100011B1#);
   S313C : aliased constant Code_Point_Array := (16#313C#,16#100011B2#);
   S313D : aliased constant Code_Point_Array := (16#313D#,16#100011B3#);
   S313E : aliased constant Code_Point_Array := (16#313E#,16#100011B4#);
   S313F : aliased constant Code_Point_Array := (16#313F#,16#100011B5#);
   S3140 : aliased constant Code_Point_Array := (16#3140#,16#1000111A#);
   S3141 : aliased constant Code_Point_Array := (16#3141#,16#10001106#);
   S3142 : aliased constant Code_Point_Array := (16#3142#,16#10001107#);
   S3143 : aliased constant Code_Point_Array := (16#3143#,16#10001108#);
   S3144 : aliased constant Code_Point_Array := (16#3144#,16#10001121#);
   S3145 : aliased constant Code_Point_Array := (16#3145#,16#10001109#);
   S3146 : aliased constant Code_Point_Array := (16#3146#,16#1000110A#);
   S3147 : aliased constant Code_Point_Array := (16#3147#,16#1000110B#);
   S3148 : aliased constant Code_Point_Array := (16#3148#,16#1000110C#);
   S3149 : aliased constant Code_Point_Array := (16#3149#,16#1000110D#);
   S314A : aliased constant Code_Point_Array := (16#314A#,16#1000110E#);
   S314B : aliased constant Code_Point_Array := (16#314B#,16#1000110F#);
   S314C : aliased constant Code_Point_Array := (16#314C#,16#10001110#);
   S314D : aliased constant Code_Point_Array := (16#314D#,16#10001111#);
   S314E : aliased constant Code_Point_Array := (16#314E#,16#10001112#);
   S314F : aliased constant Code_Point_Array := (16#314F#,16#10001161#);
   S3150 : aliased constant Code_Point_Array := (16#3150#,16#10001162#);
   S3151 : aliased constant Code_Point_Array := (16#3151#,16#10001163#);
   S3152 : aliased constant Code_Point_Array := (16#3152#,16#10001164#);
   S3153 : aliased constant Code_Point_Array := (16#3153#,16#10001165#);
   S3154 : aliased constant Code_Point_Array := (16#3154#,16#10001166#);
   S3155 : aliased constant Code_Point_Array := (16#3155#,16#10001167#);
   S3156 : aliased constant Code_Point_Array := (16#3156#,16#10001168#);
   S3157 : aliased constant Code_Point_Array := (16#3157#,16#10001169#);
   S3158 : aliased constant Code_Point_Array := (16#3158#,16#1000116A#);
   S3159 : aliased constant Code_Point_Array := (16#3159#,16#1000116B#);
   S315A : aliased constant Code_Point_Array := (16#315A#,16#1000116C#);
   S315B : aliased constant Code_Point_Array := (16#315B#,16#1000116D#);
   S315C : aliased constant Code_Point_Array := (16#315C#,16#1000116E#);
   S315D : aliased constant Code_Point_Array := (16#315D#,16#1000116F#);
   S315E : aliased constant Code_Point_Array := (16#315E#,16#10001170#);
   S315F : aliased constant Code_Point_Array := (16#315F#,16#10001171#);
   S3160 : aliased constant Code_Point_Array := (16#3160#,16#10001172#);
   S3161 : aliased constant Code_Point_Array := (16#3161#,16#10001173#);
   S3162 : aliased constant Code_Point_Array := (16#3162#,16#10001174#);
   S3163 : aliased constant Code_Point_Array := (16#3163#,16#10001175#);
   S3164 : aliased constant Code_Point_Array := (16#3164#,16#10001160#);
   S3165 : aliased constant Code_Point_Array := (16#3165#,16#10001114#);
   S3166 : aliased constant Code_Point_Array := (16#3166#,16#10001115#);
   S3167 : aliased constant Code_Point_Array := (16#3167#,16#100011C7#);
   S3168 : aliased constant Code_Point_Array := (16#3168#,16#100011C8#);
   S3169 : aliased constant Code_Point_Array := (16#3169#,16#100011CC#);
   S316A : aliased constant Code_Point_Array := (16#316A#,16#100011CE#);
   S316B : aliased constant Code_Point_Array := (16#316B#,16#100011D3#);
   S316C : aliased constant Code_Point_Array := (16#316C#,16#100011D7#);
   S316D : aliased constant Code_Point_Array := (16#316D#,16#100011D9#);
   S316E : aliased constant Code_Point_Array := (16#316E#,16#1000111C#);
   S316F : aliased constant Code_Point_Array := (16#316F#,16#100011DD#);
   S3170 : aliased constant Code_Point_Array := (16#3170#,16#100011DF#);
   S3171 : aliased constant Code_Point_Array := (16#3171#,16#1000111D#);
   S3172 : aliased constant Code_Point_Array := (16#3172#,16#1000111E#);
   S3173 : aliased constant Code_Point_Array := (16#3173#,16#10001120#);
   S3174 : aliased constant Code_Point_Array := (16#3174#,16#10001122#);
   S3175 : aliased constant Code_Point_Array := (16#3175#,16#10001123#);
   S3176 : aliased constant Code_Point_Array := (16#3176#,16#10001127#);
   S3177 : aliased constant Code_Point_Array := (16#3177#,16#10001129#);
   S3178 : aliased constant Code_Point_Array := (16#3178#,16#1000112B#);
   S3179 : aliased constant Code_Point_Array := (16#3179#,16#1000112C#);
   S317A : aliased constant Code_Point_Array := (16#317A#,16#1000112D#);
   S317B : aliased constant Code_Point_Array := (16#317B#,16#1000112E#);
   S317C : aliased constant Code_Point_Array := (16#317C#,16#1000112F#);
   S317D : aliased constant Code_Point_Array := (16#317D#,16#10001132#);
   S317E : aliased constant Code_Point_Array := (16#317E#,16#10001136#);
   S317F : aliased constant Code_Point_Array := (16#317F#,16#10001140#);
   S3180 : aliased constant Code_Point_Array := (16#3180#,16#10001147#);
   S3181 : aliased constant Code_Point_Array := (16#3181#,16#1000114C#);
   S3182 : aliased constant Code_Point_Array := (16#3182#,16#100011F1#);
   S3183 : aliased constant Code_Point_Array := (16#3183#,16#100011F2#);
   S3184 : aliased constant Code_Point_Array := (16#3184#,16#10001157#);
   S3185 : aliased constant Code_Point_Array := (16#3185#,16#10001158#);
   S3186 : aliased constant Code_Point_Array := (16#3186#,16#10001159#);
   S3187 : aliased constant Code_Point_Array := (16#3187#,16#10001184#);
   S3188 : aliased constant Code_Point_Array := (16#3188#,16#10001185#);
   S3189 : aliased constant Code_Point_Array := (16#3189#,16#10001188#);
   S318A : aliased constant Code_Point_Array := (16#318A#,16#10001191#);
   S318B : aliased constant Code_Point_Array := (16#318B#,16#10001192#);
   S318C : aliased constant Code_Point_Array := (16#318C#,16#10001194#);
   S318D : aliased constant Code_Point_Array := (16#318D#,16#1000119E#);
   S318E : aliased constant Code_Point_Array := (16#318E#,16#100011A1#);
   S3192 : aliased constant Code_Point_Array := (16#3192#,16#10004E00#);
   S3193 : aliased constant Code_Point_Array := (16#3193#,16#10004E8C#);
   S3194 : aliased constant Code_Point_Array := (16#3194#,16#10004E09#);
   S3195 : aliased constant Code_Point_Array := (16#3195#,16#100056DB#);
   S3196 : aliased constant Code_Point_Array := (16#3196#,16#10004E0A#);
   S3197 : aliased constant Code_Point_Array := (16#3197#,16#10004E2D#);
   S3198 : aliased constant Code_Point_Array := (16#3198#,16#10004E0B#);
   S3199 : aliased constant Code_Point_Array := (16#3199#,16#10007532#);
   S319A : aliased constant Code_Point_Array := (16#319A#,16#10004E59#);
   S319B : aliased constant Code_Point_Array := (16#319B#,16#10004E19#);
   S319C : aliased constant Code_Point_Array := (16#319C#,16#10004E01#);
   S319D : aliased constant Code_Point_Array := (16#319D#,16#10005929#);
   S319E : aliased constant Code_Point_Array := (16#319E#,16#10005730#);
   S319F : aliased constant Code_Point_Array := (16#319F#,16#10004EBA#);
   S3200 : aliased constant Code_Point_Array := (16#3200#,16#10000028#,16#1100#,16#29#);
   S3201 : aliased constant Code_Point_Array := (16#3201#,16#10000028#,16#1102#,16#29#);
   S3202 : aliased constant Code_Point_Array := (16#3202#,16#10000028#,16#1103#,16#29#);
   S3203 : aliased constant Code_Point_Array := (16#3203#,16#10000028#,16#1105#,16#29#);
   S3204 : aliased constant Code_Point_Array := (16#3204#,16#10000028#,16#1106#,16#29#);
   S3205 : aliased constant Code_Point_Array := (16#3205#,16#10000028#,16#1107#,16#29#);
   S3206 : aliased constant Code_Point_Array := (16#3206#,16#10000028#,16#1109#,16#29#);
   S3207 : aliased constant Code_Point_Array := (16#3207#,16#10000028#,16#110B#,16#29#);
   S3208 : aliased constant Code_Point_Array := (16#3208#,16#10000028#,16#110C#,16#29#);
   S3209 : aliased constant Code_Point_Array := (16#3209#,16#10000028#,16#110E#,16#29#);
   S320A : aliased constant Code_Point_Array := (16#320A#,16#10000028#,16#110F#,16#29#);
   S320B : aliased constant Code_Point_Array := (16#320B#,16#10000028#,16#1110#,16#29#);
   S320C : aliased constant Code_Point_Array := (16#320C#,16#10000028#,16#1111#,16#29#);
   S320D : aliased constant Code_Point_Array := (16#320D#,16#10000028#,16#1112#,16#29#);
   S320E : aliased constant Code_Point_Array := (16#320E#,16#10000028#,16#1100#,16#1161#,16#29#);
   S320F : aliased constant Code_Point_Array := (16#320F#,16#10000028#,16#1102#,16#1161#,16#29#);
   S3210 : aliased constant Code_Point_Array := (16#3210#,16#10000028#,16#1103#,16#1161#,16#29#);
   S3211 : aliased constant Code_Point_Array := (16#3211#,16#10000028#,16#1105#,16#1161#,16#29#);
   S3212 : aliased constant Code_Point_Array := (16#3212#,16#10000028#,16#1106#,16#1161#,16#29#);
   S3213 : aliased constant Code_Point_Array := (16#3213#,16#10000028#,16#1107#,16#1161#,16#29#);
   S3214 : aliased constant Code_Point_Array := (16#3214#,16#10000028#,16#1109#,16#1161#,16#29#);
   S3215 : aliased constant Code_Point_Array := (16#3215#,16#10000028#,16#110B#,16#1161#,16#29#);
   S3216 : aliased constant Code_Point_Array := (16#3216#,16#10000028#,16#110C#,16#1161#,16#29#);
   S3217 : aliased constant Code_Point_Array := (16#3217#,16#10000028#,16#110E#,16#1161#,16#29#);
   S3218 : aliased constant Code_Point_Array := (16#3218#,16#10000028#,16#110F#,16#1161#,16#29#);
   S3219 : aliased constant Code_Point_Array := (16#3219#,16#10000028#,16#1110#,16#1161#,16#29#);
   S321A : aliased constant Code_Point_Array := (16#321A#,16#10000028#,16#1111#,16#1161#,16#29#);
   S321B : aliased constant Code_Point_Array := (16#321B#,16#10000028#,16#1112#,16#1161#,16#29#);
   S321C : aliased constant Code_Point_Array := (16#321C#,16#10000028#,16#110C#,16#116E#,16#29#);
   S321D : aliased constant Code_Point_Array := (16#321D#,16#10000028#,16#110B#,16#1169#,16#110C#,16#1165#,16#11AB#,16#29#);
   S321E : aliased constant Code_Point_Array := (16#321E#,16#10000028#,16#110B#,16#1169#,16#1112#,16#116E#,16#29#);
   S3220 : aliased constant Code_Point_Array := (16#3220#,16#10000028#,16#4E00#,16#29#);
   S3221 : aliased constant Code_Point_Array := (16#3221#,16#10000028#,16#4E8C#,16#29#);
   S3222 : aliased constant Code_Point_Array := (16#3222#,16#10000028#,16#4E09#,16#29#);
   S3223 : aliased constant Code_Point_Array := (16#3223#,16#10000028#,16#56DB#,16#29#);
   S3224 : aliased constant Code_Point_Array := (16#3224#,16#10000028#,16#4E94#,16#29#);
   S3225 : aliased constant Code_Point_Array := (16#3225#,16#10000028#,16#516D#,16#29#);
   S3226 : aliased constant Code_Point_Array := (16#3226#,16#10000028#,16#4E03#,16#29#);
   S3227 : aliased constant Code_Point_Array := (16#3227#,16#10000028#,16#516B#,16#29#);
   S3228 : aliased constant Code_Point_Array := (16#3228#,16#10000028#,16#4E5D#,16#29#);
   S3229 : aliased constant Code_Point_Array := (16#3229#,16#10000028#,16#5341#,16#29#);
   S322A : aliased constant Code_Point_Array := (16#322A#,16#10000028#,16#6708#,16#29#);
   S322B : aliased constant Code_Point_Array := (16#322B#,16#10000028#,16#706B#,16#29#);
   S322C : aliased constant Code_Point_Array := (16#322C#,16#10000028#,16#6C34#,16#29#);
   S322D : aliased constant Code_Point_Array := (16#322D#,16#10000028#,16#6728#,16#29#);
   S322E : aliased constant Code_Point_Array := (16#322E#,16#10000028#,16#91D1#,16#29#);
   S322F : aliased constant Code_Point_Array := (16#322F#,16#10000028#,16#571F#,16#29#);
   S3230 : aliased constant Code_Point_Array := (16#3230#,16#10000028#,16#65E5#,16#29#);
   S3231 : aliased constant Code_Point_Array := (16#3231#,16#10000028#,16#682A#,16#29#);
   S3232 : aliased constant Code_Point_Array := (16#3232#,16#10000028#,16#6709#,16#29#);
   S3233 : aliased constant Code_Point_Array := (16#3233#,16#10000028#,16#793E#,16#29#);
   S3234 : aliased constant Code_Point_Array := (16#3234#,16#10000028#,16#540D#,16#29#);
   S3235 : aliased constant Code_Point_Array := (16#3235#,16#10000028#,16#7279#,16#29#);
   S3236 : aliased constant Code_Point_Array := (16#3236#,16#10000028#,16#8CA1#,16#29#);
   S3237 : aliased constant Code_Point_Array := (16#3237#,16#10000028#,16#795D#,16#29#);
   S3238 : aliased constant Code_Point_Array := (16#3238#,16#10000028#,16#52B4#,16#29#);
   S3239 : aliased constant Code_Point_Array := (16#3239#,16#10000028#,16#4EE3#,16#29#);
   S323A : aliased constant Code_Point_Array := (16#323A#,16#10000028#,16#547C#,16#29#);
   S323B : aliased constant Code_Point_Array := (16#323B#,16#10000028#,16#5B66#,16#29#);
   S323C : aliased constant Code_Point_Array := (16#323C#,16#10000028#,16#76E3#,16#29#);
   S323D : aliased constant Code_Point_Array := (16#323D#,16#10000028#,16#4F01#,16#29#);
   S323E : aliased constant Code_Point_Array := (16#323E#,16#10000028#,16#8CC7#,16#29#);
   S323F : aliased constant Code_Point_Array := (16#323F#,16#10000028#,16#5354#,16#29#);
   S3240 : aliased constant Code_Point_Array := (16#3240#,16#10000028#,16#796D#,16#29#);
   S3241 : aliased constant Code_Point_Array := (16#3241#,16#10000028#,16#4F11#,16#29#);
   S3242 : aliased constant Code_Point_Array := (16#3242#,16#10000028#,16#81EA#,16#29#);
   S3243 : aliased constant Code_Point_Array := (16#3243#,16#10000028#,16#81F3#,16#29#);
   S3244 : aliased constant Code_Point_Array := (16#3244#,16#1000554F#);
   S3245 : aliased constant Code_Point_Array := (16#3245#,16#10005E7C#);
   S3246 : aliased constant Code_Point_Array := (16#3246#,16#10006587#);
   S3247 : aliased constant Code_Point_Array := (16#3247#,16#10007B8F#);
   S3250 : aliased constant Code_Point_Array := (16#3250#,16#10000050#,16#54#,16#45#);
   S3251 : aliased constant Code_Point_Array := (16#3251#,16#10000032#,16#31#);
   S3252 : aliased constant Code_Point_Array := (16#3252#,16#10000032#,16#32#);
   S3253 : aliased constant Code_Point_Array := (16#3253#,16#10000032#,16#33#);
   S3254 : aliased constant Code_Point_Array := (16#3254#,16#10000032#,16#34#);
   S3255 : aliased constant Code_Point_Array := (16#3255#,16#10000032#,16#35#);
   S3256 : aliased constant Code_Point_Array := (16#3256#,16#10000032#,16#36#);
   S3257 : aliased constant Code_Point_Array := (16#3257#,16#10000032#,16#37#);
   S3258 : aliased constant Code_Point_Array := (16#3258#,16#10000032#,16#38#);
   S3259 : aliased constant Code_Point_Array := (16#3259#,16#10000032#,16#39#);
   S325A : aliased constant Code_Point_Array := (16#325A#,16#10000033#,16#30#);
   S325B : aliased constant Code_Point_Array := (16#325B#,16#10000033#,16#31#);
   S325C : aliased constant Code_Point_Array := (16#325C#,16#10000033#,16#32#);
   S325D : aliased constant Code_Point_Array := (16#325D#,16#10000033#,16#33#);
   S325E : aliased constant Code_Point_Array := (16#325E#,16#10000033#,16#34#);
   S325F : aliased constant Code_Point_Array := (16#325F#,16#10000033#,16#35#);
   S3260 : aliased constant Code_Point_Array := (16#3260#,16#10001100#);
   S3261 : aliased constant Code_Point_Array := (16#3261#,16#10001102#);
   S3262 : aliased constant Code_Point_Array := (16#3262#,16#10001103#);
   S3263 : aliased constant Code_Point_Array := (16#3263#,16#10001105#);
   S3264 : aliased constant Code_Point_Array := (16#3264#,16#10001106#);
   S3265 : aliased constant Code_Point_Array := (16#3265#,16#10001107#);
   S3266 : aliased constant Code_Point_Array := (16#3266#,16#10001109#);
   S3267 : aliased constant Code_Point_Array := (16#3267#,16#1000110B#);
   S3268 : aliased constant Code_Point_Array := (16#3268#,16#1000110C#);
   S3269 : aliased constant Code_Point_Array := (16#3269#,16#1000110E#);
   S326A : aliased constant Code_Point_Array := (16#326A#,16#1000110F#);
   S326B : aliased constant Code_Point_Array := (16#326B#,16#10001110#);
   S326C : aliased constant Code_Point_Array := (16#326C#,16#10001111#);
   S326D : aliased constant Code_Point_Array := (16#326D#,16#10001112#);
   S326E : aliased constant Code_Point_Array := (16#326E#,16#10001100#,16#1161#);
   S326F : aliased constant Code_Point_Array := (16#326F#,16#10001102#,16#1161#);
   S3270 : aliased constant Code_Point_Array := (16#3270#,16#10001103#,16#1161#);
   S3271 : aliased constant Code_Point_Array := (16#3271#,16#10001105#,16#1161#);
   S3272 : aliased constant Code_Point_Array := (16#3272#,16#10001106#,16#1161#);
   S3273 : aliased constant Code_Point_Array := (16#3273#,16#10001107#,16#1161#);
   S3274 : aliased constant Code_Point_Array := (16#3274#,16#10001109#,16#1161#);
   S3275 : aliased constant Code_Point_Array := (16#3275#,16#1000110B#,16#1161#);
   S3276 : aliased constant Code_Point_Array := (16#3276#,16#1000110C#,16#1161#);
   S3277 : aliased constant Code_Point_Array := (16#3277#,16#1000110E#,16#1161#);
   S3278 : aliased constant Code_Point_Array := (16#3278#,16#1000110F#,16#1161#);
   S3279 : aliased constant Code_Point_Array := (16#3279#,16#10001110#,16#1161#);
   S327A : aliased constant Code_Point_Array := (16#327A#,16#10001111#,16#1161#);
   S327B : aliased constant Code_Point_Array := (16#327B#,16#10001112#,16#1161#);
   S327C : aliased constant Code_Point_Array := (16#327C#,16#1000110E#,16#1161#,16#11B7#,16#1100#,16#1169#);
   S327D : aliased constant Code_Point_Array := (16#327D#,16#1000110C#,16#116E#,16#110B#,16#1174#);
   S327E : aliased constant Code_Point_Array := (16#327E#,16#1000110B#,16#116E#);
   S3280 : aliased constant Code_Point_Array := (16#3280#,16#10004E00#);
   S3281 : aliased constant Code_Point_Array := (16#3281#,16#10004E8C#);
   S3282 : aliased constant Code_Point_Array := (16#3282#,16#10004E09#);
   S3283 : aliased constant Code_Point_Array := (16#3283#,16#100056DB#);
   S3284 : aliased constant Code_Point_Array := (16#3284#,16#10004E94#);
   S3285 : aliased constant Code_Point_Array := (16#3285#,16#1000516D#);
   S3286 : aliased constant Code_Point_Array := (16#3286#,16#10004E03#);
   S3287 : aliased constant Code_Point_Array := (16#3287#,16#1000516B#);
   S3288 : aliased constant Code_Point_Array := (16#3288#,16#10004E5D#);
   S3289 : aliased constant Code_Point_Array := (16#3289#,16#10005341#);
   S328A : aliased constant Code_Point_Array := (16#328A#,16#10006708#);
   S328B : aliased constant Code_Point_Array := (16#328B#,16#1000706B#);
   S328C : aliased constant Code_Point_Array := (16#328C#,16#10006C34#);
   S328D : aliased constant Code_Point_Array := (16#328D#,16#10006728#);
   S328E : aliased constant Code_Point_Array := (16#328E#,16#100091D1#);
   S328F : aliased constant Code_Point_Array := (16#328F#,16#1000571F#);
   S3290 : aliased constant Code_Point_Array := (16#3290#,16#100065E5#);
   S3291 : aliased constant Code_Point_Array := (16#3291#,16#1000682A#);
   S3292 : aliased constant Code_Point_Array := (16#3292#,16#10006709#);
   S3293 : aliased constant Code_Point_Array := (16#3293#,16#1000793E#);
   S3294 : aliased constant Code_Point_Array := (16#3294#,16#1000540D#);
   S3295 : aliased constant Code_Point_Array := (16#3295#,16#10007279#);
   S3296 : aliased constant Code_Point_Array := (16#3296#,16#10008CA1#);
   S3297 : aliased constant Code_Point_Array := (16#3297#,16#1000795D#);
   S3298 : aliased constant Code_Point_Array := (16#3298#,16#100052B4#);
   S3299 : aliased constant Code_Point_Array := (16#3299#,16#100079D8#);
   S329A : aliased constant Code_Point_Array := (16#329A#,16#10007537#);
   S329B : aliased constant Code_Point_Array := (16#329B#,16#10005973#);
   S329C : aliased constant Code_Point_Array := (16#329C#,16#10009069#);
   S329D : aliased constant Code_Point_Array := (16#329D#,16#1000512A#);
   S329E : aliased constant Code_Point_Array := (16#329E#,16#10005370#);
   S329F : aliased constant Code_Point_Array := (16#329F#,16#10006CE8#);
   S32A0 : aliased constant Code_Point_Array := (16#32A0#,16#10009805#);
   S32A1 : aliased constant Code_Point_Array := (16#32A1#,16#10004F11#);
   S32A2 : aliased constant Code_Point_Array := (16#32A2#,16#10005199#);
   S32A3 : aliased constant Code_Point_Array := (16#32A3#,16#10006B63#);
   S32A4 : aliased constant Code_Point_Array := (16#32A4#,16#10004E0A#);
   S32A5 : aliased constant Code_Point_Array := (16#32A5#,16#10004E2D#);
   S32A6 : aliased constant Code_Point_Array := (16#32A6#,16#10004E0B#);
   S32A7 : aliased constant Code_Point_Array := (16#32A7#,16#10005DE6#);
   S32A8 : aliased constant Code_Point_Array := (16#32A8#,16#100053F3#);
   S32A9 : aliased constant Code_Point_Array := (16#32A9#,16#1000533B#);
   S32AA : aliased constant Code_Point_Array := (16#32AA#,16#10005B97#);
   S32AB : aliased constant Code_Point_Array := (16#32AB#,16#10005B66#);
   S32AC : aliased constant Code_Point_Array := (16#32AC#,16#100076E3#);
   S32AD : aliased constant Code_Point_Array := (16#32AD#,16#10004F01#);
   S32AE : aliased constant Code_Point_Array := (16#32AE#,16#10008CC7#);
   S32AF : aliased constant Code_Point_Array := (16#32AF#,16#10005354#);
   S32B0 : aliased constant Code_Point_Array := (16#32B0#,16#1000591C#);
   S32B1 : aliased constant Code_Point_Array := (16#32B1#,16#10000033#,16#36#);
   S32B2 : aliased constant Code_Point_Array := (16#32B2#,16#10000033#,16#37#);
   S32B3 : aliased constant Code_Point_Array := (16#32B3#,16#10000033#,16#38#);
   S32B4 : aliased constant Code_Point_Array := (16#32B4#,16#10000033#,16#39#);
   S32B5 : aliased constant Code_Point_Array := (16#32B5#,16#10000034#,16#30#);
   S32B6 : aliased constant Code_Point_Array := (16#32B6#,16#10000034#,16#31#);
   S32B7 : aliased constant Code_Point_Array := (16#32B7#,16#10000034#,16#32#);
   S32B8 : aliased constant Code_Point_Array := (16#32B8#,16#10000034#,16#33#);
   S32B9 : aliased constant Code_Point_Array := (16#32B9#,16#10000034#,16#34#);
   S32BA : aliased constant Code_Point_Array := (16#32BA#,16#10000034#,16#35#);
   S32BB : aliased constant Code_Point_Array := (16#32BB#,16#10000034#,16#36#);
   S32BC : aliased constant Code_Point_Array := (16#32BC#,16#10000034#,16#37#);
   S32BD : aliased constant Code_Point_Array := (16#32BD#,16#10000034#,16#38#);
   S32BE : aliased constant Code_Point_Array := (16#32BE#,16#10000034#,16#39#);
   S32BF : aliased constant Code_Point_Array := (16#32BF#,16#10000035#,16#30#);
   S32C0 : aliased constant Code_Point_Array := (16#32C0#,16#10000031#,16#6708#);
   S32C1 : aliased constant Code_Point_Array := (16#32C1#,16#10000032#,16#6708#);
   S32C2 : aliased constant Code_Point_Array := (16#32C2#,16#10000033#,16#6708#);
   S32C3 : aliased constant Code_Point_Array := (16#32C3#,16#10000034#,16#6708#);
   S32C4 : aliased constant Code_Point_Array := (16#32C4#,16#10000035#,16#6708#);
   S32C5 : aliased constant Code_Point_Array := (16#32C5#,16#10000036#,16#6708#);
   S32C6 : aliased constant Code_Point_Array := (16#32C6#,16#10000037#,16#6708#);
   S32C7 : aliased constant Code_Point_Array := (16#32C7#,16#10000038#,16#6708#);
   S32C8 : aliased constant Code_Point_Array := (16#32C8#,16#10000039#,16#6708#);
   S32C9 : aliased constant Code_Point_Array := (16#32C9#,16#10000031#,16#30#,16#6708#);
   S32CA : aliased constant Code_Point_Array := (16#32CA#,16#10000031#,16#31#,16#6708#);
   S32CB : aliased constant Code_Point_Array := (16#32CB#,16#10000031#,16#32#,16#6708#);
   S32CC : aliased constant Code_Point_Array := (16#32CC#,16#10000048#,16#67#);
   S32CD : aliased constant Code_Point_Array := (16#32CD#,16#10000065#,16#72#,16#67#);
   S32CE : aliased constant Code_Point_Array := (16#32CE#,16#10000065#,16#56#);
   S32CF : aliased constant Code_Point_Array := (16#32CF#,16#1000004C#,16#54#,16#44#);
   S32D0 : aliased constant Code_Point_Array := (16#32D0#,16#100030A2#);
   S32D1 : aliased constant Code_Point_Array := (16#32D1#,16#100030A4#);
   S32D2 : aliased constant Code_Point_Array := (16#32D2#,16#100030A6#);
   S32D3 : aliased constant Code_Point_Array := (16#32D3#,16#100030A8#);
   S32D4 : aliased constant Code_Point_Array := (16#32D4#,16#100030AA#);
   S32D5 : aliased constant Code_Point_Array := (16#32D5#,16#100030AB#);
   S32D6 : aliased constant Code_Point_Array := (16#32D6#,16#100030AD#);
   S32D7 : aliased constant Code_Point_Array := (16#32D7#,16#100030AF#);
   S32D8 : aliased constant Code_Point_Array := (16#32D8#,16#100030B1#);
   S32D9 : aliased constant Code_Point_Array := (16#32D9#,16#100030B3#);
   S32DA : aliased constant Code_Point_Array := (16#32DA#,16#100030B5#);
   S32DB : aliased constant Code_Point_Array := (16#32DB#,16#100030B7#);
   S32DC : aliased constant Code_Point_Array := (16#32DC#,16#100030B9#);
   S32DD : aliased constant Code_Point_Array := (16#32DD#,16#100030BB#);
   S32DE : aliased constant Code_Point_Array := (16#32DE#,16#100030BD#);
   S32DF : aliased constant Code_Point_Array := (16#32DF#,16#100030BF#);
   S32E0 : aliased constant Code_Point_Array := (16#32E0#,16#100030C1#);
   S32E1 : aliased constant Code_Point_Array := (16#32E1#,16#100030C4#);
   S32E2 : aliased constant Code_Point_Array := (16#32E2#,16#100030C6#);
   S32E3 : aliased constant Code_Point_Array := (16#32E3#,16#100030C8#);
   S32E4 : aliased constant Code_Point_Array := (16#32E4#,16#100030CA#);
   S32E5 : aliased constant Code_Point_Array := (16#32E5#,16#100030CB#);
   S32E6 : aliased constant Code_Point_Array := (16#32E6#,16#100030CC#);
   S32E7 : aliased constant Code_Point_Array := (16#32E7#,16#100030CD#);
   S32E8 : aliased constant Code_Point_Array := (16#32E8#,16#100030CE#);
   S32E9 : aliased constant Code_Point_Array := (16#32E9#,16#100030CF#);
   S32EA : aliased constant Code_Point_Array := (16#32EA#,16#100030D2#);
   S32EB : aliased constant Code_Point_Array := (16#32EB#,16#100030D5#);
   S32EC : aliased constant Code_Point_Array := (16#32EC#,16#100030D8#);
   S32ED : aliased constant Code_Point_Array := (16#32ED#,16#100030DB#);
   S32EE : aliased constant Code_Point_Array := (16#32EE#,16#100030DE#);
   S32EF : aliased constant Code_Point_Array := (16#32EF#,16#100030DF#);
   S32F0 : aliased constant Code_Point_Array := (16#32F0#,16#100030E0#);
   S32F1 : aliased constant Code_Point_Array := (16#32F1#,16#100030E1#);
   S32F2 : aliased constant Code_Point_Array := (16#32F2#,16#100030E2#);
   S32F3 : aliased constant Code_Point_Array := (16#32F3#,16#100030E4#);
   S32F4 : aliased constant Code_Point_Array := (16#32F4#,16#100030E6#);
   S32F5 : aliased constant Code_Point_Array := (16#32F5#,16#100030E8#);
   S32F6 : aliased constant Code_Point_Array := (16#32F6#,16#100030E9#);
   S32F7 : aliased constant Code_Point_Array := (16#32F7#,16#100030EA#);
   S32F8 : aliased constant Code_Point_Array := (16#32F8#,16#100030EB#);
   S32F9 : aliased constant Code_Point_Array := (16#32F9#,16#100030EC#);
   S32FA : aliased constant Code_Point_Array := (16#32FA#,16#100030ED#);
   S32FB : aliased constant Code_Point_Array := (16#32FB#,16#100030EF#);
   S32FC : aliased constant Code_Point_Array := (16#32FC#,16#100030F0#);
   S32FD : aliased constant Code_Point_Array := (16#32FD#,16#100030F1#);
   S32FE : aliased constant Code_Point_Array := (16#32FE#,16#100030F2#);
   S32FF : aliased constant Code_Point_Array := (16#32FF#,16#10004EE4#,16#548C#);
   S3300 : aliased constant Code_Point_Array := (16#3300#,16#100030A2#,16#30D1#,16#30FC#,16#30C8#);
   S3301 : aliased constant Code_Point_Array := (16#3301#,16#100030A2#,16#30EB#,16#30D5#,16#30A1#);
   S3302 : aliased constant Code_Point_Array := (16#3302#,16#100030A2#,16#30F3#,16#30DA#,16#30A2#);
   S3303 : aliased constant Code_Point_Array := (16#3303#,16#100030A2#,16#30FC#,16#30EB#);
   S3304 : aliased constant Code_Point_Array := (16#3304#,16#100030A4#,16#30CB#,16#30F3#,16#30B0#);
   S3305 : aliased constant Code_Point_Array := (16#3305#,16#100030A4#,16#30F3#,16#30C1#);
   S3306 : aliased constant Code_Point_Array := (16#3306#,16#100030A6#,16#30A9#,16#30F3#);
   S3307 : aliased constant Code_Point_Array := (16#3307#,16#100030A8#,16#30B9#,16#30AF#,16#30FC#,16#30C9#);
   S3308 : aliased constant Code_Point_Array := (16#3308#,16#100030A8#,16#30FC#,16#30AB#,16#30FC#);
   S3309 : aliased constant Code_Point_Array := (16#3309#,16#100030AA#,16#30F3#,16#30B9#);
   S330A : aliased constant Code_Point_Array := (16#330A#,16#100030AA#,16#30FC#,16#30E0#);
   S330B : aliased constant Code_Point_Array := (16#330B#,16#100030AB#,16#30A4#,16#30EA#);
   S330C : aliased constant Code_Point_Array := (16#330C#,16#100030AB#,16#30E9#,16#30C3#,16#30C8#);
   S330D : aliased constant Code_Point_Array := (16#330D#,16#100030AB#,16#30ED#,16#30EA#,16#30FC#);
   S330E : aliased constant Code_Point_Array := (16#330E#,16#100030AC#,16#30ED#,16#30F3#);
   S330F : aliased constant Code_Point_Array := (16#330F#,16#100030AC#,16#30F3#,16#30DE#);
   S3310 : aliased constant Code_Point_Array := (16#3310#,16#100030AE#,16#30AC#);
   S3311 : aliased constant Code_Point_Array := (16#3311#,16#100030AE#,16#30CB#,16#30FC#);
   S3312 : aliased constant Code_Point_Array := (16#3312#,16#100030AD#,16#30E5#,16#30EA#,16#30FC#);
   S3313 : aliased constant Code_Point_Array := (16#3313#,16#100030AE#,16#30EB#,16#30C0#,16#30FC#);
   S3314 : aliased constant Code_Point_Array := (16#3314#,16#100030AD#,16#30ED#);
   S3315 : aliased constant Code_Point_Array := (16#3315#,16#100030AD#,16#30ED#,16#30B0#,16#30E9#,16#30E0#);
   S3316 : aliased constant Code_Point_Array := (16#3316#,16#100030AD#,16#30ED#,16#30E1#,16#30FC#,16#30C8#,16#30EB#);
   S3317 : aliased constant Code_Point_Array := (16#3317#,16#100030AD#,16#30ED#,16#30EF#,16#30C3#,16#30C8#);
   S3318 : aliased constant Code_Point_Array := (16#3318#,16#100030B0#,16#30E9#,16#30E0#);
   S3319 : aliased constant Code_Point_Array := (16#3319#,16#100030B0#,16#30E9#,16#30E0#,16#30C8#,16#30F3#);
   S331A : aliased constant Code_Point_Array := (16#331A#,16#100030AF#,16#30EB#,16#30BC#,16#30A4#,16#30ED#);
   S331B : aliased constant Code_Point_Array := (16#331B#,16#100030AF#,16#30ED#,16#30FC#,16#30CD#);
   S331C : aliased constant Code_Point_Array := (16#331C#,16#100030B1#,16#30FC#,16#30B9#);
   S331D : aliased constant Code_Point_Array := (16#331D#,16#100030B3#,16#30EB#,16#30CA#);
   S331E : aliased constant Code_Point_Array := (16#331E#,16#100030B3#,16#30FC#,16#30DD#);
   S331F : aliased constant Code_Point_Array := (16#331F#,16#100030B5#,16#30A4#,16#30AF#,16#30EB#);
   S3320 : aliased constant Code_Point_Array := (16#3320#,16#100030B5#,16#30F3#,16#30C1#,16#30FC#,16#30E0#);
   S3321 : aliased constant Code_Point_Array := (16#3321#,16#100030B7#,16#30EA#,16#30F3#,16#30B0#);
   S3322 : aliased constant Code_Point_Array := (16#3322#,16#100030BB#,16#30F3#,16#30C1#);
   S3323 : aliased constant Code_Point_Array := (16#3323#,16#100030BB#,16#30F3#,16#30C8#);
   S3324 : aliased constant Code_Point_Array := (16#3324#,16#100030C0#,16#30FC#,16#30B9#);
   S3325 : aliased constant Code_Point_Array := (16#3325#,16#100030C7#,16#30B7#);
   S3326 : aliased constant Code_Point_Array := (16#3326#,16#100030C9#,16#30EB#);
   S3327 : aliased constant Code_Point_Array := (16#3327#,16#100030C8#,16#30F3#);
   S3328 : aliased constant Code_Point_Array := (16#3328#,16#100030CA#,16#30CE#);
   S3329 : aliased constant Code_Point_Array := (16#3329#,16#100030CE#,16#30C3#,16#30C8#);
   S332A : aliased constant Code_Point_Array := (16#332A#,16#100030CF#,16#30A4#,16#30C4#);
   S332B : aliased constant Code_Point_Array := (16#332B#,16#100030D1#,16#30FC#,16#30BB#,16#30F3#,16#30C8#);
   S332C : aliased constant Code_Point_Array := (16#332C#,16#100030D1#,16#30FC#,16#30C4#);
   S332D : aliased constant Code_Point_Array := (16#332D#,16#100030D0#,16#30FC#,16#30EC#,16#30EB#);
   S332E : aliased constant Code_Point_Array := (16#332E#,16#100030D4#,16#30A2#,16#30B9#,16#30C8#,16#30EB#);
   S332F : aliased constant Code_Point_Array := (16#332F#,16#100030D4#,16#30AF#,16#30EB#);
   S3330 : aliased constant Code_Point_Array := (16#3330#,16#100030D4#,16#30B3#);
   S3331 : aliased constant Code_Point_Array := (16#3331#,16#100030D3#,16#30EB#);
   S3332 : aliased constant Code_Point_Array := (16#3332#,16#100030D5#,16#30A1#,16#30E9#,16#30C3#,16#30C9#);
   S3333 : aliased constant Code_Point_Array := (16#3333#,16#100030D5#,16#30A3#,16#30FC#,16#30C8#);
   S3334 : aliased constant Code_Point_Array := (16#3334#,16#100030D6#,16#30C3#,16#30B7#,16#30A7#,16#30EB#);
   S3335 : aliased constant Code_Point_Array := (16#3335#,16#100030D5#,16#30E9#,16#30F3#);
   S3336 : aliased constant Code_Point_Array := (16#3336#,16#100030D8#,16#30AF#,16#30BF#,16#30FC#,16#30EB#);
   S3337 : aliased constant Code_Point_Array := (16#3337#,16#100030DA#,16#30BD#);
   S3338 : aliased constant Code_Point_Array := (16#3338#,16#100030DA#,16#30CB#,16#30D2#);
   S3339 : aliased constant Code_Point_Array := (16#3339#,16#100030D8#,16#30EB#,16#30C4#);
   S333A : aliased constant Code_Point_Array := (16#333A#,16#100030DA#,16#30F3#,16#30B9#);
   S333B : aliased constant Code_Point_Array := (16#333B#,16#100030DA#,16#30FC#,16#30B8#);
   S333C : aliased constant Code_Point_Array := (16#333C#,16#100030D9#,16#30FC#,16#30BF#);
   S333D : aliased constant Code_Point_Array := (16#333D#,16#100030DD#,16#30A4#,16#30F3#,16#30C8#);
   S333E : aliased constant Code_Point_Array := (16#333E#,16#100030DC#,16#30EB#,16#30C8#);
   S333F : aliased constant Code_Point_Array := (16#333F#,16#100030DB#,16#30F3#);
   S3340 : aliased constant Code_Point_Array := (16#3340#,16#100030DD#,16#30F3#,16#30C9#);
   S3341 : aliased constant Code_Point_Array := (16#3341#,16#100030DB#,16#30FC#,16#30EB#);
   S3342 : aliased constant Code_Point_Array := (16#3342#,16#100030DB#,16#30FC#,16#30F3#);
   S3343 : aliased constant Code_Point_Array := (16#3343#,16#100030DE#,16#30A4#,16#30AF#,16#30ED#);
   S3344 : aliased constant Code_Point_Array := (16#3344#,16#100030DE#,16#30A4#,16#30EB#);
   S3345 : aliased constant Code_Point_Array := (16#3345#,16#100030DE#,16#30C3#,16#30CF#);
   S3346 : aliased constant Code_Point_Array := (16#3346#,16#100030DE#,16#30EB#,16#30AF#);
   S3347 : aliased constant Code_Point_Array := (16#3347#,16#100030DE#,16#30F3#,16#30B7#,16#30E7#,16#30F3#);
   S3348 : aliased constant Code_Point_Array := (16#3348#,16#100030DF#,16#30AF#,16#30ED#,16#30F3#);
   S3349 : aliased constant Code_Point_Array := (16#3349#,16#100030DF#,16#30EA#);
   S334A : aliased constant Code_Point_Array := (16#334A#,16#100030DF#,16#30EA#,16#30D0#,16#30FC#,16#30EB#);
   S334B : aliased constant Code_Point_Array := (16#334B#,16#100030E1#,16#30AC#);
   S334C : aliased constant Code_Point_Array := (16#334C#,16#100030E1#,16#30AC#,16#30C8#,16#30F3#);
   S334D : aliased constant Code_Point_Array := (16#334D#,16#100030E1#,16#30FC#,16#30C8#,16#30EB#);
   S334E : aliased constant Code_Point_Array := (16#334E#,16#100030E4#,16#30FC#,16#30C9#);
   S334F : aliased constant Code_Point_Array := (16#334F#,16#100030E4#,16#30FC#,16#30EB#);
   S3350 : aliased constant Code_Point_Array := (16#3350#,16#100030E6#,16#30A2#,16#30F3#);
   S3351 : aliased constant Code_Point_Array := (16#3351#,16#100030EA#,16#30C3#,16#30C8#,16#30EB#);
   S3352 : aliased constant Code_Point_Array := (16#3352#,16#100030EA#,16#30E9#);
   S3353 : aliased constant Code_Point_Array := (16#3353#,16#100030EB#,16#30D4#,16#30FC#);
   S3354 : aliased constant Code_Point_Array := (16#3354#,16#100030EB#,16#30FC#,16#30D6#,16#30EB#);
   S3355 : aliased constant Code_Point_Array := (16#3355#,16#100030EC#,16#30E0#);
   S3356 : aliased constant Code_Point_Array := (16#3356#,16#100030EC#,16#30F3#,16#30C8#,16#30B2#,16#30F3#);
   S3357 : aliased constant Code_Point_Array := (16#3357#,16#100030EF#,16#30C3#,16#30C8#);
   S3358 : aliased constant Code_Point_Array := (16#3358#,16#10000030#,16#70B9#);
   S3359 : aliased constant Code_Point_Array := (16#3359#,16#10000031#,16#70B9#);
   S335A : aliased constant Code_Point_Array := (16#335A#,16#10000032#,16#70B9#);
   S335B : aliased constant Code_Point_Array := (16#335B#,16#10000033#,16#70B9#);
   S335C : aliased constant Code_Point_Array := (16#335C#,16#10000034#,16#70B9#);
   S335D : aliased constant Code_Point_Array := (16#335D#,16#10000035#,16#70B9#);
   S335E : aliased constant Code_Point_Array := (16#335E#,16#10000036#,16#70B9#);
   S335F : aliased constant Code_Point_Array := (16#335F#,16#10000037#,16#70B9#);
   S3360 : aliased constant Code_Point_Array := (16#3360#,16#10000038#,16#70B9#);
   S3361 : aliased constant Code_Point_Array := (16#3361#,16#10000039#,16#70B9#);
   S3362 : aliased constant Code_Point_Array := (16#3362#,16#10000031#,16#30#,16#70B9#);
   S3363 : aliased constant Code_Point_Array := (16#3363#,16#10000031#,16#31#,16#70B9#);
   S3364 : aliased constant Code_Point_Array := (16#3364#,16#10000031#,16#32#,16#70B9#);
   S3365 : aliased constant Code_Point_Array := (16#3365#,16#10000031#,16#33#,16#70B9#);
   S3366 : aliased constant Code_Point_Array := (16#3366#,16#10000031#,16#34#,16#70B9#);
   S3367 : aliased constant Code_Point_Array := (16#3367#,16#10000031#,16#35#,16#70B9#);
   S3368 : aliased constant Code_Point_Array := (16#3368#,16#10000031#,16#36#,16#70B9#);
   S3369 : aliased constant Code_Point_Array := (16#3369#,16#10000031#,16#37#,16#70B9#);
   S336A : aliased constant Code_Point_Array := (16#336A#,16#10000031#,16#38#,16#70B9#);
   S336B : aliased constant Code_Point_Array := (16#336B#,16#10000031#,16#39#,16#70B9#);
   S336C : aliased constant Code_Point_Array := (16#336C#,16#10000032#,16#30#,16#70B9#);
   S336D : aliased constant Code_Point_Array := (16#336D#,16#10000032#,16#31#,16#70B9#);
   S336E : aliased constant Code_Point_Array := (16#336E#,16#10000032#,16#32#,16#70B9#);
   S336F : aliased constant Code_Point_Array := (16#336F#,16#10000032#,16#33#,16#70B9#);
   S3370 : aliased constant Code_Point_Array := (16#3370#,16#10000032#,16#34#,16#70B9#);
   S3371 : aliased constant Code_Point_Array := (16#3371#,16#10000068#,16#50#,16#61#);
   S3372 : aliased constant Code_Point_Array := (16#3372#,16#10000064#,16#61#);
   S3373 : aliased constant Code_Point_Array := (16#3373#,16#10000041#,16#55#);
   S3374 : aliased constant Code_Point_Array := (16#3374#,16#10000062#,16#61#,16#72#);
   S3375 : aliased constant Code_Point_Array := (16#3375#,16#1000006F#,16#56#);
   S3376 : aliased constant Code_Point_Array := (16#3376#,16#10000070#,16#63#);
   S3377 : aliased constant Code_Point_Array := (16#3377#,16#10000064#,16#6D#);
   S3378 : aliased constant Code_Point_Array := (16#3378#,16#10000064#,16#6D#,16#B2#);
   S3379 : aliased constant Code_Point_Array := (16#3379#,16#10000064#,16#6D#,16#B3#);
   S337A : aliased constant Code_Point_Array := (16#337A#,16#10000049#,16#55#);
   S337B : aliased constant Code_Point_Array := (16#337B#,16#10005E73#,16#6210#);
   S337C : aliased constant Code_Point_Array := (16#337C#,16#1000662D#,16#548C#);
   S337D : aliased constant Code_Point_Array := (16#337D#,16#10005927#,16#6B63#);
   S337E : aliased constant Code_Point_Array := (16#337E#,16#1000660E#,16#6CBB#);
   S337F : aliased constant Code_Point_Array := (16#337F#,16#1000682A#,16#5F0F#,16#4F1A#,16#793E#);
   S3380 : aliased constant Code_Point_Array := (16#3380#,16#10000070#,16#41#);
   S3381 : aliased constant Code_Point_Array := (16#3381#,16#1000006E#,16#41#);
   S3382 : aliased constant Code_Point_Array := (16#3382#,16#100003BC#,16#41#);
   S3383 : aliased constant Code_Point_Array := (16#3383#,16#1000006D#,16#41#);
   S3384 : aliased constant Code_Point_Array := (16#3384#,16#1000006B#,16#41#);
   S3385 : aliased constant Code_Point_Array := (16#3385#,16#1000004B#,16#42#);
   S3386 : aliased constant Code_Point_Array := (16#3386#,16#1000004D#,16#42#);
   S3387 : aliased constant Code_Point_Array := (16#3387#,16#10000047#,16#42#);
   S3388 : aliased constant Code_Point_Array := (16#3388#,16#10000063#,16#61#,16#6C#);
   S3389 : aliased constant Code_Point_Array := (16#3389#,16#1000006B#,16#63#,16#61#,16#6C#);
   S338A : aliased constant Code_Point_Array := (16#338A#,16#10000070#,16#46#);
   S338B : aliased constant Code_Point_Array := (16#338B#,16#1000006E#,16#46#);
   S338C : aliased constant Code_Point_Array := (16#338C#,16#100003BC#,16#46#);
   S338D : aliased constant Code_Point_Array := (16#338D#,16#100003BC#,16#67#);
   S338E : aliased constant Code_Point_Array := (16#338E#,16#1000006D#,16#67#);
   S338F : aliased constant Code_Point_Array := (16#338F#,16#1000006B#,16#67#);
   S3390 : aliased constant Code_Point_Array := (16#3390#,16#10000048#,16#7A#);
   S3391 : aliased constant Code_Point_Array := (16#3391#,16#1000006B#,16#48#,16#7A#);
   S3392 : aliased constant Code_Point_Array := (16#3392#,16#1000004D#,16#48#,16#7A#);
   S3393 : aliased constant Code_Point_Array := (16#3393#,16#10000047#,16#48#,16#7A#);
   S3394 : aliased constant Code_Point_Array := (16#3394#,16#10000054#,16#48#,16#7A#);
   S3395 : aliased constant Code_Point_Array := (16#3395#,16#100003BC#,16#2113#);
   S3396 : aliased constant Code_Point_Array := (16#3396#,16#1000006D#,16#2113#);
   S3397 : aliased constant Code_Point_Array := (16#3397#,16#10000064#,16#2113#);
   S3398 : aliased constant Code_Point_Array := (16#3398#,16#1000006B#,16#2113#);
   S3399 : aliased constant Code_Point_Array := (16#3399#,16#10000066#,16#6D#);
   S339A : aliased constant Code_Point_Array := (16#339A#,16#1000006E#,16#6D#);
   S339B : aliased constant Code_Point_Array := (16#339B#,16#100003BC#,16#6D#);
   S339C : aliased constant Code_Point_Array := (16#339C#,16#1000006D#,16#6D#);
   S339D : aliased constant Code_Point_Array := (16#339D#,16#10000063#,16#6D#);
   S339E : aliased constant Code_Point_Array := (16#339E#,16#1000006B#,16#6D#);
   S339F : aliased constant Code_Point_Array := (16#339F#,16#1000006D#,16#6D#,16#B2#);
   S33A0 : aliased constant Code_Point_Array := (16#33A0#,16#10000063#,16#6D#,16#B2#);
   S33A1 : aliased constant Code_Point_Array := (16#33A1#,16#1000006D#,16#B2#);
   S33A2 : aliased constant Code_Point_Array := (16#33A2#,16#1000006B#,16#6D#,16#B2#);
   S33A3 : aliased constant Code_Point_Array := (16#33A3#,16#1000006D#,16#6D#,16#B3#);
   S33A4 : aliased constant Code_Point_Array := (16#33A4#,16#10000063#,16#6D#,16#B3#);
   S33A5 : aliased constant Code_Point_Array := (16#33A5#,16#1000006D#,16#B3#);
   S33A6 : aliased constant Code_Point_Array := (16#33A6#,16#1000006B#,16#6D#,16#B3#);
   S33A7 : aliased constant Code_Point_Array := (16#33A7#,16#1000006D#,16#2215#,16#73#);
   S33A8 : aliased constant Code_Point_Array := (16#33A8#,16#1000006D#,16#2215#,16#73#,16#B2#);
   S33A9 : aliased constant Code_Point_Array := (16#33A9#,16#10000050#,16#61#);
   S33AA : aliased constant Code_Point_Array := (16#33AA#,16#1000006B#,16#50#,16#61#);
   S33AB : aliased constant Code_Point_Array := (16#33AB#,16#1000004D#,16#50#,16#61#);
   S33AC : aliased constant Code_Point_Array := (16#33AC#,16#10000047#,16#50#,16#61#);
   S33AD : aliased constant Code_Point_Array := (16#33AD#,16#10000072#,16#61#,16#64#);
   S33AE : aliased constant Code_Point_Array := (16#33AE#,16#10000072#,16#61#,16#64#,16#2215#,16#73#);
   S33AF : aliased constant Code_Point_Array := (16#33AF#,16#10000072#,16#61#,16#64#,16#2215#,16#73#,16#B2#);
   S33B0 : aliased constant Code_Point_Array := (16#33B0#,16#10000070#,16#73#);
   S33B1 : aliased constant Code_Point_Array := (16#33B1#,16#1000006E#,16#73#);
   S33B2 : aliased constant Code_Point_Array := (16#33B2#,16#100003BC#,16#73#);
   S33B3 : aliased constant Code_Point_Array := (16#33B3#,16#1000006D#,16#73#);
   S33B4 : aliased constant Code_Point_Array := (16#33B4#,16#10000070#,16#56#);
   S33B5 : aliased constant Code_Point_Array := (16#33B5#,16#1000006E#,16#56#);
   S33B6 : aliased constant Code_Point_Array := (16#33B6#,16#100003BC#,16#56#);
   S33B7 : aliased constant Code_Point_Array := (16#33B7#,16#1000006D#,16#56#);
   S33B8 : aliased constant Code_Point_Array := (16#33B8#,16#1000006B#,16#56#);
   S33B9 : aliased constant Code_Point_Array := (16#33B9#,16#1000004D#,16#56#);
   S33BA : aliased constant Code_Point_Array := (16#33BA#,16#10000070#,16#57#);
   S33BB : aliased constant Code_Point_Array := (16#33BB#,16#1000006E#,16#57#);
   S33BC : aliased constant Code_Point_Array := (16#33BC#,16#100003BC#,16#57#);
   S33BD : aliased constant Code_Point_Array := (16#33BD#,16#1000006D#,16#57#);
   S33BE : aliased constant Code_Point_Array := (16#33BE#,16#1000006B#,16#57#);
   S33BF : aliased constant Code_Point_Array := (16#33BF#,16#1000004D#,16#57#);
   S33C0 : aliased constant Code_Point_Array := (16#33C0#,16#1000006B#,16#3A9#);
   S33C1 : aliased constant Code_Point_Array := (16#33C1#,16#1000004D#,16#3A9#);
   S33C2 : aliased constant Code_Point_Array := (16#33C2#,16#10000061#,16#2E#,16#6D#,16#2E#);
   S33C3 : aliased constant Code_Point_Array := (16#33C3#,16#10000042#,16#71#);
   S33C4 : aliased constant Code_Point_Array := (16#33C4#,16#10000063#,16#63#);
   S33C5 : aliased constant Code_Point_Array := (16#33C5#,16#10000063#,16#64#);
   S33C6 : aliased constant Code_Point_Array := (16#33C6#,16#10000043#,16#2215#,16#6B#,16#67#);
   S33C7 : aliased constant Code_Point_Array := (16#33C7#,16#10000043#,16#6F#,16#2E#);
   S33C8 : aliased constant Code_Point_Array := (16#33C8#,16#10000064#,16#42#);
   S33C9 : aliased constant Code_Point_Array := (16#33C9#,16#10000047#,16#79#);
   S33CA : aliased constant Code_Point_Array := (16#33CA#,16#10000068#,16#61#);
   S33CB : aliased constant Code_Point_Array := (16#33CB#,16#10000048#,16#50#);
   S33CC : aliased constant Code_Point_Array := (16#33CC#,16#10000069#,16#6E#);
   S33CD : aliased constant Code_Point_Array := (16#33CD#,16#1000004B#,16#4B#);
   S33CE : aliased constant Code_Point_Array := (16#33CE#,16#1000004B#,16#4D#);
   S33CF : aliased constant Code_Point_Array := (16#33CF#,16#1000006B#,16#74#);
   S33D0 : aliased constant Code_Point_Array := (16#33D0#,16#1000006C#,16#6D#);
   S33D1 : aliased constant Code_Point_Array := (16#33D1#,16#1000006C#,16#6E#);
   S33D2 : aliased constant Code_Point_Array := (16#33D2#,16#1000006C#,16#6F#,16#67#);
   S33D3 : aliased constant Code_Point_Array := (16#33D3#,16#1000006C#,16#78#);
   S33D4 : aliased constant Code_Point_Array := (16#33D4#,16#1000006D#,16#62#);
   S33D5 : aliased constant Code_Point_Array := (16#33D5#,16#1000006D#,16#69#,16#6C#);
   S33D6 : aliased constant Code_Point_Array := (16#33D6#,16#1000006D#,16#6F#,16#6C#);
   S33D7 : aliased constant Code_Point_Array := (16#33D7#,16#10000050#,16#48#);
   S33D8 : aliased constant Code_Point_Array := (16#33D8#,16#10000070#,16#2E#,16#6D#,16#2E#);
   S33D9 : aliased constant Code_Point_Array := (16#33D9#,16#10000050#,16#50#,16#4D#);
   S33DA : aliased constant Code_Point_Array := (16#33DA#,16#10000050#,16#52#);
   S33DB : aliased constant Code_Point_Array := (16#33DB#,16#10000073#,16#72#);
   S33DC : aliased constant Code_Point_Array := (16#33DC#,16#10000053#,16#76#);
   S33DD : aliased constant Code_Point_Array := (16#33DD#,16#10000057#,16#62#);
   S33DE : aliased constant Code_Point_Array := (16#33DE#,16#10000056#,16#2215#,16#6D#);
   S33DF : aliased constant Code_Point_Array := (16#33DF#,16#10000041#,16#2215#,16#6D#);
   S33E0 : aliased constant Code_Point_Array := (16#33E0#,16#10000031#,16#65E5#);
   S33E1 : aliased constant Code_Point_Array := (16#33E1#,16#10000032#,16#65E5#);
   S33E2 : aliased constant Code_Point_Array := (16#33E2#,16#10000033#,16#65E5#);
   S33E3 : aliased constant Code_Point_Array := (16#33E3#,16#10000034#,16#65E5#);
   S33E4 : aliased constant Code_Point_Array := (16#33E4#,16#10000035#,16#65E5#);
   S33E5 : aliased constant Code_Point_Array := (16#33E5#,16#10000036#,16#65E5#);
   S33E6 : aliased constant Code_Point_Array := (16#33E6#,16#10000037#,16#65E5#);
   S33E7 : aliased constant Code_Point_Array := (16#33E7#,16#10000038#,16#65E5#);
   S33E8 : aliased constant Code_Point_Array := (16#33E8#,16#10000039#,16#65E5#);
   S33E9 : aliased constant Code_Point_Array := (16#33E9#,16#10000031#,16#30#,16#65E5#);
   S33EA : aliased constant Code_Point_Array := (16#33EA#,16#10000031#,16#31#,16#65E5#);
   S33EB : aliased constant Code_Point_Array := (16#33EB#,16#10000031#,16#32#,16#65E5#);
   S33EC : aliased constant Code_Point_Array := (16#33EC#,16#10000031#,16#33#,16#65E5#);
   S33ED : aliased constant Code_Point_Array := (16#33ED#,16#10000031#,16#34#,16#65E5#);
   S33EE : aliased constant Code_Point_Array := (16#33EE#,16#10000031#,16#35#,16#65E5#);
   S33EF : aliased constant Code_Point_Array := (16#33EF#,16#10000031#,16#36#,16#65E5#);
   S33F0 : aliased constant Code_Point_Array := (16#33F0#,16#10000031#,16#37#,16#65E5#);
   S33F1 : aliased constant Code_Point_Array := (16#33F1#,16#10000031#,16#38#,16#65E5#);
   S33F2 : aliased constant Code_Point_Array := (16#33F2#,16#10000031#,16#39#,16#65E5#);
   S33F3 : aliased constant Code_Point_Array := (16#33F3#,16#10000032#,16#30#,16#65E5#);
   S33F4 : aliased constant Code_Point_Array := (16#33F4#,16#10000032#,16#31#,16#65E5#);
   S33F5 : aliased constant Code_Point_Array := (16#33F5#,16#10000032#,16#32#,16#65E5#);
   S33F6 : aliased constant Code_Point_Array := (16#33F6#,16#10000032#,16#33#,16#65E5#);
   S33F7 : aliased constant Code_Point_Array := (16#33F7#,16#10000032#,16#34#,16#65E5#);
   S33F8 : aliased constant Code_Point_Array := (16#33F8#,16#10000032#,16#35#,16#65E5#);
   S33F9 : aliased constant Code_Point_Array := (16#33F9#,16#10000032#,16#36#,16#65E5#);
   S33FA : aliased constant Code_Point_Array := (16#33FA#,16#10000032#,16#37#,16#65E5#);
   S33FB : aliased constant Code_Point_Array := (16#33FB#,16#10000032#,16#38#,16#65E5#);
   S33FC : aliased constant Code_Point_Array := (16#33FC#,16#10000032#,16#39#,16#65E5#);
   S33FD : aliased constant Code_Point_Array := (16#33FD#,16#10000033#,16#30#,16#65E5#);
   S33FE : aliased constant Code_Point_Array := (16#33FE#,16#10000033#,16#31#,16#65E5#);
   S33FF : aliased constant Code_Point_Array := (16#33FF#,16#10000067#,16#61#,16#6C#);
   SA69C : aliased constant Code_Point_Array := (16#A69C#,16#1000044A#);
   SA69D : aliased constant Code_Point_Array := (16#A69D#,16#1000044C#);
   SA770 : aliased constant Code_Point_Array := (16#A770#,16#1000A76F#);
   SA7F2 : aliased constant Code_Point_Array := (16#A7F2#,16#10000043#);
   SA7F3 : aliased constant Code_Point_Array := (16#A7F3#,16#10000046#);
   SA7F4 : aliased constant Code_Point_Array := (16#A7F4#,16#10000051#);
   SA7F8 : aliased constant Code_Point_Array := (16#A7F8#,16#10000126#);
   SA7F9 : aliased constant Code_Point_Array := (16#A7F9#,16#10000153#);
   SAB5C : aliased constant Code_Point_Array := (16#AB5C#,16#1000A727#);
   SAB5D : aliased constant Code_Point_Array := (16#AB5D#,16#1000AB37#);
   SAB5E : aliased constant Code_Point_Array := (16#AB5E#,16#1000026B#);
   SAB5F : aliased constant Code_Point_Array := (16#AB5F#,16#1000AB52#);
   SAB69 : aliased constant Code_Point_Array := (16#AB69#,16#1000028D#);
   SF900 : aliased constant Code_Point_Array := (16#F900#,16#8C48#);
   SF901 : aliased constant Code_Point_Array := (16#F901#,16#66F4#);
   SF902 : aliased constant Code_Point_Array := (16#F902#,16#8ECA#);
   SF903 : aliased constant Code_Point_Array := (16#F903#,16#8CC8#);
   SF904 : aliased constant Code_Point_Array := (16#F904#,16#6ED1#);
   SF905 : aliased constant Code_Point_Array := (16#F905#,16#4E32#);
   SF906 : aliased constant Code_Point_Array := (16#F906#,16#53E5#);
   SF907 : aliased constant Code_Point_Array := (16#F907#,16#9F9C#);
   SF908 : aliased constant Code_Point_Array := (16#F908#,16#9F9C#);
   SF909 : aliased constant Code_Point_Array := (16#F909#,16#5951#);
   SF90A : aliased constant Code_Point_Array := (16#F90A#,16#91D1#);
   SF90B : aliased constant Code_Point_Array := (16#F90B#,16#5587#);
   SF90C : aliased constant Code_Point_Array := (16#F90C#,16#5948#);
   SF90D : aliased constant Code_Point_Array := (16#F90D#,16#61F6#);
   SF90E : aliased constant Code_Point_Array := (16#F90E#,16#7669#);
   SF90F : aliased constant Code_Point_Array := (16#F90F#,16#7F85#);
   SF910 : aliased constant Code_Point_Array := (16#F910#,16#863F#);
   SF911 : aliased constant Code_Point_Array := (16#F911#,16#87BA#);
   SF912 : aliased constant Code_Point_Array := (16#F912#,16#88F8#);
   SF913 : aliased constant Code_Point_Array := (16#F913#,16#908F#);
   SF914 : aliased constant Code_Point_Array := (16#F914#,16#6A02#);
   SF915 : aliased constant Code_Point_Array := (16#F915#,16#6D1B#);
   SF916 : aliased constant Code_Point_Array := (16#F916#,16#70D9#);
   SF917 : aliased constant Code_Point_Array := (16#F917#,16#73DE#);
   SF918 : aliased constant Code_Point_Array := (16#F918#,16#843D#);
   SF919 : aliased constant Code_Point_Array := (16#F919#,16#916A#);
   SF91A : aliased constant Code_Point_Array := (16#F91A#,16#99F1#);
   SF91B : aliased constant Code_Point_Array := (16#F91B#,16#4E82#);
   SF91C : aliased constant Code_Point_Array := (16#F91C#,16#5375#);
   SF91D : aliased constant Code_Point_Array := (16#F91D#,16#6B04#);
   SF91E : aliased constant Code_Point_Array := (16#F91E#,16#721B#);
   SF91F : aliased constant Code_Point_Array := (16#F91F#,16#862D#);
   SF920 : aliased constant Code_Point_Array := (16#F920#,16#9E1E#);
   SF921 : aliased constant Code_Point_Array := (16#F921#,16#5D50#);
   SF922 : aliased constant Code_Point_Array := (16#F922#,16#6FEB#);
   SF923 : aliased constant Code_Point_Array := (16#F923#,16#85CD#);
   SF924 : aliased constant Code_Point_Array := (16#F924#,16#8964#);
   SF925 : aliased constant Code_Point_Array := (16#F925#,16#62C9#);
   SF926 : aliased constant Code_Point_Array := (16#F926#,16#81D8#);
   SF927 : aliased constant Code_Point_Array := (16#F927#,16#881F#);
   SF928 : aliased constant Code_Point_Array := (16#F928#,16#5ECA#);
   SF929 : aliased constant Code_Point_Array := (16#F929#,16#6717#);
   SF92A : aliased constant Code_Point_Array := (16#F92A#,16#6D6A#);
   SF92B : aliased constant Code_Point_Array := (16#F92B#,16#72FC#);
   SF92C : aliased constant Code_Point_Array := (16#F92C#,16#90CE#);
   SF92D : aliased constant Code_Point_Array := (16#F92D#,16#4F86#);
   SF92E : aliased constant Code_Point_Array := (16#F92E#,16#51B7#);
   SF92F : aliased constant Code_Point_Array := (16#F92F#,16#52DE#);
   SF930 : aliased constant Code_Point_Array := (16#F930#,16#64C4#);
   SF931 : aliased constant Code_Point_Array := (16#F931#,16#6AD3#);
   SF932 : aliased constant Code_Point_Array := (16#F932#,16#7210#);
   SF933 : aliased constant Code_Point_Array := (16#F933#,16#76E7#);
   SF934 : aliased constant Code_Point_Array := (16#F934#,16#8001#);
   SF935 : aliased constant Code_Point_Array := (16#F935#,16#8606#);
   SF936 : aliased constant Code_Point_Array := (16#F936#,16#865C#);
   SF937 : aliased constant Code_Point_Array := (16#F937#,16#8DEF#);
   SF938 : aliased constant Code_Point_Array := (16#F938#,16#9732#);
   SF939 : aliased constant Code_Point_Array := (16#F939#,16#9B6F#);
   SF93A : aliased constant Code_Point_Array := (16#F93A#,16#9DFA#);
   SF93B : aliased constant Code_Point_Array := (16#F93B#,16#788C#);
   SF93C : aliased constant Code_Point_Array := (16#F93C#,16#797F#);
   SF93D : aliased constant Code_Point_Array := (16#F93D#,16#7DA0#);
   SF93E : aliased constant Code_Point_Array := (16#F93E#,16#83C9#);
   SF93F : aliased constant Code_Point_Array := (16#F93F#,16#9304#);
   SF940 : aliased constant Code_Point_Array := (16#F940#,16#9E7F#);
   SF941 : aliased constant Code_Point_Array := (16#F941#,16#8AD6#);
   SF942 : aliased constant Code_Point_Array := (16#F942#,16#58DF#);
   SF943 : aliased constant Code_Point_Array := (16#F943#,16#5F04#);
   SF944 : aliased constant Code_Point_Array := (16#F944#,16#7C60#);
   SF945 : aliased constant Code_Point_Array := (16#F945#,16#807E#);
   SF946 : aliased constant Code_Point_Array := (16#F946#,16#7262#);
   SF947 : aliased constant Code_Point_Array := (16#F947#,16#78CA#);
   SF948 : aliased constant Code_Point_Array := (16#F948#,16#8CC2#);
   SF949 : aliased constant Code_Point_Array := (16#F949#,16#96F7#);
   SF94A : aliased constant Code_Point_Array := (16#F94A#,16#58D8#);
   SF94B : aliased constant Code_Point_Array := (16#F94B#,16#5C62#);
   SF94C : aliased constant Code_Point_Array := (16#F94C#,16#6A13#);
   SF94D : aliased constant Code_Point_Array := (16#F94D#,16#6DDA#);
   SF94E : aliased constant Code_Point_Array := (16#F94E#,16#6F0F#);
   SF94F : aliased constant Code_Point_Array := (16#F94F#,16#7D2F#);
   SF950 : aliased constant Code_Point_Array := (16#F950#,16#7E37#);
   SF951 : aliased constant Code_Point_Array := (16#F951#,16#964B#);
   SF952 : aliased constant Code_Point_Array := (16#F952#,16#52D2#);
   SF953 : aliased constant Code_Point_Array := (16#F953#,16#808B#);
   SF954 : aliased constant Code_Point_Array := (16#F954#,16#51DC#);
   SF955 : aliased constant Code_Point_Array := (16#F955#,16#51CC#);
   SF956 : aliased constant Code_Point_Array := (16#F956#,16#7A1C#);
   SF957 : aliased constant Code_Point_Array := (16#F957#,16#7DBE#);
   SF958 : aliased constant Code_Point_Array := (16#F958#,16#83F1#);
   SF959 : aliased constant Code_Point_Array := (16#F959#,16#9675#);
   SF95A : aliased constant Code_Point_Array := (16#F95A#,16#8B80#);
   SF95B : aliased constant Code_Point_Array := (16#F95B#,16#62CF#);
   SF95C : aliased constant Code_Point_Array := (16#F95C#,16#6A02#);
   SF95D : aliased constant Code_Point_Array := (16#F95D#,16#8AFE#);
   SF95E : aliased constant Code_Point_Array := (16#F95E#,16#4E39#);
   SF95F : aliased constant Code_Point_Array := (16#F95F#,16#5BE7#);
   SF960 : aliased constant Code_Point_Array := (16#F960#,16#6012#);
   SF961 : aliased constant Code_Point_Array := (16#F961#,16#7387#);
   SF962 : aliased constant Code_Point_Array := (16#F962#,16#7570#);
   SF963 : aliased constant Code_Point_Array := (16#F963#,16#5317#);
   SF964 : aliased constant Code_Point_Array := (16#F964#,16#78FB#);
   SF965 : aliased constant Code_Point_Array := (16#F965#,16#4FBF#);
   SF966 : aliased constant Code_Point_Array := (16#F966#,16#5FA9#);
   SF967 : aliased constant Code_Point_Array := (16#F967#,16#4E0D#);
   SF968 : aliased constant Code_Point_Array := (16#F968#,16#6CCC#);
   SF969 : aliased constant Code_Point_Array := (16#F969#,16#6578#);
   SF96A : aliased constant Code_Point_Array := (16#F96A#,16#7D22#);
   SF96B : aliased constant Code_Point_Array := (16#F96B#,16#53C3#);
   SF96C : aliased constant Code_Point_Array := (16#F96C#,16#585E#);
   SF96D : aliased constant Code_Point_Array := (16#F96D#,16#7701#);
   SF96E : aliased constant Code_Point_Array := (16#F96E#,16#8449#);
   SF96F : aliased constant Code_Point_Array := (16#F96F#,16#8AAA#);
   SF970 : aliased constant Code_Point_Array := (16#F970#,16#6BBA#);
   SF971 : aliased constant Code_Point_Array := (16#F971#,16#8FB0#);
   SF972 : aliased constant Code_Point_Array := (16#F972#,16#6C88#);
   SF973 : aliased constant Code_Point_Array := (16#F973#,16#62FE#);
   SF974 : aliased constant Code_Point_Array := (16#F974#,16#82E5#);
   SF975 : aliased constant Code_Point_Array := (16#F975#,16#63A0#);
   SF976 : aliased constant Code_Point_Array := (16#F976#,16#7565#);
   SF977 : aliased constant Code_Point_Array := (16#F977#,16#4EAE#);
   SF978 : aliased constant Code_Point_Array := (16#F978#,16#5169#);
   SF979 : aliased constant Code_Point_Array := (16#F979#,16#51C9#);
   SF97A : aliased constant Code_Point_Array := (16#F97A#,16#6881#);
   SF97B : aliased constant Code_Point_Array := (16#F97B#,16#7CE7#);
   SF97C : aliased constant Code_Point_Array := (16#F97C#,16#826F#);
   SF97D : aliased constant Code_Point_Array := (16#F97D#,16#8AD2#);
   SF97E : aliased constant Code_Point_Array := (16#F97E#,16#91CF#);
   SF97F : aliased constant Code_Point_Array := (16#F97F#,16#52F5#);
   SF980 : aliased constant Code_Point_Array := (16#F980#,16#5442#);
   SF981 : aliased constant Code_Point_Array := (16#F981#,16#5973#);
   SF982 : aliased constant Code_Point_Array := (16#F982#,16#5EEC#);
   SF983 : aliased constant Code_Point_Array := (16#F983#,16#65C5#);
   SF984 : aliased constant Code_Point_Array := (16#F984#,16#6FFE#);
   SF985 : aliased constant Code_Point_Array := (16#F985#,16#792A#);
   SF986 : aliased constant Code_Point_Array := (16#F986#,16#95AD#);
   SF987 : aliased constant Code_Point_Array := (16#F987#,16#9A6A#);
   SF988 : aliased constant Code_Point_Array := (16#F988#,16#9E97#);
   SF989 : aliased constant Code_Point_Array := (16#F989#,16#9ECE#);
   SF98A : aliased constant Code_Point_Array := (16#F98A#,16#529B#);
   SF98B : aliased constant Code_Point_Array := (16#F98B#,16#66C6#);
   SF98C : aliased constant Code_Point_Array := (16#F98C#,16#6B77#);
   SF98D : aliased constant Code_Point_Array := (16#F98D#,16#8F62#);
   SF98E : aliased constant Code_Point_Array := (16#F98E#,16#5E74#);
   SF98F : aliased constant Code_Point_Array := (16#F98F#,16#6190#);
   SF990 : aliased constant Code_Point_Array := (16#F990#,16#6200#);
   SF991 : aliased constant Code_Point_Array := (16#F991#,16#649A#);
   SF992 : aliased constant Code_Point_Array := (16#F992#,16#6F23#);
   SF993 : aliased constant Code_Point_Array := (16#F993#,16#7149#);
   SF994 : aliased constant Code_Point_Array := (16#F994#,16#7489#);
   SF995 : aliased constant Code_Point_Array := (16#F995#,16#79CA#);
   SF996 : aliased constant Code_Point_Array := (16#F996#,16#7DF4#);
   SF997 : aliased constant Code_Point_Array := (16#F997#,16#806F#);
   SF998 : aliased constant Code_Point_Array := (16#F998#,16#8F26#);
   SF999 : aliased constant Code_Point_Array := (16#F999#,16#84EE#);
   SF99A : aliased constant Code_Point_Array := (16#F99A#,16#9023#);
   SF99B : aliased constant Code_Point_Array := (16#F99B#,16#934A#);
   SF99C : aliased constant Code_Point_Array := (16#F99C#,16#5217#);
   SF99D : aliased constant Code_Point_Array := (16#F99D#,16#52A3#);
   SF99E : aliased constant Code_Point_Array := (16#F99E#,16#54BD#);
   SF99F : aliased constant Code_Point_Array := (16#F99F#,16#70C8#);
   SF9A0 : aliased constant Code_Point_Array := (16#F9A0#,16#88C2#);
   SF9A1 : aliased constant Code_Point_Array := (16#F9A1#,16#8AAA#);
   SF9A2 : aliased constant Code_Point_Array := (16#F9A2#,16#5EC9#);
   SF9A3 : aliased constant Code_Point_Array := (16#F9A3#,16#5FF5#);
   SF9A4 : aliased constant Code_Point_Array := (16#F9A4#,16#637B#);
   SF9A5 : aliased constant Code_Point_Array := (16#F9A5#,16#6BAE#);
   SF9A6 : aliased constant Code_Point_Array := (16#F9A6#,16#7C3E#);
   SF9A7 : aliased constant Code_Point_Array := (16#F9A7#,16#7375#);
   SF9A8 : aliased constant Code_Point_Array := (16#F9A8#,16#4EE4#);
   SF9A9 : aliased constant Code_Point_Array := (16#F9A9#,16#56F9#);
   SF9AA : aliased constant Code_Point_Array := (16#F9AA#,16#5BE7#);
   SF9AB : aliased constant Code_Point_Array := (16#F9AB#,16#5DBA#);
   SF9AC : aliased constant Code_Point_Array := (16#F9AC#,16#601C#);
   SF9AD : aliased constant Code_Point_Array := (16#F9AD#,16#73B2#);
   SF9AE : aliased constant Code_Point_Array := (16#F9AE#,16#7469#);
   SF9AF : aliased constant Code_Point_Array := (16#F9AF#,16#7F9A#);
   SF9B0 : aliased constant Code_Point_Array := (16#F9B0#,16#8046#);
   SF9B1 : aliased constant Code_Point_Array := (16#F9B1#,16#9234#);
   SF9B2 : aliased constant Code_Point_Array := (16#F9B2#,16#96F6#);
   SF9B3 : aliased constant Code_Point_Array := (16#F9B3#,16#9748#);
   SF9B4 : aliased constant Code_Point_Array := (16#F9B4#,16#9818#);
   SF9B5 : aliased constant Code_Point_Array := (16#F9B5#,16#4F8B#);
   SF9B6 : aliased constant Code_Point_Array := (16#F9B6#,16#79AE#);
   SF9B7 : aliased constant Code_Point_Array := (16#F9B7#,16#91B4#);
   SF9B8 : aliased constant Code_Point_Array := (16#F9B8#,16#96B8#);
   SF9B9 : aliased constant Code_Point_Array := (16#F9B9#,16#60E1#);
   SF9BA : aliased constant Code_Point_Array := (16#F9BA#,16#4E86#);
   SF9BB : aliased constant Code_Point_Array := (16#F9BB#,16#50DA#);
   SF9BC : aliased constant Code_Point_Array := (16#F9BC#,16#5BEE#);
   SF9BD : aliased constant Code_Point_Array := (16#F9BD#,16#5C3F#);
   SF9BE : aliased constant Code_Point_Array := (16#F9BE#,16#6599#);
   SF9BF : aliased constant Code_Point_Array := (16#F9BF#,16#6A02#);
   SF9C0 : aliased constant Code_Point_Array := (16#F9C0#,16#71CE#);
   SF9C1 : aliased constant Code_Point_Array := (16#F9C1#,16#7642#);
   SF9C2 : aliased constant Code_Point_Array := (16#F9C2#,16#84FC#);
   SF9C3 : aliased constant Code_Point_Array := (16#F9C3#,16#907C#);
   SF9C4 : aliased constant Code_Point_Array := (16#F9C4#,16#9F8D#);
   SF9C5 : aliased constant Code_Point_Array := (16#F9C5#,16#6688#);
   SF9C6 : aliased constant Code_Point_Array := (16#F9C6#,16#962E#);
   SF9C7 : aliased constant Code_Point_Array := (16#F9C7#,16#5289#);
   SF9C8 : aliased constant Code_Point_Array := (16#F9C8#,16#677B#);
   SF9C9 : aliased constant Code_Point_Array := (16#F9C9#,16#67F3#);
   SF9CA : aliased constant Code_Point_Array := (16#F9CA#,16#6D41#);
   SF9CB : aliased constant Code_Point_Array := (16#F9CB#,16#6E9C#);
   SF9CC : aliased constant Code_Point_Array := (16#F9CC#,16#7409#);
   SF9CD : aliased constant Code_Point_Array := (16#F9CD#,16#7559#);
   SF9CE : aliased constant Code_Point_Array := (16#F9CE#,16#786B#);
   SF9CF : aliased constant Code_Point_Array := (16#F9CF#,16#7D10#);
   SF9D0 : aliased constant Code_Point_Array := (16#F9D0#,16#985E#);
   SF9D1 : aliased constant Code_Point_Array := (16#F9D1#,16#516D#);
   SF9D2 : aliased constant Code_Point_Array := (16#F9D2#,16#622E#);
   SF9D3 : aliased constant Code_Point_Array := (16#F9D3#,16#9678#);
   SF9D4 : aliased constant Code_Point_Array := (16#F9D4#,16#502B#);
   SF9D5 : aliased constant Code_Point_Array := (16#F9D5#,16#5D19#);
   SF9D6 : aliased constant Code_Point_Array := (16#F9D6#,16#6DEA#);
   SF9D7 : aliased constant Code_Point_Array := (16#F9D7#,16#8F2A#);
   SF9D8 : aliased constant Code_Point_Array := (16#F9D8#,16#5F8B#);
   SF9D9 : aliased constant Code_Point_Array := (16#F9D9#,16#6144#);
   SF9DA : aliased constant Code_Point_Array := (16#F9DA#,16#6817#);
   SF9DB : aliased constant Code_Point_Array := (16#F9DB#,16#7387#);
   SF9DC : aliased constant Code_Point_Array := (16#F9DC#,16#9686#);
   SF9DD : aliased constant Code_Point_Array := (16#F9DD#,16#5229#);
   SF9DE : aliased constant Code_Point_Array := (16#F9DE#,16#540F#);
   SF9DF : aliased constant Code_Point_Array := (16#F9DF#,16#5C65#);
   SF9E0 : aliased constant Code_Point_Array := (16#F9E0#,16#6613#);
   SF9E1 : aliased constant Code_Point_Array := (16#F9E1#,16#674E#);
   SF9E2 : aliased constant Code_Point_Array := (16#F9E2#,16#68A8#);
   SF9E3 : aliased constant Code_Point_Array := (16#F9E3#,16#6CE5#);
   SF9E4 : aliased constant Code_Point_Array := (16#F9E4#,16#7406#);
   SF9E5 : aliased constant Code_Point_Array := (16#F9E5#,16#75E2#);
   SF9E6 : aliased constant Code_Point_Array := (16#F9E6#,16#7F79#);
   SF9E7 : aliased constant Code_Point_Array := (16#F9E7#,16#88CF#);
   SF9E8 : aliased constant Code_Point_Array := (16#F9E8#,16#88E1#);
   SF9E9 : aliased constant Code_Point_Array := (16#F9E9#,16#91CC#);
   SF9EA : aliased constant Code_Point_Array := (16#F9EA#,16#96E2#);
   SF9EB : aliased constant Code_Point_Array := (16#F9EB#,16#533F#);
   SF9EC : aliased constant Code_Point_Array := (16#F9EC#,16#6EBA#);
   SF9ED : aliased constant Code_Point_Array := (16#F9ED#,16#541D#);
   SF9EE : aliased constant Code_Point_Array := (16#F9EE#,16#71D0#);
   SF9EF : aliased constant Code_Point_Array := (16#F9EF#,16#7498#);
   SF9F0 : aliased constant Code_Point_Array := (16#F9F0#,16#85FA#);
   SF9F1 : aliased constant Code_Point_Array := (16#F9F1#,16#96A3#);
   SF9F2 : aliased constant Code_Point_Array := (16#F9F2#,16#9C57#);
   SF9F3 : aliased constant Code_Point_Array := (16#F9F3#,16#9E9F#);
   SF9F4 : aliased constant Code_Point_Array := (16#F9F4#,16#6797#);
   SF9F5 : aliased constant Code_Point_Array := (16#F9F5#,16#6DCB#);
   SF9F6 : aliased constant Code_Point_Array := (16#F9F6#,16#81E8#);
   SF9F7 : aliased constant Code_Point_Array := (16#F9F7#,16#7ACB#);
   SF9F8 : aliased constant Code_Point_Array := (16#F9F8#,16#7B20#);
   SF9F9 : aliased constant Code_Point_Array := (16#F9F9#,16#7C92#);
   SF9FA : aliased constant Code_Point_Array := (16#F9FA#,16#72C0#);
   SF9FB : aliased constant Code_Point_Array := (16#F9FB#,16#7099#);
   SF9FC : aliased constant Code_Point_Array := (16#F9FC#,16#8B58#);
   SF9FD : aliased constant Code_Point_Array := (16#F9FD#,16#4EC0#);
   SF9FE : aliased constant Code_Point_Array := (16#F9FE#,16#8336#);
   SF9FF : aliased constant Code_Point_Array := (16#F9FF#,16#523A#);
   SFA00 : aliased constant Code_Point_Array := (16#FA00#,16#5207#);
   SFA01 : aliased constant Code_Point_Array := (16#FA01#,16#5EA6#);
   SFA02 : aliased constant Code_Point_Array := (16#FA02#,16#62D3#);
   SFA03 : aliased constant Code_Point_Array := (16#FA03#,16#7CD6#);
   SFA04 : aliased constant Code_Point_Array := (16#FA04#,16#5B85#);
   SFA05 : aliased constant Code_Point_Array := (16#FA05#,16#6D1E#);
   SFA06 : aliased constant Code_Point_Array := (16#FA06#,16#66B4#);
   SFA07 : aliased constant Code_Point_Array := (16#FA07#,16#8F3B#);
   SFA08 : aliased constant Code_Point_Array := (16#FA08#,16#884C#);
   SFA09 : aliased constant Code_Point_Array := (16#FA09#,16#964D#);
   SFA0A : aliased constant Code_Point_Array := (16#FA0A#,16#898B#);
   SFA0B : aliased constant Code_Point_Array := (16#FA0B#,16#5ED3#);
   SFA0C : aliased constant Code_Point_Array := (16#FA0C#,16#5140#);
   SFA0D : aliased constant Code_Point_Array := (16#FA0D#,16#55C0#);
   SFA10 : aliased constant Code_Point_Array := (16#FA10#,16#585A#);
   SFA12 : aliased constant Code_Point_Array := (16#FA12#,16#6674#);
   SFA15 : aliased constant Code_Point_Array := (16#FA15#,16#51DE#);
   SFA16 : aliased constant Code_Point_Array := (16#FA16#,16#732A#);
   SFA17 : aliased constant Code_Point_Array := (16#FA17#,16#76CA#);
   SFA18 : aliased constant Code_Point_Array := (16#FA18#,16#793C#);
   SFA19 : aliased constant Code_Point_Array := (16#FA19#,16#795E#);
   SFA1A : aliased constant Code_Point_Array := (16#FA1A#,16#7965#);
   SFA1B : aliased constant Code_Point_Array := (16#FA1B#,16#798F#);
   SFA1C : aliased constant Code_Point_Array := (16#FA1C#,16#9756#);
   SFA1D : aliased constant Code_Point_Array := (16#FA1D#,16#7CBE#);
   SFA1E : aliased constant Code_Point_Array := (16#FA1E#,16#7FBD#);
   SFA20 : aliased constant Code_Point_Array := (16#FA20#,16#8612#);
   SFA22 : aliased constant Code_Point_Array := (16#FA22#,16#8AF8#);
   SFA25 : aliased constant Code_Point_Array := (16#FA25#,16#9038#);
   SFA26 : aliased constant Code_Point_Array := (16#FA26#,16#90FD#);
   SFA2A : aliased constant Code_Point_Array := (16#FA2A#,16#98EF#);
   SFA2B : aliased constant Code_Point_Array := (16#FA2B#,16#98FC#);
   SFA2C : aliased constant Code_Point_Array := (16#FA2C#,16#9928#);
   SFA2D : aliased constant Code_Point_Array := (16#FA2D#,16#9DB4#);
   SFA2E : aliased constant Code_Point_Array := (16#FA2E#,16#90DE#);
   SFA2F : aliased constant Code_Point_Array := (16#FA2F#,16#96B7#);
   SFA30 : aliased constant Code_Point_Array := (16#FA30#,16#4FAE#);
   SFA31 : aliased constant Code_Point_Array := (16#FA31#,16#50E7#);
   SFA32 : aliased constant Code_Point_Array := (16#FA32#,16#514D#);
   SFA33 : aliased constant Code_Point_Array := (16#FA33#,16#52C9#);
   SFA34 : aliased constant Code_Point_Array := (16#FA34#,16#52E4#);
   SFA35 : aliased constant Code_Point_Array := (16#FA35#,16#5351#);
   SFA36 : aliased constant Code_Point_Array := (16#FA36#,16#559D#);
   SFA37 : aliased constant Code_Point_Array := (16#FA37#,16#5606#);
   SFA38 : aliased constant Code_Point_Array := (16#FA38#,16#5668#);
   SFA39 : aliased constant Code_Point_Array := (16#FA39#,16#5840#);
   SFA3A : aliased constant Code_Point_Array := (16#FA3A#,16#58A8#);
   SFA3B : aliased constant Code_Point_Array := (16#FA3B#,16#5C64#);
   SFA3C : aliased constant Code_Point_Array := (16#FA3C#,16#5C6E#);
   SFA3D : aliased constant Code_Point_Array := (16#FA3D#,16#6094#);
   SFA3E : aliased constant Code_Point_Array := (16#FA3E#,16#6168#);
   SFA3F : aliased constant Code_Point_Array := (16#FA3F#,16#618E#);
   SFA40 : aliased constant Code_Point_Array := (16#FA40#,16#61F2#);
   SFA41 : aliased constant Code_Point_Array := (16#FA41#,16#654F#);
   SFA42 : aliased constant Code_Point_Array := (16#FA42#,16#65E2#);
   SFA43 : aliased constant Code_Point_Array := (16#FA43#,16#6691#);
   SFA44 : aliased constant Code_Point_Array := (16#FA44#,16#6885#);
   SFA45 : aliased constant Code_Point_Array := (16#FA45#,16#6D77#);
   SFA46 : aliased constant Code_Point_Array := (16#FA46#,16#6E1A#);
   SFA47 : aliased constant Code_Point_Array := (16#FA47#,16#6F22#);
   SFA48 : aliased constant Code_Point_Array := (16#FA48#,16#716E#);
   SFA49 : aliased constant Code_Point_Array := (16#FA49#,16#722B#);
   SFA4A : aliased constant Code_Point_Array := (16#FA4A#,16#7422#);
   SFA4B : aliased constant Code_Point_Array := (16#FA4B#,16#7891#);
   SFA4C : aliased constant Code_Point_Array := (16#FA4C#,16#793E#);
   SFA4D : aliased constant Code_Point_Array := (16#FA4D#,16#7949#);
   SFA4E : aliased constant Code_Point_Array := (16#FA4E#,16#7948#);
   SFA4F : aliased constant Code_Point_Array := (16#FA4F#,16#7950#);
   SFA50 : aliased constant Code_Point_Array := (16#FA50#,16#7956#);
   SFA51 : aliased constant Code_Point_Array := (16#FA51#,16#795D#);
   SFA52 : aliased constant Code_Point_Array := (16#FA52#,16#798D#);
   SFA53 : aliased constant Code_Point_Array := (16#FA53#,16#798E#);
   SFA54 : aliased constant Code_Point_Array := (16#FA54#,16#7A40#);
   SFA55 : aliased constant Code_Point_Array := (16#FA55#,16#7A81#);
   SFA56 : aliased constant Code_Point_Array := (16#FA56#,16#7BC0#);
   SFA57 : aliased constant Code_Point_Array := (16#FA57#,16#7DF4#);
   SFA58 : aliased constant Code_Point_Array := (16#FA58#,16#7E09#);
   SFA59 : aliased constant Code_Point_Array := (16#FA59#,16#7E41#);
   SFA5A : aliased constant Code_Point_Array := (16#FA5A#,16#7F72#);
   SFA5B : aliased constant Code_Point_Array := (16#FA5B#,16#8005#);
   SFA5C : aliased constant Code_Point_Array := (16#FA5C#,16#81ED#);
   SFA5D : aliased constant Code_Point_Array := (16#FA5D#,16#8279#);
   SFA5E : aliased constant Code_Point_Array := (16#FA5E#,16#8279#);
   SFA5F : aliased constant Code_Point_Array := (16#FA5F#,16#8457#);
   SFA60 : aliased constant Code_Point_Array := (16#FA60#,16#8910#);
   SFA61 : aliased constant Code_Point_Array := (16#FA61#,16#8996#);
   SFA62 : aliased constant Code_Point_Array := (16#FA62#,16#8B01#);
   SFA63 : aliased constant Code_Point_Array := (16#FA63#,16#8B39#);
   SFA64 : aliased constant Code_Point_Array := (16#FA64#,16#8CD3#);
   SFA65 : aliased constant Code_Point_Array := (16#FA65#,16#8D08#);
   SFA66 : aliased constant Code_Point_Array := (16#FA66#,16#8FB6#);
   SFA67 : aliased constant Code_Point_Array := (16#FA67#,16#9038#);
   SFA68 : aliased constant Code_Point_Array := (16#FA68#,16#96E3#);
   SFA69 : aliased constant Code_Point_Array := (16#FA69#,16#97FF#);
   SFA6A : aliased constant Code_Point_Array := (16#FA6A#,16#983B#);
   SFA6B : aliased constant Code_Point_Array := (16#FA6B#,16#6075#);
   SFA6C : aliased constant Code_Point_Array := (16#FA6C#,16#242EE#);
   SFA6D : aliased constant Code_Point_Array := (16#FA6D#,16#8218#);
   SFA70 : aliased constant Code_Point_Array := (16#FA70#,16#4E26#);
   SFA71 : aliased constant Code_Point_Array := (16#FA71#,16#51B5#);
   SFA72 : aliased constant Code_Point_Array := (16#FA72#,16#5168#);
   SFA73 : aliased constant Code_Point_Array := (16#FA73#,16#4F80#);
   SFA74 : aliased constant Code_Point_Array := (16#FA74#,16#5145#);
   SFA75 : aliased constant Code_Point_Array := (16#FA75#,16#5180#);
   SFA76 : aliased constant Code_Point_Array := (16#FA76#,16#52C7#);
   SFA77 : aliased constant Code_Point_Array := (16#FA77#,16#52FA#);
   SFA78 : aliased constant Code_Point_Array := (16#FA78#,16#559D#);
   SFA79 : aliased constant Code_Point_Array := (16#FA79#,16#5555#);
   SFA7A : aliased constant Code_Point_Array := (16#FA7A#,16#5599#);
   SFA7B : aliased constant Code_Point_Array := (16#FA7B#,16#55E2#);
   SFA7C : aliased constant Code_Point_Array := (16#FA7C#,16#585A#);
   SFA7D : aliased constant Code_Point_Array := (16#FA7D#,16#58B3#);
   SFA7E : aliased constant Code_Point_Array := (16#FA7E#,16#5944#);
   SFA7F : aliased constant Code_Point_Array := (16#FA7F#,16#5954#);
   SFA80 : aliased constant Code_Point_Array := (16#FA80#,16#5A62#);
   SFA81 : aliased constant Code_Point_Array := (16#FA81#,16#5B28#);
   SFA82 : aliased constant Code_Point_Array := (16#FA82#,16#5ED2#);
   SFA83 : aliased constant Code_Point_Array := (16#FA83#,16#5ED9#);
   SFA84 : aliased constant Code_Point_Array := (16#FA84#,16#5F69#);
   SFA85 : aliased constant Code_Point_Array := (16#FA85#,16#5FAD#);
   SFA86 : aliased constant Code_Point_Array := (16#FA86#,16#60D8#);
   SFA87 : aliased constant Code_Point_Array := (16#FA87#,16#614E#);
   SFA88 : aliased constant Code_Point_Array := (16#FA88#,16#6108#);
   SFA89 : aliased constant Code_Point_Array := (16#FA89#,16#618E#);
   SFA8A : aliased constant Code_Point_Array := (16#FA8A#,16#6160#);
   SFA8B : aliased constant Code_Point_Array := (16#FA8B#,16#61F2#);
   SFA8C : aliased constant Code_Point_Array := (16#FA8C#,16#6234#);
   SFA8D : aliased constant Code_Point_Array := (16#FA8D#,16#63C4#);
   SFA8E : aliased constant Code_Point_Array := (16#FA8E#,16#641C#);
   SFA8F : aliased constant Code_Point_Array := (16#FA8F#,16#6452#);
   SFA90 : aliased constant Code_Point_Array := (16#FA90#,16#6556#);
   SFA91 : aliased constant Code_Point_Array := (16#FA91#,16#6674#);
   SFA92 : aliased constant Code_Point_Array := (16#FA92#,16#6717#);
   SFA93 : aliased constant Code_Point_Array := (16#FA93#,16#671B#);
   SFA94 : aliased constant Code_Point_Array := (16#FA94#,16#6756#);
   SFA95 : aliased constant Code_Point_Array := (16#FA95#,16#6B79#);
   SFA96 : aliased constant Code_Point_Array := (16#FA96#,16#6BBA#);
   SFA97 : aliased constant Code_Point_Array := (16#FA97#,16#6D41#);
   SFA98 : aliased constant Code_Point_Array := (16#FA98#,16#6EDB#);
   SFA99 : aliased constant Code_Point_Array := (16#FA99#,16#6ECB#);
   SFA9A : aliased constant Code_Point_Array := (16#FA9A#,16#6F22#);
   SFA9B : aliased constant Code_Point_Array := (16#FA9B#,16#701E#);
   SFA9C : aliased constant Code_Point_Array := (16#FA9C#,16#716E#);
   SFA9D : aliased constant Code_Point_Array := (16#FA9D#,16#77A7#);
   SFA9E : aliased constant Code_Point_Array := (16#FA9E#,16#7235#);
   SFA9F : aliased constant Code_Point_Array := (16#FA9F#,16#72AF#);
   SFAA0 : aliased constant Code_Point_Array := (16#FAA0#,16#732A#);
   SFAA1 : aliased constant Code_Point_Array := (16#FAA1#,16#7471#);
   SFAA2 : aliased constant Code_Point_Array := (16#FAA2#,16#7506#);
   SFAA3 : aliased constant Code_Point_Array := (16#FAA3#,16#753B#);
   SFAA4 : aliased constant Code_Point_Array := (16#FAA4#,16#761D#);
   SFAA5 : aliased constant Code_Point_Array := (16#FAA5#,16#761F#);
   SFAA6 : aliased constant Code_Point_Array := (16#FAA6#,16#76CA#);
   SFAA7 : aliased constant Code_Point_Array := (16#FAA7#,16#76DB#);
   SFAA8 : aliased constant Code_Point_Array := (16#FAA8#,16#76F4#);
   SFAA9 : aliased constant Code_Point_Array := (16#FAA9#,16#774A#);
   SFAAA : aliased constant Code_Point_Array := (16#FAAA#,16#7740#);
   SFAAB : aliased constant Code_Point_Array := (16#FAAB#,16#78CC#);
   SFAAC : aliased constant Code_Point_Array := (16#FAAC#,16#7AB1#);
   SFAAD : aliased constant Code_Point_Array := (16#FAAD#,16#7BC0#);
   SFAAE : aliased constant Code_Point_Array := (16#FAAE#,16#7C7B#);
   SFAAF : aliased constant Code_Point_Array := (16#FAAF#,16#7D5B#);
   SFAB0 : aliased constant Code_Point_Array := (16#FAB0#,16#7DF4#);
   SFAB1 : aliased constant Code_Point_Array := (16#FAB1#,16#7F3E#);
   SFAB2 : aliased constant Code_Point_Array := (16#FAB2#,16#8005#);
   SFAB3 : aliased constant Code_Point_Array := (16#FAB3#,16#8352#);
   SFAB4 : aliased constant Code_Point_Array := (16#FAB4#,16#83EF#);
   SFAB5 : aliased constant Code_Point_Array := (16#FAB5#,16#8779#);
   SFAB6 : aliased constant Code_Point_Array := (16#FAB6#,16#8941#);
   SFAB7 : aliased constant Code_Point_Array := (16#FAB7#,16#8986#);
   SFAB8 : aliased constant Code_Point_Array := (16#FAB8#,16#8996#);
   SFAB9 : aliased constant Code_Point_Array := (16#FAB9#,16#8ABF#);
   SFABA : aliased constant Code_Point_Array := (16#FABA#,16#8AF8#);
   SFABB : aliased constant Code_Point_Array := (16#FABB#,16#8ACB#);
   SFABC : aliased constant Code_Point_Array := (16#FABC#,16#8B01#);
   SFABD : aliased constant Code_Point_Array := (16#FABD#,16#8AFE#);
   SFABE : aliased constant Code_Point_Array := (16#FABE#,16#8AED#);
   SFABF : aliased constant Code_Point_Array := (16#FABF#,16#8B39#);
   SFAC0 : aliased constant Code_Point_Array := (16#FAC0#,16#8B8A#);
   SFAC1 : aliased constant Code_Point_Array := (16#FAC1#,16#8D08#);
   SFAC2 : aliased constant Code_Point_Array := (16#FAC2#,16#8F38#);
   SFAC3 : aliased constant Code_Point_Array := (16#FAC3#,16#9072#);
   SFAC4 : aliased constant Code_Point_Array := (16#FAC4#,16#9199#);
   SFAC5 : aliased constant Code_Point_Array := (16#FAC5#,16#9276#);
   SFAC6 : aliased constant Code_Point_Array := (16#FAC6#,16#967C#);
   SFAC7 : aliased constant Code_Point_Array := (16#FAC7#,16#96E3#);
   SFAC8 : aliased constant Code_Point_Array := (16#FAC8#,16#9756#);
   SFAC9 : aliased constant Code_Point_Array := (16#FAC9#,16#97DB#);
   SFACA : aliased constant Code_Point_Array := (16#FACA#,16#97FF#);
   SFACB : aliased constant Code_Point_Array := (16#FACB#,16#980B#);
   SFACC : aliased constant Code_Point_Array := (16#FACC#,16#983B#);
   SFACD : aliased constant Code_Point_Array := (16#FACD#,16#9B12#);
   SFACE : aliased constant Code_Point_Array := (16#FACE#,16#9F9C#);
   SFACF : aliased constant Code_Point_Array := (16#FACF#,16#2284A#);
   SFAD0 : aliased constant Code_Point_Array := (16#FAD0#,16#22844#);
   SFAD1 : aliased constant Code_Point_Array := (16#FAD1#,16#233D5#);
   SFAD2 : aliased constant Code_Point_Array := (16#FAD2#,16#3B9D#);
   SFAD3 : aliased constant Code_Point_Array := (16#FAD3#,16#4018#);
   SFAD4 : aliased constant Code_Point_Array := (16#FAD4#,16#4039#);
   SFAD5 : aliased constant Code_Point_Array := (16#FAD5#,16#25249#);
   SFAD6 : aliased constant Code_Point_Array := (16#FAD6#,16#25CD0#);
   SFAD7 : aliased constant Code_Point_Array := (16#FAD7#,16#27ED3#);
   SFAD8 : aliased constant Code_Point_Array := (16#FAD8#,16#9F43#);
   SFAD9 : aliased constant Code_Point_Array := (16#FAD9#,16#9F8E#);
   SFB00 : aliased constant Code_Point_Array := (16#FB00#,16#10000066#,16#66#);
   SFB01 : aliased constant Code_Point_Array := (16#FB01#,16#10000066#,16#69#);
   SFB02 : aliased constant Code_Point_Array := (16#FB02#,16#10000066#,16#6C#);
   SFB03 : aliased constant Code_Point_Array := (16#FB03#,16#10000066#,16#66#,16#69#);
   SFB04 : aliased constant Code_Point_Array := (16#FB04#,16#10000066#,16#66#,16#6C#);
   SFB05 : aliased constant Code_Point_Array := (16#FB05#,16#1000017F#,16#74#);
   SFB06 : aliased constant Code_Point_Array := (16#FB06#,16#10000073#,16#74#);
   SFB13 : aliased constant Code_Point_Array := (16#FB13#,16#10000574#,16#576#);
   SFB14 : aliased constant Code_Point_Array := (16#FB14#,16#10000574#,16#565#);
   SFB15 : aliased constant Code_Point_Array := (16#FB15#,16#10000574#,16#56B#);
   SFB16 : aliased constant Code_Point_Array := (16#FB16#,16#1000057E#,16#576#);
   SFB17 : aliased constant Code_Point_Array := (16#FB17#,16#10000574#,16#56D#);
   SFB1D : aliased constant Code_Point_Array := (16#FB1D#,16#5D9#,16#5B4#);
   SFB1F : aliased constant Code_Point_Array := (16#FB1F#,16#5F2#,16#5B7#);
   SFB20 : aliased constant Code_Point_Array := (16#FB20#,16#100005E2#);
   SFB21 : aliased constant Code_Point_Array := (16#FB21#,16#100005D0#);
   SFB22 : aliased constant Code_Point_Array := (16#FB22#,16#100005D3#);
   SFB23 : aliased constant Code_Point_Array := (16#FB23#,16#100005D4#);
   SFB24 : aliased constant Code_Point_Array := (16#FB24#,16#100005DB#);
   SFB25 : aliased constant Code_Point_Array := (16#FB25#,16#100005DC#);
   SFB26 : aliased constant Code_Point_Array := (16#FB26#,16#100005DD#);
   SFB27 : aliased constant Code_Point_Array := (16#FB27#,16#100005E8#);
   SFB28 : aliased constant Code_Point_Array := (16#FB28#,16#100005EA#);
   SFB29 : aliased constant Code_Point_Array := (16#FB29#,16#1000002B#);
   SFB2A : aliased constant Code_Point_Array := (16#FB2A#,16#5E9#,16#5C1#);
   SFB2B : aliased constant Code_Point_Array := (16#FB2B#,16#5E9#,16#5C2#);
   SFB2C : aliased constant Code_Point_Array := (16#FB2C#,16#FB49#,16#5C1#);
   SFB2D : aliased constant Code_Point_Array := (16#FB2D#,16#FB49#,16#5C2#);
   SFB2E : aliased constant Code_Point_Array := (16#FB2E#,16#5D0#,16#5B7#);
   SFB2F : aliased constant Code_Point_Array := (16#FB2F#,16#5D0#,16#5B8#);
   SFB30 : aliased constant Code_Point_Array := (16#FB30#,16#5D0#,16#5BC#);
   SFB31 : aliased constant Code_Point_Array := (16#FB31#,16#5D1#,16#5BC#);
   SFB32 : aliased constant Code_Point_Array := (16#FB32#,16#5D2#,16#5BC#);
   SFB33 : aliased constant Code_Point_Array := (16#FB33#,16#5D3#,16#5BC#);
   SFB34 : aliased constant Code_Point_Array := (16#FB34#,16#5D4#,16#5BC#);
   SFB35 : aliased constant Code_Point_Array := (16#FB35#,16#5D5#,16#5BC#);
   SFB36 : aliased constant Code_Point_Array := (16#FB36#,16#5D6#,16#5BC#);
   SFB38 : aliased constant Code_Point_Array := (16#FB38#,16#5D8#,16#5BC#);
   SFB39 : aliased constant Code_Point_Array := (16#FB39#,16#5D9#,16#5BC#);
   SFB3A : aliased constant Code_Point_Array := (16#FB3A#,16#5DA#,16#5BC#);
   SFB3B : aliased constant Code_Point_Array := (16#FB3B#,16#5DB#,16#5BC#);
   SFB3C : aliased constant Code_Point_Array := (16#FB3C#,16#5DC#,16#5BC#);
   SFB3E : aliased constant Code_Point_Array := (16#FB3E#,16#5DE#,16#5BC#);
   SFB40 : aliased constant Code_Point_Array := (16#FB40#,16#5E0#,16#5BC#);
   SFB41 : aliased constant Code_Point_Array := (16#FB41#,16#5E1#,16#5BC#);
   SFB43 : aliased constant Code_Point_Array := (16#FB43#,16#5E3#,16#5BC#);
   SFB44 : aliased constant Code_Point_Array := (16#FB44#,16#5E4#,16#5BC#);
   SFB46 : aliased constant Code_Point_Array := (16#FB46#,16#5E6#,16#5BC#);
   SFB47 : aliased constant Code_Point_Array := (16#FB47#,16#5E7#,16#5BC#);
   SFB48 : aliased constant Code_Point_Array := (16#FB48#,16#5E8#,16#5BC#);
   SFB49 : aliased constant Code_Point_Array := (16#FB49#,16#5E9#,16#5BC#);
   SFB4A : aliased constant Code_Point_Array := (16#FB4A#,16#5EA#,16#5BC#);
   SFB4B : aliased constant Code_Point_Array := (16#FB4B#,16#5D5#,16#5B9#);
   SFB4C : aliased constant Code_Point_Array := (16#FB4C#,16#5D1#,16#5BF#);
   SFB4D : aliased constant Code_Point_Array := (16#FB4D#,16#5DB#,16#5BF#);
   SFB4E : aliased constant Code_Point_Array := (16#FB4E#,16#5E4#,16#5BF#);
   SFB4F : aliased constant Code_Point_Array := (16#FB4F#,16#100005D0#,16#5DC#);
   SFB50 : aliased constant Code_Point_Array := (16#FB50#,16#10000671#);
   SFB51 : aliased constant Code_Point_Array := (16#FB51#,16#40000671#);
   SFB52 : aliased constant Code_Point_Array := (16#FB52#,16#1000067B#);
   SFB53 : aliased constant Code_Point_Array := (16#FB53#,16#4000067B#);
   SFB54 : aliased constant Code_Point_Array := (16#FB54#,16#2000067B#);
   SFB55 : aliased constant Code_Point_Array := (16#FB55#,16#3000067B#);
   SFB56 : aliased constant Code_Point_Array := (16#FB56#,16#1000067E#);
   SFB57 : aliased constant Code_Point_Array := (16#FB57#,16#4000067E#);
   SFB58 : aliased constant Code_Point_Array := (16#FB58#,16#2000067E#);
   SFB59 : aliased constant Code_Point_Array := (16#FB59#,16#3000067E#);
   SFB5A : aliased constant Code_Point_Array := (16#FB5A#,16#10000680#);
   SFB5B : aliased constant Code_Point_Array := (16#FB5B#,16#40000680#);
   SFB5C : aliased constant Code_Point_Array := (16#FB5C#,16#20000680#);
   SFB5D : aliased constant Code_Point_Array := (16#FB5D#,16#30000680#);
   SFB5E : aliased constant Code_Point_Array := (16#FB5E#,16#1000067A#);
   SFB5F : aliased constant Code_Point_Array := (16#FB5F#,16#4000067A#);
   SFB60 : aliased constant Code_Point_Array := (16#FB60#,16#2000067A#);
   SFB61 : aliased constant Code_Point_Array := (16#FB61#,16#3000067A#);
   SFB62 : aliased constant Code_Point_Array := (16#FB62#,16#1000067F#);
   SFB63 : aliased constant Code_Point_Array := (16#FB63#,16#4000067F#);
   SFB64 : aliased constant Code_Point_Array := (16#FB64#,16#2000067F#);
   SFB65 : aliased constant Code_Point_Array := (16#FB65#,16#3000067F#);
   SFB66 : aliased constant Code_Point_Array := (16#FB66#,16#10000679#);
   SFB67 : aliased constant Code_Point_Array := (16#FB67#,16#40000679#);
   SFB68 : aliased constant Code_Point_Array := (16#FB68#,16#20000679#);
   SFB69 : aliased constant Code_Point_Array := (16#FB69#,16#30000679#);
   SFB6A : aliased constant Code_Point_Array := (16#FB6A#,16#100006A4#);
   SFB6B : aliased constant Code_Point_Array := (16#FB6B#,16#400006A4#);
   SFB6C : aliased constant Code_Point_Array := (16#FB6C#,16#200006A4#);
   SFB6D : aliased constant Code_Point_Array := (16#FB6D#,16#300006A4#);
   SFB6E : aliased constant Code_Point_Array := (16#FB6E#,16#100006A6#);
   SFB6F : aliased constant Code_Point_Array := (16#FB6F#,16#400006A6#);
   SFB70 : aliased constant Code_Point_Array := (16#FB70#,16#200006A6#);
   SFB71 : aliased constant Code_Point_Array := (16#FB71#,16#300006A6#);
   SFB72 : aliased constant Code_Point_Array := (16#FB72#,16#10000684#);
   SFB73 : aliased constant Code_Point_Array := (16#FB73#,16#40000684#);
   SFB74 : aliased constant Code_Point_Array := (16#FB74#,16#20000684#);
   SFB75 : aliased constant Code_Point_Array := (16#FB75#,16#30000684#);
   SFB76 : aliased constant Code_Point_Array := (16#FB76#,16#10000683#);
   SFB77 : aliased constant Code_Point_Array := (16#FB77#,16#40000683#);
   SFB78 : aliased constant Code_Point_Array := (16#FB78#,16#20000683#);
   SFB79 : aliased constant Code_Point_Array := (16#FB79#,16#30000683#);
   SFB7A : aliased constant Code_Point_Array := (16#FB7A#,16#10000686#);
   SFB7B : aliased constant Code_Point_Array := (16#FB7B#,16#40000686#);
   SFB7C : aliased constant Code_Point_Array := (16#FB7C#,16#20000686#);
   SFB7D : aliased constant Code_Point_Array := (16#FB7D#,16#30000686#);
   SFB7E : aliased constant Code_Point_Array := (16#FB7E#,16#10000687#);
   SFB7F : aliased constant Code_Point_Array := (16#FB7F#,16#40000687#);
   SFB80 : aliased constant Code_Point_Array := (16#FB80#,16#20000687#);
   SFB81 : aliased constant Code_Point_Array := (16#FB81#,16#30000687#);
   SFB82 : aliased constant Code_Point_Array := (16#FB82#,16#1000068D#);
   SFB83 : aliased constant Code_Point_Array := (16#FB83#,16#4000068D#);
   SFB84 : aliased constant Code_Point_Array := (16#FB84#,16#1000068C#);
   SFB85 : aliased constant Code_Point_Array := (16#FB85#,16#4000068C#);
   SFB86 : aliased constant Code_Point_Array := (16#FB86#,16#1000068E#);
   SFB87 : aliased constant Code_Point_Array := (16#FB87#,16#4000068E#);
   SFB88 : aliased constant Code_Point_Array := (16#FB88#,16#10000688#);
   SFB89 : aliased constant Code_Point_Array := (16#FB89#,16#40000688#);
   SFB8A : aliased constant Code_Point_Array := (16#FB8A#,16#10000698#);
   SFB8B : aliased constant Code_Point_Array := (16#FB8B#,16#40000698#);
   SFB8C : aliased constant Code_Point_Array := (16#FB8C#,16#10000691#);
   SFB8D : aliased constant Code_Point_Array := (16#FB8D#,16#40000691#);
   SFB8E : aliased constant Code_Point_Array := (16#FB8E#,16#100006A9#);
   SFB8F : aliased constant Code_Point_Array := (16#FB8F#,16#400006A9#);
   SFB90 : aliased constant Code_Point_Array := (16#FB90#,16#200006A9#);
   SFB91 : aliased constant Code_Point_Array := (16#FB91#,16#300006A9#);
   SFB92 : aliased constant Code_Point_Array := (16#FB92#,16#100006AF#);
   SFB93 : aliased constant Code_Point_Array := (16#FB93#,16#400006AF#);
   SFB94 : aliased constant Code_Point_Array := (16#FB94#,16#200006AF#);
   SFB95 : aliased constant Code_Point_Array := (16#FB95#,16#300006AF#);
   SFB96 : aliased constant Code_Point_Array := (16#FB96#,16#100006B3#);
   SFB97 : aliased constant Code_Point_Array := (16#FB97#,16#400006B3#);
   SFB98 : aliased constant Code_Point_Array := (16#FB98#,16#200006B3#);
   SFB99 : aliased constant Code_Point_Array := (16#FB99#,16#300006B3#);
   SFB9A : aliased constant Code_Point_Array := (16#FB9A#,16#100006B1#);
   SFB9B : aliased constant Code_Point_Array := (16#FB9B#,16#400006B1#);
   SFB9C : aliased constant Code_Point_Array := (16#FB9C#,16#200006B1#);
   SFB9D : aliased constant Code_Point_Array := (16#FB9D#,16#300006B1#);
   SFB9E : aliased constant Code_Point_Array := (16#FB9E#,16#100006BA#);
   SFB9F : aliased constant Code_Point_Array := (16#FB9F#,16#400006BA#);
   SFBA0 : aliased constant Code_Point_Array := (16#FBA0#,16#100006BB#);
   SFBA1 : aliased constant Code_Point_Array := (16#FBA1#,16#400006BB#);
   SFBA2 : aliased constant Code_Point_Array := (16#FBA2#,16#200006BB#);
   SFBA3 : aliased constant Code_Point_Array := (16#FBA3#,16#300006BB#);
   SFBA4 : aliased constant Code_Point_Array := (16#FBA4#,16#100006C0#);
   SFBA5 : aliased constant Code_Point_Array := (16#FBA5#,16#400006C0#);
   SFBA6 : aliased constant Code_Point_Array := (16#FBA6#,16#100006C1#);
   SFBA7 : aliased constant Code_Point_Array := (16#FBA7#,16#400006C1#);
   SFBA8 : aliased constant Code_Point_Array := (16#FBA8#,16#200006C1#);
   SFBA9 : aliased constant Code_Point_Array := (16#FBA9#,16#300006C1#);
   SFBAA : aliased constant Code_Point_Array := (16#FBAA#,16#100006BE#);
   SFBAB : aliased constant Code_Point_Array := (16#FBAB#,16#400006BE#);
   SFBAC : aliased constant Code_Point_Array := (16#FBAC#,16#200006BE#);
   SFBAD : aliased constant Code_Point_Array := (16#FBAD#,16#300006BE#);
   SFBAE : aliased constant Code_Point_Array := (16#FBAE#,16#100006D2#);
   SFBAF : aliased constant Code_Point_Array := (16#FBAF#,16#400006D2#);
   SFBB0 : aliased constant Code_Point_Array := (16#FBB0#,16#100006D3#);
   SFBB1 : aliased constant Code_Point_Array := (16#FBB1#,16#400006D3#);
   SFBD3 : aliased constant Code_Point_Array := (16#FBD3#,16#100006AD#);
   SFBD4 : aliased constant Code_Point_Array := (16#FBD4#,16#400006AD#);
   SFBD5 : aliased constant Code_Point_Array := (16#FBD5#,16#200006AD#);
   SFBD6 : aliased constant Code_Point_Array := (16#FBD6#,16#300006AD#);
   SFBD7 : aliased constant Code_Point_Array := (16#FBD7#,16#100006C7#);
   SFBD8 : aliased constant Code_Point_Array := (16#FBD8#,16#400006C7#);
   SFBD9 : aliased constant Code_Point_Array := (16#FBD9#,16#100006C6#);
   SFBDA : aliased constant Code_Point_Array := (16#FBDA#,16#400006C6#);
   SFBDB : aliased constant Code_Point_Array := (16#FBDB#,16#100006C8#);
   SFBDC : aliased constant Code_Point_Array := (16#FBDC#,16#400006C8#);
   SFBDD : aliased constant Code_Point_Array := (16#FBDD#,16#10000677#);
   SFBDE : aliased constant Code_Point_Array := (16#FBDE#,16#100006CB#);
   SFBDF : aliased constant Code_Point_Array := (16#FBDF#,16#400006CB#);
   SFBE0 : aliased constant Code_Point_Array := (16#FBE0#,16#100006C5#);
   SFBE1 : aliased constant Code_Point_Array := (16#FBE1#,16#400006C5#);
   SFBE2 : aliased constant Code_Point_Array := (16#FBE2#,16#100006C9#);
   SFBE3 : aliased constant Code_Point_Array := (16#FBE3#,16#400006C9#);
   SFBE4 : aliased constant Code_Point_Array := (16#FBE4#,16#100006D0#);
   SFBE5 : aliased constant Code_Point_Array := (16#FBE5#,16#400006D0#);
   SFBE6 : aliased constant Code_Point_Array := (16#FBE6#,16#200006D0#);
   SFBE7 : aliased constant Code_Point_Array := (16#FBE7#,16#300006D0#);
   SFBE8 : aliased constant Code_Point_Array := (16#FBE8#,16#20000649#);
   SFBE9 : aliased constant Code_Point_Array := (16#FBE9#,16#30000649#);
   SFBEA : aliased constant Code_Point_Array := (16#FBEA#,16#10000626#,16#627#);
   SFBEB : aliased constant Code_Point_Array := (16#FBEB#,16#40000626#,16#627#);
   SFBEC : aliased constant Code_Point_Array := (16#FBEC#,16#10000626#,16#6D5#);
   SFBED : aliased constant Code_Point_Array := (16#FBED#,16#40000626#,16#6D5#);
   SFBEE : aliased constant Code_Point_Array := (16#FBEE#,16#10000626#,16#648#);
   SFBEF : aliased constant Code_Point_Array := (16#FBEF#,16#40000626#,16#648#);
   SFBF0 : aliased constant Code_Point_Array := (16#FBF0#,16#10000626#,16#6C7#);
   SFBF1 : aliased constant Code_Point_Array := (16#FBF1#,16#40000626#,16#6C7#);
   SFBF2 : aliased constant Code_Point_Array := (16#FBF2#,16#10000626#,16#6C6#);
   SFBF3 : aliased constant Code_Point_Array := (16#FBF3#,16#40000626#,16#6C6#);
   SFBF4 : aliased constant Code_Point_Array := (16#FBF4#,16#10000626#,16#6C8#);
   SFBF5 : aliased constant Code_Point_Array := (16#FBF5#,16#40000626#,16#6C8#);
   SFBF6 : aliased constant Code_Point_Array := (16#FBF6#,16#10000626#,16#6D0#);
   SFBF7 : aliased constant Code_Point_Array := (16#FBF7#,16#40000626#,16#6D0#);
   SFBF8 : aliased constant Code_Point_Array := (16#FBF8#,16#20000626#,16#6D0#);
   SFBF9 : aliased constant Code_Point_Array := (16#FBF9#,16#10000626#,16#649#);
   SFBFA : aliased constant Code_Point_Array := (16#FBFA#,16#40000626#,16#649#);
   SFBFB : aliased constant Code_Point_Array := (16#FBFB#,16#20000626#,16#649#);
   SFBFC : aliased constant Code_Point_Array := (16#FBFC#,16#100006CC#);
   SFBFD : aliased constant Code_Point_Array := (16#FBFD#,16#400006CC#);
   SFBFE : aliased constant Code_Point_Array := (16#FBFE#,16#200006CC#);
   SFBFF : aliased constant Code_Point_Array := (16#FBFF#,16#300006CC#);
   SFC00 : aliased constant Code_Point_Array := (16#FC00#,16#10000626#,16#62C#);
   SFC01 : aliased constant Code_Point_Array := (16#FC01#,16#10000626#,16#62D#);
   SFC02 : aliased constant Code_Point_Array := (16#FC02#,16#10000626#,16#645#);
   SFC03 : aliased constant Code_Point_Array := (16#FC03#,16#10000626#,16#649#);
   SFC04 : aliased constant Code_Point_Array := (16#FC04#,16#10000626#,16#64A#);
   SFC05 : aliased constant Code_Point_Array := (16#FC05#,16#10000628#,16#62C#);
   SFC06 : aliased constant Code_Point_Array := (16#FC06#,16#10000628#,16#62D#);
   SFC07 : aliased constant Code_Point_Array := (16#FC07#,16#10000628#,16#62E#);
   SFC08 : aliased constant Code_Point_Array := (16#FC08#,16#10000628#,16#645#);
   SFC09 : aliased constant Code_Point_Array := (16#FC09#,16#10000628#,16#649#);
   SFC0A : aliased constant Code_Point_Array := (16#FC0A#,16#10000628#,16#64A#);
   SFC0B : aliased constant Code_Point_Array := (16#FC0B#,16#1000062A#,16#62C#);
   SFC0C : aliased constant Code_Point_Array := (16#FC0C#,16#1000062A#,16#62D#);
   SFC0D : aliased constant Code_Point_Array := (16#FC0D#,16#1000062A#,16#62E#);
   SFC0E : aliased constant Code_Point_Array := (16#FC0E#,16#1000062A#,16#645#);
   SFC0F : aliased constant Code_Point_Array := (16#FC0F#,16#1000062A#,16#649#);
   SFC10 : aliased constant Code_Point_Array := (16#FC10#,16#1000062A#,16#64A#);
   SFC11 : aliased constant Code_Point_Array := (16#FC11#,16#1000062B#,16#62C#);
   SFC12 : aliased constant Code_Point_Array := (16#FC12#,16#1000062B#,16#645#);
   SFC13 : aliased constant Code_Point_Array := (16#FC13#,16#1000062B#,16#649#);
   SFC14 : aliased constant Code_Point_Array := (16#FC14#,16#1000062B#,16#64A#);
   SFC15 : aliased constant Code_Point_Array := (16#FC15#,16#1000062C#,16#62D#);
   SFC16 : aliased constant Code_Point_Array := (16#FC16#,16#1000062C#,16#645#);
   SFC17 : aliased constant Code_Point_Array := (16#FC17#,16#1000062D#,16#62C#);
   SFC18 : aliased constant Code_Point_Array := (16#FC18#,16#1000062D#,16#645#);
   SFC19 : aliased constant Code_Point_Array := (16#FC19#,16#1000062E#,16#62C#);
   SFC1A : aliased constant Code_Point_Array := (16#FC1A#,16#1000062E#,16#62D#);
   SFC1B : aliased constant Code_Point_Array := (16#FC1B#,16#1000062E#,16#645#);
   SFC1C : aliased constant Code_Point_Array := (16#FC1C#,16#10000633#,16#62C#);
   SFC1D : aliased constant Code_Point_Array := (16#FC1D#,16#10000633#,16#62D#);
   SFC1E : aliased constant Code_Point_Array := (16#FC1E#,16#10000633#,16#62E#);
   SFC1F : aliased constant Code_Point_Array := (16#FC1F#,16#10000633#,16#645#);
   SFC20 : aliased constant Code_Point_Array := (16#FC20#,16#10000635#,16#62D#);
   SFC21 : aliased constant Code_Point_Array := (16#FC21#,16#10000635#,16#645#);
   SFC22 : aliased constant Code_Point_Array := (16#FC22#,16#10000636#,16#62C#);
   SFC23 : aliased constant Code_Point_Array := (16#FC23#,16#10000636#,16#62D#);
   SFC24 : aliased constant Code_Point_Array := (16#FC24#,16#10000636#,16#62E#);
   SFC25 : aliased constant Code_Point_Array := (16#FC25#,16#10000636#,16#645#);
   SFC26 : aliased constant Code_Point_Array := (16#FC26#,16#10000637#,16#62D#);
   SFC27 : aliased constant Code_Point_Array := (16#FC27#,16#10000637#,16#645#);
   SFC28 : aliased constant Code_Point_Array := (16#FC28#,16#10000638#,16#645#);
   SFC29 : aliased constant Code_Point_Array := (16#FC29#,16#10000639#,16#62C#);
   SFC2A : aliased constant Code_Point_Array := (16#FC2A#,16#10000639#,16#645#);
   SFC2B : aliased constant Code_Point_Array := (16#FC2B#,16#1000063A#,16#62C#);
   SFC2C : aliased constant Code_Point_Array := (16#FC2C#,16#1000063A#,16#645#);
   SFC2D : aliased constant Code_Point_Array := (16#FC2D#,16#10000641#,16#62C#);
   SFC2E : aliased constant Code_Point_Array := (16#FC2E#,16#10000641#,16#62D#);
   SFC2F : aliased constant Code_Point_Array := (16#FC2F#,16#10000641#,16#62E#);
   SFC30 : aliased constant Code_Point_Array := (16#FC30#,16#10000641#,16#645#);
   SFC31 : aliased constant Code_Point_Array := (16#FC31#,16#10000641#,16#649#);
   SFC32 : aliased constant Code_Point_Array := (16#FC32#,16#10000641#,16#64A#);
   SFC33 : aliased constant Code_Point_Array := (16#FC33#,16#10000642#,16#62D#);
   SFC34 : aliased constant Code_Point_Array := (16#FC34#,16#10000642#,16#645#);
   SFC35 : aliased constant Code_Point_Array := (16#FC35#,16#10000642#,16#649#);
   SFC36 : aliased constant Code_Point_Array := (16#FC36#,16#10000642#,16#64A#);
   SFC37 : aliased constant Code_Point_Array := (16#FC37#,16#10000643#,16#627#);
   SFC38 : aliased constant Code_Point_Array := (16#FC38#,16#10000643#,16#62C#);
   SFC39 : aliased constant Code_Point_Array := (16#FC39#,16#10000643#,16#62D#);
   SFC3A : aliased constant Code_Point_Array := (16#FC3A#,16#10000643#,16#62E#);
   SFC3B : aliased constant Code_Point_Array := (16#FC3B#,16#10000643#,16#644#);
   SFC3C : aliased constant Code_Point_Array := (16#FC3C#,16#10000643#,16#645#);
   SFC3D : aliased constant Code_Point_Array := (16#FC3D#,16#10000643#,16#649#);
   SFC3E : aliased constant Code_Point_Array := (16#FC3E#,16#10000643#,16#64A#);
   SFC3F : aliased constant Code_Point_Array := (16#FC3F#,16#10000644#,16#62C#);
   SFC40 : aliased constant Code_Point_Array := (16#FC40#,16#10000644#,16#62D#);
   SFC41 : aliased constant Code_Point_Array := (16#FC41#,16#10000644#,16#62E#);
   SFC42 : aliased constant Code_Point_Array := (16#FC42#,16#10000644#,16#645#);
   SFC43 : aliased constant Code_Point_Array := (16#FC43#,16#10000644#,16#649#);
   SFC44 : aliased constant Code_Point_Array := (16#FC44#,16#10000644#,16#64A#);
   SFC45 : aliased constant Code_Point_Array := (16#FC45#,16#10000645#,16#62C#);
   SFC46 : aliased constant Code_Point_Array := (16#FC46#,16#10000645#,16#62D#);
   SFC47 : aliased constant Code_Point_Array := (16#FC47#,16#10000645#,16#62E#);
   SFC48 : aliased constant Code_Point_Array := (16#FC48#,16#10000645#,16#645#);
   SFC49 : aliased constant Code_Point_Array := (16#FC49#,16#10000645#,16#649#);
   SFC4A : aliased constant Code_Point_Array := (16#FC4A#,16#10000645#,16#64A#);
   SFC4B : aliased constant Code_Point_Array := (16#FC4B#,16#10000646#,16#62C#);
   SFC4C : aliased constant Code_Point_Array := (16#FC4C#,16#10000646#,16#62D#);
   SFC4D : aliased constant Code_Point_Array := (16#FC4D#,16#10000646#,16#62E#);
   SFC4E : aliased constant Code_Point_Array := (16#FC4E#,16#10000646#,16#645#);
   SFC4F : aliased constant Code_Point_Array := (16#FC4F#,16#10000646#,16#649#);
   SFC50 : aliased constant Code_Point_Array := (16#FC50#,16#10000646#,16#64A#);
   SFC51 : aliased constant Code_Point_Array := (16#FC51#,16#10000647#,16#62C#);
   SFC52 : aliased constant Code_Point_Array := (16#FC52#,16#10000647#,16#645#);
   SFC53 : aliased constant Code_Point_Array := (16#FC53#,16#10000647#,16#649#);
   SFC54 : aliased constant Code_Point_Array := (16#FC54#,16#10000647#,16#64A#);
   SFC55 : aliased constant Code_Point_Array := (16#FC55#,16#1000064A#,16#62C#);
   SFC56 : aliased constant Code_Point_Array := (16#FC56#,16#1000064A#,16#62D#);
   SFC57 : aliased constant Code_Point_Array := (16#FC57#,16#1000064A#,16#62E#);
   SFC58 : aliased constant Code_Point_Array := (16#FC58#,16#1000064A#,16#645#);
   SFC59 : aliased constant Code_Point_Array := (16#FC59#,16#1000064A#,16#649#);
   SFC5A : aliased constant Code_Point_Array := (16#FC5A#,16#1000064A#,16#64A#);
   SFC5B : aliased constant Code_Point_Array := (16#FC5B#,16#10000630#,16#670#);
   SFC5C : aliased constant Code_Point_Array := (16#FC5C#,16#10000631#,16#670#);
   SFC5D : aliased constant Code_Point_Array := (16#FC5D#,16#10000649#,16#670#);
   SFC5E : aliased constant Code_Point_Array := (16#FC5E#,16#10000020#,16#64C#,16#651#);
   SFC5F : aliased constant Code_Point_Array := (16#FC5F#,16#10000020#,16#64D#,16#651#);
   SFC60 : aliased constant Code_Point_Array := (16#FC60#,16#10000020#,16#64E#,16#651#);
   SFC61 : aliased constant Code_Point_Array := (16#FC61#,16#10000020#,16#64F#,16#651#);
   SFC62 : aliased constant Code_Point_Array := (16#FC62#,16#10000020#,16#650#,16#651#);
   SFC63 : aliased constant Code_Point_Array := (16#FC63#,16#10000020#,16#651#,16#670#);
   SFC64 : aliased constant Code_Point_Array := (16#FC64#,16#40000626#,16#631#);
   SFC65 : aliased constant Code_Point_Array := (16#FC65#,16#40000626#,16#632#);
   SFC66 : aliased constant Code_Point_Array := (16#FC66#,16#40000626#,16#645#);
   SFC67 : aliased constant Code_Point_Array := (16#FC67#,16#40000626#,16#646#);
   SFC68 : aliased constant Code_Point_Array := (16#FC68#,16#40000626#,16#649#);
   SFC69 : aliased constant Code_Point_Array := (16#FC69#,16#40000626#,16#64A#);
   SFC6A : aliased constant Code_Point_Array := (16#FC6A#,16#40000628#,16#631#);
   SFC6B : aliased constant Code_Point_Array := (16#FC6B#,16#40000628#,16#632#);
   SFC6C : aliased constant Code_Point_Array := (16#FC6C#,16#40000628#,16#645#);
   SFC6D : aliased constant Code_Point_Array := (16#FC6D#,16#40000628#,16#646#);
   SFC6E : aliased constant Code_Point_Array := (16#FC6E#,16#40000628#,16#649#);
   SFC6F : aliased constant Code_Point_Array := (16#FC6F#,16#40000628#,16#64A#);
   SFC70 : aliased constant Code_Point_Array := (16#FC70#,16#4000062A#,16#631#);
   SFC71 : aliased constant Code_Point_Array := (16#FC71#,16#4000062A#,16#632#);
   SFC72 : aliased constant Code_Point_Array := (16#FC72#,16#4000062A#,16#645#);
   SFC73 : aliased constant Code_Point_Array := (16#FC73#,16#4000062A#,16#646#);
   SFC74 : aliased constant Code_Point_Array := (16#FC74#,16#4000062A#,16#649#);
   SFC75 : aliased constant Code_Point_Array := (16#FC75#,16#4000062A#,16#64A#);
   SFC76 : aliased constant Code_Point_Array := (16#FC76#,16#4000062B#,16#631#);
   SFC77 : aliased constant Code_Point_Array := (16#FC77#,16#4000062B#,16#632#);
   SFC78 : aliased constant Code_Point_Array := (16#FC78#,16#4000062B#,16#645#);
   SFC79 : aliased constant Code_Point_Array := (16#FC79#,16#4000062B#,16#646#);
   SFC7A : aliased constant Code_Point_Array := (16#FC7A#,16#4000062B#,16#649#);
   SFC7B : aliased constant Code_Point_Array := (16#FC7B#,16#4000062B#,16#64A#);
   SFC7C : aliased constant Code_Point_Array := (16#FC7C#,16#40000641#,16#649#);
   SFC7D : aliased constant Code_Point_Array := (16#FC7D#,16#40000641#,16#64A#);
   SFC7E : aliased constant Code_Point_Array := (16#FC7E#,16#40000642#,16#649#);
   SFC7F : aliased constant Code_Point_Array := (16#FC7F#,16#40000642#,16#64A#);
   SFC80 : aliased constant Code_Point_Array := (16#FC80#,16#40000643#,16#627#);
   SFC81 : aliased constant Code_Point_Array := (16#FC81#,16#40000643#,16#644#);
   SFC82 : aliased constant Code_Point_Array := (16#FC82#,16#40000643#,16#645#);
   SFC83 : aliased constant Code_Point_Array := (16#FC83#,16#40000643#,16#649#);
   SFC84 : aliased constant Code_Point_Array := (16#FC84#,16#40000643#,16#64A#);
   SFC85 : aliased constant Code_Point_Array := (16#FC85#,16#40000644#,16#645#);
   SFC86 : aliased constant Code_Point_Array := (16#FC86#,16#40000644#,16#649#);
   SFC87 : aliased constant Code_Point_Array := (16#FC87#,16#40000644#,16#64A#);
   SFC88 : aliased constant Code_Point_Array := (16#FC88#,16#40000645#,16#627#);
   SFC89 : aliased constant Code_Point_Array := (16#FC89#,16#40000645#,16#645#);
   SFC8A : aliased constant Code_Point_Array := (16#FC8A#,16#40000646#,16#631#);
   SFC8B : aliased constant Code_Point_Array := (16#FC8B#,16#40000646#,16#632#);
   SFC8C : aliased constant Code_Point_Array := (16#FC8C#,16#40000646#,16#645#);
   SFC8D : aliased constant Code_Point_Array := (16#FC8D#,16#40000646#,16#646#);
   SFC8E : aliased constant Code_Point_Array := (16#FC8E#,16#40000646#,16#649#);
   SFC8F : aliased constant Code_Point_Array := (16#FC8F#,16#40000646#,16#64A#);
   SFC90 : aliased constant Code_Point_Array := (16#FC90#,16#40000649#,16#670#);
   SFC91 : aliased constant Code_Point_Array := (16#FC91#,16#4000064A#,16#631#);
   SFC92 : aliased constant Code_Point_Array := (16#FC92#,16#4000064A#,16#632#);
   SFC93 : aliased constant Code_Point_Array := (16#FC93#,16#4000064A#,16#645#);
   SFC94 : aliased constant Code_Point_Array := (16#FC94#,16#4000064A#,16#646#);
   SFC95 : aliased constant Code_Point_Array := (16#FC95#,16#4000064A#,16#649#);
   SFC96 : aliased constant Code_Point_Array := (16#FC96#,16#4000064A#,16#64A#);
   SFC97 : aliased constant Code_Point_Array := (16#FC97#,16#20000626#,16#62C#);
   SFC98 : aliased constant Code_Point_Array := (16#FC98#,16#20000626#,16#62D#);
   SFC99 : aliased constant Code_Point_Array := (16#FC99#,16#20000626#,16#62E#);
   SFC9A : aliased constant Code_Point_Array := (16#FC9A#,16#20000626#,16#645#);
   SFC9B : aliased constant Code_Point_Array := (16#FC9B#,16#20000626#,16#647#);
   SFC9C : aliased constant Code_Point_Array := (16#FC9C#,16#20000628#,16#62C#);
   SFC9D : aliased constant Code_Point_Array := (16#FC9D#,16#20000628#,16#62D#);
   SFC9E : aliased constant Code_Point_Array := (16#FC9E#,16#20000628#,16#62E#);
   SFC9F : aliased constant Code_Point_Array := (16#FC9F#,16#20000628#,16#645#);
   SFCA0 : aliased constant Code_Point_Array := (16#FCA0#,16#20000628#,16#647#);
   SFCA1 : aliased constant Code_Point_Array := (16#FCA1#,16#2000062A#,16#62C#);
   SFCA2 : aliased constant Code_Point_Array := (16#FCA2#,16#2000062A#,16#62D#);
   SFCA3 : aliased constant Code_Point_Array := (16#FCA3#,16#2000062A#,16#62E#);
   SFCA4 : aliased constant Code_Point_Array := (16#FCA4#,16#2000062A#,16#645#);
   SFCA5 : aliased constant Code_Point_Array := (16#FCA5#,16#2000062A#,16#647#);
   SFCA6 : aliased constant Code_Point_Array := (16#FCA6#,16#2000062B#,16#645#);
   SFCA7 : aliased constant Code_Point_Array := (16#FCA7#,16#2000062C#,16#62D#);
   SFCA8 : aliased constant Code_Point_Array := (16#FCA8#,16#2000062C#,16#645#);
   SFCA9 : aliased constant Code_Point_Array := (16#FCA9#,16#2000062D#,16#62C#);
   SFCAA : aliased constant Code_Point_Array := (16#FCAA#,16#2000062D#,16#645#);
   SFCAB : aliased constant Code_Point_Array := (16#FCAB#,16#2000062E#,16#62C#);
   SFCAC : aliased constant Code_Point_Array := (16#FCAC#,16#2000062E#,16#645#);
   SFCAD : aliased constant Code_Point_Array := (16#FCAD#,16#20000633#,16#62C#);
   SFCAE : aliased constant Code_Point_Array := (16#FCAE#,16#20000633#,16#62D#);
   SFCAF : aliased constant Code_Point_Array := (16#FCAF#,16#20000633#,16#62E#);
   SFCB0 : aliased constant Code_Point_Array := (16#FCB0#,16#20000633#,16#645#);
   SFCB1 : aliased constant Code_Point_Array := (16#FCB1#,16#20000635#,16#62D#);
   SFCB2 : aliased constant Code_Point_Array := (16#FCB2#,16#20000635#,16#62E#);
   SFCB3 : aliased constant Code_Point_Array := (16#FCB3#,16#20000635#,16#645#);
   SFCB4 : aliased constant Code_Point_Array := (16#FCB4#,16#20000636#,16#62C#);
   SFCB5 : aliased constant Code_Point_Array := (16#FCB5#,16#20000636#,16#62D#);
   SFCB6 : aliased constant Code_Point_Array := (16#FCB6#,16#20000636#,16#62E#);
   SFCB7 : aliased constant Code_Point_Array := (16#FCB7#,16#20000636#,16#645#);
   SFCB8 : aliased constant Code_Point_Array := (16#FCB8#,16#20000637#,16#62D#);
   SFCB9 : aliased constant Code_Point_Array := (16#FCB9#,16#20000638#,16#645#);
   SFCBA : aliased constant Code_Point_Array := (16#FCBA#,16#20000639#,16#62C#);
   SFCBB : aliased constant Code_Point_Array := (16#FCBB#,16#20000639#,16#645#);
   SFCBC : aliased constant Code_Point_Array := (16#FCBC#,16#2000063A#,16#62C#);
   SFCBD : aliased constant Code_Point_Array := (16#FCBD#,16#2000063A#,16#645#);
   SFCBE : aliased constant Code_Point_Array := (16#FCBE#,16#20000641#,16#62C#);
   SFCBF : aliased constant Code_Point_Array := (16#FCBF#,16#20000641#,16#62D#);
   SFCC0 : aliased constant Code_Point_Array := (16#FCC0#,16#20000641#,16#62E#);
   SFCC1 : aliased constant Code_Point_Array := (16#FCC1#,16#20000641#,16#645#);
   SFCC2 : aliased constant Code_Point_Array := (16#FCC2#,16#20000642#,16#62D#);
   SFCC3 : aliased constant Code_Point_Array := (16#FCC3#,16#20000642#,16#645#);
   SFCC4 : aliased constant Code_Point_Array := (16#FCC4#,16#20000643#,16#62C#);
   SFCC5 : aliased constant Code_Point_Array := (16#FCC5#,16#20000643#,16#62D#);
   SFCC6 : aliased constant Code_Point_Array := (16#FCC6#,16#20000643#,16#62E#);
   SFCC7 : aliased constant Code_Point_Array := (16#FCC7#,16#20000643#,16#644#);
   SFCC8 : aliased constant Code_Point_Array := (16#FCC8#,16#20000643#,16#645#);
   SFCC9 : aliased constant Code_Point_Array := (16#FCC9#,16#20000644#,16#62C#);
   SFCCA : aliased constant Code_Point_Array := (16#FCCA#,16#20000644#,16#62D#);
   SFCCB : aliased constant Code_Point_Array := (16#FCCB#,16#20000644#,16#62E#);
   SFCCC : aliased constant Code_Point_Array := (16#FCCC#,16#20000644#,16#645#);
   SFCCD : aliased constant Code_Point_Array := (16#FCCD#,16#20000644#,16#647#);
   SFCCE : aliased constant Code_Point_Array := (16#FCCE#,16#20000645#,16#62C#);
   SFCCF : aliased constant Code_Point_Array := (16#FCCF#,16#20000645#,16#62D#);
   SFCD0 : aliased constant Code_Point_Array := (16#FCD0#,16#20000645#,16#62E#);
   SFCD1 : aliased constant Code_Point_Array := (16#FCD1#,16#20000645#,16#645#);
   SFCD2 : aliased constant Code_Point_Array := (16#FCD2#,16#20000646#,16#62C#);
   SFCD3 : aliased constant Code_Point_Array := (16#FCD3#,16#20000646#,16#62D#);
   SFCD4 : aliased constant Code_Point_Array := (16#FCD4#,16#20000646#,16#62E#);
   SFCD5 : aliased constant Code_Point_Array := (16#FCD5#,16#20000646#,16#645#);
   SFCD6 : aliased constant Code_Point_Array := (16#FCD6#,16#20000646#,16#647#);
   SFCD7 : aliased constant Code_Point_Array := (16#FCD7#,16#20000647#,16#62C#);
   SFCD8 : aliased constant Code_Point_Array := (16#FCD8#,16#20000647#,16#645#);
   SFCD9 : aliased constant Code_Point_Array := (16#FCD9#,16#20000647#,16#670#);
   SFCDA : aliased constant Code_Point_Array := (16#FCDA#,16#2000064A#,16#62C#);
   SFCDB : aliased constant Code_Point_Array := (16#FCDB#,16#2000064A#,16#62D#);
   SFCDC : aliased constant Code_Point_Array := (16#FCDC#,16#2000064A#,16#62E#);
   SFCDD : aliased constant Code_Point_Array := (16#FCDD#,16#2000064A#,16#645#);
   SFCDE : aliased constant Code_Point_Array := (16#FCDE#,16#2000064A#,16#647#);
   SFCDF : aliased constant Code_Point_Array := (16#FCDF#,16#30000626#,16#645#);
   SFCE0 : aliased constant Code_Point_Array := (16#FCE0#,16#30000626#,16#647#);
   SFCE1 : aliased constant Code_Point_Array := (16#FCE1#,16#30000628#,16#645#);
   SFCE2 : aliased constant Code_Point_Array := (16#FCE2#,16#30000628#,16#647#);
   SFCE3 : aliased constant Code_Point_Array := (16#FCE3#,16#3000062A#,16#645#);
   SFCE4 : aliased constant Code_Point_Array := (16#FCE4#,16#3000062A#,16#647#);
   SFCE5 : aliased constant Code_Point_Array := (16#FCE5#,16#3000062B#,16#645#);
   SFCE6 : aliased constant Code_Point_Array := (16#FCE6#,16#3000062B#,16#647#);
   SFCE7 : aliased constant Code_Point_Array := (16#FCE7#,16#30000633#,16#645#);
   SFCE8 : aliased constant Code_Point_Array := (16#FCE8#,16#30000633#,16#647#);
   SFCE9 : aliased constant Code_Point_Array := (16#FCE9#,16#30000634#,16#645#);
   SFCEA : aliased constant Code_Point_Array := (16#FCEA#,16#30000634#,16#647#);
   SFCEB : aliased constant Code_Point_Array := (16#FCEB#,16#30000643#,16#644#);
   SFCEC : aliased constant Code_Point_Array := (16#FCEC#,16#30000643#,16#645#);
   SFCED : aliased constant Code_Point_Array := (16#FCED#,16#30000644#,16#645#);
   SFCEE : aliased constant Code_Point_Array := (16#FCEE#,16#30000646#,16#645#);
   SFCEF : aliased constant Code_Point_Array := (16#FCEF#,16#30000646#,16#647#);
   SFCF0 : aliased constant Code_Point_Array := (16#FCF0#,16#3000064A#,16#645#);
   SFCF1 : aliased constant Code_Point_Array := (16#FCF1#,16#3000064A#,16#647#);
   SFCF2 : aliased constant Code_Point_Array := (16#FCF2#,16#30000640#,16#64E#,16#651#);
   SFCF3 : aliased constant Code_Point_Array := (16#FCF3#,16#30000640#,16#64F#,16#651#);
   SFCF4 : aliased constant Code_Point_Array := (16#FCF4#,16#30000640#,16#650#,16#651#);
   SFCF5 : aliased constant Code_Point_Array := (16#FCF5#,16#10000637#,16#649#);
   SFCF6 : aliased constant Code_Point_Array := (16#FCF6#,16#10000637#,16#64A#);
   SFCF7 : aliased constant Code_Point_Array := (16#FCF7#,16#10000639#,16#649#);
   SFCF8 : aliased constant Code_Point_Array := (16#FCF8#,16#10000639#,16#64A#);
   SFCF9 : aliased constant Code_Point_Array := (16#FCF9#,16#1000063A#,16#649#);
   SFCFA : aliased constant Code_Point_Array := (16#FCFA#,16#1000063A#,16#64A#);
   SFCFB : aliased constant Code_Point_Array := (16#FCFB#,16#10000633#,16#649#);
   SFCFC : aliased constant Code_Point_Array := (16#FCFC#,16#10000633#,16#64A#);
   SFCFD : aliased constant Code_Point_Array := (16#FCFD#,16#10000634#,16#649#);
   SFCFE : aliased constant Code_Point_Array := (16#FCFE#,16#10000634#,16#64A#);
   SFCFF : aliased constant Code_Point_Array := (16#FCFF#,16#1000062D#,16#649#);
   SFD00 : aliased constant Code_Point_Array := (16#FD00#,16#1000062D#,16#64A#);
   SFD01 : aliased constant Code_Point_Array := (16#FD01#,16#1000062C#,16#649#);
   SFD02 : aliased constant Code_Point_Array := (16#FD02#,16#1000062C#,16#64A#);
   SFD03 : aliased constant Code_Point_Array := (16#FD03#,16#1000062E#,16#649#);
   SFD04 : aliased constant Code_Point_Array := (16#FD04#,16#1000062E#,16#64A#);
   SFD05 : aliased constant Code_Point_Array := (16#FD05#,16#10000635#,16#649#);
   SFD06 : aliased constant Code_Point_Array := (16#FD06#,16#10000635#,16#64A#);
   SFD07 : aliased constant Code_Point_Array := (16#FD07#,16#10000636#,16#649#);
   SFD08 : aliased constant Code_Point_Array := (16#FD08#,16#10000636#,16#64A#);
   SFD09 : aliased constant Code_Point_Array := (16#FD09#,16#10000634#,16#62C#);
   SFD0A : aliased constant Code_Point_Array := (16#FD0A#,16#10000634#,16#62D#);
   SFD0B : aliased constant Code_Point_Array := (16#FD0B#,16#10000634#,16#62E#);
   SFD0C : aliased constant Code_Point_Array := (16#FD0C#,16#10000634#,16#645#);
   SFD0D : aliased constant Code_Point_Array := (16#FD0D#,16#10000634#,16#631#);
   SFD0E : aliased constant Code_Point_Array := (16#FD0E#,16#10000633#,16#631#);
   SFD0F : aliased constant Code_Point_Array := (16#FD0F#,16#10000635#,16#631#);
   SFD10 : aliased constant Code_Point_Array := (16#FD10#,16#10000636#,16#631#);
   SFD11 : aliased constant Code_Point_Array := (16#FD11#,16#40000637#,16#649#);
   SFD12 : aliased constant Code_Point_Array := (16#FD12#,16#40000637#,16#64A#);
   SFD13 : aliased constant Code_Point_Array := (16#FD13#,16#40000639#,16#649#);
   SFD14 : aliased constant Code_Point_Array := (16#FD14#,16#40000639#,16#64A#);
   SFD15 : aliased constant Code_Point_Array := (16#FD15#,16#4000063A#,16#649#);
   SFD16 : aliased constant Code_Point_Array := (16#FD16#,16#4000063A#,16#64A#);
   SFD17 : aliased constant Code_Point_Array := (16#FD17#,16#40000633#,16#649#);
   SFD18 : aliased constant Code_Point_Array := (16#FD18#,16#40000633#,16#64A#);
   SFD19 : aliased constant Code_Point_Array := (16#FD19#,16#40000634#,16#649#);
   SFD1A : aliased constant Code_Point_Array := (16#FD1A#,16#40000634#,16#64A#);
   SFD1B : aliased constant Code_Point_Array := (16#FD1B#,16#4000062D#,16#649#);
   SFD1C : aliased constant Code_Point_Array := (16#FD1C#,16#4000062D#,16#64A#);
   SFD1D : aliased constant Code_Point_Array := (16#FD1D#,16#4000062C#,16#649#);
   SFD1E : aliased constant Code_Point_Array := (16#FD1E#,16#4000062C#,16#64A#);
   SFD1F : aliased constant Code_Point_Array := (16#FD1F#,16#4000062E#,16#649#);
   SFD20 : aliased constant Code_Point_Array := (16#FD20#,16#4000062E#,16#64A#);
   SFD21 : aliased constant Code_Point_Array := (16#FD21#,16#40000635#,16#649#);
   SFD22 : aliased constant Code_Point_Array := (16#FD22#,16#40000635#,16#64A#);
   SFD23 : aliased constant Code_Point_Array := (16#FD23#,16#40000636#,16#649#);
   SFD24 : aliased constant Code_Point_Array := (16#FD24#,16#40000636#,16#64A#);
   SFD25 : aliased constant Code_Point_Array := (16#FD25#,16#40000634#,16#62C#);
   SFD26 : aliased constant Code_Point_Array := (16#FD26#,16#40000634#,16#62D#);
   SFD27 : aliased constant Code_Point_Array := (16#FD27#,16#40000634#,16#62E#);
   SFD28 : aliased constant Code_Point_Array := (16#FD28#,16#40000634#,16#645#);
   SFD29 : aliased constant Code_Point_Array := (16#FD29#,16#40000634#,16#631#);
   SFD2A : aliased constant Code_Point_Array := (16#FD2A#,16#40000633#,16#631#);
   SFD2B : aliased constant Code_Point_Array := (16#FD2B#,16#40000635#,16#631#);
   SFD2C : aliased constant Code_Point_Array := (16#FD2C#,16#40000636#,16#631#);
   SFD2D : aliased constant Code_Point_Array := (16#FD2D#,16#20000634#,16#62C#);
   SFD2E : aliased constant Code_Point_Array := (16#FD2E#,16#20000634#,16#62D#);
   SFD2F : aliased constant Code_Point_Array := (16#FD2F#,16#20000634#,16#62E#);
   SFD30 : aliased constant Code_Point_Array := (16#FD30#,16#20000634#,16#645#);
   SFD31 : aliased constant Code_Point_Array := (16#FD31#,16#20000633#,16#647#);
   SFD32 : aliased constant Code_Point_Array := (16#FD32#,16#20000634#,16#647#);
   SFD33 : aliased constant Code_Point_Array := (16#FD33#,16#20000637#,16#645#);
   SFD34 : aliased constant Code_Point_Array := (16#FD34#,16#30000633#,16#62C#);
   SFD35 : aliased constant Code_Point_Array := (16#FD35#,16#30000633#,16#62D#);
   SFD36 : aliased constant Code_Point_Array := (16#FD36#,16#30000633#,16#62E#);
   SFD37 : aliased constant Code_Point_Array := (16#FD37#,16#30000634#,16#62C#);
   SFD38 : aliased constant Code_Point_Array := (16#FD38#,16#30000634#,16#62D#);
   SFD39 : aliased constant Code_Point_Array := (16#FD39#,16#30000634#,16#62E#);
   SFD3A : aliased constant Code_Point_Array := (16#FD3A#,16#30000637#,16#645#);
   SFD3B : aliased constant Code_Point_Array := (16#FD3B#,16#30000638#,16#645#);
   SFD3C : aliased constant Code_Point_Array := (16#FD3C#,16#40000627#,16#64B#);
   SFD3D : aliased constant Code_Point_Array := (16#FD3D#,16#10000627#,16#64B#);
   SFD50 : aliased constant Code_Point_Array := (16#FD50#,16#2000062A#,16#62C#,16#645#);
   SFD51 : aliased constant Code_Point_Array := (16#FD51#,16#4000062A#,16#62D#,16#62C#);
   SFD52 : aliased constant Code_Point_Array := (16#FD52#,16#2000062A#,16#62D#,16#62C#);
   SFD53 : aliased constant Code_Point_Array := (16#FD53#,16#2000062A#,16#62D#,16#645#);
   SFD54 : aliased constant Code_Point_Array := (16#FD54#,16#2000062A#,16#62E#,16#645#);
   SFD55 : aliased constant Code_Point_Array := (16#FD55#,16#2000062A#,16#645#,16#62C#);
   SFD56 : aliased constant Code_Point_Array := (16#FD56#,16#2000062A#,16#645#,16#62D#);
   SFD57 : aliased constant Code_Point_Array := (16#FD57#,16#2000062A#,16#645#,16#62E#);
   SFD58 : aliased constant Code_Point_Array := (16#FD58#,16#4000062C#,16#645#,16#62D#);
   SFD59 : aliased constant Code_Point_Array := (16#FD59#,16#2000062C#,16#645#,16#62D#);
   SFD5A : aliased constant Code_Point_Array := (16#FD5A#,16#4000062D#,16#645#,16#64A#);
   SFD5B : aliased constant Code_Point_Array := (16#FD5B#,16#4000062D#,16#645#,16#649#);
   SFD5C : aliased constant Code_Point_Array := (16#FD5C#,16#20000633#,16#62D#,16#62C#);
   SFD5D : aliased constant Code_Point_Array := (16#FD5D#,16#20000633#,16#62C#,16#62D#);
   SFD5E : aliased constant Code_Point_Array := (16#FD5E#,16#40000633#,16#62C#,16#649#);
   SFD5F : aliased constant Code_Point_Array := (16#FD5F#,16#40000633#,16#645#,16#62D#);
   SFD60 : aliased constant Code_Point_Array := (16#FD60#,16#20000633#,16#645#,16#62D#);
   SFD61 : aliased constant Code_Point_Array := (16#FD61#,16#20000633#,16#645#,16#62C#);
   SFD62 : aliased constant Code_Point_Array := (16#FD62#,16#40000633#,16#645#,16#645#);
   SFD63 : aliased constant Code_Point_Array := (16#FD63#,16#20000633#,16#645#,16#645#);
   SFD64 : aliased constant Code_Point_Array := (16#FD64#,16#40000635#,16#62D#,16#62D#);
   SFD65 : aliased constant Code_Point_Array := (16#FD65#,16#20000635#,16#62D#,16#62D#);
   SFD66 : aliased constant Code_Point_Array := (16#FD66#,16#40000635#,16#645#,16#645#);
   SFD67 : aliased constant Code_Point_Array := (16#FD67#,16#40000634#,16#62D#,16#645#);
   SFD68 : aliased constant Code_Point_Array := (16#FD68#,16#20000634#,16#62D#,16#645#);
   SFD69 : aliased constant Code_Point_Array := (16#FD69#,16#40000634#,16#62C#,16#64A#);
   SFD6A : aliased constant Code_Point_Array := (16#FD6A#,16#40000634#,16#645#,16#62E#);
   SFD6B : aliased constant Code_Point_Array := (16#FD6B#,16#20000634#,16#645#,16#62E#);
   SFD6C : aliased constant Code_Point_Array := (16#FD6C#,16#40000634#,16#645#,16#645#);
   SFD6D : aliased constant Code_Point_Array := (16#FD6D#,16#20000634#,16#645#,16#645#);
   SFD6E : aliased constant Code_Point_Array := (16#FD6E#,16#40000636#,16#62D#,16#649#);
   SFD6F : aliased constant Code_Point_Array := (16#FD6F#,16#40000636#,16#62E#,16#645#);
   SFD70 : aliased constant Code_Point_Array := (16#FD70#,16#20000636#,16#62E#,16#645#);
   SFD71 : aliased constant Code_Point_Array := (16#FD71#,16#40000637#,16#645#,16#62D#);
   SFD72 : aliased constant Code_Point_Array := (16#FD72#,16#20000637#,16#645#,16#62D#);
   SFD73 : aliased constant Code_Point_Array := (16#FD73#,16#20000637#,16#645#,16#645#);
   SFD74 : aliased constant Code_Point_Array := (16#FD74#,16#40000637#,16#645#,16#64A#);
   SFD75 : aliased constant Code_Point_Array := (16#FD75#,16#40000639#,16#62C#,16#645#);
   SFD76 : aliased constant Code_Point_Array := (16#FD76#,16#40000639#,16#645#,16#645#);
   SFD77 : aliased constant Code_Point_Array := (16#FD77#,16#20000639#,16#645#,16#645#);
   SFD78 : aliased constant Code_Point_Array := (16#FD78#,16#40000639#,16#645#,16#649#);
   SFD79 : aliased constant Code_Point_Array := (16#FD79#,16#4000063A#,16#645#,16#645#);
   SFD7A : aliased constant Code_Point_Array := (16#FD7A#,16#4000063A#,16#645#,16#64A#);
   SFD7B : aliased constant Code_Point_Array := (16#FD7B#,16#4000063A#,16#645#,16#649#);
   SFD7C : aliased constant Code_Point_Array := (16#FD7C#,16#40000641#,16#62E#,16#645#);
   SFD7D : aliased constant Code_Point_Array := (16#FD7D#,16#20000641#,16#62E#,16#645#);
   SFD7E : aliased constant Code_Point_Array := (16#FD7E#,16#40000642#,16#645#,16#62D#);
   SFD7F : aliased constant Code_Point_Array := (16#FD7F#,16#40000642#,16#645#,16#645#);
   SFD80 : aliased constant Code_Point_Array := (16#FD80#,16#40000644#,16#62D#,16#645#);
   SFD81 : aliased constant Code_Point_Array := (16#FD81#,16#40000644#,16#62D#,16#64A#);
   SFD82 : aliased constant Code_Point_Array := (16#FD82#,16#40000644#,16#62D#,16#649#);
   SFD83 : aliased constant Code_Point_Array := (16#FD83#,16#20000644#,16#62C#,16#62C#);
   SFD84 : aliased constant Code_Point_Array := (16#FD84#,16#40000644#,16#62C#,16#62C#);
   SFD85 : aliased constant Code_Point_Array := (16#FD85#,16#40000644#,16#62E#,16#645#);
   SFD86 : aliased constant Code_Point_Array := (16#FD86#,16#20000644#,16#62E#,16#645#);
   SFD87 : aliased constant Code_Point_Array := (16#FD87#,16#40000644#,16#645#,16#62D#);
   SFD88 : aliased constant Code_Point_Array := (16#FD88#,16#20000644#,16#645#,16#62D#);
   SFD89 : aliased constant Code_Point_Array := (16#FD89#,16#20000645#,16#62D#,16#62C#);
   SFD8A : aliased constant Code_Point_Array := (16#FD8A#,16#20000645#,16#62D#,16#645#);
   SFD8B : aliased constant Code_Point_Array := (16#FD8B#,16#40000645#,16#62D#,16#64A#);
   SFD8C : aliased constant Code_Point_Array := (16#FD8C#,16#20000645#,16#62C#,16#62D#);
   SFD8D : aliased constant Code_Point_Array := (16#FD8D#,16#20000645#,16#62C#,16#645#);
   SFD8E : aliased constant Code_Point_Array := (16#FD8E#,16#20000645#,16#62E#,16#62C#);
   SFD8F : aliased constant Code_Point_Array := (16#FD8F#,16#20000645#,16#62E#,16#645#);
   SFD92 : aliased constant Code_Point_Array := (16#FD92#,16#20000645#,16#62C#,16#62E#);
   SFD93 : aliased constant Code_Point_Array := (16#FD93#,16#20000647#,16#645#,16#62C#);
   SFD94 : aliased constant Code_Point_Array := (16#FD94#,16#20000647#,16#645#,16#645#);
   SFD95 : aliased constant Code_Point_Array := (16#FD95#,16#20000646#,16#62D#,16#645#);
   SFD96 : aliased constant Code_Point_Array := (16#FD96#,16#40000646#,16#62D#,16#649#);
   SFD97 : aliased constant Code_Point_Array := (16#FD97#,16#40000646#,16#62C#,16#645#);
   SFD98 : aliased constant Code_Point_Array := (16#FD98#,16#20000646#,16#62C#,16#645#);
   SFD99 : aliased constant Code_Point_Array := (16#FD99#,16#40000646#,16#62C#,16#649#);
   SFD9A : aliased constant Code_Point_Array := (16#FD9A#,16#40000646#,16#645#,16#64A#);
   SFD9B : aliased constant Code_Point_Array := (16#FD9B#,16#40000646#,16#645#,16#649#);
   SFD9C : aliased constant Code_Point_Array := (16#FD9C#,16#4000064A#,16#645#,16#645#);
   SFD9D : aliased constant Code_Point_Array := (16#FD9D#,16#2000064A#,16#645#,16#645#);
   SFD9E : aliased constant Code_Point_Array := (16#FD9E#,16#40000628#,16#62E#,16#64A#);
   SFD9F : aliased constant Code_Point_Array := (16#FD9F#,16#4000062A#,16#62C#,16#64A#);
   SFDA0 : aliased constant Code_Point_Array := (16#FDA0#,16#4000062A#,16#62C#,16#649#);
   SFDA1 : aliased constant Code_Point_Array := (16#FDA1#,16#4000062A#,16#62E#,16#64A#);
   SFDA2 : aliased constant Code_Point_Array := (16#FDA2#,16#4000062A#,16#62E#,16#649#);
   SFDA3 : aliased constant Code_Point_Array := (16#FDA3#,16#4000062A#,16#645#,16#64A#);
   SFDA4 : aliased constant Code_Point_Array := (16#FDA4#,16#4000062A#,16#645#,16#649#);
   SFDA5 : aliased constant Code_Point_Array := (16#FDA5#,16#4000062C#,16#645#,16#64A#);
   SFDA6 : aliased constant Code_Point_Array := (16#FDA6#,16#4000062C#,16#62D#,16#649#);
   SFDA7 : aliased constant Code_Point_Array := (16#FDA7#,16#4000062C#,16#645#,16#649#);
   SFDA8 : aliased constant Code_Point_Array := (16#FDA8#,16#40000633#,16#62E#,16#649#);
   SFDA9 : aliased constant Code_Point_Array := (16#FDA9#,16#40000635#,16#62D#,16#64A#);
   SFDAA : aliased constant Code_Point_Array := (16#FDAA#,16#40000634#,16#62D#,16#64A#);
   SFDAB : aliased constant Code_Point_Array := (16#FDAB#,16#40000636#,16#62D#,16#64A#);
   SFDAC : aliased constant Code_Point_Array := (16#FDAC#,16#40000644#,16#62C#,16#64A#);
   SFDAD : aliased constant Code_Point_Array := (16#FDAD#,16#40000644#,16#645#,16#64A#);
   SFDAE : aliased constant Code_Point_Array := (16#FDAE#,16#4000064A#,16#62D#,16#64A#);
   SFDAF : aliased constant Code_Point_Array := (16#FDAF#,16#4000064A#,16#62C#,16#64A#);
   SFDB0 : aliased constant Code_Point_Array := (16#FDB0#,16#4000064A#,16#645#,16#64A#);
   SFDB1 : aliased constant Code_Point_Array := (16#FDB1#,16#40000645#,16#645#,16#64A#);
   SFDB2 : aliased constant Code_Point_Array := (16#FDB2#,16#40000642#,16#645#,16#64A#);
   SFDB3 : aliased constant Code_Point_Array := (16#FDB3#,16#40000646#,16#62D#,16#64A#);
   SFDB4 : aliased constant Code_Point_Array := (16#FDB4#,16#20000642#,16#645#,16#62D#);
   SFDB5 : aliased constant Code_Point_Array := (16#FDB5#,16#20000644#,16#62D#,16#645#);
   SFDB6 : aliased constant Code_Point_Array := (16#FDB6#,16#40000639#,16#645#,16#64A#);
   SFDB7 : aliased constant Code_Point_Array := (16#FDB7#,16#40000643#,16#645#,16#64A#);
   SFDB8 : aliased constant Code_Point_Array := (16#FDB8#,16#20000646#,16#62C#,16#62D#);
   SFDB9 : aliased constant Code_Point_Array := (16#FDB9#,16#40000645#,16#62E#,16#64A#);
   SFDBA : aliased constant Code_Point_Array := (16#FDBA#,16#20000644#,16#62C#,16#645#);
   SFDBB : aliased constant Code_Point_Array := (16#FDBB#,16#40000643#,16#645#,16#645#);
   SFDBC : aliased constant Code_Point_Array := (16#FDBC#,16#40000644#,16#62C#,16#645#);
   SFDBD : aliased constant Code_Point_Array := (16#FDBD#,16#40000646#,16#62C#,16#62D#);
   SFDBE : aliased constant Code_Point_Array := (16#FDBE#,16#4000062C#,16#62D#,16#64A#);
   SFDBF : aliased constant Code_Point_Array := (16#FDBF#,16#4000062D#,16#62C#,16#64A#);
   SFDC0 : aliased constant Code_Point_Array := (16#FDC0#,16#40000645#,16#62C#,16#64A#);
   SFDC1 : aliased constant Code_Point_Array := (16#FDC1#,16#40000641#,16#645#,16#64A#);
   SFDC2 : aliased constant Code_Point_Array := (16#FDC2#,16#40000628#,16#62D#,16#64A#);
   SFDC3 : aliased constant Code_Point_Array := (16#FDC3#,16#20000643#,16#645#,16#645#);
   SFDC4 : aliased constant Code_Point_Array := (16#FDC4#,16#20000639#,16#62C#,16#645#);
   SFDC5 : aliased constant Code_Point_Array := (16#FDC5#,16#20000635#,16#645#,16#645#);
   SFDC6 : aliased constant Code_Point_Array := (16#FDC6#,16#40000633#,16#62E#,16#64A#);
   SFDC7 : aliased constant Code_Point_Array := (16#FDC7#,16#40000646#,16#62C#,16#64A#);
   SFDF0 : aliased constant Code_Point_Array := (16#FDF0#,16#10000635#,16#644#,16#6D2#);
   SFDF1 : aliased constant Code_Point_Array := (16#FDF1#,16#10000642#,16#644#,16#6D2#);
   SFDF2 : aliased constant Code_Point_Array := (16#FDF2#,16#10000627#,16#644#,16#644#,16#647#);
   SFDF3 : aliased constant Code_Point_Array := (16#FDF3#,16#10000627#,16#643#,16#628#,16#631#);
   SFDF4 : aliased constant Code_Point_Array := (16#FDF4#,16#10000645#,16#62D#,16#645#,16#62F#);
   SFDF5 : aliased constant Code_Point_Array := (16#FDF5#,16#10000635#,16#644#,16#639#,16#645#);
   SFDF6 : aliased constant Code_Point_Array := (16#FDF6#,16#10000631#,16#633#,16#648#,16#644#);
   SFDF7 : aliased constant Code_Point_Array := (16#FDF7#,16#10000639#,16#644#,16#64A#,16#647#);
   SFDF8 : aliased constant Code_Point_Array := (16#FDF8#,16#10000648#,16#633#,16#644#,16#645#);
   SFDF9 : aliased constant Code_Point_Array := (16#FDF9#,16#10000635#,16#644#,16#649#);
   SFDFA : aliased constant Code_Point_Array := (16#FDFA#,16#10000635#,16#644#,16#649#,16#20#,16#627#,16#644#,16#644#,16#647#,16#20#,16#639#,16#644#,16#64A#,16#647#,16#20#,16#648#,16#633#,16#644#,16#645#);
   SFDFB : aliased constant Code_Point_Array := (16#FDFB#,16#1000062C#,16#644#,16#20#,16#62C#,16#644#,16#627#,16#644#,16#647#);
   SFDFC : aliased constant Code_Point_Array := (16#FDFC#,16#10000631#,16#6CC#,16#627#,16#644#);
   SFE10 : aliased constant Code_Point_Array := (16#FE10#,16#1000002C#);
   SFE11 : aliased constant Code_Point_Array := (16#FE11#,16#10003001#);
   SFE12 : aliased constant Code_Point_Array := (16#FE12#,16#10003002#);
   SFE13 : aliased constant Code_Point_Array := (16#FE13#,16#1000003A#);
   SFE14 : aliased constant Code_Point_Array := (16#FE14#,16#1000003B#);
   SFE15 : aliased constant Code_Point_Array := (16#FE15#,16#10000021#);
   SFE16 : aliased constant Code_Point_Array := (16#FE16#,16#1000003F#);
   SFE17 : aliased constant Code_Point_Array := (16#FE17#,16#10003016#);
   SFE18 : aliased constant Code_Point_Array := (16#FE18#,16#10003017#);
   SFE19 : aliased constant Code_Point_Array := (16#FE19#,16#10002026#);
   SFE30 : aliased constant Code_Point_Array := (16#FE30#,16#10002025#);
   SFE31 : aliased constant Code_Point_Array := (16#FE31#,16#10002014#);
   SFE32 : aliased constant Code_Point_Array := (16#FE32#,16#10002013#);
   SFE33 : aliased constant Code_Point_Array := (16#FE33#,16#1000005F#);
   SFE34 : aliased constant Code_Point_Array := (16#FE34#,16#1000005F#);
   SFE35 : aliased constant Code_Point_Array := (16#FE35#,16#10000028#);
   SFE36 : aliased constant Code_Point_Array := (16#FE36#,16#10000029#);
   SFE37 : aliased constant Code_Point_Array := (16#FE37#,16#1000007B#);
   SFE38 : aliased constant Code_Point_Array := (16#FE38#,16#1000007D#);
   SFE39 : aliased constant Code_Point_Array := (16#FE39#,16#10003014#);
   SFE3A : aliased constant Code_Point_Array := (16#FE3A#,16#10003015#);
   SFE3B : aliased constant Code_Point_Array := (16#FE3B#,16#10003010#);
   SFE3C : aliased constant Code_Point_Array := (16#FE3C#,16#10003011#);
   SFE3D : aliased constant Code_Point_Array := (16#FE3D#,16#1000300A#);
   SFE3E : aliased constant Code_Point_Array := (16#FE3E#,16#1000300B#);
   SFE3F : aliased constant Code_Point_Array := (16#FE3F#,16#10003008#);
   SFE40 : aliased constant Code_Point_Array := (16#FE40#,16#10003009#);
   SFE41 : aliased constant Code_Point_Array := (16#FE41#,16#1000300C#);
   SFE42 : aliased constant Code_Point_Array := (16#FE42#,16#1000300D#);
   SFE43 : aliased constant Code_Point_Array := (16#FE43#,16#1000300E#);
   SFE44 : aliased constant Code_Point_Array := (16#FE44#,16#1000300F#);
   SFE47 : aliased constant Code_Point_Array := (16#FE47#,16#1000005B#);
   SFE48 : aliased constant Code_Point_Array := (16#FE48#,16#1000005D#);
   SFE49 : aliased constant Code_Point_Array := (16#FE49#,16#1000203E#);
   SFE4A : aliased constant Code_Point_Array := (16#FE4A#,16#1000203E#);
   SFE4B : aliased constant Code_Point_Array := (16#FE4B#,16#1000203E#);
   SFE4C : aliased constant Code_Point_Array := (16#FE4C#,16#1000203E#);
   SFE4D : aliased constant Code_Point_Array := (16#FE4D#,16#1000005F#);
   SFE4E : aliased constant Code_Point_Array := (16#FE4E#,16#1000005F#);
   SFE4F : aliased constant Code_Point_Array := (16#FE4F#,16#1000005F#);
   SFE50 : aliased constant Code_Point_Array := (16#FE50#,16#1000002C#);
   SFE51 : aliased constant Code_Point_Array := (16#FE51#,16#10003001#);
   SFE52 : aliased constant Code_Point_Array := (16#FE52#,16#1000002E#);
   SFE54 : aliased constant Code_Point_Array := (16#FE54#,16#1000003B#);
   SFE55 : aliased constant Code_Point_Array := (16#FE55#,16#1000003A#);
   SFE56 : aliased constant Code_Point_Array := (16#FE56#,16#1000003F#);
   SFE57 : aliased constant Code_Point_Array := (16#FE57#,16#10000021#);
   SFE58 : aliased constant Code_Point_Array := (16#FE58#,16#10002014#);
   SFE59 : aliased constant Code_Point_Array := (16#FE59#,16#10000028#);
   SFE5A : aliased constant Code_Point_Array := (16#FE5A#,16#10000029#);
   SFE5B : aliased constant Code_Point_Array := (16#FE5B#,16#1000007B#);
   SFE5C : aliased constant Code_Point_Array := (16#FE5C#,16#1000007D#);
   SFE5D : aliased constant Code_Point_Array := (16#FE5D#,16#10003014#);
   SFE5E : aliased constant Code_Point_Array := (16#FE5E#,16#10003015#);
   SFE5F : aliased constant Code_Point_Array := (16#FE5F#,16#10000023#);
   SFE60 : aliased constant Code_Point_Array := (16#FE60#,16#10000026#);
   SFE61 : aliased constant Code_Point_Array := (16#FE61#,16#1000002A#);
   SFE62 : aliased constant Code_Point_Array := (16#FE62#,16#1000002B#);
   SFE63 : aliased constant Code_Point_Array := (16#FE63#,16#1000002D#);
   SFE64 : aliased constant Code_Point_Array := (16#FE64#,16#1000003C#);
   SFE65 : aliased constant Code_Point_Array := (16#FE65#,16#1000003E#);
   SFE66 : aliased constant Code_Point_Array := (16#FE66#,16#1000003D#);
   SFE68 : aliased constant Code_Point_Array := (16#FE68#,16#1000005C#);
   SFE69 : aliased constant Code_Point_Array := (16#FE69#,16#10000024#);
   SFE6A : aliased constant Code_Point_Array := (16#FE6A#,16#10000025#);
   SFE6B : aliased constant Code_Point_Array := (16#FE6B#,16#10000040#);
   SFE70 : aliased constant Code_Point_Array := (16#FE70#,16#10000020#,16#64B#);
   SFE71 : aliased constant Code_Point_Array := (16#FE71#,16#30000640#,16#64B#);
   SFE72 : aliased constant Code_Point_Array := (16#FE72#,16#10000020#,16#64C#);
   SFE74 : aliased constant Code_Point_Array := (16#FE74#,16#10000020#,16#64D#);
   SFE76 : aliased constant Code_Point_Array := (16#FE76#,16#10000020#,16#64E#);
   SFE77 : aliased constant Code_Point_Array := (16#FE77#,16#30000640#,16#64E#);
   SFE78 : aliased constant Code_Point_Array := (16#FE78#,16#10000020#,16#64F#);
   SFE79 : aliased constant Code_Point_Array := (16#FE79#,16#30000640#,16#64F#);
   SFE7A : aliased constant Code_Point_Array := (16#FE7A#,16#10000020#,16#650#);
   SFE7B : aliased constant Code_Point_Array := (16#FE7B#,16#30000640#,16#650#);
   SFE7C : aliased constant Code_Point_Array := (16#FE7C#,16#10000020#,16#651#);
   SFE7D : aliased constant Code_Point_Array := (16#FE7D#,16#30000640#,16#651#);
   SFE7E : aliased constant Code_Point_Array := (16#FE7E#,16#10000020#,16#652#);
   SFE7F : aliased constant Code_Point_Array := (16#FE7F#,16#30000640#,16#652#);
   SFE80 : aliased constant Code_Point_Array := (16#FE80#,16#10000621#);
   SFE81 : aliased constant Code_Point_Array := (16#FE81#,16#10000622#);
   SFE82 : aliased constant Code_Point_Array := (16#FE82#,16#40000622#);
   SFE83 : aliased constant Code_Point_Array := (16#FE83#,16#10000623#);
   SFE84 : aliased constant Code_Point_Array := (16#FE84#,16#40000623#);
   SFE85 : aliased constant Code_Point_Array := (16#FE85#,16#10000624#);
   SFE86 : aliased constant Code_Point_Array := (16#FE86#,16#40000624#);
   SFE87 : aliased constant Code_Point_Array := (16#FE87#,16#10000625#);
   SFE88 : aliased constant Code_Point_Array := (16#FE88#,16#40000625#);
   SFE89 : aliased constant Code_Point_Array := (16#FE89#,16#10000626#);
   SFE8A : aliased constant Code_Point_Array := (16#FE8A#,16#40000626#);
   SFE8B : aliased constant Code_Point_Array := (16#FE8B#,16#20000626#);
   SFE8C : aliased constant Code_Point_Array := (16#FE8C#,16#30000626#);
   SFE8D : aliased constant Code_Point_Array := (16#FE8D#,16#10000627#);
   SFE8E : aliased constant Code_Point_Array := (16#FE8E#,16#40000627#);
   SFE8F : aliased constant Code_Point_Array := (16#FE8F#,16#10000628#);
   SFE90 : aliased constant Code_Point_Array := (16#FE90#,16#40000628#);
   SFE91 : aliased constant Code_Point_Array := (16#FE91#,16#20000628#);
   SFE92 : aliased constant Code_Point_Array := (16#FE92#,16#30000628#);
   SFE93 : aliased constant Code_Point_Array := (16#FE93#,16#10000629#);
   SFE94 : aliased constant Code_Point_Array := (16#FE94#,16#40000629#);
   SFE95 : aliased constant Code_Point_Array := (16#FE95#,16#1000062A#);
   SFE96 : aliased constant Code_Point_Array := (16#FE96#,16#4000062A#);
   SFE97 : aliased constant Code_Point_Array := (16#FE97#,16#2000062A#);
   SFE98 : aliased constant Code_Point_Array := (16#FE98#,16#3000062A#);
   SFE99 : aliased constant Code_Point_Array := (16#FE99#,16#1000062B#);
   SFE9A : aliased constant Code_Point_Array := (16#FE9A#,16#4000062B#);
   SFE9B : aliased constant Code_Point_Array := (16#FE9B#,16#2000062B#);
   SFE9C : aliased constant Code_Point_Array := (16#FE9C#,16#3000062B#);
   SFE9D : aliased constant Code_Point_Array := (16#FE9D#,16#1000062C#);
   SFE9E : aliased constant Code_Point_Array := (16#FE9E#,16#4000062C#);
   SFE9F : aliased constant Code_Point_Array := (16#FE9F#,16#2000062C#);
   SFEA0 : aliased constant Code_Point_Array := (16#FEA0#,16#3000062C#);
   SFEA1 : aliased constant Code_Point_Array := (16#FEA1#,16#1000062D#);
   SFEA2 : aliased constant Code_Point_Array := (16#FEA2#,16#4000062D#);
   SFEA3 : aliased constant Code_Point_Array := (16#FEA3#,16#2000062D#);
   SFEA4 : aliased constant Code_Point_Array := (16#FEA4#,16#3000062D#);
   SFEA5 : aliased constant Code_Point_Array := (16#FEA5#,16#1000062E#);
   SFEA6 : aliased constant Code_Point_Array := (16#FEA6#,16#4000062E#);
   SFEA7 : aliased constant Code_Point_Array := (16#FEA7#,16#2000062E#);
   SFEA8 : aliased constant Code_Point_Array := (16#FEA8#,16#3000062E#);
   SFEA9 : aliased constant Code_Point_Array := (16#FEA9#,16#1000062F#);
   SFEAA : aliased constant Code_Point_Array := (16#FEAA#,16#4000062F#);
   SFEAB : aliased constant Code_Point_Array := (16#FEAB#,16#10000630#);
   SFEAC : aliased constant Code_Point_Array := (16#FEAC#,16#40000630#);
   SFEAD : aliased constant Code_Point_Array := (16#FEAD#,16#10000631#);
   SFEAE : aliased constant Code_Point_Array := (16#FEAE#,16#40000631#);
   SFEAF : aliased constant Code_Point_Array := (16#FEAF#,16#10000632#);
   SFEB0 : aliased constant Code_Point_Array := (16#FEB0#,16#40000632#);
   SFEB1 : aliased constant Code_Point_Array := (16#FEB1#,16#10000633#);
   SFEB2 : aliased constant Code_Point_Array := (16#FEB2#,16#40000633#);
   SFEB3 : aliased constant Code_Point_Array := (16#FEB3#,16#20000633#);
   SFEB4 : aliased constant Code_Point_Array := (16#FEB4#,16#30000633#);
   SFEB5 : aliased constant Code_Point_Array := (16#FEB5#,16#10000634#);
   SFEB6 : aliased constant Code_Point_Array := (16#FEB6#,16#40000634#);
   SFEB7 : aliased constant Code_Point_Array := (16#FEB7#,16#20000634#);
   SFEB8 : aliased constant Code_Point_Array := (16#FEB8#,16#30000634#);
   SFEB9 : aliased constant Code_Point_Array := (16#FEB9#,16#10000635#);
   SFEBA : aliased constant Code_Point_Array := (16#FEBA#,16#40000635#);
   SFEBB : aliased constant Code_Point_Array := (16#FEBB#,16#20000635#);
   SFEBC : aliased constant Code_Point_Array := (16#FEBC#,16#30000635#);
   SFEBD : aliased constant Code_Point_Array := (16#FEBD#,16#10000636#);
   SFEBE : aliased constant Code_Point_Array := (16#FEBE#,16#40000636#);
   SFEBF : aliased constant Code_Point_Array := (16#FEBF#,16#20000636#);
   SFEC0 : aliased constant Code_Point_Array := (16#FEC0#,16#30000636#);
   SFEC1 : aliased constant Code_Point_Array := (16#FEC1#,16#10000637#);
   SFEC2 : aliased constant Code_Point_Array := (16#FEC2#,16#40000637#);
   SFEC3 : aliased constant Code_Point_Array := (16#FEC3#,16#20000637#);
   SFEC4 : aliased constant Code_Point_Array := (16#FEC4#,16#30000637#);
   SFEC5 : aliased constant Code_Point_Array := (16#FEC5#,16#10000638#);
   SFEC6 : aliased constant Code_Point_Array := (16#FEC6#,16#40000638#);
   SFEC7 : aliased constant Code_Point_Array := (16#FEC7#,16#20000638#);
   SFEC8 : aliased constant Code_Point_Array := (16#FEC8#,16#30000638#);
   SFEC9 : aliased constant Code_Point_Array := (16#FEC9#,16#10000639#);
   SFECA : aliased constant Code_Point_Array := (16#FECA#,16#40000639#);
   SFECB : aliased constant Code_Point_Array := (16#FECB#,16#20000639#);
   SFECC : aliased constant Code_Point_Array := (16#FECC#,16#30000639#);
   SFECD : aliased constant Code_Point_Array := (16#FECD#,16#1000063A#);
   SFECE : aliased constant Code_Point_Array := (16#FECE#,16#4000063A#);
   SFECF : aliased constant Code_Point_Array := (16#FECF#,16#2000063A#);
   SFED0 : aliased constant Code_Point_Array := (16#FED0#,16#3000063A#);
   SFED1 : aliased constant Code_Point_Array := (16#FED1#,16#10000641#);
   SFED2 : aliased constant Code_Point_Array := (16#FED2#,16#40000641#);
   SFED3 : aliased constant Code_Point_Array := (16#FED3#,16#20000641#);
   SFED4 : aliased constant Code_Point_Array := (16#FED4#,16#30000641#);
   SFED5 : aliased constant Code_Point_Array := (16#FED5#,16#10000642#);
   SFED6 : aliased constant Code_Point_Array := (16#FED6#,16#40000642#);
   SFED7 : aliased constant Code_Point_Array := (16#FED7#,16#20000642#);
   SFED8 : aliased constant Code_Point_Array := (16#FED8#,16#30000642#);
   SFED9 : aliased constant Code_Point_Array := (16#FED9#,16#10000643#);
   SFEDA : aliased constant Code_Point_Array := (16#FEDA#,16#40000643#);
   SFEDB : aliased constant Code_Point_Array := (16#FEDB#,16#20000643#);
   SFEDC : aliased constant Code_Point_Array := (16#FEDC#,16#30000643#);
   SFEDD : aliased constant Code_Point_Array := (16#FEDD#,16#10000644#);
   SFEDE : aliased constant Code_Point_Array := (16#FEDE#,16#40000644#);
   SFEDF : aliased constant Code_Point_Array := (16#FEDF#,16#20000644#);
   SFEE0 : aliased constant Code_Point_Array := (16#FEE0#,16#30000644#);
   SFEE1 : aliased constant Code_Point_Array := (16#FEE1#,16#10000645#);
   SFEE2 : aliased constant Code_Point_Array := (16#FEE2#,16#40000645#);
   SFEE3 : aliased constant Code_Point_Array := (16#FEE3#,16#20000645#);
   SFEE4 : aliased constant Code_Point_Array := (16#FEE4#,16#30000645#);
   SFEE5 : aliased constant Code_Point_Array := (16#FEE5#,16#10000646#);
   SFEE6 : aliased constant Code_Point_Array := (16#FEE6#,16#40000646#);
   SFEE7 : aliased constant Code_Point_Array := (16#FEE7#,16#20000646#);
   SFEE8 : aliased constant Code_Point_Array := (16#FEE8#,16#30000646#);
   SFEE9 : aliased constant Code_Point_Array := (16#FEE9#,16#10000647#);
   SFEEA : aliased constant Code_Point_Array := (16#FEEA#,16#40000647#);
   SFEEB : aliased constant Code_Point_Array := (16#FEEB#,16#20000647#);
   SFEEC : aliased constant Code_Point_Array := (16#FEEC#,16#30000647#);
   SFEED : aliased constant Code_Point_Array := (16#FEED#,16#10000648#);
   SFEEE : aliased constant Code_Point_Array := (16#FEEE#,16#40000648#);
   SFEEF : aliased constant Code_Point_Array := (16#FEEF#,16#10000649#);
   SFEF0 : aliased constant Code_Point_Array := (16#FEF0#,16#40000649#);
   SFEF1 : aliased constant Code_Point_Array := (16#FEF1#,16#1000064A#);
   SFEF2 : aliased constant Code_Point_Array := (16#FEF2#,16#4000064A#);
   SFEF3 : aliased constant Code_Point_Array := (16#FEF3#,16#2000064A#);
   SFEF4 : aliased constant Code_Point_Array := (16#FEF4#,16#3000064A#);
   SFEF5 : aliased constant Code_Point_Array := (16#FEF5#,16#10000644#,16#622#);
   SFEF6 : aliased constant Code_Point_Array := (16#FEF6#,16#40000644#,16#622#);
   SFEF7 : aliased constant Code_Point_Array := (16#FEF7#,16#10000644#,16#623#);
   SFEF8 : aliased constant Code_Point_Array := (16#FEF8#,16#40000644#,16#623#);
   SFEF9 : aliased constant Code_Point_Array := (16#FEF9#,16#10000644#,16#625#);
   SFEFA : aliased constant Code_Point_Array := (16#FEFA#,16#40000644#,16#625#);
   SFEFB : aliased constant Code_Point_Array := (16#FEFB#,16#10000644#,16#627#);
   SFEFC : aliased constant Code_Point_Array := (16#FEFC#,16#40000644#,16#627#);
   SFF01 : aliased constant Code_Point_Array := (16#FF01#,16#10000021#);
   SFF02 : aliased constant Code_Point_Array := (16#FF02#,16#10000022#);
   SFF03 : aliased constant Code_Point_Array := (16#FF03#,16#10000023#);
   SFF04 : aliased constant Code_Point_Array := (16#FF04#,16#10000024#);
   SFF05 : aliased constant Code_Point_Array := (16#FF05#,16#10000025#);
   SFF06 : aliased constant Code_Point_Array := (16#FF06#,16#10000026#);
   SFF07 : aliased constant Code_Point_Array := (16#FF07#,16#10000027#);
   SFF08 : aliased constant Code_Point_Array := (16#FF08#,16#10000028#);
   SFF09 : aliased constant Code_Point_Array := (16#FF09#,16#10000029#);
   SFF0A : aliased constant Code_Point_Array := (16#FF0A#,16#1000002A#);
   SFF0B : aliased constant Code_Point_Array := (16#FF0B#,16#1000002B#);
   SFF0C : aliased constant Code_Point_Array := (16#FF0C#,16#1000002C#);
   SFF0D : aliased constant Code_Point_Array := (16#FF0D#,16#1000002D#);
   SFF0E : aliased constant Code_Point_Array := (16#FF0E#,16#1000002E#);
   SFF0F : aliased constant Code_Point_Array := (16#FF0F#,16#1000002F#);
   SFF10 : aliased constant Code_Point_Array := (16#FF10#,16#10000030#);
   SFF11 : aliased constant Code_Point_Array := (16#FF11#,16#10000031#);
   SFF12 : aliased constant Code_Point_Array := (16#FF12#,16#10000032#);
   SFF13 : aliased constant Code_Point_Array := (16#FF13#,16#10000033#);
   SFF14 : aliased constant Code_Point_Array := (16#FF14#,16#10000034#);
   SFF15 : aliased constant Code_Point_Array := (16#FF15#,16#10000035#);
   SFF16 : aliased constant Code_Point_Array := (16#FF16#,16#10000036#);
   SFF17 : aliased constant Code_Point_Array := (16#FF17#,16#10000037#);
   SFF18 : aliased constant Code_Point_Array := (16#FF18#,16#10000038#);
   SFF19 : aliased constant Code_Point_Array := (16#FF19#,16#10000039#);
   SFF1A : aliased constant Code_Point_Array := (16#FF1A#,16#1000003A#);
   SFF1B : aliased constant Code_Point_Array := (16#FF1B#,16#1000003B#);
   SFF1C : aliased constant Code_Point_Array := (16#FF1C#,16#1000003C#);
   SFF1D : aliased constant Code_Point_Array := (16#FF1D#,16#1000003D#);
   SFF1E : aliased constant Code_Point_Array := (16#FF1E#,16#1000003E#);
   SFF1F : aliased constant Code_Point_Array := (16#FF1F#,16#1000003F#);
   SFF20 : aliased constant Code_Point_Array := (16#FF20#,16#10000040#);
   SFF21 : aliased constant Code_Point_Array := (16#FF21#,16#10000041#);
   SFF22 : aliased constant Code_Point_Array := (16#FF22#,16#10000042#);
   SFF23 : aliased constant Code_Point_Array := (16#FF23#,16#10000043#);
   SFF24 : aliased constant Code_Point_Array := (16#FF24#,16#10000044#);
   SFF25 : aliased constant Code_Point_Array := (16#FF25#,16#10000045#);
   SFF26 : aliased constant Code_Point_Array := (16#FF26#,16#10000046#);
   SFF27 : aliased constant Code_Point_Array := (16#FF27#,16#10000047#);
   SFF28 : aliased constant Code_Point_Array := (16#FF28#,16#10000048#);
   SFF29 : aliased constant Code_Point_Array := (16#FF29#,16#10000049#);
   SFF2A : aliased constant Code_Point_Array := (16#FF2A#,16#1000004A#);
   SFF2B : aliased constant Code_Point_Array := (16#FF2B#,16#1000004B#);
   SFF2C : aliased constant Code_Point_Array := (16#FF2C#,16#1000004C#);
   SFF2D : aliased constant Code_Point_Array := (16#FF2D#,16#1000004D#);
   SFF2E : aliased constant Code_Point_Array := (16#FF2E#,16#1000004E#);
   SFF2F : aliased constant Code_Point_Array := (16#FF2F#,16#1000004F#);
   SFF30 : aliased constant Code_Point_Array := (16#FF30#,16#10000050#);
   SFF31 : aliased constant Code_Point_Array := (16#FF31#,16#10000051#);
   SFF32 : aliased constant Code_Point_Array := (16#FF32#,16#10000052#);
   SFF33 : aliased constant Code_Point_Array := (16#FF33#,16#10000053#);
   SFF34 : aliased constant Code_Point_Array := (16#FF34#,16#10000054#);
   SFF35 : aliased constant Code_Point_Array := (16#FF35#,16#10000055#);
   SFF36 : aliased constant Code_Point_Array := (16#FF36#,16#10000056#);
   SFF37 : aliased constant Code_Point_Array := (16#FF37#,16#10000057#);
   SFF38 : aliased constant Code_Point_Array := (16#FF38#,16#10000058#);
   SFF39 : aliased constant Code_Point_Array := (16#FF39#,16#10000059#);
   SFF3A : aliased constant Code_Point_Array := (16#FF3A#,16#1000005A#);
   SFF3B : aliased constant Code_Point_Array := (16#FF3B#,16#1000005B#);
   SFF3C : aliased constant Code_Point_Array := (16#FF3C#,16#1000005C#);
   SFF3D : aliased constant Code_Point_Array := (16#FF3D#,16#1000005D#);
   SFF3E : aliased constant Code_Point_Array := (16#FF3E#,16#1000005E#);
   SFF3F : aliased constant Code_Point_Array := (16#FF3F#,16#1000005F#);
   SFF40 : aliased constant Code_Point_Array := (16#FF40#,16#10000060#);
   SFF41 : aliased constant Code_Point_Array := (16#FF41#,16#10000061#);
   SFF42 : aliased constant Code_Point_Array := (16#FF42#,16#10000062#);
   SFF43 : aliased constant Code_Point_Array := (16#FF43#,16#10000063#);
   SFF44 : aliased constant Code_Point_Array := (16#FF44#,16#10000064#);
   SFF45 : aliased constant Code_Point_Array := (16#FF45#,16#10000065#);
   SFF46 : aliased constant Code_Point_Array := (16#FF46#,16#10000066#);
   SFF47 : aliased constant Code_Point_Array := (16#FF47#,16#10000067#);
   SFF48 : aliased constant Code_Point_Array := (16#FF48#,16#10000068#);
   SFF49 : aliased constant Code_Point_Array := (16#FF49#,16#10000069#);
   SFF4A : aliased constant Code_Point_Array := (16#FF4A#,16#1000006A#);
   SFF4B : aliased constant Code_Point_Array := (16#FF4B#,16#1000006B#);
   SFF4C : aliased constant Code_Point_Array := (16#FF4C#,16#1000006C#);
   SFF4D : aliased constant Code_Point_Array := (16#FF4D#,16#1000006D#);
   SFF4E : aliased constant Code_Point_Array := (16#FF4E#,16#1000006E#);
   SFF4F : aliased constant Code_Point_Array := (16#FF4F#,16#1000006F#);
   SFF50 : aliased constant Code_Point_Array := (16#FF50#,16#10000070#);
   SFF51 : aliased constant Code_Point_Array := (16#FF51#,16#10000071#);
   SFF52 : aliased constant Code_Point_Array := (16#FF52#,16#10000072#);
   SFF53 : aliased constant Code_Point_Array := (16#FF53#,16#10000073#);
   SFF54 : aliased constant Code_Point_Array := (16#FF54#,16#10000074#);
   SFF55 : aliased constant Code_Point_Array := (16#FF55#,16#10000075#);
   SFF56 : aliased constant Code_Point_Array := (16#FF56#,16#10000076#);
   SFF57 : aliased constant Code_Point_Array := (16#FF57#,16#10000077#);
   SFF58 : aliased constant Code_Point_Array := (16#FF58#,16#10000078#);
   SFF59 : aliased constant Code_Point_Array := (16#FF59#,16#10000079#);
   SFF5A : aliased constant Code_Point_Array := (16#FF5A#,16#1000007A#);
   SFF5B : aliased constant Code_Point_Array := (16#FF5B#,16#1000007B#);
   SFF5C : aliased constant Code_Point_Array := (16#FF5C#,16#1000007C#);
   SFF5D : aliased constant Code_Point_Array := (16#FF5D#,16#1000007D#);
   SFF5E : aliased constant Code_Point_Array := (16#FF5E#,16#1000007E#);
   SFF5F : aliased constant Code_Point_Array := (16#FF5F#,16#10002985#);
   SFF60 : aliased constant Code_Point_Array := (16#FF60#,16#10002986#);
   SFF61 : aliased constant Code_Point_Array := (16#FF61#,16#10003002#);
   SFF62 : aliased constant Code_Point_Array := (16#FF62#,16#1000300C#);
   SFF63 : aliased constant Code_Point_Array := (16#FF63#,16#1000300D#);
   SFF64 : aliased constant Code_Point_Array := (16#FF64#,16#10003001#);
   SFF65 : aliased constant Code_Point_Array := (16#FF65#,16#100030FB#);
   SFF66 : aliased constant Code_Point_Array := (16#FF66#,16#100030F2#);
   SFF67 : aliased constant Code_Point_Array := (16#FF67#,16#100030A1#);
   SFF68 : aliased constant Code_Point_Array := (16#FF68#,16#100030A3#);
   SFF69 : aliased constant Code_Point_Array := (16#FF69#,16#100030A5#);
   SFF6A : aliased constant Code_Point_Array := (16#FF6A#,16#100030A7#);
   SFF6B : aliased constant Code_Point_Array := (16#FF6B#,16#100030A9#);
   SFF6C : aliased constant Code_Point_Array := (16#FF6C#,16#100030E3#);
   SFF6D : aliased constant Code_Point_Array := (16#FF6D#,16#100030E5#);
   SFF6E : aliased constant Code_Point_Array := (16#FF6E#,16#100030E7#);
   SFF6F : aliased constant Code_Point_Array := (16#FF6F#,16#100030C3#);
   SFF70 : aliased constant Code_Point_Array := (16#FF70#,16#100030FC#);
   SFF71 : aliased constant Code_Point_Array := (16#FF71#,16#100030A2#);
   SFF72 : aliased constant Code_Point_Array := (16#FF72#,16#100030A4#);
   SFF73 : aliased constant Code_Point_Array := (16#FF73#,16#100030A6#);
   SFF74 : aliased constant Code_Point_Array := (16#FF74#,16#100030A8#);
   SFF75 : aliased constant Code_Point_Array := (16#FF75#,16#100030AA#);
   SFF76 : aliased constant Code_Point_Array := (16#FF76#,16#100030AB#);
   SFF77 : aliased constant Code_Point_Array := (16#FF77#,16#100030AD#);
   SFF78 : aliased constant Code_Point_Array := (16#FF78#,16#100030AF#);
   SFF79 : aliased constant Code_Point_Array := (16#FF79#,16#100030B1#);
   SFF7A : aliased constant Code_Point_Array := (16#FF7A#,16#100030B3#);
   SFF7B : aliased constant Code_Point_Array := (16#FF7B#,16#100030B5#);
   SFF7C : aliased constant Code_Point_Array := (16#FF7C#,16#100030B7#);
   SFF7D : aliased constant Code_Point_Array := (16#FF7D#,16#100030B9#);
   SFF7E : aliased constant Code_Point_Array := (16#FF7E#,16#100030BB#);
   SFF7F : aliased constant Code_Point_Array := (16#FF7F#,16#100030BD#);
   SFF80 : aliased constant Code_Point_Array := (16#FF80#,16#100030BF#);
   SFF81 : aliased constant Code_Point_Array := (16#FF81#,16#100030C1#);
   SFF82 : aliased constant Code_Point_Array := (16#FF82#,16#100030C4#);
   SFF83 : aliased constant Code_Point_Array := (16#FF83#,16#100030C6#);
   SFF84 : aliased constant Code_Point_Array := (16#FF84#,16#100030C8#);
   SFF85 : aliased constant Code_Point_Array := (16#FF85#,16#100030CA#);
   SFF86 : aliased constant Code_Point_Array := (16#FF86#,16#100030CB#);
   SFF87 : aliased constant Code_Point_Array := (16#FF87#,16#100030CC#);
   SFF88 : aliased constant Code_Point_Array := (16#FF88#,16#100030CD#);
   SFF89 : aliased constant Code_Point_Array := (16#FF89#,16#100030CE#);
   SFF8A : aliased constant Code_Point_Array := (16#FF8A#,16#100030CF#);
   SFF8B : aliased constant Code_Point_Array := (16#FF8B#,16#100030D2#);
   SFF8C : aliased constant Code_Point_Array := (16#FF8C#,16#100030D5#);
   SFF8D : aliased constant Code_Point_Array := (16#FF8D#,16#100030D8#);
   SFF8E : aliased constant Code_Point_Array := (16#FF8E#,16#100030DB#);
   SFF8F : aliased constant Code_Point_Array := (16#FF8F#,16#100030DE#);
   SFF90 : aliased constant Code_Point_Array := (16#FF90#,16#100030DF#);
   SFF91 : aliased constant Code_Point_Array := (16#FF91#,16#100030E0#);
   SFF92 : aliased constant Code_Point_Array := (16#FF92#,16#100030E1#);
   SFF93 : aliased constant Code_Point_Array := (16#FF93#,16#100030E2#);
   SFF94 : aliased constant Code_Point_Array := (16#FF94#,16#100030E4#);
   SFF95 : aliased constant Code_Point_Array := (16#FF95#,16#100030E6#);
   SFF96 : aliased constant Code_Point_Array := (16#FF96#,16#100030E8#);
   SFF97 : aliased constant Code_Point_Array := (16#FF97#,16#100030E9#);
   SFF98 : aliased constant Code_Point_Array := (16#FF98#,16#100030EA#);
   SFF99 : aliased constant Code_Point_Array := (16#FF99#,16#100030EB#);
   SFF9A : aliased constant Code_Point_Array := (16#FF9A#,16#100030EC#);
   SFF9B : aliased constant Code_Point_Array := (16#FF9B#,16#100030ED#);
   SFF9C : aliased constant Code_Point_Array := (16#FF9C#,16#100030EF#);
   SFF9D : aliased constant Code_Point_Array := (16#FF9D#,16#100030F3#);
   SFF9E : aliased constant Code_Point_Array := (16#FF9E#,16#10003099#);
   SFF9F : aliased constant Code_Point_Array := (16#FF9F#,16#1000309A#);
   SFFA0 : aliased constant Code_Point_Array := (16#FFA0#,16#10003164#);
   SFFA1 : aliased constant Code_Point_Array := (16#FFA1#,16#10003131#);
   SFFA2 : aliased constant Code_Point_Array := (16#FFA2#,16#10003132#);
   SFFA3 : aliased constant Code_Point_Array := (16#FFA3#,16#10003133#);
   SFFA4 : aliased constant Code_Point_Array := (16#FFA4#,16#10003134#);
   SFFA5 : aliased constant Code_Point_Array := (16#FFA5#,16#10003135#);
   SFFA6 : aliased constant Code_Point_Array := (16#FFA6#,16#10003136#);
   SFFA7 : aliased constant Code_Point_Array := (16#FFA7#,16#10003137#);
   SFFA8 : aliased constant Code_Point_Array := (16#FFA8#,16#10003138#);
   SFFA9 : aliased constant Code_Point_Array := (16#FFA9#,16#10003139#);
   SFFAA : aliased constant Code_Point_Array := (16#FFAA#,16#1000313A#);
   SFFAB : aliased constant Code_Point_Array := (16#FFAB#,16#1000313B#);
   SFFAC : aliased constant Code_Point_Array := (16#FFAC#,16#1000313C#);
   SFFAD : aliased constant Code_Point_Array := (16#FFAD#,16#1000313D#);
   SFFAE : aliased constant Code_Point_Array := (16#FFAE#,16#1000313E#);
   SFFAF : aliased constant Code_Point_Array := (16#FFAF#,16#1000313F#);
   SFFB0 : aliased constant Code_Point_Array := (16#FFB0#,16#10003140#);
   SFFB1 : aliased constant Code_Point_Array := (16#FFB1#,16#10003141#);
   SFFB2 : aliased constant Code_Point_Array := (16#FFB2#,16#10003142#);
   SFFB3 : aliased constant Code_Point_Array := (16#FFB3#,16#10003143#);
   SFFB4 : aliased constant Code_Point_Array := (16#FFB4#,16#10003144#);
   SFFB5 : aliased constant Code_Point_Array := (16#FFB5#,16#10003145#);
   SFFB6 : aliased constant Code_Point_Array := (16#FFB6#,16#10003146#);
   SFFB7 : aliased constant Code_Point_Array := (16#FFB7#,16#10003147#);
   SFFB8 : aliased constant Code_Point_Array := (16#FFB8#,16#10003148#);
   SFFB9 : aliased constant Code_Point_Array := (16#FFB9#,16#10003149#);
   SFFBA : aliased constant Code_Point_Array := (16#FFBA#,16#1000314A#);
   SFFBB : aliased constant Code_Point_Array := (16#FFBB#,16#1000314B#);
   SFFBC : aliased constant Code_Point_Array := (16#FFBC#,16#1000314C#);
   SFFBD : aliased constant Code_Point_Array := (16#FFBD#,16#1000314D#);
   SFFBE : aliased constant Code_Point_Array := (16#FFBE#,16#1000314E#);
   SFFC2 : aliased constant Code_Point_Array := (16#FFC2#,16#1000314F#);
   SFFC3 : aliased constant Code_Point_Array := (16#FFC3#,16#10003150#);
   SFFC4 : aliased constant Code_Point_Array := (16#FFC4#,16#10003151#);
   SFFC5 : aliased constant Code_Point_Array := (16#FFC5#,16#10003152#);
   SFFC6 : aliased constant Code_Point_Array := (16#FFC6#,16#10003153#);
   SFFC7 : aliased constant Code_Point_Array := (16#FFC7#,16#10003154#);
   SFFCA : aliased constant Code_Point_Array := (16#FFCA#,16#10003155#);
   SFFCB : aliased constant Code_Point_Array := (16#FFCB#,16#10003156#);
   SFFCC : aliased constant Code_Point_Array := (16#FFCC#,16#10003157#);
   SFFCD : aliased constant Code_Point_Array := (16#FFCD#,16#10003158#);
   SFFCE : aliased constant Code_Point_Array := (16#FFCE#,16#10003159#);
   SFFCF : aliased constant Code_Point_Array := (16#FFCF#,16#1000315A#);
   SFFD2 : aliased constant Code_Point_Array := (16#FFD2#,16#1000315B#);
   SFFD3 : aliased constant Code_Point_Array := (16#FFD3#,16#1000315C#);
   SFFD4 : aliased constant Code_Point_Array := (16#FFD4#,16#1000315D#);
   SFFD5 : aliased constant Code_Point_Array := (16#FFD5#,16#1000315E#);
   SFFD6 : aliased constant Code_Point_Array := (16#FFD6#,16#1000315F#);
   SFFD7 : aliased constant Code_Point_Array := (16#FFD7#,16#10003160#);
   SFFDA : aliased constant Code_Point_Array := (16#FFDA#,16#10003161#);
   SFFDB : aliased constant Code_Point_Array := (16#FFDB#,16#10003162#);
   SFFDC : aliased constant Code_Point_Array := (16#FFDC#,16#10003163#);
   SFFE0 : aliased constant Code_Point_Array := (16#FFE0#,16#100000A2#);
   SFFE1 : aliased constant Code_Point_Array := (16#FFE1#,16#100000A3#);
   SFFE2 : aliased constant Code_Point_Array := (16#FFE2#,16#100000AC#);
   SFFE3 : aliased constant Code_Point_Array := (16#FFE3#,16#100000AF#);
   SFFE4 : aliased constant Code_Point_Array := (16#FFE4#,16#100000A6#);
   SFFE5 : aliased constant Code_Point_Array := (16#FFE5#,16#100000A5#);
   SFFE6 : aliased constant Code_Point_Array := (16#FFE6#,16#100020A9#);
   SFFE8 : aliased constant Code_Point_Array := (16#FFE8#,16#10002502#);
   SFFE9 : aliased constant Code_Point_Array := (16#FFE9#,16#10002190#);
   SFFEA : aliased constant Code_Point_Array := (16#FFEA#,16#10002191#);
   SFFEB : aliased constant Code_Point_Array := (16#FFEB#,16#10002192#);
   SFFEC : aliased constant Code_Point_Array := (16#FFEC#,16#10002193#);
   SFFED : aliased constant Code_Point_Array := (16#FFED#,16#100025A0#);
   SFFEE : aliased constant Code_Point_Array := (16#FFEE#,16#100025CB#);
   S10781 : aliased constant Code_Point_Array := (16#10781#,16#100002D0#);
   S10782 : aliased constant Code_Point_Array := (16#10782#,16#100002D1#);
   S10783 : aliased constant Code_Point_Array := (16#10783#,16#100000E6#);
   S10784 : aliased constant Code_Point_Array := (16#10784#,16#10000299#);
   S10785 : aliased constant Code_Point_Array := (16#10785#,16#10000253#);
   S10787 : aliased constant Code_Point_Array := (16#10787#,16#100002A3#);
   S10788 : aliased constant Code_Point_Array := (16#10788#,16#1000AB66#);
   S10789 : aliased constant Code_Point_Array := (16#10789#,16#100002A5#);
   S1078A : aliased constant Code_Point_Array := (16#1078A#,16#100002A4#);
   S1078B : aliased constant Code_Point_Array := (16#1078B#,16#10000256#);
   S1078C : aliased constant Code_Point_Array := (16#1078C#,16#10000257#);
   S1078D : aliased constant Code_Point_Array := (16#1078D#,16#10001D91#);
   S1078E : aliased constant Code_Point_Array := (16#1078E#,16#10000258#);
   S1078F : aliased constant Code_Point_Array := (16#1078F#,16#1000025E#);
   S10790 : aliased constant Code_Point_Array := (16#10790#,16#100002A9#);
   S10791 : aliased constant Code_Point_Array := (16#10791#,16#10000264#);
   S10792 : aliased constant Code_Point_Array := (16#10792#,16#10000262#);
   S10793 : aliased constant Code_Point_Array := (16#10793#,16#10000260#);
   S10794 : aliased constant Code_Point_Array := (16#10794#,16#1000029B#);
   S10795 : aliased constant Code_Point_Array := (16#10795#,16#10000127#);
   S10796 : aliased constant Code_Point_Array := (16#10796#,16#1000029C#);
   S10797 : aliased constant Code_Point_Array := (16#10797#,16#10000267#);
   S10798 : aliased constant Code_Point_Array := (16#10798#,16#10000284#);
   S10799 : aliased constant Code_Point_Array := (16#10799#,16#100002AA#);
   S1079A : aliased constant Code_Point_Array := (16#1079A#,16#100002AB#);
   S1079B : aliased constant Code_Point_Array := (16#1079B#,16#1000026C#);
   S1079C : aliased constant Code_Point_Array := (16#1079C#,16#1001DF04#);
   S1079D : aliased constant Code_Point_Array := (16#1079D#,16#1000A78E#);
   S1079E : aliased constant Code_Point_Array := (16#1079E#,16#1000026E#);
   S1079F : aliased constant Code_Point_Array := (16#1079F#,16#1001DF05#);
   S107A0 : aliased constant Code_Point_Array := (16#107A0#,16#1000028E#);
   S107A1 : aliased constant Code_Point_Array := (16#107A1#,16#1001DF06#);
   S107A2 : aliased constant Code_Point_Array := (16#107A2#,16#100000F8#);
   S107A3 : aliased constant Code_Point_Array := (16#107A3#,16#10000276#);
   S107A4 : aliased constant Code_Point_Array := (16#107A4#,16#10000277#);
   S107A5 : aliased constant Code_Point_Array := (16#107A5#,16#10000071#);
   S107A6 : aliased constant Code_Point_Array := (16#107A6#,16#1000027A#);
   S107A7 : aliased constant Code_Point_Array := (16#107A7#,16#1001DF08#);
   S107A8 : aliased constant Code_Point_Array := (16#107A8#,16#1000027D#);
   S107A9 : aliased constant Code_Point_Array := (16#107A9#,16#1000027E#);
   S107AA : aliased constant Code_Point_Array := (16#107AA#,16#10000280#);
   S107AB : aliased constant Code_Point_Array := (16#107AB#,16#100002A8#);
   S107AC : aliased constant Code_Point_Array := (16#107AC#,16#100002A6#);
   S107AD : aliased constant Code_Point_Array := (16#107AD#,16#1000AB67#);
   S107AE : aliased constant Code_Point_Array := (16#107AE#,16#100002A7#);
   S107AF : aliased constant Code_Point_Array := (16#107AF#,16#10000288#);
   S107B0 : aliased constant Code_Point_Array := (16#107B0#,16#10002C71#);
   S107B2 : aliased constant Code_Point_Array := (16#107B2#,16#1000028F#);
   S107B3 : aliased constant Code_Point_Array := (16#107B3#,16#100002A1#);
   S107B4 : aliased constant Code_Point_Array := (16#107B4#,16#100002A2#);
   S107B5 : aliased constant Code_Point_Array := (16#107B5#,16#10000298#);
   S107B6 : aliased constant Code_Point_Array := (16#107B6#,16#100001C0#);
   S107B7 : aliased constant Code_Point_Array := (16#107B7#,16#100001C1#);
   S107B8 : aliased constant Code_Point_Array := (16#107B8#,16#100001C2#);
   S107B9 : aliased constant Code_Point_Array := (16#107B9#,16#1001DF0A#);
   S107BA : aliased constant Code_Point_Array := (16#107BA#,16#1001DF1E#);
   S1109A : aliased constant Code_Point_Array := (16#1109A#,16#11099#,16#110BA#);
   S1109C : aliased constant Code_Point_Array := (16#1109C#,16#1109B#,16#110BA#);
   S110AB : aliased constant Code_Point_Array := (16#110AB#,16#110A5#,16#110BA#);
   S1112E : aliased constant Code_Point_Array := (16#1112E#,16#11131#,16#11127#);
   S1112F : aliased constant Code_Point_Array := (16#1112F#,16#11132#,16#11127#);
   S1134B : aliased constant Code_Point_Array := (16#1134B#,16#11347#,16#1133E#);
   S1134C : aliased constant Code_Point_Array := (16#1134C#,16#11347#,16#11357#);
   S114BB : aliased constant Code_Point_Array := (16#114BB#,16#114B9#,16#114BA#);
   S114BC : aliased constant Code_Point_Array := (16#114BC#,16#114B9#,16#114B0#);
   S114BE : aliased constant Code_Point_Array := (16#114BE#,16#114B9#,16#114BD#);
   S115BA : aliased constant Code_Point_Array := (16#115BA#,16#115B8#,16#115AF#);
   S115BB : aliased constant Code_Point_Array := (16#115BB#,16#115B9#,16#115AF#);
   S11938 : aliased constant Code_Point_Array := (16#11938#,16#11935#,16#11930#);
   S1D15E : aliased constant Code_Point_Array := (16#1D15E#,16#1D157#,16#1D165#);
   S1D15F : aliased constant Code_Point_Array := (16#1D15F#,16#1D158#,16#1D165#);
   S1D160 : aliased constant Code_Point_Array := (16#1D160#,16#1D15F#,16#1D16E#);
   S1D161 : aliased constant Code_Point_Array := (16#1D161#,16#1D15F#,16#1D16F#);
   S1D162 : aliased constant Code_Point_Array := (16#1D162#,16#1D15F#,16#1D170#);
   S1D163 : aliased constant Code_Point_Array := (16#1D163#,16#1D15F#,16#1D171#);
   S1D164 : aliased constant Code_Point_Array := (16#1D164#,16#1D15F#,16#1D172#);
   S1D1BB : aliased constant Code_Point_Array := (16#1D1BB#,16#1D1B9#,16#1D165#);
   S1D1BC : aliased constant Code_Point_Array := (16#1D1BC#,16#1D1BA#,16#1D165#);
   S1D1BD : aliased constant Code_Point_Array := (16#1D1BD#,16#1D1BB#,16#1D16E#);
   S1D1BE : aliased constant Code_Point_Array := (16#1D1BE#,16#1D1BC#,16#1D16E#);
   S1D1BF : aliased constant Code_Point_Array := (16#1D1BF#,16#1D1BB#,16#1D16F#);
   S1D1C0 : aliased constant Code_Point_Array := (16#1D1C0#,16#1D1BC#,16#1D16F#);
   S1D400 : aliased constant Code_Point_Array := (16#1D400#,16#10000041#);
   S1D401 : aliased constant Code_Point_Array := (16#1D401#,16#10000042#);
   S1D402 : aliased constant Code_Point_Array := (16#1D402#,16#10000043#);
   S1D403 : aliased constant Code_Point_Array := (16#1D403#,16#10000044#);
   S1D404 : aliased constant Code_Point_Array := (16#1D404#,16#10000045#);
   S1D405 : aliased constant Code_Point_Array := (16#1D405#,16#10000046#);
   S1D406 : aliased constant Code_Point_Array := (16#1D406#,16#10000047#);
   S1D407 : aliased constant Code_Point_Array := (16#1D407#,16#10000048#);
   S1D408 : aliased constant Code_Point_Array := (16#1D408#,16#10000049#);
   S1D409 : aliased constant Code_Point_Array := (16#1D409#,16#1000004A#);
   S1D40A : aliased constant Code_Point_Array := (16#1D40A#,16#1000004B#);
   S1D40B : aliased constant Code_Point_Array := (16#1D40B#,16#1000004C#);
   S1D40C : aliased constant Code_Point_Array := (16#1D40C#,16#1000004D#);
   S1D40D : aliased constant Code_Point_Array := (16#1D40D#,16#1000004E#);
   S1D40E : aliased constant Code_Point_Array := (16#1D40E#,16#1000004F#);
   S1D40F : aliased constant Code_Point_Array := (16#1D40F#,16#10000050#);
   S1D410 : aliased constant Code_Point_Array := (16#1D410#,16#10000051#);
   S1D411 : aliased constant Code_Point_Array := (16#1D411#,16#10000052#);
   S1D412 : aliased constant Code_Point_Array := (16#1D412#,16#10000053#);
   S1D413 : aliased constant Code_Point_Array := (16#1D413#,16#10000054#);
   S1D414 : aliased constant Code_Point_Array := (16#1D414#,16#10000055#);
   S1D415 : aliased constant Code_Point_Array := (16#1D415#,16#10000056#);
   S1D416 : aliased constant Code_Point_Array := (16#1D416#,16#10000057#);
   S1D417 : aliased constant Code_Point_Array := (16#1D417#,16#10000058#);
   S1D418 : aliased constant Code_Point_Array := (16#1D418#,16#10000059#);
   S1D419 : aliased constant Code_Point_Array := (16#1D419#,16#1000005A#);
   S1D41A : aliased constant Code_Point_Array := (16#1D41A#,16#10000061#);
   S1D41B : aliased constant Code_Point_Array := (16#1D41B#,16#10000062#);
   S1D41C : aliased constant Code_Point_Array := (16#1D41C#,16#10000063#);
   S1D41D : aliased constant Code_Point_Array := (16#1D41D#,16#10000064#);
   S1D41E : aliased constant Code_Point_Array := (16#1D41E#,16#10000065#);
   S1D41F : aliased constant Code_Point_Array := (16#1D41F#,16#10000066#);
   S1D420 : aliased constant Code_Point_Array := (16#1D420#,16#10000067#);
   S1D421 : aliased constant Code_Point_Array := (16#1D421#,16#10000068#);
   S1D422 : aliased constant Code_Point_Array := (16#1D422#,16#10000069#);
   S1D423 : aliased constant Code_Point_Array := (16#1D423#,16#1000006A#);
   S1D424 : aliased constant Code_Point_Array := (16#1D424#,16#1000006B#);
   S1D425 : aliased constant Code_Point_Array := (16#1D425#,16#1000006C#);
   S1D426 : aliased constant Code_Point_Array := (16#1D426#,16#1000006D#);
   S1D427 : aliased constant Code_Point_Array := (16#1D427#,16#1000006E#);
   S1D428 : aliased constant Code_Point_Array := (16#1D428#,16#1000006F#);
   S1D429 : aliased constant Code_Point_Array := (16#1D429#,16#10000070#);
   S1D42A : aliased constant Code_Point_Array := (16#1D42A#,16#10000071#);
   S1D42B : aliased constant Code_Point_Array := (16#1D42B#,16#10000072#);
   S1D42C : aliased constant Code_Point_Array := (16#1D42C#,16#10000073#);
   S1D42D : aliased constant Code_Point_Array := (16#1D42D#,16#10000074#);
   S1D42E : aliased constant Code_Point_Array := (16#1D42E#,16#10000075#);
   S1D42F : aliased constant Code_Point_Array := (16#1D42F#,16#10000076#);
   S1D430 : aliased constant Code_Point_Array := (16#1D430#,16#10000077#);
   S1D431 : aliased constant Code_Point_Array := (16#1D431#,16#10000078#);
   S1D432 : aliased constant Code_Point_Array := (16#1D432#,16#10000079#);
   S1D433 : aliased constant Code_Point_Array := (16#1D433#,16#1000007A#);
   S1D434 : aliased constant Code_Point_Array := (16#1D434#,16#10000041#);
   S1D435 : aliased constant Code_Point_Array := (16#1D435#,16#10000042#);
   S1D436 : aliased constant Code_Point_Array := (16#1D436#,16#10000043#);
   S1D437 : aliased constant Code_Point_Array := (16#1D437#,16#10000044#);
   S1D438 : aliased constant Code_Point_Array := (16#1D438#,16#10000045#);
   S1D439 : aliased constant Code_Point_Array := (16#1D439#,16#10000046#);
   S1D43A : aliased constant Code_Point_Array := (16#1D43A#,16#10000047#);
   S1D43B : aliased constant Code_Point_Array := (16#1D43B#,16#10000048#);
   S1D43C : aliased constant Code_Point_Array := (16#1D43C#,16#10000049#);
   S1D43D : aliased constant Code_Point_Array := (16#1D43D#,16#1000004A#);
   S1D43E : aliased constant Code_Point_Array := (16#1D43E#,16#1000004B#);
   S1D43F : aliased constant Code_Point_Array := (16#1D43F#,16#1000004C#);
   S1D440 : aliased constant Code_Point_Array := (16#1D440#,16#1000004D#);
   S1D441 : aliased constant Code_Point_Array := (16#1D441#,16#1000004E#);
   S1D442 : aliased constant Code_Point_Array := (16#1D442#,16#1000004F#);
   S1D443 : aliased constant Code_Point_Array := (16#1D443#,16#10000050#);
   S1D444 : aliased constant Code_Point_Array := (16#1D444#,16#10000051#);
   S1D445 : aliased constant Code_Point_Array := (16#1D445#,16#10000052#);
   S1D446 : aliased constant Code_Point_Array := (16#1D446#,16#10000053#);
   S1D447 : aliased constant Code_Point_Array := (16#1D447#,16#10000054#);
   S1D448 : aliased constant Code_Point_Array := (16#1D448#,16#10000055#);
   S1D449 : aliased constant Code_Point_Array := (16#1D449#,16#10000056#);
   S1D44A : aliased constant Code_Point_Array := (16#1D44A#,16#10000057#);
   S1D44B : aliased constant Code_Point_Array := (16#1D44B#,16#10000058#);
   S1D44C : aliased constant Code_Point_Array := (16#1D44C#,16#10000059#);
   S1D44D : aliased constant Code_Point_Array := (16#1D44D#,16#1000005A#);
   S1D44E : aliased constant Code_Point_Array := (16#1D44E#,16#10000061#);
   S1D44F : aliased constant Code_Point_Array := (16#1D44F#,16#10000062#);
   S1D450 : aliased constant Code_Point_Array := (16#1D450#,16#10000063#);
   S1D451 : aliased constant Code_Point_Array := (16#1D451#,16#10000064#);
   S1D452 : aliased constant Code_Point_Array := (16#1D452#,16#10000065#);
   S1D453 : aliased constant Code_Point_Array := (16#1D453#,16#10000066#);
   S1D454 : aliased constant Code_Point_Array := (16#1D454#,16#10000067#);
   S1D456 : aliased constant Code_Point_Array := (16#1D456#,16#10000069#);
   S1D457 : aliased constant Code_Point_Array := (16#1D457#,16#1000006A#);
   S1D458 : aliased constant Code_Point_Array := (16#1D458#,16#1000006B#);
   S1D459 : aliased constant Code_Point_Array := (16#1D459#,16#1000006C#);
   S1D45A : aliased constant Code_Point_Array := (16#1D45A#,16#1000006D#);
   S1D45B : aliased constant Code_Point_Array := (16#1D45B#,16#1000006E#);
   S1D45C : aliased constant Code_Point_Array := (16#1D45C#,16#1000006F#);
   S1D45D : aliased constant Code_Point_Array := (16#1D45D#,16#10000070#);
   S1D45E : aliased constant Code_Point_Array := (16#1D45E#,16#10000071#);
   S1D45F : aliased constant Code_Point_Array := (16#1D45F#,16#10000072#);
   S1D460 : aliased constant Code_Point_Array := (16#1D460#,16#10000073#);
   S1D461 : aliased constant Code_Point_Array := (16#1D461#,16#10000074#);
   S1D462 : aliased constant Code_Point_Array := (16#1D462#,16#10000075#);
   S1D463 : aliased constant Code_Point_Array := (16#1D463#,16#10000076#);
   S1D464 : aliased constant Code_Point_Array := (16#1D464#,16#10000077#);
   S1D465 : aliased constant Code_Point_Array := (16#1D465#,16#10000078#);
   S1D466 : aliased constant Code_Point_Array := (16#1D466#,16#10000079#);
   S1D467 : aliased constant Code_Point_Array := (16#1D467#,16#1000007A#);
   S1D468 : aliased constant Code_Point_Array := (16#1D468#,16#10000041#);
   S1D469 : aliased constant Code_Point_Array := (16#1D469#,16#10000042#);
   S1D46A : aliased constant Code_Point_Array := (16#1D46A#,16#10000043#);
   S1D46B : aliased constant Code_Point_Array := (16#1D46B#,16#10000044#);
   S1D46C : aliased constant Code_Point_Array := (16#1D46C#,16#10000045#);
   S1D46D : aliased constant Code_Point_Array := (16#1D46D#,16#10000046#);
   S1D46E : aliased constant Code_Point_Array := (16#1D46E#,16#10000047#);
   S1D46F : aliased constant Code_Point_Array := (16#1D46F#,16#10000048#);
   S1D470 : aliased constant Code_Point_Array := (16#1D470#,16#10000049#);
   S1D471 : aliased constant Code_Point_Array := (16#1D471#,16#1000004A#);
   S1D472 : aliased constant Code_Point_Array := (16#1D472#,16#1000004B#);
   S1D473 : aliased constant Code_Point_Array := (16#1D473#,16#1000004C#);
   S1D474 : aliased constant Code_Point_Array := (16#1D474#,16#1000004D#);
   S1D475 : aliased constant Code_Point_Array := (16#1D475#,16#1000004E#);
   S1D476 : aliased constant Code_Point_Array := (16#1D476#,16#1000004F#);
   S1D477 : aliased constant Code_Point_Array := (16#1D477#,16#10000050#);
   S1D478 : aliased constant Code_Point_Array := (16#1D478#,16#10000051#);
   S1D479 : aliased constant Code_Point_Array := (16#1D479#,16#10000052#);
   S1D47A : aliased constant Code_Point_Array := (16#1D47A#,16#10000053#);
   S1D47B : aliased constant Code_Point_Array := (16#1D47B#,16#10000054#);
   S1D47C : aliased constant Code_Point_Array := (16#1D47C#,16#10000055#);
   S1D47D : aliased constant Code_Point_Array := (16#1D47D#,16#10000056#);
   S1D47E : aliased constant Code_Point_Array := (16#1D47E#,16#10000057#);
   S1D47F : aliased constant Code_Point_Array := (16#1D47F#,16#10000058#);
   S1D480 : aliased constant Code_Point_Array := (16#1D480#,16#10000059#);
   S1D481 : aliased constant Code_Point_Array := (16#1D481#,16#1000005A#);
   S1D482 : aliased constant Code_Point_Array := (16#1D482#,16#10000061#);
   S1D483 : aliased constant Code_Point_Array := (16#1D483#,16#10000062#);
   S1D484 : aliased constant Code_Point_Array := (16#1D484#,16#10000063#);
   S1D485 : aliased constant Code_Point_Array := (16#1D485#,16#10000064#);
   S1D486 : aliased constant Code_Point_Array := (16#1D486#,16#10000065#);
   S1D487 : aliased constant Code_Point_Array := (16#1D487#,16#10000066#);
   S1D488 : aliased constant Code_Point_Array := (16#1D488#,16#10000067#);
   S1D489 : aliased constant Code_Point_Array := (16#1D489#,16#10000068#);
   S1D48A : aliased constant Code_Point_Array := (16#1D48A#,16#10000069#);
   S1D48B : aliased constant Code_Point_Array := (16#1D48B#,16#1000006A#);
   S1D48C : aliased constant Code_Point_Array := (16#1D48C#,16#1000006B#);
   S1D48D : aliased constant Code_Point_Array := (16#1D48D#,16#1000006C#);
   S1D48E : aliased constant Code_Point_Array := (16#1D48E#,16#1000006D#);
   S1D48F : aliased constant Code_Point_Array := (16#1D48F#,16#1000006E#);
   S1D490 : aliased constant Code_Point_Array := (16#1D490#,16#1000006F#);
   S1D491 : aliased constant Code_Point_Array := (16#1D491#,16#10000070#);
   S1D492 : aliased constant Code_Point_Array := (16#1D492#,16#10000071#);
   S1D493 : aliased constant Code_Point_Array := (16#1D493#,16#10000072#);
   S1D494 : aliased constant Code_Point_Array := (16#1D494#,16#10000073#);
   S1D495 : aliased constant Code_Point_Array := (16#1D495#,16#10000074#);
   S1D496 : aliased constant Code_Point_Array := (16#1D496#,16#10000075#);
   S1D497 : aliased constant Code_Point_Array := (16#1D497#,16#10000076#);
   S1D498 : aliased constant Code_Point_Array := (16#1D498#,16#10000077#);
   S1D499 : aliased constant Code_Point_Array := (16#1D499#,16#10000078#);
   S1D49A : aliased constant Code_Point_Array := (16#1D49A#,16#10000079#);
   S1D49B : aliased constant Code_Point_Array := (16#1D49B#,16#1000007A#);
   S1D49C : aliased constant Code_Point_Array := (16#1D49C#,16#10000041#);
   S1D49E : aliased constant Code_Point_Array := (16#1D49E#,16#10000043#);
   S1D49F : aliased constant Code_Point_Array := (16#1D49F#,16#10000044#);
   S1D4A2 : aliased constant Code_Point_Array := (16#1D4A2#,16#10000047#);
   S1D4A5 : aliased constant Code_Point_Array := (16#1D4A5#,16#1000004A#);
   S1D4A6 : aliased constant Code_Point_Array := (16#1D4A6#,16#1000004B#);
   S1D4A9 : aliased constant Code_Point_Array := (16#1D4A9#,16#1000004E#);
   S1D4AA : aliased constant Code_Point_Array := (16#1D4AA#,16#1000004F#);
   S1D4AB : aliased constant Code_Point_Array := (16#1D4AB#,16#10000050#);
   S1D4AC : aliased constant Code_Point_Array := (16#1D4AC#,16#10000051#);
   S1D4AE : aliased constant Code_Point_Array := (16#1D4AE#,16#10000053#);
   S1D4AF : aliased constant Code_Point_Array := (16#1D4AF#,16#10000054#);
   S1D4B0 : aliased constant Code_Point_Array := (16#1D4B0#,16#10000055#);
   S1D4B1 : aliased constant Code_Point_Array := (16#1D4B1#,16#10000056#);
   S1D4B2 : aliased constant Code_Point_Array := (16#1D4B2#,16#10000057#);
   S1D4B3 : aliased constant Code_Point_Array := (16#1D4B3#,16#10000058#);
   S1D4B4 : aliased constant Code_Point_Array := (16#1D4B4#,16#10000059#);
   S1D4B5 : aliased constant Code_Point_Array := (16#1D4B5#,16#1000005A#);
   S1D4B6 : aliased constant Code_Point_Array := (16#1D4B6#,16#10000061#);
   S1D4B7 : aliased constant Code_Point_Array := (16#1D4B7#,16#10000062#);
   S1D4B8 : aliased constant Code_Point_Array := (16#1D4B8#,16#10000063#);
   S1D4B9 : aliased constant Code_Point_Array := (16#1D4B9#,16#10000064#);
   S1D4BB : aliased constant Code_Point_Array := (16#1D4BB#,16#10000066#);
   S1D4BD : aliased constant Code_Point_Array := (16#1D4BD#,16#10000068#);
   S1D4BE : aliased constant Code_Point_Array := (16#1D4BE#,16#10000069#);
   S1D4BF : aliased constant Code_Point_Array := (16#1D4BF#,16#1000006A#);
   S1D4C0 : aliased constant Code_Point_Array := (16#1D4C0#,16#1000006B#);
   S1D4C1 : aliased constant Code_Point_Array := (16#1D4C1#,16#1000006C#);
   S1D4C2 : aliased constant Code_Point_Array := (16#1D4C2#,16#1000006D#);
   S1D4C3 : aliased constant Code_Point_Array := (16#1D4C3#,16#1000006E#);
   S1D4C5 : aliased constant Code_Point_Array := (16#1D4C5#,16#10000070#);
   S1D4C6 : aliased constant Code_Point_Array := (16#1D4C6#,16#10000071#);
   S1D4C7 : aliased constant Code_Point_Array := (16#1D4C7#,16#10000072#);
   S1D4C8 : aliased constant Code_Point_Array := (16#1D4C8#,16#10000073#);
   S1D4C9 : aliased constant Code_Point_Array := (16#1D4C9#,16#10000074#);
   S1D4CA : aliased constant Code_Point_Array := (16#1D4CA#,16#10000075#);
   S1D4CB : aliased constant Code_Point_Array := (16#1D4CB#,16#10000076#);
   S1D4CC : aliased constant Code_Point_Array := (16#1D4CC#,16#10000077#);
   S1D4CD : aliased constant Code_Point_Array := (16#1D4CD#,16#10000078#);
   S1D4CE : aliased constant Code_Point_Array := (16#1D4CE#,16#10000079#);
   S1D4CF : aliased constant Code_Point_Array := (16#1D4CF#,16#1000007A#);
   S1D4D0 : aliased constant Code_Point_Array := (16#1D4D0#,16#10000041#);
   S1D4D1 : aliased constant Code_Point_Array := (16#1D4D1#,16#10000042#);
   S1D4D2 : aliased constant Code_Point_Array := (16#1D4D2#,16#10000043#);
   S1D4D3 : aliased constant Code_Point_Array := (16#1D4D3#,16#10000044#);
   S1D4D4 : aliased constant Code_Point_Array := (16#1D4D4#,16#10000045#);
   S1D4D5 : aliased constant Code_Point_Array := (16#1D4D5#,16#10000046#);
   S1D4D6 : aliased constant Code_Point_Array := (16#1D4D6#,16#10000047#);
   S1D4D7 : aliased constant Code_Point_Array := (16#1D4D7#,16#10000048#);
   S1D4D8 : aliased constant Code_Point_Array := (16#1D4D8#,16#10000049#);
   S1D4D9 : aliased constant Code_Point_Array := (16#1D4D9#,16#1000004A#);
   S1D4DA : aliased constant Code_Point_Array := (16#1D4DA#,16#1000004B#);
   S1D4DB : aliased constant Code_Point_Array := (16#1D4DB#,16#1000004C#);
   S1D4DC : aliased constant Code_Point_Array := (16#1D4DC#,16#1000004D#);
   S1D4DD : aliased constant Code_Point_Array := (16#1D4DD#,16#1000004E#);
   S1D4DE : aliased constant Code_Point_Array := (16#1D4DE#,16#1000004F#);
   S1D4DF : aliased constant Code_Point_Array := (16#1D4DF#,16#10000050#);
   S1D4E0 : aliased constant Code_Point_Array := (16#1D4E0#,16#10000051#);
   S1D4E1 : aliased constant Code_Point_Array := (16#1D4E1#,16#10000052#);
   S1D4E2 : aliased constant Code_Point_Array := (16#1D4E2#,16#10000053#);
   S1D4E3 : aliased constant Code_Point_Array := (16#1D4E3#,16#10000054#);
   S1D4E4 : aliased constant Code_Point_Array := (16#1D4E4#,16#10000055#);
   S1D4E5 : aliased constant Code_Point_Array := (16#1D4E5#,16#10000056#);
   S1D4E6 : aliased constant Code_Point_Array := (16#1D4E6#,16#10000057#);
   S1D4E7 : aliased constant Code_Point_Array := (16#1D4E7#,16#10000058#);
   S1D4E8 : aliased constant Code_Point_Array := (16#1D4E8#,16#10000059#);
   S1D4E9 : aliased constant Code_Point_Array := (16#1D4E9#,16#1000005A#);
   S1D4EA : aliased constant Code_Point_Array := (16#1D4EA#,16#10000061#);
   S1D4EB : aliased constant Code_Point_Array := (16#1D4EB#,16#10000062#);
   S1D4EC : aliased constant Code_Point_Array := (16#1D4EC#,16#10000063#);
   S1D4ED : aliased constant Code_Point_Array := (16#1D4ED#,16#10000064#);
   S1D4EE : aliased constant Code_Point_Array := (16#1D4EE#,16#10000065#);
   S1D4EF : aliased constant Code_Point_Array := (16#1D4EF#,16#10000066#);
   S1D4F0 : aliased constant Code_Point_Array := (16#1D4F0#,16#10000067#);
   S1D4F1 : aliased constant Code_Point_Array := (16#1D4F1#,16#10000068#);
   S1D4F2 : aliased constant Code_Point_Array := (16#1D4F2#,16#10000069#);
   S1D4F3 : aliased constant Code_Point_Array := (16#1D4F3#,16#1000006A#);
   S1D4F4 : aliased constant Code_Point_Array := (16#1D4F4#,16#1000006B#);
   S1D4F5 : aliased constant Code_Point_Array := (16#1D4F5#,16#1000006C#);
   S1D4F6 : aliased constant Code_Point_Array := (16#1D4F6#,16#1000006D#);
   S1D4F7 : aliased constant Code_Point_Array := (16#1D4F7#,16#1000006E#);
   S1D4F8 : aliased constant Code_Point_Array := (16#1D4F8#,16#1000006F#);
   S1D4F9 : aliased constant Code_Point_Array := (16#1D4F9#,16#10000070#);
   S1D4FA : aliased constant Code_Point_Array := (16#1D4FA#,16#10000071#);
   S1D4FB : aliased constant Code_Point_Array := (16#1D4FB#,16#10000072#);
   S1D4FC : aliased constant Code_Point_Array := (16#1D4FC#,16#10000073#);
   S1D4FD : aliased constant Code_Point_Array := (16#1D4FD#,16#10000074#);
   S1D4FE : aliased constant Code_Point_Array := (16#1D4FE#,16#10000075#);
   S1D4FF : aliased constant Code_Point_Array := (16#1D4FF#,16#10000076#);
   S1D500 : aliased constant Code_Point_Array := (16#1D500#,16#10000077#);
   S1D501 : aliased constant Code_Point_Array := (16#1D501#,16#10000078#);
   S1D502 : aliased constant Code_Point_Array := (16#1D502#,16#10000079#);
   S1D503 : aliased constant Code_Point_Array := (16#1D503#,16#1000007A#);
   S1D504 : aliased constant Code_Point_Array := (16#1D504#,16#10000041#);
   S1D505 : aliased constant Code_Point_Array := (16#1D505#,16#10000042#);
   S1D507 : aliased constant Code_Point_Array := (16#1D507#,16#10000044#);
   S1D508 : aliased constant Code_Point_Array := (16#1D508#,16#10000045#);
   S1D509 : aliased constant Code_Point_Array := (16#1D509#,16#10000046#);
   S1D50A : aliased constant Code_Point_Array := (16#1D50A#,16#10000047#);
   S1D50D : aliased constant Code_Point_Array := (16#1D50D#,16#1000004A#);
   S1D50E : aliased constant Code_Point_Array := (16#1D50E#,16#1000004B#);
   S1D50F : aliased constant Code_Point_Array := (16#1D50F#,16#1000004C#);
   S1D510 : aliased constant Code_Point_Array := (16#1D510#,16#1000004D#);
   S1D511 : aliased constant Code_Point_Array := (16#1D511#,16#1000004E#);
   S1D512 : aliased constant Code_Point_Array := (16#1D512#,16#1000004F#);
   S1D513 : aliased constant Code_Point_Array := (16#1D513#,16#10000050#);
   S1D514 : aliased constant Code_Point_Array := (16#1D514#,16#10000051#);
   S1D516 : aliased constant Code_Point_Array := (16#1D516#,16#10000053#);
   S1D517 : aliased constant Code_Point_Array := (16#1D517#,16#10000054#);
   S1D518 : aliased constant Code_Point_Array := (16#1D518#,16#10000055#);
   S1D519 : aliased constant Code_Point_Array := (16#1D519#,16#10000056#);
   S1D51A : aliased constant Code_Point_Array := (16#1D51A#,16#10000057#);
   S1D51B : aliased constant Code_Point_Array := (16#1D51B#,16#10000058#);
   S1D51C : aliased constant Code_Point_Array := (16#1D51C#,16#10000059#);
   S1D51E : aliased constant Code_Point_Array := (16#1D51E#,16#10000061#);
   S1D51F : aliased constant Code_Point_Array := (16#1D51F#,16#10000062#);
   S1D520 : aliased constant Code_Point_Array := (16#1D520#,16#10000063#);
   S1D521 : aliased constant Code_Point_Array := (16#1D521#,16#10000064#);
   S1D522 : aliased constant Code_Point_Array := (16#1D522#,16#10000065#);
   S1D523 : aliased constant Code_Point_Array := (16#1D523#,16#10000066#);
   S1D524 : aliased constant Code_Point_Array := (16#1D524#,16#10000067#);
   S1D525 : aliased constant Code_Point_Array := (16#1D525#,16#10000068#);
   S1D526 : aliased constant Code_Point_Array := (16#1D526#,16#10000069#);
   S1D527 : aliased constant Code_Point_Array := (16#1D527#,16#1000006A#);
   S1D528 : aliased constant Code_Point_Array := (16#1D528#,16#1000006B#);
   S1D529 : aliased constant Code_Point_Array := (16#1D529#,16#1000006C#);
   S1D52A : aliased constant Code_Point_Array := (16#1D52A#,16#1000006D#);
   S1D52B : aliased constant Code_Point_Array := (16#1D52B#,16#1000006E#);
   S1D52C : aliased constant Code_Point_Array := (16#1D52C#,16#1000006F#);
   S1D52D : aliased constant Code_Point_Array := (16#1D52D#,16#10000070#);
   S1D52E : aliased constant Code_Point_Array := (16#1D52E#,16#10000071#);
   S1D52F : aliased constant Code_Point_Array := (16#1D52F#,16#10000072#);
   S1D530 : aliased constant Code_Point_Array := (16#1D530#,16#10000073#);
   S1D531 : aliased constant Code_Point_Array := (16#1D531#,16#10000074#);
   S1D532 : aliased constant Code_Point_Array := (16#1D532#,16#10000075#);
   S1D533 : aliased constant Code_Point_Array := (16#1D533#,16#10000076#);
   S1D534 : aliased constant Code_Point_Array := (16#1D534#,16#10000077#);
   S1D535 : aliased constant Code_Point_Array := (16#1D535#,16#10000078#);
   S1D536 : aliased constant Code_Point_Array := (16#1D536#,16#10000079#);
   S1D537 : aliased constant Code_Point_Array := (16#1D537#,16#1000007A#);
   S1D538 : aliased constant Code_Point_Array := (16#1D538#,16#10000041#);
   S1D539 : aliased constant Code_Point_Array := (16#1D539#,16#10000042#);
   S1D53B : aliased constant Code_Point_Array := (16#1D53B#,16#10000044#);
   S1D53C : aliased constant Code_Point_Array := (16#1D53C#,16#10000045#);
   S1D53D : aliased constant Code_Point_Array := (16#1D53D#,16#10000046#);
   S1D53E : aliased constant Code_Point_Array := (16#1D53E#,16#10000047#);
   S1D540 : aliased constant Code_Point_Array := (16#1D540#,16#10000049#);
   S1D541 : aliased constant Code_Point_Array := (16#1D541#,16#1000004A#);
   S1D542 : aliased constant Code_Point_Array := (16#1D542#,16#1000004B#);
   S1D543 : aliased constant Code_Point_Array := (16#1D543#,16#1000004C#);
   S1D544 : aliased constant Code_Point_Array := (16#1D544#,16#1000004D#);
   S1D546 : aliased constant Code_Point_Array := (16#1D546#,16#1000004F#);
   S1D54A : aliased constant Code_Point_Array := (16#1D54A#,16#10000053#);
   S1D54B : aliased constant Code_Point_Array := (16#1D54B#,16#10000054#);
   S1D54C : aliased constant Code_Point_Array := (16#1D54C#,16#10000055#);
   S1D54D : aliased constant Code_Point_Array := (16#1D54D#,16#10000056#);
   S1D54E : aliased constant Code_Point_Array := (16#1D54E#,16#10000057#);
   S1D54F : aliased constant Code_Point_Array := (16#1D54F#,16#10000058#);
   S1D550 : aliased constant Code_Point_Array := (16#1D550#,16#10000059#);
   S1D552 : aliased constant Code_Point_Array := (16#1D552#,16#10000061#);
   S1D553 : aliased constant Code_Point_Array := (16#1D553#,16#10000062#);
   S1D554 : aliased constant Code_Point_Array := (16#1D554#,16#10000063#);
   S1D555 : aliased constant Code_Point_Array := (16#1D555#,16#10000064#);
   S1D556 : aliased constant Code_Point_Array := (16#1D556#,16#10000065#);
   S1D557 : aliased constant Code_Point_Array := (16#1D557#,16#10000066#);
   S1D558 : aliased constant Code_Point_Array := (16#1D558#,16#10000067#);
   S1D559 : aliased constant Code_Point_Array := (16#1D559#,16#10000068#);
   S1D55A : aliased constant Code_Point_Array := (16#1D55A#,16#10000069#);
   S1D55B : aliased constant Code_Point_Array := (16#1D55B#,16#1000006A#);
   S1D55C : aliased constant Code_Point_Array := (16#1D55C#,16#1000006B#);
   S1D55D : aliased constant Code_Point_Array := (16#1D55D#,16#1000006C#);
   S1D55E : aliased constant Code_Point_Array := (16#1D55E#,16#1000006D#);
   S1D55F : aliased constant Code_Point_Array := (16#1D55F#,16#1000006E#);
   S1D560 : aliased constant Code_Point_Array := (16#1D560#,16#1000006F#);
   S1D561 : aliased constant Code_Point_Array := (16#1D561#,16#10000070#);
   S1D562 : aliased constant Code_Point_Array := (16#1D562#,16#10000071#);
   S1D563 : aliased constant Code_Point_Array := (16#1D563#,16#10000072#);
   S1D564 : aliased constant Code_Point_Array := (16#1D564#,16#10000073#);
   S1D565 : aliased constant Code_Point_Array := (16#1D565#,16#10000074#);
   S1D566 : aliased constant Code_Point_Array := (16#1D566#,16#10000075#);
   S1D567 : aliased constant Code_Point_Array := (16#1D567#,16#10000076#);
   S1D568 : aliased constant Code_Point_Array := (16#1D568#,16#10000077#);
   S1D569 : aliased constant Code_Point_Array := (16#1D569#,16#10000078#);
   S1D56A : aliased constant Code_Point_Array := (16#1D56A#,16#10000079#);
   S1D56B : aliased constant Code_Point_Array := (16#1D56B#,16#1000007A#);
   S1D56C : aliased constant Code_Point_Array := (16#1D56C#,16#10000041#);
   S1D56D : aliased constant Code_Point_Array := (16#1D56D#,16#10000042#);
   S1D56E : aliased constant Code_Point_Array := (16#1D56E#,16#10000043#);
   S1D56F : aliased constant Code_Point_Array := (16#1D56F#,16#10000044#);
   S1D570 : aliased constant Code_Point_Array := (16#1D570#,16#10000045#);
   S1D571 : aliased constant Code_Point_Array := (16#1D571#,16#10000046#);
   S1D572 : aliased constant Code_Point_Array := (16#1D572#,16#10000047#);
   S1D573 : aliased constant Code_Point_Array := (16#1D573#,16#10000048#);
   S1D574 : aliased constant Code_Point_Array := (16#1D574#,16#10000049#);
   S1D575 : aliased constant Code_Point_Array := (16#1D575#,16#1000004A#);
   S1D576 : aliased constant Code_Point_Array := (16#1D576#,16#1000004B#);
   S1D577 : aliased constant Code_Point_Array := (16#1D577#,16#1000004C#);
   S1D578 : aliased constant Code_Point_Array := (16#1D578#,16#1000004D#);
   S1D579 : aliased constant Code_Point_Array := (16#1D579#,16#1000004E#);
   S1D57A : aliased constant Code_Point_Array := (16#1D57A#,16#1000004F#);
   S1D57B : aliased constant Code_Point_Array := (16#1D57B#,16#10000050#);
   S1D57C : aliased constant Code_Point_Array := (16#1D57C#,16#10000051#);
   S1D57D : aliased constant Code_Point_Array := (16#1D57D#,16#10000052#);
   S1D57E : aliased constant Code_Point_Array := (16#1D57E#,16#10000053#);
   S1D57F : aliased constant Code_Point_Array := (16#1D57F#,16#10000054#);
   S1D580 : aliased constant Code_Point_Array := (16#1D580#,16#10000055#);
   S1D581 : aliased constant Code_Point_Array := (16#1D581#,16#10000056#);
   S1D582 : aliased constant Code_Point_Array := (16#1D582#,16#10000057#);
   S1D583 : aliased constant Code_Point_Array := (16#1D583#,16#10000058#);
   S1D584 : aliased constant Code_Point_Array := (16#1D584#,16#10000059#);
   S1D585 : aliased constant Code_Point_Array := (16#1D585#,16#1000005A#);
   S1D586 : aliased constant Code_Point_Array := (16#1D586#,16#10000061#);
   S1D587 : aliased constant Code_Point_Array := (16#1D587#,16#10000062#);
   S1D588 : aliased constant Code_Point_Array := (16#1D588#,16#10000063#);
   S1D589 : aliased constant Code_Point_Array := (16#1D589#,16#10000064#);
   S1D58A : aliased constant Code_Point_Array := (16#1D58A#,16#10000065#);
   S1D58B : aliased constant Code_Point_Array := (16#1D58B#,16#10000066#);
   S1D58C : aliased constant Code_Point_Array := (16#1D58C#,16#10000067#);
   S1D58D : aliased constant Code_Point_Array := (16#1D58D#,16#10000068#);
   S1D58E : aliased constant Code_Point_Array := (16#1D58E#,16#10000069#);
   S1D58F : aliased constant Code_Point_Array := (16#1D58F#,16#1000006A#);
   S1D590 : aliased constant Code_Point_Array := (16#1D590#,16#1000006B#);
   S1D591 : aliased constant Code_Point_Array := (16#1D591#,16#1000006C#);
   S1D592 : aliased constant Code_Point_Array := (16#1D592#,16#1000006D#);
   S1D593 : aliased constant Code_Point_Array := (16#1D593#,16#1000006E#);
   S1D594 : aliased constant Code_Point_Array := (16#1D594#,16#1000006F#);
   S1D595 : aliased constant Code_Point_Array := (16#1D595#,16#10000070#);
   S1D596 : aliased constant Code_Point_Array := (16#1D596#,16#10000071#);
   S1D597 : aliased constant Code_Point_Array := (16#1D597#,16#10000072#);
   S1D598 : aliased constant Code_Point_Array := (16#1D598#,16#10000073#);
   S1D599 : aliased constant Code_Point_Array := (16#1D599#,16#10000074#);
   S1D59A : aliased constant Code_Point_Array := (16#1D59A#,16#10000075#);
   S1D59B : aliased constant Code_Point_Array := (16#1D59B#,16#10000076#);
   S1D59C : aliased constant Code_Point_Array := (16#1D59C#,16#10000077#);
   S1D59D : aliased constant Code_Point_Array := (16#1D59D#,16#10000078#);
   S1D59E : aliased constant Code_Point_Array := (16#1D59E#,16#10000079#);
   S1D59F : aliased constant Code_Point_Array := (16#1D59F#,16#1000007A#);
   S1D5A0 : aliased constant Code_Point_Array := (16#1D5A0#,16#10000041#);
   S1D5A1 : aliased constant Code_Point_Array := (16#1D5A1#,16#10000042#);
   S1D5A2 : aliased constant Code_Point_Array := (16#1D5A2#,16#10000043#);
   S1D5A3 : aliased constant Code_Point_Array := (16#1D5A3#,16#10000044#);
   S1D5A4 : aliased constant Code_Point_Array := (16#1D5A4#,16#10000045#);
   S1D5A5 : aliased constant Code_Point_Array := (16#1D5A5#,16#10000046#);
   S1D5A6 : aliased constant Code_Point_Array := (16#1D5A6#,16#10000047#);
   S1D5A7 : aliased constant Code_Point_Array := (16#1D5A7#,16#10000048#);
   S1D5A8 : aliased constant Code_Point_Array := (16#1D5A8#,16#10000049#);
   S1D5A9 : aliased constant Code_Point_Array := (16#1D5A9#,16#1000004A#);
   S1D5AA : aliased constant Code_Point_Array := (16#1D5AA#,16#1000004B#);
   S1D5AB : aliased constant Code_Point_Array := (16#1D5AB#,16#1000004C#);
   S1D5AC : aliased constant Code_Point_Array := (16#1D5AC#,16#1000004D#);
   S1D5AD : aliased constant Code_Point_Array := (16#1D5AD#,16#1000004E#);
   S1D5AE : aliased constant Code_Point_Array := (16#1D5AE#,16#1000004F#);
   S1D5AF : aliased constant Code_Point_Array := (16#1D5AF#,16#10000050#);
   S1D5B0 : aliased constant Code_Point_Array := (16#1D5B0#,16#10000051#);
   S1D5B1 : aliased constant Code_Point_Array := (16#1D5B1#,16#10000052#);
   S1D5B2 : aliased constant Code_Point_Array := (16#1D5B2#,16#10000053#);
   S1D5B3 : aliased constant Code_Point_Array := (16#1D5B3#,16#10000054#);
   S1D5B4 : aliased constant Code_Point_Array := (16#1D5B4#,16#10000055#);
   S1D5B5 : aliased constant Code_Point_Array := (16#1D5B5#,16#10000056#);
   S1D5B6 : aliased constant Code_Point_Array := (16#1D5B6#,16#10000057#);
   S1D5B7 : aliased constant Code_Point_Array := (16#1D5B7#,16#10000058#);
   S1D5B8 : aliased constant Code_Point_Array := (16#1D5B8#,16#10000059#);
   S1D5B9 : aliased constant Code_Point_Array := (16#1D5B9#,16#1000005A#);
   S1D5BA : aliased constant Code_Point_Array := (16#1D5BA#,16#10000061#);
   S1D5BB : aliased constant Code_Point_Array := (16#1D5BB#,16#10000062#);
   S1D5BC : aliased constant Code_Point_Array := (16#1D5BC#,16#10000063#);
   S1D5BD : aliased constant Code_Point_Array := (16#1D5BD#,16#10000064#);
   S1D5BE : aliased constant Code_Point_Array := (16#1D5BE#,16#10000065#);
   S1D5BF : aliased constant Code_Point_Array := (16#1D5BF#,16#10000066#);
   S1D5C0 : aliased constant Code_Point_Array := (16#1D5C0#,16#10000067#);
   S1D5C1 : aliased constant Code_Point_Array := (16#1D5C1#,16#10000068#);
   S1D5C2 : aliased constant Code_Point_Array := (16#1D5C2#,16#10000069#);
   S1D5C3 : aliased constant Code_Point_Array := (16#1D5C3#,16#1000006A#);
   S1D5C4 : aliased constant Code_Point_Array := (16#1D5C4#,16#1000006B#);
   S1D5C5 : aliased constant Code_Point_Array := (16#1D5C5#,16#1000006C#);
   S1D5C6 : aliased constant Code_Point_Array := (16#1D5C6#,16#1000006D#);
   S1D5C7 : aliased constant Code_Point_Array := (16#1D5C7#,16#1000006E#);
   S1D5C8 : aliased constant Code_Point_Array := (16#1D5C8#,16#1000006F#);
   S1D5C9 : aliased constant Code_Point_Array := (16#1D5C9#,16#10000070#);
   S1D5CA : aliased constant Code_Point_Array := (16#1D5CA#,16#10000071#);
   S1D5CB : aliased constant Code_Point_Array := (16#1D5CB#,16#10000072#);
   S1D5CC : aliased constant Code_Point_Array := (16#1D5CC#,16#10000073#);
   S1D5CD : aliased constant Code_Point_Array := (16#1D5CD#,16#10000074#);
   S1D5CE : aliased constant Code_Point_Array := (16#1D5CE#,16#10000075#);
   S1D5CF : aliased constant Code_Point_Array := (16#1D5CF#,16#10000076#);
   S1D5D0 : aliased constant Code_Point_Array := (16#1D5D0#,16#10000077#);
   S1D5D1 : aliased constant Code_Point_Array := (16#1D5D1#,16#10000078#);
   S1D5D2 : aliased constant Code_Point_Array := (16#1D5D2#,16#10000079#);
   S1D5D3 : aliased constant Code_Point_Array := (16#1D5D3#,16#1000007A#);
   S1D5D4 : aliased constant Code_Point_Array := (16#1D5D4#,16#10000041#);
   S1D5D5 : aliased constant Code_Point_Array := (16#1D5D5#,16#10000042#);
   S1D5D6 : aliased constant Code_Point_Array := (16#1D5D6#,16#10000043#);
   S1D5D7 : aliased constant Code_Point_Array := (16#1D5D7#,16#10000044#);
   S1D5D8 : aliased constant Code_Point_Array := (16#1D5D8#,16#10000045#);
   S1D5D9 : aliased constant Code_Point_Array := (16#1D5D9#,16#10000046#);
   S1D5DA : aliased constant Code_Point_Array := (16#1D5DA#,16#10000047#);
   S1D5DB : aliased constant Code_Point_Array := (16#1D5DB#,16#10000048#);
   S1D5DC : aliased constant Code_Point_Array := (16#1D5DC#,16#10000049#);
   S1D5DD : aliased constant Code_Point_Array := (16#1D5DD#,16#1000004A#);
   S1D5DE : aliased constant Code_Point_Array := (16#1D5DE#,16#1000004B#);
   S1D5DF : aliased constant Code_Point_Array := (16#1D5DF#,16#1000004C#);
   S1D5E0 : aliased constant Code_Point_Array := (16#1D5E0#,16#1000004D#);
   S1D5E1 : aliased constant Code_Point_Array := (16#1D5E1#,16#1000004E#);
   S1D5E2 : aliased constant Code_Point_Array := (16#1D5E2#,16#1000004F#);
   S1D5E3 : aliased constant Code_Point_Array := (16#1D5E3#,16#10000050#);
   S1D5E4 : aliased constant Code_Point_Array := (16#1D5E4#,16#10000051#);
   S1D5E5 : aliased constant Code_Point_Array := (16#1D5E5#,16#10000052#);
   S1D5E6 : aliased constant Code_Point_Array := (16#1D5E6#,16#10000053#);
   S1D5E7 : aliased constant Code_Point_Array := (16#1D5E7#,16#10000054#);
   S1D5E8 : aliased constant Code_Point_Array := (16#1D5E8#,16#10000055#);
   S1D5E9 : aliased constant Code_Point_Array := (16#1D5E9#,16#10000056#);
   S1D5EA : aliased constant Code_Point_Array := (16#1D5EA#,16#10000057#);
   S1D5EB : aliased constant Code_Point_Array := (16#1D5EB#,16#10000058#);
   S1D5EC : aliased constant Code_Point_Array := (16#1D5EC#,16#10000059#);
   S1D5ED : aliased constant Code_Point_Array := (16#1D5ED#,16#1000005A#);
   S1D5EE : aliased constant Code_Point_Array := (16#1D5EE#,16#10000061#);
   S1D5EF : aliased constant Code_Point_Array := (16#1D5EF#,16#10000062#);
   S1D5F0 : aliased constant Code_Point_Array := (16#1D5F0#,16#10000063#);
   S1D5F1 : aliased constant Code_Point_Array := (16#1D5F1#,16#10000064#);
   S1D5F2 : aliased constant Code_Point_Array := (16#1D5F2#,16#10000065#);
   S1D5F3 : aliased constant Code_Point_Array := (16#1D5F3#,16#10000066#);
   S1D5F4 : aliased constant Code_Point_Array := (16#1D5F4#,16#10000067#);
   S1D5F5 : aliased constant Code_Point_Array := (16#1D5F5#,16#10000068#);
   S1D5F6 : aliased constant Code_Point_Array := (16#1D5F6#,16#10000069#);
   S1D5F7 : aliased constant Code_Point_Array := (16#1D5F7#,16#1000006A#);
   S1D5F8 : aliased constant Code_Point_Array := (16#1D5F8#,16#1000006B#);
   S1D5F9 : aliased constant Code_Point_Array := (16#1D5F9#,16#1000006C#);
   S1D5FA : aliased constant Code_Point_Array := (16#1D5FA#,16#1000006D#);
   S1D5FB : aliased constant Code_Point_Array := (16#1D5FB#,16#1000006E#);
   S1D5FC : aliased constant Code_Point_Array := (16#1D5FC#,16#1000006F#);
   S1D5FD : aliased constant Code_Point_Array := (16#1D5FD#,16#10000070#);
   S1D5FE : aliased constant Code_Point_Array := (16#1D5FE#,16#10000071#);
   S1D5FF : aliased constant Code_Point_Array := (16#1D5FF#,16#10000072#);
   S1D600 : aliased constant Code_Point_Array := (16#1D600#,16#10000073#);
   S1D601 : aliased constant Code_Point_Array := (16#1D601#,16#10000074#);
   S1D602 : aliased constant Code_Point_Array := (16#1D602#,16#10000075#);
   S1D603 : aliased constant Code_Point_Array := (16#1D603#,16#10000076#);
   S1D604 : aliased constant Code_Point_Array := (16#1D604#,16#10000077#);
   S1D605 : aliased constant Code_Point_Array := (16#1D605#,16#10000078#);
   S1D606 : aliased constant Code_Point_Array := (16#1D606#,16#10000079#);
   S1D607 : aliased constant Code_Point_Array := (16#1D607#,16#1000007A#);
   S1D608 : aliased constant Code_Point_Array := (16#1D608#,16#10000041#);
   S1D609 : aliased constant Code_Point_Array := (16#1D609#,16#10000042#);
   S1D60A : aliased constant Code_Point_Array := (16#1D60A#,16#10000043#);
   S1D60B : aliased constant Code_Point_Array := (16#1D60B#,16#10000044#);
   S1D60C : aliased constant Code_Point_Array := (16#1D60C#,16#10000045#);
   S1D60D : aliased constant Code_Point_Array := (16#1D60D#,16#10000046#);
   S1D60E : aliased constant Code_Point_Array := (16#1D60E#,16#10000047#);
   S1D60F : aliased constant Code_Point_Array := (16#1D60F#,16#10000048#);
   S1D610 : aliased constant Code_Point_Array := (16#1D610#,16#10000049#);
   S1D611 : aliased constant Code_Point_Array := (16#1D611#,16#1000004A#);
   S1D612 : aliased constant Code_Point_Array := (16#1D612#,16#1000004B#);
   S1D613 : aliased constant Code_Point_Array := (16#1D613#,16#1000004C#);
   S1D614 : aliased constant Code_Point_Array := (16#1D614#,16#1000004D#);
   S1D615 : aliased constant Code_Point_Array := (16#1D615#,16#1000004E#);
   S1D616 : aliased constant Code_Point_Array := (16#1D616#,16#1000004F#);
   S1D617 : aliased constant Code_Point_Array := (16#1D617#,16#10000050#);
   S1D618 : aliased constant Code_Point_Array := (16#1D618#,16#10000051#);
   S1D619 : aliased constant Code_Point_Array := (16#1D619#,16#10000052#);
   S1D61A : aliased constant Code_Point_Array := (16#1D61A#,16#10000053#);
   S1D61B : aliased constant Code_Point_Array := (16#1D61B#,16#10000054#);
   S1D61C : aliased constant Code_Point_Array := (16#1D61C#,16#10000055#);
   S1D61D : aliased constant Code_Point_Array := (16#1D61D#,16#10000056#);
   S1D61E : aliased constant Code_Point_Array := (16#1D61E#,16#10000057#);
   S1D61F : aliased constant Code_Point_Array := (16#1D61F#,16#10000058#);
   S1D620 : aliased constant Code_Point_Array := (16#1D620#,16#10000059#);
   S1D621 : aliased constant Code_Point_Array := (16#1D621#,16#1000005A#);
   S1D622 : aliased constant Code_Point_Array := (16#1D622#,16#10000061#);
   S1D623 : aliased constant Code_Point_Array := (16#1D623#,16#10000062#);
   S1D624 : aliased constant Code_Point_Array := (16#1D624#,16#10000063#);
   S1D625 : aliased constant Code_Point_Array := (16#1D625#,16#10000064#);
   S1D626 : aliased constant Code_Point_Array := (16#1D626#,16#10000065#);
   S1D627 : aliased constant Code_Point_Array := (16#1D627#,16#10000066#);
   S1D628 : aliased constant Code_Point_Array := (16#1D628#,16#10000067#);
   S1D629 : aliased constant Code_Point_Array := (16#1D629#,16#10000068#);
   S1D62A : aliased constant Code_Point_Array := (16#1D62A#,16#10000069#);
   S1D62B : aliased constant Code_Point_Array := (16#1D62B#,16#1000006A#);
   S1D62C : aliased constant Code_Point_Array := (16#1D62C#,16#1000006B#);
   S1D62D : aliased constant Code_Point_Array := (16#1D62D#,16#1000006C#);
   S1D62E : aliased constant Code_Point_Array := (16#1D62E#,16#1000006D#);
   S1D62F : aliased constant Code_Point_Array := (16#1D62F#,16#1000006E#);
   S1D630 : aliased constant Code_Point_Array := (16#1D630#,16#1000006F#);
   S1D631 : aliased constant Code_Point_Array := (16#1D631#,16#10000070#);
   S1D632 : aliased constant Code_Point_Array := (16#1D632#,16#10000071#);
   S1D633 : aliased constant Code_Point_Array := (16#1D633#,16#10000072#);
   S1D634 : aliased constant Code_Point_Array := (16#1D634#,16#10000073#);
   S1D635 : aliased constant Code_Point_Array := (16#1D635#,16#10000074#);
   S1D636 : aliased constant Code_Point_Array := (16#1D636#,16#10000075#);
   S1D637 : aliased constant Code_Point_Array := (16#1D637#,16#10000076#);
   S1D638 : aliased constant Code_Point_Array := (16#1D638#,16#10000077#);
   S1D639 : aliased constant Code_Point_Array := (16#1D639#,16#10000078#);
   S1D63A : aliased constant Code_Point_Array := (16#1D63A#,16#10000079#);
   S1D63B : aliased constant Code_Point_Array := (16#1D63B#,16#1000007A#);
   S1D63C : aliased constant Code_Point_Array := (16#1D63C#,16#10000041#);
   S1D63D : aliased constant Code_Point_Array := (16#1D63D#,16#10000042#);
   S1D63E : aliased constant Code_Point_Array := (16#1D63E#,16#10000043#);
   S1D63F : aliased constant Code_Point_Array := (16#1D63F#,16#10000044#);
   S1D640 : aliased constant Code_Point_Array := (16#1D640#,16#10000045#);
   S1D641 : aliased constant Code_Point_Array := (16#1D641#,16#10000046#);
   S1D642 : aliased constant Code_Point_Array := (16#1D642#,16#10000047#);
   S1D643 : aliased constant Code_Point_Array := (16#1D643#,16#10000048#);
   S1D644 : aliased constant Code_Point_Array := (16#1D644#,16#10000049#);
   S1D645 : aliased constant Code_Point_Array := (16#1D645#,16#1000004A#);
   S1D646 : aliased constant Code_Point_Array := (16#1D646#,16#1000004B#);
   S1D647 : aliased constant Code_Point_Array := (16#1D647#,16#1000004C#);
   S1D648 : aliased constant Code_Point_Array := (16#1D648#,16#1000004D#);
   S1D649 : aliased constant Code_Point_Array := (16#1D649#,16#1000004E#);
   S1D64A : aliased constant Code_Point_Array := (16#1D64A#,16#1000004F#);
   S1D64B : aliased constant Code_Point_Array := (16#1D64B#,16#10000050#);
   S1D64C : aliased constant Code_Point_Array := (16#1D64C#,16#10000051#);
   S1D64D : aliased constant Code_Point_Array := (16#1D64D#,16#10000052#);
   S1D64E : aliased constant Code_Point_Array := (16#1D64E#,16#10000053#);
   S1D64F : aliased constant Code_Point_Array := (16#1D64F#,16#10000054#);
   S1D650 : aliased constant Code_Point_Array := (16#1D650#,16#10000055#);
   S1D651 : aliased constant Code_Point_Array := (16#1D651#,16#10000056#);
   S1D652 : aliased constant Code_Point_Array := (16#1D652#,16#10000057#);
   S1D653 : aliased constant Code_Point_Array := (16#1D653#,16#10000058#);
   S1D654 : aliased constant Code_Point_Array := (16#1D654#,16#10000059#);
   S1D655 : aliased constant Code_Point_Array := (16#1D655#,16#1000005A#);
   S1D656 : aliased constant Code_Point_Array := (16#1D656#,16#10000061#);
   S1D657 : aliased constant Code_Point_Array := (16#1D657#,16#10000062#);
   S1D658 : aliased constant Code_Point_Array := (16#1D658#,16#10000063#);
   S1D659 : aliased constant Code_Point_Array := (16#1D659#,16#10000064#);
   S1D65A : aliased constant Code_Point_Array := (16#1D65A#,16#10000065#);
   S1D65B : aliased constant Code_Point_Array := (16#1D65B#,16#10000066#);
   S1D65C : aliased constant Code_Point_Array := (16#1D65C#,16#10000067#);
   S1D65D : aliased constant Code_Point_Array := (16#1D65D#,16#10000068#);
   S1D65E : aliased constant Code_Point_Array := (16#1D65E#,16#10000069#);
   S1D65F : aliased constant Code_Point_Array := (16#1D65F#,16#1000006A#);
   S1D660 : aliased constant Code_Point_Array := (16#1D660#,16#1000006B#);
   S1D661 : aliased constant Code_Point_Array := (16#1D661#,16#1000006C#);
   S1D662 : aliased constant Code_Point_Array := (16#1D662#,16#1000006D#);
   S1D663 : aliased constant Code_Point_Array := (16#1D663#,16#1000006E#);
   S1D664 : aliased constant Code_Point_Array := (16#1D664#,16#1000006F#);
   S1D665 : aliased constant Code_Point_Array := (16#1D665#,16#10000070#);
   S1D666 : aliased constant Code_Point_Array := (16#1D666#,16#10000071#);
   S1D667 : aliased constant Code_Point_Array := (16#1D667#,16#10000072#);
   S1D668 : aliased constant Code_Point_Array := (16#1D668#,16#10000073#);
   S1D669 : aliased constant Code_Point_Array := (16#1D669#,16#10000074#);
   S1D66A : aliased constant Code_Point_Array := (16#1D66A#,16#10000075#);
   S1D66B : aliased constant Code_Point_Array := (16#1D66B#,16#10000076#);
   S1D66C : aliased constant Code_Point_Array := (16#1D66C#,16#10000077#);
   S1D66D : aliased constant Code_Point_Array := (16#1D66D#,16#10000078#);
   S1D66E : aliased constant Code_Point_Array := (16#1D66E#,16#10000079#);
   S1D66F : aliased constant Code_Point_Array := (16#1D66F#,16#1000007A#);
   S1D670 : aliased constant Code_Point_Array := (16#1D670#,16#10000041#);
   S1D671 : aliased constant Code_Point_Array := (16#1D671#,16#10000042#);
   S1D672 : aliased constant Code_Point_Array := (16#1D672#,16#10000043#);
   S1D673 : aliased constant Code_Point_Array := (16#1D673#,16#10000044#);
   S1D674 : aliased constant Code_Point_Array := (16#1D674#,16#10000045#);
   S1D675 : aliased constant Code_Point_Array := (16#1D675#,16#10000046#);
   S1D676 : aliased constant Code_Point_Array := (16#1D676#,16#10000047#);
   S1D677 : aliased constant Code_Point_Array := (16#1D677#,16#10000048#);
   S1D678 : aliased constant Code_Point_Array := (16#1D678#,16#10000049#);
   S1D679 : aliased constant Code_Point_Array := (16#1D679#,16#1000004A#);
   S1D67A : aliased constant Code_Point_Array := (16#1D67A#,16#1000004B#);
   S1D67B : aliased constant Code_Point_Array := (16#1D67B#,16#1000004C#);
   S1D67C : aliased constant Code_Point_Array := (16#1D67C#,16#1000004D#);
   S1D67D : aliased constant Code_Point_Array := (16#1D67D#,16#1000004E#);
   S1D67E : aliased constant Code_Point_Array := (16#1D67E#,16#1000004F#);
   S1D67F : aliased constant Code_Point_Array := (16#1D67F#,16#10000050#);
   S1D680 : aliased constant Code_Point_Array := (16#1D680#,16#10000051#);
   S1D681 : aliased constant Code_Point_Array := (16#1D681#,16#10000052#);
   S1D682 : aliased constant Code_Point_Array := (16#1D682#,16#10000053#);
   S1D683 : aliased constant Code_Point_Array := (16#1D683#,16#10000054#);
   S1D684 : aliased constant Code_Point_Array := (16#1D684#,16#10000055#);
   S1D685 : aliased constant Code_Point_Array := (16#1D685#,16#10000056#);
   S1D686 : aliased constant Code_Point_Array := (16#1D686#,16#10000057#);
   S1D687 : aliased constant Code_Point_Array := (16#1D687#,16#10000058#);
   S1D688 : aliased constant Code_Point_Array := (16#1D688#,16#10000059#);
   S1D689 : aliased constant Code_Point_Array := (16#1D689#,16#1000005A#);
   S1D68A : aliased constant Code_Point_Array := (16#1D68A#,16#10000061#);
   S1D68B : aliased constant Code_Point_Array := (16#1D68B#,16#10000062#);
   S1D68C : aliased constant Code_Point_Array := (16#1D68C#,16#10000063#);
   S1D68D : aliased constant Code_Point_Array := (16#1D68D#,16#10000064#);
   S1D68E : aliased constant Code_Point_Array := (16#1D68E#,16#10000065#);
   S1D68F : aliased constant Code_Point_Array := (16#1D68F#,16#10000066#);
   S1D690 : aliased constant Code_Point_Array := (16#1D690#,16#10000067#);
   S1D691 : aliased constant Code_Point_Array := (16#1D691#,16#10000068#);
   S1D692 : aliased constant Code_Point_Array := (16#1D692#,16#10000069#);
   S1D693 : aliased constant Code_Point_Array := (16#1D693#,16#1000006A#);
   S1D694 : aliased constant Code_Point_Array := (16#1D694#,16#1000006B#);
   S1D695 : aliased constant Code_Point_Array := (16#1D695#,16#1000006C#);
   S1D696 : aliased constant Code_Point_Array := (16#1D696#,16#1000006D#);
   S1D697 : aliased constant Code_Point_Array := (16#1D697#,16#1000006E#);
   S1D698 : aliased constant Code_Point_Array := (16#1D698#,16#1000006F#);
   S1D699 : aliased constant Code_Point_Array := (16#1D699#,16#10000070#);
   S1D69A : aliased constant Code_Point_Array := (16#1D69A#,16#10000071#);
   S1D69B : aliased constant Code_Point_Array := (16#1D69B#,16#10000072#);
   S1D69C : aliased constant Code_Point_Array := (16#1D69C#,16#10000073#);
   S1D69D : aliased constant Code_Point_Array := (16#1D69D#,16#10000074#);
   S1D69E : aliased constant Code_Point_Array := (16#1D69E#,16#10000075#);
   S1D69F : aliased constant Code_Point_Array := (16#1D69F#,16#10000076#);
   S1D6A0 : aliased constant Code_Point_Array := (16#1D6A0#,16#10000077#);
   S1D6A1 : aliased constant Code_Point_Array := (16#1D6A1#,16#10000078#);
   S1D6A2 : aliased constant Code_Point_Array := (16#1D6A2#,16#10000079#);
   S1D6A3 : aliased constant Code_Point_Array := (16#1D6A3#,16#1000007A#);
   S1D6A4 : aliased constant Code_Point_Array := (16#1D6A4#,16#10000131#);
   S1D6A5 : aliased constant Code_Point_Array := (16#1D6A5#,16#10000237#);
   S1D6A8 : aliased constant Code_Point_Array := (16#1D6A8#,16#10000391#);
   S1D6A9 : aliased constant Code_Point_Array := (16#1D6A9#,16#10000392#);
   S1D6AA : aliased constant Code_Point_Array := (16#1D6AA#,16#10000393#);
   S1D6AB : aliased constant Code_Point_Array := (16#1D6AB#,16#10000394#);
   S1D6AC : aliased constant Code_Point_Array := (16#1D6AC#,16#10000395#);
   S1D6AD : aliased constant Code_Point_Array := (16#1D6AD#,16#10000396#);
   S1D6AE : aliased constant Code_Point_Array := (16#1D6AE#,16#10000397#);
   S1D6AF : aliased constant Code_Point_Array := (16#1D6AF#,16#10000398#);
   S1D6B0 : aliased constant Code_Point_Array := (16#1D6B0#,16#10000399#);
   S1D6B1 : aliased constant Code_Point_Array := (16#1D6B1#,16#1000039A#);
   S1D6B2 : aliased constant Code_Point_Array := (16#1D6B2#,16#1000039B#);
   S1D6B3 : aliased constant Code_Point_Array := (16#1D6B3#,16#1000039C#);
   S1D6B4 : aliased constant Code_Point_Array := (16#1D6B4#,16#1000039D#);
   S1D6B5 : aliased constant Code_Point_Array := (16#1D6B5#,16#1000039E#);
   S1D6B6 : aliased constant Code_Point_Array := (16#1D6B6#,16#1000039F#);
   S1D6B7 : aliased constant Code_Point_Array := (16#1D6B7#,16#100003A0#);
   S1D6B8 : aliased constant Code_Point_Array := (16#1D6B8#,16#100003A1#);
   S1D6B9 : aliased constant Code_Point_Array := (16#1D6B9#,16#100003F4#);
   S1D6BA : aliased constant Code_Point_Array := (16#1D6BA#,16#100003A3#);
   S1D6BB : aliased constant Code_Point_Array := (16#1D6BB#,16#100003A4#);
   S1D6BC : aliased constant Code_Point_Array := (16#1D6BC#,16#100003A5#);
   S1D6BD : aliased constant Code_Point_Array := (16#1D6BD#,16#100003A6#);
   S1D6BE : aliased constant Code_Point_Array := (16#1D6BE#,16#100003A7#);
   S1D6BF : aliased constant Code_Point_Array := (16#1D6BF#,16#100003A8#);
   S1D6C0 : aliased constant Code_Point_Array := (16#1D6C0#,16#100003A9#);
   S1D6C1 : aliased constant Code_Point_Array := (16#1D6C1#,16#10002207#);
   S1D6C2 : aliased constant Code_Point_Array := (16#1D6C2#,16#100003B1#);
   S1D6C3 : aliased constant Code_Point_Array := (16#1D6C3#,16#100003B2#);
   S1D6C4 : aliased constant Code_Point_Array := (16#1D6C4#,16#100003B3#);
   S1D6C5 : aliased constant Code_Point_Array := (16#1D6C5#,16#100003B4#);
   S1D6C6 : aliased constant Code_Point_Array := (16#1D6C6#,16#100003B5#);
   S1D6C7 : aliased constant Code_Point_Array := (16#1D6C7#,16#100003B6#);
   S1D6C8 : aliased constant Code_Point_Array := (16#1D6C8#,16#100003B7#);
   S1D6C9 : aliased constant Code_Point_Array := (16#1D6C9#,16#100003B8#);
   S1D6CA : aliased constant Code_Point_Array := (16#1D6CA#,16#100003B9#);
   S1D6CB : aliased constant Code_Point_Array := (16#1D6CB#,16#100003BA#);
   S1D6CC : aliased constant Code_Point_Array := (16#1D6CC#,16#100003BB#);
   S1D6CD : aliased constant Code_Point_Array := (16#1D6CD#,16#100003BC#);
   S1D6CE : aliased constant Code_Point_Array := (16#1D6CE#,16#100003BD#);
   S1D6CF : aliased constant Code_Point_Array := (16#1D6CF#,16#100003BE#);
   S1D6D0 : aliased constant Code_Point_Array := (16#1D6D0#,16#100003BF#);
   S1D6D1 : aliased constant Code_Point_Array := (16#1D6D1#,16#100003C0#);
   S1D6D2 : aliased constant Code_Point_Array := (16#1D6D2#,16#100003C1#);
   S1D6D3 : aliased constant Code_Point_Array := (16#1D6D3#,16#100003C2#);
   S1D6D4 : aliased constant Code_Point_Array := (16#1D6D4#,16#100003C3#);
   S1D6D5 : aliased constant Code_Point_Array := (16#1D6D5#,16#100003C4#);
   S1D6D6 : aliased constant Code_Point_Array := (16#1D6D6#,16#100003C5#);
   S1D6D7 : aliased constant Code_Point_Array := (16#1D6D7#,16#100003C6#);
   S1D6D8 : aliased constant Code_Point_Array := (16#1D6D8#,16#100003C7#);
   S1D6D9 : aliased constant Code_Point_Array := (16#1D6D9#,16#100003C8#);
   S1D6DA : aliased constant Code_Point_Array := (16#1D6DA#,16#100003C9#);
   S1D6DB : aliased constant Code_Point_Array := (16#1D6DB#,16#10002202#);
   S1D6DC : aliased constant Code_Point_Array := (16#1D6DC#,16#100003F5#);
   S1D6DD : aliased constant Code_Point_Array := (16#1D6DD#,16#100003D1#);
   S1D6DE : aliased constant Code_Point_Array := (16#1D6DE#,16#100003F0#);
   S1D6DF : aliased constant Code_Point_Array := (16#1D6DF#,16#100003D5#);
   S1D6E0 : aliased constant Code_Point_Array := (16#1D6E0#,16#100003F1#);
   S1D6E1 : aliased constant Code_Point_Array := (16#1D6E1#,16#100003D6#);
   S1D6E2 : aliased constant Code_Point_Array := (16#1D6E2#,16#10000391#);
   S1D6E3 : aliased constant Code_Point_Array := (16#1D6E3#,16#10000392#);
   S1D6E4 : aliased constant Code_Point_Array := (16#1D6E4#,16#10000393#);
   S1D6E5 : aliased constant Code_Point_Array := (16#1D6E5#,16#10000394#);
   S1D6E6 : aliased constant Code_Point_Array := (16#1D6E6#,16#10000395#);
   S1D6E7 : aliased constant Code_Point_Array := (16#1D6E7#,16#10000396#);
   S1D6E8 : aliased constant Code_Point_Array := (16#1D6E8#,16#10000397#);
   S1D6E9 : aliased constant Code_Point_Array := (16#1D6E9#,16#10000398#);
   S1D6EA : aliased constant Code_Point_Array := (16#1D6EA#,16#10000399#);
   S1D6EB : aliased constant Code_Point_Array := (16#1D6EB#,16#1000039A#);
   S1D6EC : aliased constant Code_Point_Array := (16#1D6EC#,16#1000039B#);
   S1D6ED : aliased constant Code_Point_Array := (16#1D6ED#,16#1000039C#);
   S1D6EE : aliased constant Code_Point_Array := (16#1D6EE#,16#1000039D#);
   S1D6EF : aliased constant Code_Point_Array := (16#1D6EF#,16#1000039E#);
   S1D6F0 : aliased constant Code_Point_Array := (16#1D6F0#,16#1000039F#);
   S1D6F1 : aliased constant Code_Point_Array := (16#1D6F1#,16#100003A0#);
   S1D6F2 : aliased constant Code_Point_Array := (16#1D6F2#,16#100003A1#);
   S1D6F3 : aliased constant Code_Point_Array := (16#1D6F3#,16#100003F4#);
   S1D6F4 : aliased constant Code_Point_Array := (16#1D6F4#,16#100003A3#);
   S1D6F5 : aliased constant Code_Point_Array := (16#1D6F5#,16#100003A4#);
   S1D6F6 : aliased constant Code_Point_Array := (16#1D6F6#,16#100003A5#);
   S1D6F7 : aliased constant Code_Point_Array := (16#1D6F7#,16#100003A6#);
   S1D6F8 : aliased constant Code_Point_Array := (16#1D6F8#,16#100003A7#);
   S1D6F9 : aliased constant Code_Point_Array := (16#1D6F9#,16#100003A8#);
   S1D6FA : aliased constant Code_Point_Array := (16#1D6FA#,16#100003A9#);
   S1D6FB : aliased constant Code_Point_Array := (16#1D6FB#,16#10002207#);
   S1D6FC : aliased constant Code_Point_Array := (16#1D6FC#,16#100003B1#);
   S1D6FD : aliased constant Code_Point_Array := (16#1D6FD#,16#100003B2#);
   S1D6FE : aliased constant Code_Point_Array := (16#1D6FE#,16#100003B3#);
   S1D6FF : aliased constant Code_Point_Array := (16#1D6FF#,16#100003B4#);
   S1D700 : aliased constant Code_Point_Array := (16#1D700#,16#100003B5#);
   S1D701 : aliased constant Code_Point_Array := (16#1D701#,16#100003B6#);
   S1D702 : aliased constant Code_Point_Array := (16#1D702#,16#100003B7#);
   S1D703 : aliased constant Code_Point_Array := (16#1D703#,16#100003B8#);
   S1D704 : aliased constant Code_Point_Array := (16#1D704#,16#100003B9#);
   S1D705 : aliased constant Code_Point_Array := (16#1D705#,16#100003BA#);
   S1D706 : aliased constant Code_Point_Array := (16#1D706#,16#100003BB#);
   S1D707 : aliased constant Code_Point_Array := (16#1D707#,16#100003BC#);
   S1D708 : aliased constant Code_Point_Array := (16#1D708#,16#100003BD#);
   S1D709 : aliased constant Code_Point_Array := (16#1D709#,16#100003BE#);
   S1D70A : aliased constant Code_Point_Array := (16#1D70A#,16#100003BF#);
   S1D70B : aliased constant Code_Point_Array := (16#1D70B#,16#100003C0#);
   S1D70C : aliased constant Code_Point_Array := (16#1D70C#,16#100003C1#);
   S1D70D : aliased constant Code_Point_Array := (16#1D70D#,16#100003C2#);
   S1D70E : aliased constant Code_Point_Array := (16#1D70E#,16#100003C3#);
   S1D70F : aliased constant Code_Point_Array := (16#1D70F#,16#100003C4#);
   S1D710 : aliased constant Code_Point_Array := (16#1D710#,16#100003C5#);
   S1D711 : aliased constant Code_Point_Array := (16#1D711#,16#100003C6#);
   S1D712 : aliased constant Code_Point_Array := (16#1D712#,16#100003C7#);
   S1D713 : aliased constant Code_Point_Array := (16#1D713#,16#100003C8#);
   S1D714 : aliased constant Code_Point_Array := (16#1D714#,16#100003C9#);
   S1D715 : aliased constant Code_Point_Array := (16#1D715#,16#10002202#);
   S1D716 : aliased constant Code_Point_Array := (16#1D716#,16#100003F5#);
   S1D717 : aliased constant Code_Point_Array := (16#1D717#,16#100003D1#);
   S1D718 : aliased constant Code_Point_Array := (16#1D718#,16#100003F0#);
   S1D719 : aliased constant Code_Point_Array := (16#1D719#,16#100003D5#);
   S1D71A : aliased constant Code_Point_Array := (16#1D71A#,16#100003F1#);
   S1D71B : aliased constant Code_Point_Array := (16#1D71B#,16#100003D6#);
   S1D71C : aliased constant Code_Point_Array := (16#1D71C#,16#10000391#);
   S1D71D : aliased constant Code_Point_Array := (16#1D71D#,16#10000392#);
   S1D71E : aliased constant Code_Point_Array := (16#1D71E#,16#10000393#);
   S1D71F : aliased constant Code_Point_Array := (16#1D71F#,16#10000394#);
   S1D720 : aliased constant Code_Point_Array := (16#1D720#,16#10000395#);
   S1D721 : aliased constant Code_Point_Array := (16#1D721#,16#10000396#);
   S1D722 : aliased constant Code_Point_Array := (16#1D722#,16#10000397#);
   S1D723 : aliased constant Code_Point_Array := (16#1D723#,16#10000398#);
   S1D724 : aliased constant Code_Point_Array := (16#1D724#,16#10000399#);
   S1D725 : aliased constant Code_Point_Array := (16#1D725#,16#1000039A#);
   S1D726 : aliased constant Code_Point_Array := (16#1D726#,16#1000039B#);
   S1D727 : aliased constant Code_Point_Array := (16#1D727#,16#1000039C#);
   S1D728 : aliased constant Code_Point_Array := (16#1D728#,16#1000039D#);
   S1D729 : aliased constant Code_Point_Array := (16#1D729#,16#1000039E#);
   S1D72A : aliased constant Code_Point_Array := (16#1D72A#,16#1000039F#);
   S1D72B : aliased constant Code_Point_Array := (16#1D72B#,16#100003A0#);
   S1D72C : aliased constant Code_Point_Array := (16#1D72C#,16#100003A1#);
   S1D72D : aliased constant Code_Point_Array := (16#1D72D#,16#100003F4#);
   S1D72E : aliased constant Code_Point_Array := (16#1D72E#,16#100003A3#);
   S1D72F : aliased constant Code_Point_Array := (16#1D72F#,16#100003A4#);
   S1D730 : aliased constant Code_Point_Array := (16#1D730#,16#100003A5#);
   S1D731 : aliased constant Code_Point_Array := (16#1D731#,16#100003A6#);
   S1D732 : aliased constant Code_Point_Array := (16#1D732#,16#100003A7#);
   S1D733 : aliased constant Code_Point_Array := (16#1D733#,16#100003A8#);
   S1D734 : aliased constant Code_Point_Array := (16#1D734#,16#100003A9#);
   S1D735 : aliased constant Code_Point_Array := (16#1D735#,16#10002207#);
   S1D736 : aliased constant Code_Point_Array := (16#1D736#,16#100003B1#);
   S1D737 : aliased constant Code_Point_Array := (16#1D737#,16#100003B2#);
   S1D738 : aliased constant Code_Point_Array := (16#1D738#,16#100003B3#);
   S1D739 : aliased constant Code_Point_Array := (16#1D739#,16#100003B4#);
   S1D73A : aliased constant Code_Point_Array := (16#1D73A#,16#100003B5#);
   S1D73B : aliased constant Code_Point_Array := (16#1D73B#,16#100003B6#);
   S1D73C : aliased constant Code_Point_Array := (16#1D73C#,16#100003B7#);
   S1D73D : aliased constant Code_Point_Array := (16#1D73D#,16#100003B8#);
   S1D73E : aliased constant Code_Point_Array := (16#1D73E#,16#100003B9#);
   S1D73F : aliased constant Code_Point_Array := (16#1D73F#,16#100003BA#);
   S1D740 : aliased constant Code_Point_Array := (16#1D740#,16#100003BB#);
   S1D741 : aliased constant Code_Point_Array := (16#1D741#,16#100003BC#);
   S1D742 : aliased constant Code_Point_Array := (16#1D742#,16#100003BD#);
   S1D743 : aliased constant Code_Point_Array := (16#1D743#,16#100003BE#);
   S1D744 : aliased constant Code_Point_Array := (16#1D744#,16#100003BF#);
   S1D745 : aliased constant Code_Point_Array := (16#1D745#,16#100003C0#);
   S1D746 : aliased constant Code_Point_Array := (16#1D746#,16#100003C1#);
   S1D747 : aliased constant Code_Point_Array := (16#1D747#,16#100003C2#);
   S1D748 : aliased constant Code_Point_Array := (16#1D748#,16#100003C3#);
   S1D749 : aliased constant Code_Point_Array := (16#1D749#,16#100003C4#);
   S1D74A : aliased constant Code_Point_Array := (16#1D74A#,16#100003C5#);
   S1D74B : aliased constant Code_Point_Array := (16#1D74B#,16#100003C6#);
   S1D74C : aliased constant Code_Point_Array := (16#1D74C#,16#100003C7#);
   S1D74D : aliased constant Code_Point_Array := (16#1D74D#,16#100003C8#);
   S1D74E : aliased constant Code_Point_Array := (16#1D74E#,16#100003C9#);
   S1D74F : aliased constant Code_Point_Array := (16#1D74F#,16#10002202#);
   S1D750 : aliased constant Code_Point_Array := (16#1D750#,16#100003F5#);
   S1D751 : aliased constant Code_Point_Array := (16#1D751#,16#100003D1#);
   S1D752 : aliased constant Code_Point_Array := (16#1D752#,16#100003F0#);
   S1D753 : aliased constant Code_Point_Array := (16#1D753#,16#100003D5#);
   S1D754 : aliased constant Code_Point_Array := (16#1D754#,16#100003F1#);
   S1D755 : aliased constant Code_Point_Array := (16#1D755#,16#100003D6#);
   S1D756 : aliased constant Code_Point_Array := (16#1D756#,16#10000391#);
   S1D757 : aliased constant Code_Point_Array := (16#1D757#,16#10000392#);
   S1D758 : aliased constant Code_Point_Array := (16#1D758#,16#10000393#);
   S1D759 : aliased constant Code_Point_Array := (16#1D759#,16#10000394#);
   S1D75A : aliased constant Code_Point_Array := (16#1D75A#,16#10000395#);
   S1D75B : aliased constant Code_Point_Array := (16#1D75B#,16#10000396#);
   S1D75C : aliased constant Code_Point_Array := (16#1D75C#,16#10000397#);
   S1D75D : aliased constant Code_Point_Array := (16#1D75D#,16#10000398#);
   S1D75E : aliased constant Code_Point_Array := (16#1D75E#,16#10000399#);
   S1D75F : aliased constant Code_Point_Array := (16#1D75F#,16#1000039A#);
   S1D760 : aliased constant Code_Point_Array := (16#1D760#,16#1000039B#);
   S1D761 : aliased constant Code_Point_Array := (16#1D761#,16#1000039C#);
   S1D762 : aliased constant Code_Point_Array := (16#1D762#,16#1000039D#);
   S1D763 : aliased constant Code_Point_Array := (16#1D763#,16#1000039E#);
   S1D764 : aliased constant Code_Point_Array := (16#1D764#,16#1000039F#);
   S1D765 : aliased constant Code_Point_Array := (16#1D765#,16#100003A0#);
   S1D766 : aliased constant Code_Point_Array := (16#1D766#,16#100003A1#);
   S1D767 : aliased constant Code_Point_Array := (16#1D767#,16#100003F4#);
   S1D768 : aliased constant Code_Point_Array := (16#1D768#,16#100003A3#);
   S1D769 : aliased constant Code_Point_Array := (16#1D769#,16#100003A4#);
   S1D76A : aliased constant Code_Point_Array := (16#1D76A#,16#100003A5#);
   S1D76B : aliased constant Code_Point_Array := (16#1D76B#,16#100003A6#);
   S1D76C : aliased constant Code_Point_Array := (16#1D76C#,16#100003A7#);
   S1D76D : aliased constant Code_Point_Array := (16#1D76D#,16#100003A8#);
   S1D76E : aliased constant Code_Point_Array := (16#1D76E#,16#100003A9#);
   S1D76F : aliased constant Code_Point_Array := (16#1D76F#,16#10002207#);
   S1D770 : aliased constant Code_Point_Array := (16#1D770#,16#100003B1#);
   S1D771 : aliased constant Code_Point_Array := (16#1D771#,16#100003B2#);
   S1D772 : aliased constant Code_Point_Array := (16#1D772#,16#100003B3#);
   S1D773 : aliased constant Code_Point_Array := (16#1D773#,16#100003B4#);
   S1D774 : aliased constant Code_Point_Array := (16#1D774#,16#100003B5#);
   S1D775 : aliased constant Code_Point_Array := (16#1D775#,16#100003B6#);
   S1D776 : aliased constant Code_Point_Array := (16#1D776#,16#100003B7#);
   S1D777 : aliased constant Code_Point_Array := (16#1D777#,16#100003B8#);
   S1D778 : aliased constant Code_Point_Array := (16#1D778#,16#100003B9#);
   S1D779 : aliased constant Code_Point_Array := (16#1D779#,16#100003BA#);
   S1D77A : aliased constant Code_Point_Array := (16#1D77A#,16#100003BB#);
   S1D77B : aliased constant Code_Point_Array := (16#1D77B#,16#100003BC#);
   S1D77C : aliased constant Code_Point_Array := (16#1D77C#,16#100003BD#);
   S1D77D : aliased constant Code_Point_Array := (16#1D77D#,16#100003BE#);
   S1D77E : aliased constant Code_Point_Array := (16#1D77E#,16#100003BF#);
   S1D77F : aliased constant Code_Point_Array := (16#1D77F#,16#100003C0#);
   S1D780 : aliased constant Code_Point_Array := (16#1D780#,16#100003C1#);
   S1D781 : aliased constant Code_Point_Array := (16#1D781#,16#100003C2#);
   S1D782 : aliased constant Code_Point_Array := (16#1D782#,16#100003C3#);
   S1D783 : aliased constant Code_Point_Array := (16#1D783#,16#100003C4#);
   S1D784 : aliased constant Code_Point_Array := (16#1D784#,16#100003C5#);
   S1D785 : aliased constant Code_Point_Array := (16#1D785#,16#100003C6#);
   S1D786 : aliased constant Code_Point_Array := (16#1D786#,16#100003C7#);
   S1D787 : aliased constant Code_Point_Array := (16#1D787#,16#100003C8#);
   S1D788 : aliased constant Code_Point_Array := (16#1D788#,16#100003C9#);
   S1D789 : aliased constant Code_Point_Array := (16#1D789#,16#10002202#);
   S1D78A : aliased constant Code_Point_Array := (16#1D78A#,16#100003F5#);
   S1D78B : aliased constant Code_Point_Array := (16#1D78B#,16#100003D1#);
   S1D78C : aliased constant Code_Point_Array := (16#1D78C#,16#100003F0#);
   S1D78D : aliased constant Code_Point_Array := (16#1D78D#,16#100003D5#);
   S1D78E : aliased constant Code_Point_Array := (16#1D78E#,16#100003F1#);
   S1D78F : aliased constant Code_Point_Array := (16#1D78F#,16#100003D6#);
   S1D790 : aliased constant Code_Point_Array := (16#1D790#,16#10000391#);
   S1D791 : aliased constant Code_Point_Array := (16#1D791#,16#10000392#);
   S1D792 : aliased constant Code_Point_Array := (16#1D792#,16#10000393#);
   S1D793 : aliased constant Code_Point_Array := (16#1D793#,16#10000394#);
   S1D794 : aliased constant Code_Point_Array := (16#1D794#,16#10000395#);
   S1D795 : aliased constant Code_Point_Array := (16#1D795#,16#10000396#);
   S1D796 : aliased constant Code_Point_Array := (16#1D796#,16#10000397#);
   S1D797 : aliased constant Code_Point_Array := (16#1D797#,16#10000398#);
   S1D798 : aliased constant Code_Point_Array := (16#1D798#,16#10000399#);
   S1D799 : aliased constant Code_Point_Array := (16#1D799#,16#1000039A#);
   S1D79A : aliased constant Code_Point_Array := (16#1D79A#,16#1000039B#);
   S1D79B : aliased constant Code_Point_Array := (16#1D79B#,16#1000039C#);
   S1D79C : aliased constant Code_Point_Array := (16#1D79C#,16#1000039D#);
   S1D79D : aliased constant Code_Point_Array := (16#1D79D#,16#1000039E#);
   S1D79E : aliased constant Code_Point_Array := (16#1D79E#,16#1000039F#);
   S1D79F : aliased constant Code_Point_Array := (16#1D79F#,16#100003A0#);
   S1D7A0 : aliased constant Code_Point_Array := (16#1D7A0#,16#100003A1#);
   S1D7A1 : aliased constant Code_Point_Array := (16#1D7A1#,16#100003F4#);
   S1D7A2 : aliased constant Code_Point_Array := (16#1D7A2#,16#100003A3#);
   S1D7A3 : aliased constant Code_Point_Array := (16#1D7A3#,16#100003A4#);
   S1D7A4 : aliased constant Code_Point_Array := (16#1D7A4#,16#100003A5#);
   S1D7A5 : aliased constant Code_Point_Array := (16#1D7A5#,16#100003A6#);
   S1D7A6 : aliased constant Code_Point_Array := (16#1D7A6#,16#100003A7#);
   S1D7A7 : aliased constant Code_Point_Array := (16#1D7A7#,16#100003A8#);
   S1D7A8 : aliased constant Code_Point_Array := (16#1D7A8#,16#100003A9#);
   S1D7A9 : aliased constant Code_Point_Array := (16#1D7A9#,16#10002207#);
   S1D7AA : aliased constant Code_Point_Array := (16#1D7AA#,16#100003B1#);
   S1D7AB : aliased constant Code_Point_Array := (16#1D7AB#,16#100003B2#);
   S1D7AC : aliased constant Code_Point_Array := (16#1D7AC#,16#100003B3#);
   S1D7AD : aliased constant Code_Point_Array := (16#1D7AD#,16#100003B4#);
   S1D7AE : aliased constant Code_Point_Array := (16#1D7AE#,16#100003B5#);
   S1D7AF : aliased constant Code_Point_Array := (16#1D7AF#,16#100003B6#);
   S1D7B0 : aliased constant Code_Point_Array := (16#1D7B0#,16#100003B7#);
   S1D7B1 : aliased constant Code_Point_Array := (16#1D7B1#,16#100003B8#);
   S1D7B2 : aliased constant Code_Point_Array := (16#1D7B2#,16#100003B9#);
   S1D7B3 : aliased constant Code_Point_Array := (16#1D7B3#,16#100003BA#);
   S1D7B4 : aliased constant Code_Point_Array := (16#1D7B4#,16#100003BB#);
   S1D7B5 : aliased constant Code_Point_Array := (16#1D7B5#,16#100003BC#);
   S1D7B6 : aliased constant Code_Point_Array := (16#1D7B6#,16#100003BD#);
   S1D7B7 : aliased constant Code_Point_Array := (16#1D7B7#,16#100003BE#);
   S1D7B8 : aliased constant Code_Point_Array := (16#1D7B8#,16#100003BF#);
   S1D7B9 : aliased constant Code_Point_Array := (16#1D7B9#,16#100003C0#);
   S1D7BA : aliased constant Code_Point_Array := (16#1D7BA#,16#100003C1#);
   S1D7BB : aliased constant Code_Point_Array := (16#1D7BB#,16#100003C2#);
   S1D7BC : aliased constant Code_Point_Array := (16#1D7BC#,16#100003C3#);
   S1D7BD : aliased constant Code_Point_Array := (16#1D7BD#,16#100003C4#);
   S1D7BE : aliased constant Code_Point_Array := (16#1D7BE#,16#100003C5#);
   S1D7BF : aliased constant Code_Point_Array := (16#1D7BF#,16#100003C6#);
   S1D7C0 : aliased constant Code_Point_Array := (16#1D7C0#,16#100003C7#);
   S1D7C1 : aliased constant Code_Point_Array := (16#1D7C1#,16#100003C8#);
   S1D7C2 : aliased constant Code_Point_Array := (16#1D7C2#,16#100003C9#);
   S1D7C3 : aliased constant Code_Point_Array := (16#1D7C3#,16#10002202#);
   S1D7C4 : aliased constant Code_Point_Array := (16#1D7C4#,16#100003F5#);
   S1D7C5 : aliased constant Code_Point_Array := (16#1D7C5#,16#100003D1#);
   S1D7C6 : aliased constant Code_Point_Array := (16#1D7C6#,16#100003F0#);
   S1D7C7 : aliased constant Code_Point_Array := (16#1D7C7#,16#100003D5#);
   S1D7C8 : aliased constant Code_Point_Array := (16#1D7C8#,16#100003F1#);
   S1D7C9 : aliased constant Code_Point_Array := (16#1D7C9#,16#100003D6#);
   S1D7CA : aliased constant Code_Point_Array := (16#1D7CA#,16#100003DC#);
   S1D7CB : aliased constant Code_Point_Array := (16#1D7CB#,16#100003DD#);
   S1D7CE : aliased constant Code_Point_Array := (16#1D7CE#,16#10000030#);
   S1D7CF : aliased constant Code_Point_Array := (16#1D7CF#,16#10000031#);
   S1D7D0 : aliased constant Code_Point_Array := (16#1D7D0#,16#10000032#);
   S1D7D1 : aliased constant Code_Point_Array := (16#1D7D1#,16#10000033#);
   S1D7D2 : aliased constant Code_Point_Array := (16#1D7D2#,16#10000034#);
   S1D7D3 : aliased constant Code_Point_Array := (16#1D7D3#,16#10000035#);
   S1D7D4 : aliased constant Code_Point_Array := (16#1D7D4#,16#10000036#);
   S1D7D5 : aliased constant Code_Point_Array := (16#1D7D5#,16#10000037#);
   S1D7D6 : aliased constant Code_Point_Array := (16#1D7D6#,16#10000038#);
   S1D7D7 : aliased constant Code_Point_Array := (16#1D7D7#,16#10000039#);
   S1D7D8 : aliased constant Code_Point_Array := (16#1D7D8#,16#10000030#);
   S1D7D9 : aliased constant Code_Point_Array := (16#1D7D9#,16#10000031#);
   S1D7DA : aliased constant Code_Point_Array := (16#1D7DA#,16#10000032#);
   S1D7DB : aliased constant Code_Point_Array := (16#1D7DB#,16#10000033#);
   S1D7DC : aliased constant Code_Point_Array := (16#1D7DC#,16#10000034#);
   S1D7DD : aliased constant Code_Point_Array := (16#1D7DD#,16#10000035#);
   S1D7DE : aliased constant Code_Point_Array := (16#1D7DE#,16#10000036#);
   S1D7DF : aliased constant Code_Point_Array := (16#1D7DF#,16#10000037#);
   S1D7E0 : aliased constant Code_Point_Array := (16#1D7E0#,16#10000038#);
   S1D7E1 : aliased constant Code_Point_Array := (16#1D7E1#,16#10000039#);
   S1D7E2 : aliased constant Code_Point_Array := (16#1D7E2#,16#10000030#);
   S1D7E3 : aliased constant Code_Point_Array := (16#1D7E3#,16#10000031#);
   S1D7E4 : aliased constant Code_Point_Array := (16#1D7E4#,16#10000032#);
   S1D7E5 : aliased constant Code_Point_Array := (16#1D7E5#,16#10000033#);
   S1D7E6 : aliased constant Code_Point_Array := (16#1D7E6#,16#10000034#);
   S1D7E7 : aliased constant Code_Point_Array := (16#1D7E7#,16#10000035#);
   S1D7E8 : aliased constant Code_Point_Array := (16#1D7E8#,16#10000036#);
   S1D7E9 : aliased constant Code_Point_Array := (16#1D7E9#,16#10000037#);
   S1D7EA : aliased constant Code_Point_Array := (16#1D7EA#,16#10000038#);
   S1D7EB : aliased constant Code_Point_Array := (16#1D7EB#,16#10000039#);
   S1D7EC : aliased constant Code_Point_Array := (16#1D7EC#,16#10000030#);
   S1D7ED : aliased constant Code_Point_Array := (16#1D7ED#,16#10000031#);
   S1D7EE : aliased constant Code_Point_Array := (16#1D7EE#,16#10000032#);
   S1D7EF : aliased constant Code_Point_Array := (16#1D7EF#,16#10000033#);
   S1D7F0 : aliased constant Code_Point_Array := (16#1D7F0#,16#10000034#);
   S1D7F1 : aliased constant Code_Point_Array := (16#1D7F1#,16#10000035#);
   S1D7F2 : aliased constant Code_Point_Array := (16#1D7F2#,16#10000036#);
   S1D7F3 : aliased constant Code_Point_Array := (16#1D7F3#,16#10000037#);
   S1D7F4 : aliased constant Code_Point_Array := (16#1D7F4#,16#10000038#);
   S1D7F5 : aliased constant Code_Point_Array := (16#1D7F5#,16#10000039#);
   S1D7F6 : aliased constant Code_Point_Array := (16#1D7F6#,16#10000030#);
   S1D7F7 : aliased constant Code_Point_Array := (16#1D7F7#,16#10000031#);
   S1D7F8 : aliased constant Code_Point_Array := (16#1D7F8#,16#10000032#);
   S1D7F9 : aliased constant Code_Point_Array := (16#1D7F9#,16#10000033#);
   S1D7FA : aliased constant Code_Point_Array := (16#1D7FA#,16#10000034#);
   S1D7FB : aliased constant Code_Point_Array := (16#1D7FB#,16#10000035#);
   S1D7FC : aliased constant Code_Point_Array := (16#1D7FC#,16#10000036#);
   S1D7FD : aliased constant Code_Point_Array := (16#1D7FD#,16#10000037#);
   S1D7FE : aliased constant Code_Point_Array := (16#1D7FE#,16#10000038#);
   S1D7FF : aliased constant Code_Point_Array := (16#1D7FF#,16#10000039#);
   S1E030 : aliased constant Code_Point_Array := (16#1E030#,16#10000430#);
   S1E031 : aliased constant Code_Point_Array := (16#1E031#,16#10000431#);
   S1E032 : aliased constant Code_Point_Array := (16#1E032#,16#10000432#);
   S1E033 : aliased constant Code_Point_Array := (16#1E033#,16#10000433#);
   S1E034 : aliased constant Code_Point_Array := (16#1E034#,16#10000434#);
   S1E035 : aliased constant Code_Point_Array := (16#1E035#,16#10000435#);
   S1E036 : aliased constant Code_Point_Array := (16#1E036#,16#10000436#);
   S1E037 : aliased constant Code_Point_Array := (16#1E037#,16#10000437#);
   S1E038 : aliased constant Code_Point_Array := (16#1E038#,16#10000438#);
   S1E039 : aliased constant Code_Point_Array := (16#1E039#,16#1000043A#);
   S1E03A : aliased constant Code_Point_Array := (16#1E03A#,16#1000043B#);
   S1E03B : aliased constant Code_Point_Array := (16#1E03B#,16#1000043C#);
   S1E03C : aliased constant Code_Point_Array := (16#1E03C#,16#1000043E#);
   S1E03D : aliased constant Code_Point_Array := (16#1E03D#,16#1000043F#);
   S1E03E : aliased constant Code_Point_Array := (16#1E03E#,16#10000440#);
   S1E03F : aliased constant Code_Point_Array := (16#1E03F#,16#10000441#);
   S1E040 : aliased constant Code_Point_Array := (16#1E040#,16#10000442#);
   S1E041 : aliased constant Code_Point_Array := (16#1E041#,16#10000443#);
   S1E042 : aliased constant Code_Point_Array := (16#1E042#,16#10000444#);
   S1E043 : aliased constant Code_Point_Array := (16#1E043#,16#10000445#);
   S1E044 : aliased constant Code_Point_Array := (16#1E044#,16#10000446#);
   S1E045 : aliased constant Code_Point_Array := (16#1E045#,16#10000447#);
   S1E046 : aliased constant Code_Point_Array := (16#1E046#,16#10000448#);
   S1E047 : aliased constant Code_Point_Array := (16#1E047#,16#1000044B#);
   S1E048 : aliased constant Code_Point_Array := (16#1E048#,16#1000044D#);
   S1E049 : aliased constant Code_Point_Array := (16#1E049#,16#1000044E#);
   S1E04A : aliased constant Code_Point_Array := (16#1E04A#,16#1000A689#);
   S1E04B : aliased constant Code_Point_Array := (16#1E04B#,16#100004D9#);
   S1E04C : aliased constant Code_Point_Array := (16#1E04C#,16#10000456#);
   S1E04D : aliased constant Code_Point_Array := (16#1E04D#,16#10000458#);
   S1E04E : aliased constant Code_Point_Array := (16#1E04E#,16#100004E9#);
   S1E04F : aliased constant Code_Point_Array := (16#1E04F#,16#100004AF#);
   S1E050 : aliased constant Code_Point_Array := (16#1E050#,16#100004CF#);
   S1E051 : aliased constant Code_Point_Array := (16#1E051#,16#10000430#);
   S1E052 : aliased constant Code_Point_Array := (16#1E052#,16#10000431#);
   S1E053 : aliased constant Code_Point_Array := (16#1E053#,16#10000432#);
   S1E054 : aliased constant Code_Point_Array := (16#1E054#,16#10000433#);
   S1E055 : aliased constant Code_Point_Array := (16#1E055#,16#10000434#);
   S1E056 : aliased constant Code_Point_Array := (16#1E056#,16#10000435#);
   S1E057 : aliased constant Code_Point_Array := (16#1E057#,16#10000436#);
   S1E058 : aliased constant Code_Point_Array := (16#1E058#,16#10000437#);
   S1E059 : aliased constant Code_Point_Array := (16#1E059#,16#10000438#);
   S1E05A : aliased constant Code_Point_Array := (16#1E05A#,16#1000043A#);
   S1E05B : aliased constant Code_Point_Array := (16#1E05B#,16#1000043B#);
   S1E05C : aliased constant Code_Point_Array := (16#1E05C#,16#1000043E#);
   S1E05D : aliased constant Code_Point_Array := (16#1E05D#,16#1000043F#);
   S1E05E : aliased constant Code_Point_Array := (16#1E05E#,16#10000441#);
   S1E05F : aliased constant Code_Point_Array := (16#1E05F#,16#10000443#);
   S1E060 : aliased constant Code_Point_Array := (16#1E060#,16#10000444#);
   S1E061 : aliased constant Code_Point_Array := (16#1E061#,16#10000445#);
   S1E062 : aliased constant Code_Point_Array := (16#1E062#,16#10000446#);
   S1E063 : aliased constant Code_Point_Array := (16#1E063#,16#10000447#);
   S1E064 : aliased constant Code_Point_Array := (16#1E064#,16#10000448#);
   S1E065 : aliased constant Code_Point_Array := (16#1E065#,16#1000044A#);
   S1E066 : aliased constant Code_Point_Array := (16#1E066#,16#1000044B#);
   S1E067 : aliased constant Code_Point_Array := (16#1E067#,16#10000491#);
   S1E068 : aliased constant Code_Point_Array := (16#1E068#,16#10000456#);
   S1E069 : aliased constant Code_Point_Array := (16#1E069#,16#10000455#);
   S1E06A : aliased constant Code_Point_Array := (16#1E06A#,16#1000045F#);
   S1E06B : aliased constant Code_Point_Array := (16#1E06B#,16#100004AB#);
   S1E06C : aliased constant Code_Point_Array := (16#1E06C#,16#1000A651#);
   S1E06D : aliased constant Code_Point_Array := (16#1E06D#,16#100004B1#);
   S1EE00 : aliased constant Code_Point_Array := (16#1EE00#,16#10000627#);
   S1EE01 : aliased constant Code_Point_Array := (16#1EE01#,16#10000628#);
   S1EE02 : aliased constant Code_Point_Array := (16#1EE02#,16#1000062C#);
   S1EE03 : aliased constant Code_Point_Array := (16#1EE03#,16#1000062F#);
   S1EE05 : aliased constant Code_Point_Array := (16#1EE05#,16#10000648#);
   S1EE06 : aliased constant Code_Point_Array := (16#1EE06#,16#10000632#);
   S1EE07 : aliased constant Code_Point_Array := (16#1EE07#,16#1000062D#);
   S1EE08 : aliased constant Code_Point_Array := (16#1EE08#,16#10000637#);
   S1EE09 : aliased constant Code_Point_Array := (16#1EE09#,16#1000064A#);
   S1EE0A : aliased constant Code_Point_Array := (16#1EE0A#,16#10000643#);
   S1EE0B : aliased constant Code_Point_Array := (16#1EE0B#,16#10000644#);
   S1EE0C : aliased constant Code_Point_Array := (16#1EE0C#,16#10000645#);
   S1EE0D : aliased constant Code_Point_Array := (16#1EE0D#,16#10000646#);
   S1EE0E : aliased constant Code_Point_Array := (16#1EE0E#,16#10000633#);
   S1EE0F : aliased constant Code_Point_Array := (16#1EE0F#,16#10000639#);
   S1EE10 : aliased constant Code_Point_Array := (16#1EE10#,16#10000641#);
   S1EE11 : aliased constant Code_Point_Array := (16#1EE11#,16#10000635#);
   S1EE12 : aliased constant Code_Point_Array := (16#1EE12#,16#10000642#);
   S1EE13 : aliased constant Code_Point_Array := (16#1EE13#,16#10000631#);
   S1EE14 : aliased constant Code_Point_Array := (16#1EE14#,16#10000634#);
   S1EE15 : aliased constant Code_Point_Array := (16#1EE15#,16#1000062A#);
   S1EE16 : aliased constant Code_Point_Array := (16#1EE16#,16#1000062B#);
   S1EE17 : aliased constant Code_Point_Array := (16#1EE17#,16#1000062E#);
   S1EE18 : aliased constant Code_Point_Array := (16#1EE18#,16#10000630#);
   S1EE19 : aliased constant Code_Point_Array := (16#1EE19#,16#10000636#);
   S1EE1A : aliased constant Code_Point_Array := (16#1EE1A#,16#10000638#);
   S1EE1B : aliased constant Code_Point_Array := (16#1EE1B#,16#1000063A#);
   S1EE1C : aliased constant Code_Point_Array := (16#1EE1C#,16#1000066E#);
   S1EE1D : aliased constant Code_Point_Array := (16#1EE1D#,16#100006BA#);
   S1EE1E : aliased constant Code_Point_Array := (16#1EE1E#,16#100006A1#);
   S1EE1F : aliased constant Code_Point_Array := (16#1EE1F#,16#1000066F#);
   S1EE21 : aliased constant Code_Point_Array := (16#1EE21#,16#10000628#);
   S1EE22 : aliased constant Code_Point_Array := (16#1EE22#,16#1000062C#);
   S1EE24 : aliased constant Code_Point_Array := (16#1EE24#,16#10000647#);
   S1EE27 : aliased constant Code_Point_Array := (16#1EE27#,16#1000062D#);
   S1EE29 : aliased constant Code_Point_Array := (16#1EE29#,16#1000064A#);
   S1EE2A : aliased constant Code_Point_Array := (16#1EE2A#,16#10000643#);
   S1EE2B : aliased constant Code_Point_Array := (16#1EE2B#,16#10000644#);
   S1EE2C : aliased constant Code_Point_Array := (16#1EE2C#,16#10000645#);
   S1EE2D : aliased constant Code_Point_Array := (16#1EE2D#,16#10000646#);
   S1EE2E : aliased constant Code_Point_Array := (16#1EE2E#,16#10000633#);
   S1EE2F : aliased constant Code_Point_Array := (16#1EE2F#,16#10000639#);
   S1EE30 : aliased constant Code_Point_Array := (16#1EE30#,16#10000641#);
   S1EE31 : aliased constant Code_Point_Array := (16#1EE31#,16#10000635#);
   S1EE32 : aliased constant Code_Point_Array := (16#1EE32#,16#10000642#);
   S1EE34 : aliased constant Code_Point_Array := (16#1EE34#,16#10000634#);
   S1EE35 : aliased constant Code_Point_Array := (16#1EE35#,16#1000062A#);
   S1EE36 : aliased constant Code_Point_Array := (16#1EE36#,16#1000062B#);
   S1EE37 : aliased constant Code_Point_Array := (16#1EE37#,16#1000062E#);
   S1EE39 : aliased constant Code_Point_Array := (16#1EE39#,16#10000636#);
   S1EE3B : aliased constant Code_Point_Array := (16#1EE3B#,16#1000063A#);
   S1EE42 : aliased constant Code_Point_Array := (16#1EE42#,16#1000062C#);
   S1EE47 : aliased constant Code_Point_Array := (16#1EE47#,16#1000062D#);
   S1EE49 : aliased constant Code_Point_Array := (16#1EE49#,16#1000064A#);
   S1EE4B : aliased constant Code_Point_Array := (16#1EE4B#,16#10000644#);
   S1EE4D : aliased constant Code_Point_Array := (16#1EE4D#,16#10000646#);
   S1EE4E : aliased constant Code_Point_Array := (16#1EE4E#,16#10000633#);
   S1EE4F : aliased constant Code_Point_Array := (16#1EE4F#,16#10000639#);
   S1EE51 : aliased constant Code_Point_Array := (16#1EE51#,16#10000635#);
   S1EE52 : aliased constant Code_Point_Array := (16#1EE52#,16#10000642#);
   S1EE54 : aliased constant Code_Point_Array := (16#1EE54#,16#10000634#);
   S1EE57 : aliased constant Code_Point_Array := (16#1EE57#,16#1000062E#);
   S1EE59 : aliased constant Code_Point_Array := (16#1EE59#,16#10000636#);
   S1EE5B : aliased constant Code_Point_Array := (16#1EE5B#,16#1000063A#);
   S1EE5D : aliased constant Code_Point_Array := (16#1EE5D#,16#100006BA#);
   S1EE5F : aliased constant Code_Point_Array := (16#1EE5F#,16#1000066F#);
   S1EE61 : aliased constant Code_Point_Array := (16#1EE61#,16#10000628#);
   S1EE62 : aliased constant Code_Point_Array := (16#1EE62#,16#1000062C#);
   S1EE64 : aliased constant Code_Point_Array := (16#1EE64#,16#10000647#);
   S1EE67 : aliased constant Code_Point_Array := (16#1EE67#,16#1000062D#);
   S1EE68 : aliased constant Code_Point_Array := (16#1EE68#,16#10000637#);
   S1EE69 : aliased constant Code_Point_Array := (16#1EE69#,16#1000064A#);
   S1EE6A : aliased constant Code_Point_Array := (16#1EE6A#,16#10000643#);
   S1EE6C : aliased constant Code_Point_Array := (16#1EE6C#,16#10000645#);
   S1EE6D : aliased constant Code_Point_Array := (16#1EE6D#,16#10000646#);
   S1EE6E : aliased constant Code_Point_Array := (16#1EE6E#,16#10000633#);
   S1EE6F : aliased constant Code_Point_Array := (16#1EE6F#,16#10000639#);
   S1EE70 : aliased constant Code_Point_Array := (16#1EE70#,16#10000641#);
   S1EE71 : aliased constant Code_Point_Array := (16#1EE71#,16#10000635#);
   S1EE72 : aliased constant Code_Point_Array := (16#1EE72#,16#10000642#);
   S1EE74 : aliased constant Code_Point_Array := (16#1EE74#,16#10000634#);
   S1EE75 : aliased constant Code_Point_Array := (16#1EE75#,16#1000062A#);
   S1EE76 : aliased constant Code_Point_Array := (16#1EE76#,16#1000062B#);
   S1EE77 : aliased constant Code_Point_Array := (16#1EE77#,16#1000062E#);
   S1EE79 : aliased constant Code_Point_Array := (16#1EE79#,16#10000636#);
   S1EE7A : aliased constant Code_Point_Array := (16#1EE7A#,16#10000638#);
   S1EE7B : aliased constant Code_Point_Array := (16#1EE7B#,16#1000063A#);
   S1EE7C : aliased constant Code_Point_Array := (16#1EE7C#,16#1000066E#);
   S1EE7E : aliased constant Code_Point_Array := (16#1EE7E#,16#100006A1#);
   S1EE80 : aliased constant Code_Point_Array := (16#1EE80#,16#10000627#);
   S1EE81 : aliased constant Code_Point_Array := (16#1EE81#,16#10000628#);
   S1EE82 : aliased constant Code_Point_Array := (16#1EE82#,16#1000062C#);
   S1EE83 : aliased constant Code_Point_Array := (16#1EE83#,16#1000062F#);
   S1EE84 : aliased constant Code_Point_Array := (16#1EE84#,16#10000647#);
   S1EE85 : aliased constant Code_Point_Array := (16#1EE85#,16#10000648#);
   S1EE86 : aliased constant Code_Point_Array := (16#1EE86#,16#10000632#);
   S1EE87 : aliased constant Code_Point_Array := (16#1EE87#,16#1000062D#);
   S1EE88 : aliased constant Code_Point_Array := (16#1EE88#,16#10000637#);
   S1EE89 : aliased constant Code_Point_Array := (16#1EE89#,16#1000064A#);
   S1EE8B : aliased constant Code_Point_Array := (16#1EE8B#,16#10000644#);
   S1EE8C : aliased constant Code_Point_Array := (16#1EE8C#,16#10000645#);
   S1EE8D : aliased constant Code_Point_Array := (16#1EE8D#,16#10000646#);
   S1EE8E : aliased constant Code_Point_Array := (16#1EE8E#,16#10000633#);
   S1EE8F : aliased constant Code_Point_Array := (16#1EE8F#,16#10000639#);
   S1EE90 : aliased constant Code_Point_Array := (16#1EE90#,16#10000641#);
   S1EE91 : aliased constant Code_Point_Array := (16#1EE91#,16#10000635#);
   S1EE92 : aliased constant Code_Point_Array := (16#1EE92#,16#10000642#);
   S1EE93 : aliased constant Code_Point_Array := (16#1EE93#,16#10000631#);
   S1EE94 : aliased constant Code_Point_Array := (16#1EE94#,16#10000634#);
   S1EE95 : aliased constant Code_Point_Array := (16#1EE95#,16#1000062A#);
   S1EE96 : aliased constant Code_Point_Array := (16#1EE96#,16#1000062B#);
   S1EE97 : aliased constant Code_Point_Array := (16#1EE97#,16#1000062E#);
   S1EE98 : aliased constant Code_Point_Array := (16#1EE98#,16#10000630#);
   S1EE99 : aliased constant Code_Point_Array := (16#1EE99#,16#10000636#);
   S1EE9A : aliased constant Code_Point_Array := (16#1EE9A#,16#10000638#);
   S1EE9B : aliased constant Code_Point_Array := (16#1EE9B#,16#1000063A#);
   S1EEA1 : aliased constant Code_Point_Array := (16#1EEA1#,16#10000628#);
   S1EEA2 : aliased constant Code_Point_Array := (16#1EEA2#,16#1000062C#);
   S1EEA3 : aliased constant Code_Point_Array := (16#1EEA3#,16#1000062F#);
   S1EEA5 : aliased constant Code_Point_Array := (16#1EEA5#,16#10000648#);
   S1EEA6 : aliased constant Code_Point_Array := (16#1EEA6#,16#10000632#);
   S1EEA7 : aliased constant Code_Point_Array := (16#1EEA7#,16#1000062D#);
   S1EEA8 : aliased constant Code_Point_Array := (16#1EEA8#,16#10000637#);
   S1EEA9 : aliased constant Code_Point_Array := (16#1EEA9#,16#1000064A#);
   S1EEAB : aliased constant Code_Point_Array := (16#1EEAB#,16#10000644#);
   S1EEAC : aliased constant Code_Point_Array := (16#1EEAC#,16#10000645#);
   S1EEAD : aliased constant Code_Point_Array := (16#1EEAD#,16#10000646#);
   S1EEAE : aliased constant Code_Point_Array := (16#1EEAE#,16#10000633#);
   S1EEAF : aliased constant Code_Point_Array := (16#1EEAF#,16#10000639#);
   S1EEB0 : aliased constant Code_Point_Array := (16#1EEB0#,16#10000641#);
   S1EEB1 : aliased constant Code_Point_Array := (16#1EEB1#,16#10000635#);
   S1EEB2 : aliased constant Code_Point_Array := (16#1EEB2#,16#10000642#);
   S1EEB3 : aliased constant Code_Point_Array := (16#1EEB3#,16#10000631#);
   S1EEB4 : aliased constant Code_Point_Array := (16#1EEB4#,16#10000634#);
   S1EEB5 : aliased constant Code_Point_Array := (16#1EEB5#,16#1000062A#);
   S1EEB6 : aliased constant Code_Point_Array := (16#1EEB6#,16#1000062B#);
   S1EEB7 : aliased constant Code_Point_Array := (16#1EEB7#,16#1000062E#);
   S1EEB8 : aliased constant Code_Point_Array := (16#1EEB8#,16#10000630#);
   S1EEB9 : aliased constant Code_Point_Array := (16#1EEB9#,16#10000636#);
   S1EEBA : aliased constant Code_Point_Array := (16#1EEBA#,16#10000638#);
   S1EEBB : aliased constant Code_Point_Array := (16#1EEBB#,16#1000063A#);
   S1F100 : aliased constant Code_Point_Array := (16#1F100#,16#10000030#,16#2E#);
   S1F101 : aliased constant Code_Point_Array := (16#1F101#,16#10000030#,16#2C#);
   S1F102 : aliased constant Code_Point_Array := (16#1F102#,16#10000031#,16#2C#);
   S1F103 : aliased constant Code_Point_Array := (16#1F103#,16#10000032#,16#2C#);
   S1F104 : aliased constant Code_Point_Array := (16#1F104#,16#10000033#,16#2C#);
   S1F105 : aliased constant Code_Point_Array := (16#1F105#,16#10000034#,16#2C#);
   S1F106 : aliased constant Code_Point_Array := (16#1F106#,16#10000035#,16#2C#);
   S1F107 : aliased constant Code_Point_Array := (16#1F107#,16#10000036#,16#2C#);
   S1F108 : aliased constant Code_Point_Array := (16#1F108#,16#10000037#,16#2C#);
   S1F109 : aliased constant Code_Point_Array := (16#1F109#,16#10000038#,16#2C#);
   S1F10A : aliased constant Code_Point_Array := (16#1F10A#,16#10000039#,16#2C#);
   S1F110 : aliased constant Code_Point_Array := (16#1F110#,16#10000028#,16#41#,16#29#);
   S1F111 : aliased constant Code_Point_Array := (16#1F111#,16#10000028#,16#42#,16#29#);
   S1F112 : aliased constant Code_Point_Array := (16#1F112#,16#10000028#,16#43#,16#29#);
   S1F113 : aliased constant Code_Point_Array := (16#1F113#,16#10000028#,16#44#,16#29#);
   S1F114 : aliased constant Code_Point_Array := (16#1F114#,16#10000028#,16#45#,16#29#);
   S1F115 : aliased constant Code_Point_Array := (16#1F115#,16#10000028#,16#46#,16#29#);
   S1F116 : aliased constant Code_Point_Array := (16#1F116#,16#10000028#,16#47#,16#29#);
   S1F117 : aliased constant Code_Point_Array := (16#1F117#,16#10000028#,16#48#,16#29#);
   S1F118 : aliased constant Code_Point_Array := (16#1F118#,16#10000028#,16#49#,16#29#);
   S1F119 : aliased constant Code_Point_Array := (16#1F119#,16#10000028#,16#4A#,16#29#);
   S1F11A : aliased constant Code_Point_Array := (16#1F11A#,16#10000028#,16#4B#,16#29#);
   S1F11B : aliased constant Code_Point_Array := (16#1F11B#,16#10000028#,16#4C#,16#29#);
   S1F11C : aliased constant Code_Point_Array := (16#1F11C#,16#10000028#,16#4D#,16#29#);
   S1F11D : aliased constant Code_Point_Array := (16#1F11D#,16#10000028#,16#4E#,16#29#);
   S1F11E : aliased constant Code_Point_Array := (16#1F11E#,16#10000028#,16#4F#,16#29#);
   S1F11F : aliased constant Code_Point_Array := (16#1F11F#,16#10000028#,16#50#,16#29#);
   S1F120 : aliased constant Code_Point_Array := (16#1F120#,16#10000028#,16#51#,16#29#);
   S1F121 : aliased constant Code_Point_Array := (16#1F121#,16#10000028#,16#52#,16#29#);
   S1F122 : aliased constant Code_Point_Array := (16#1F122#,16#10000028#,16#53#,16#29#);
   S1F123 : aliased constant Code_Point_Array := (16#1F123#,16#10000028#,16#54#,16#29#);
   S1F124 : aliased constant Code_Point_Array := (16#1F124#,16#10000028#,16#55#,16#29#);
   S1F125 : aliased constant Code_Point_Array := (16#1F125#,16#10000028#,16#56#,16#29#);
   S1F126 : aliased constant Code_Point_Array := (16#1F126#,16#10000028#,16#57#,16#29#);
   S1F127 : aliased constant Code_Point_Array := (16#1F127#,16#10000028#,16#58#,16#29#);
   S1F128 : aliased constant Code_Point_Array := (16#1F128#,16#10000028#,16#59#,16#29#);
   S1F129 : aliased constant Code_Point_Array := (16#1F129#,16#10000028#,16#5A#,16#29#);
   S1F12A : aliased constant Code_Point_Array := (16#1F12A#,16#10003014#,16#53#,16#3015#);
   S1F12B : aliased constant Code_Point_Array := (16#1F12B#,16#10000043#);
   S1F12C : aliased constant Code_Point_Array := (16#1F12C#,16#10000052#);
   S1F12D : aliased constant Code_Point_Array := (16#1F12D#,16#10000043#,16#44#);
   S1F12E : aliased constant Code_Point_Array := (16#1F12E#,16#10000057#,16#5A#);
   S1F130 : aliased constant Code_Point_Array := (16#1F130#,16#10000041#);
   S1F131 : aliased constant Code_Point_Array := (16#1F131#,16#10000042#);
   S1F132 : aliased constant Code_Point_Array := (16#1F132#,16#10000043#);
   S1F133 : aliased constant Code_Point_Array := (16#1F133#,16#10000044#);
   S1F134 : aliased constant Code_Point_Array := (16#1F134#,16#10000045#);
   S1F135 : aliased constant Code_Point_Array := (16#1F135#,16#10000046#);
   S1F136 : aliased constant Code_Point_Array := (16#1F136#,16#10000047#);
   S1F137 : aliased constant Code_Point_Array := (16#1F137#,16#10000048#);
   S1F138 : aliased constant Code_Point_Array := (16#1F138#,16#10000049#);
   S1F139 : aliased constant Code_Point_Array := (16#1F139#,16#1000004A#);
   S1F13A : aliased constant Code_Point_Array := (16#1F13A#,16#1000004B#);
   S1F13B : aliased constant Code_Point_Array := (16#1F13B#,16#1000004C#);
   S1F13C : aliased constant Code_Point_Array := (16#1F13C#,16#1000004D#);
   S1F13D : aliased constant Code_Point_Array := (16#1F13D#,16#1000004E#);
   S1F13E : aliased constant Code_Point_Array := (16#1F13E#,16#1000004F#);
   S1F13F : aliased constant Code_Point_Array := (16#1F13F#,16#10000050#);
   S1F140 : aliased constant Code_Point_Array := (16#1F140#,16#10000051#);
   S1F141 : aliased constant Code_Point_Array := (16#1F141#,16#10000052#);
   S1F142 : aliased constant Code_Point_Array := (16#1F142#,16#10000053#);
   S1F143 : aliased constant Code_Point_Array := (16#1F143#,16#10000054#);
   S1F144 : aliased constant Code_Point_Array := (16#1F144#,16#10000055#);
   S1F145 : aliased constant Code_Point_Array := (16#1F145#,16#10000056#);
   S1F146 : aliased constant Code_Point_Array := (16#1F146#,16#10000057#);
   S1F147 : aliased constant Code_Point_Array := (16#1F147#,16#10000058#);
   S1F148 : aliased constant Code_Point_Array := (16#1F148#,16#10000059#);
   S1F149 : aliased constant Code_Point_Array := (16#1F149#,16#1000005A#);
   S1F14A : aliased constant Code_Point_Array := (16#1F14A#,16#10000048#,16#56#);
   S1F14B : aliased constant Code_Point_Array := (16#1F14B#,16#1000004D#,16#56#);
   S1F14C : aliased constant Code_Point_Array := (16#1F14C#,16#10000053#,16#44#);
   S1F14D : aliased constant Code_Point_Array := (16#1F14D#,16#10000053#,16#53#);
   S1F14E : aliased constant Code_Point_Array := (16#1F14E#,16#10000050#,16#50#,16#56#);
   S1F14F : aliased constant Code_Point_Array := (16#1F14F#,16#10000057#,16#43#);
   S1F16A : aliased constant Code_Point_Array := (16#1F16A#,16#1000004D#,16#43#);
   S1F16B : aliased constant Code_Point_Array := (16#1F16B#,16#1000004D#,16#44#);
   S1F16C : aliased constant Code_Point_Array := (16#1F16C#,16#1000004D#,16#52#);
   S1F190 : aliased constant Code_Point_Array := (16#1F190#,16#10000044#,16#4A#);
   S1F200 : aliased constant Code_Point_Array := (16#1F200#,16#1000307B#,16#304B#);
   S1F201 : aliased constant Code_Point_Array := (16#1F201#,16#100030B3#,16#30B3#);
   S1F202 : aliased constant Code_Point_Array := (16#1F202#,16#100030B5#);
   S1F210 : aliased constant Code_Point_Array := (16#1F210#,16#1000624B#);
   S1F211 : aliased constant Code_Point_Array := (16#1F211#,16#10005B57#);
   S1F212 : aliased constant Code_Point_Array := (16#1F212#,16#100053CC#);
   S1F213 : aliased constant Code_Point_Array := (16#1F213#,16#100030C7#);
   S1F214 : aliased constant Code_Point_Array := (16#1F214#,16#10004E8C#);
   S1F215 : aliased constant Code_Point_Array := (16#1F215#,16#1000591A#);
   S1F216 : aliased constant Code_Point_Array := (16#1F216#,16#100089E3#);
   S1F217 : aliased constant Code_Point_Array := (16#1F217#,16#10005929#);
   S1F218 : aliased constant Code_Point_Array := (16#1F218#,16#10004EA4#);
   S1F219 : aliased constant Code_Point_Array := (16#1F219#,16#10006620#);
   S1F21A : aliased constant Code_Point_Array := (16#1F21A#,16#10007121#);
   S1F21B : aliased constant Code_Point_Array := (16#1F21B#,16#10006599#);
   S1F21C : aliased constant Code_Point_Array := (16#1F21C#,16#1000524D#);
   S1F21D : aliased constant Code_Point_Array := (16#1F21D#,16#10005F8C#);
   S1F21E : aliased constant Code_Point_Array := (16#1F21E#,16#1000518D#);
   S1F21F : aliased constant Code_Point_Array := (16#1F21F#,16#100065B0#);
   S1F220 : aliased constant Code_Point_Array := (16#1F220#,16#1000521D#);
   S1F221 : aliased constant Code_Point_Array := (16#1F221#,16#10007D42#);
   S1F222 : aliased constant Code_Point_Array := (16#1F222#,16#1000751F#);
   S1F223 : aliased constant Code_Point_Array := (16#1F223#,16#10008CA9#);
   S1F224 : aliased constant Code_Point_Array := (16#1F224#,16#100058F0#);
   S1F225 : aliased constant Code_Point_Array := (16#1F225#,16#10005439#);
   S1F226 : aliased constant Code_Point_Array := (16#1F226#,16#10006F14#);
   S1F227 : aliased constant Code_Point_Array := (16#1F227#,16#10006295#);
   S1F228 : aliased constant Code_Point_Array := (16#1F228#,16#10006355#);
   S1F229 : aliased constant Code_Point_Array := (16#1F229#,16#10004E00#);
   S1F22A : aliased constant Code_Point_Array := (16#1F22A#,16#10004E09#);
   S1F22B : aliased constant Code_Point_Array := (16#1F22B#,16#1000904A#);
   S1F22C : aliased constant Code_Point_Array := (16#1F22C#,16#10005DE6#);
   S1F22D : aliased constant Code_Point_Array := (16#1F22D#,16#10004E2D#);
   S1F22E : aliased constant Code_Point_Array := (16#1F22E#,16#100053F3#);
   S1F22F : aliased constant Code_Point_Array := (16#1F22F#,16#10006307#);
   S1F230 : aliased constant Code_Point_Array := (16#1F230#,16#10008D70#);
   S1F231 : aliased constant Code_Point_Array := (16#1F231#,16#10006253#);
   S1F232 : aliased constant Code_Point_Array := (16#1F232#,16#10007981#);
   S1F233 : aliased constant Code_Point_Array := (16#1F233#,16#10007A7A#);
   S1F234 : aliased constant Code_Point_Array := (16#1F234#,16#10005408#);
   S1F235 : aliased constant Code_Point_Array := (16#1F235#,16#10006E80#);
   S1F236 : aliased constant Code_Point_Array := (16#1F236#,16#10006709#);
   S1F237 : aliased constant Code_Point_Array := (16#1F237#,16#10006708#);
   S1F238 : aliased constant Code_Point_Array := (16#1F238#,16#10007533#);
   S1F239 : aliased constant Code_Point_Array := (16#1F239#,16#10005272#);
   S1F23A : aliased constant Code_Point_Array := (16#1F23A#,16#100055B6#);
   S1F23B : aliased constant Code_Point_Array := (16#1F23B#,16#1000914D#);
   S1F240 : aliased constant Code_Point_Array := (16#1F240#,16#10003014#,16#672C#,16#3015#);
   S1F241 : aliased constant Code_Point_Array := (16#1F241#,16#10003014#,16#4E09#,16#3015#);
   S1F242 : aliased constant Code_Point_Array := (16#1F242#,16#10003014#,16#4E8C#,16#3015#);
   S1F243 : aliased constant Code_Point_Array := (16#1F243#,16#10003014#,16#5B89#,16#3015#);
   S1F244 : aliased constant Code_Point_Array := (16#1F244#,16#10003014#,16#70B9#,16#3015#);
   S1F245 : aliased constant Code_Point_Array := (16#1F245#,16#10003014#,16#6253#,16#3015#);
   S1F246 : aliased constant Code_Point_Array := (16#1F246#,16#10003014#,16#76D7#,16#3015#);
   S1F247 : aliased constant Code_Point_Array := (16#1F247#,16#10003014#,16#52DD#,16#3015#);
   S1F248 : aliased constant Code_Point_Array := (16#1F248#,16#10003014#,16#6557#,16#3015#);
   S1F250 : aliased constant Code_Point_Array := (16#1F250#,16#10005F97#);
   S1F251 : aliased constant Code_Point_Array := (16#1F251#,16#100053EF#);
   S1FBF0 : aliased constant Code_Point_Array := (16#1FBF0#,16#10000030#);
   S1FBF1 : aliased constant Code_Point_Array := (16#1FBF1#,16#10000031#);
   S1FBF2 : aliased constant Code_Point_Array := (16#1FBF2#,16#10000032#);
   S1FBF3 : aliased constant Code_Point_Array := (16#1FBF3#,16#10000033#);
   S1FBF4 : aliased constant Code_Point_Array := (16#1FBF4#,16#10000034#);
   S1FBF5 : aliased constant Code_Point_Array := (16#1FBF5#,16#10000035#);
   S1FBF6 : aliased constant Code_Point_Array := (16#1FBF6#,16#10000036#);
   S1FBF7 : aliased constant Code_Point_Array := (16#1FBF7#,16#10000037#);
   S1FBF8 : aliased constant Code_Point_Array := (16#1FBF8#,16#10000038#);
   S1FBF9 : aliased constant Code_Point_Array := (16#1FBF9#,16#10000039#);
   S2F800 : aliased constant Code_Point_Array := (16#2F800#,16#4E3D#);
   S2F801 : aliased constant Code_Point_Array := (16#2F801#,16#4E38#);
   S2F802 : aliased constant Code_Point_Array := (16#2F802#,16#4E41#);
   S2F803 : aliased constant Code_Point_Array := (16#2F803#,16#20122#);
   S2F804 : aliased constant Code_Point_Array := (16#2F804#,16#4F60#);
   S2F805 : aliased constant Code_Point_Array := (16#2F805#,16#4FAE#);
   S2F806 : aliased constant Code_Point_Array := (16#2F806#,16#4FBB#);
   S2F807 : aliased constant Code_Point_Array := (16#2F807#,16#5002#);
   S2F808 : aliased constant Code_Point_Array := (16#2F808#,16#507A#);
   S2F809 : aliased constant Code_Point_Array := (16#2F809#,16#5099#);
   S2F80A : aliased constant Code_Point_Array := (16#2F80A#,16#50E7#);
   S2F80B : aliased constant Code_Point_Array := (16#2F80B#,16#50CF#);
   S2F80C : aliased constant Code_Point_Array := (16#2F80C#,16#349E#);
   S2F80D : aliased constant Code_Point_Array := (16#2F80D#,16#2063A#);
   S2F80E : aliased constant Code_Point_Array := (16#2F80E#,16#514D#);
   S2F80F : aliased constant Code_Point_Array := (16#2F80F#,16#5154#);
   S2F810 : aliased constant Code_Point_Array := (16#2F810#,16#5164#);
   S2F811 : aliased constant Code_Point_Array := (16#2F811#,16#5177#);
   S2F812 : aliased constant Code_Point_Array := (16#2F812#,16#2051C#);
   S2F813 : aliased constant Code_Point_Array := (16#2F813#,16#34B9#);
   S2F814 : aliased constant Code_Point_Array := (16#2F814#,16#5167#);
   S2F815 : aliased constant Code_Point_Array := (16#2F815#,16#518D#);
   S2F816 : aliased constant Code_Point_Array := (16#2F816#,16#2054B#);
   S2F817 : aliased constant Code_Point_Array := (16#2F817#,16#5197#);
   S2F818 : aliased constant Code_Point_Array := (16#2F818#,16#51A4#);
   S2F819 : aliased constant Code_Point_Array := (16#2F819#,16#4ECC#);
   S2F81A : aliased constant Code_Point_Array := (16#2F81A#,16#51AC#);
   S2F81B : aliased constant Code_Point_Array := (16#2F81B#,16#51B5#);
   S2F81C : aliased constant Code_Point_Array := (16#2F81C#,16#291DF#);
   S2F81D : aliased constant Code_Point_Array := (16#2F81D#,16#51F5#);
   S2F81E : aliased constant Code_Point_Array := (16#2F81E#,16#5203#);
   S2F81F : aliased constant Code_Point_Array := (16#2F81F#,16#34DF#);
   S2F820 : aliased constant Code_Point_Array := (16#2F820#,16#523B#);
   S2F821 : aliased constant Code_Point_Array := (16#2F821#,16#5246#);
   S2F822 : aliased constant Code_Point_Array := (16#2F822#,16#5272#);
   S2F823 : aliased constant Code_Point_Array := (16#2F823#,16#5277#);
   S2F824 : aliased constant Code_Point_Array := (16#2F824#,16#3515#);
   S2F825 : aliased constant Code_Point_Array := (16#2F825#,16#52C7#);
   S2F826 : aliased constant Code_Point_Array := (16#2F826#,16#52C9#);
   S2F827 : aliased constant Code_Point_Array := (16#2F827#,16#52E4#);
   S2F828 : aliased constant Code_Point_Array := (16#2F828#,16#52FA#);
   S2F829 : aliased constant Code_Point_Array := (16#2F829#,16#5305#);
   S2F82A : aliased constant Code_Point_Array := (16#2F82A#,16#5306#);
   S2F82B : aliased constant Code_Point_Array := (16#2F82B#,16#5317#);
   S2F82C : aliased constant Code_Point_Array := (16#2F82C#,16#5349#);
   S2F82D : aliased constant Code_Point_Array := (16#2F82D#,16#5351#);
   S2F82E : aliased constant Code_Point_Array := (16#2F82E#,16#535A#);
   S2F82F : aliased constant Code_Point_Array := (16#2F82F#,16#5373#);
   S2F830 : aliased constant Code_Point_Array := (16#2F830#,16#537D#);
   S2F831 : aliased constant Code_Point_Array := (16#2F831#,16#537F#);
   S2F832 : aliased constant Code_Point_Array := (16#2F832#,16#537F#);
   S2F833 : aliased constant Code_Point_Array := (16#2F833#,16#537F#);
   S2F834 : aliased constant Code_Point_Array := (16#2F834#,16#20A2C#);
   S2F835 : aliased constant Code_Point_Array := (16#2F835#,16#7070#);
   S2F836 : aliased constant Code_Point_Array := (16#2F836#,16#53CA#);
   S2F837 : aliased constant Code_Point_Array := (16#2F837#,16#53DF#);
   S2F838 : aliased constant Code_Point_Array := (16#2F838#,16#20B63#);
   S2F839 : aliased constant Code_Point_Array := (16#2F839#,16#53EB#);
   S2F83A : aliased constant Code_Point_Array := (16#2F83A#,16#53F1#);
   S2F83B : aliased constant Code_Point_Array := (16#2F83B#,16#5406#);
   S2F83C : aliased constant Code_Point_Array := (16#2F83C#,16#549E#);
   S2F83D : aliased constant Code_Point_Array := (16#2F83D#,16#5438#);
   S2F83E : aliased constant Code_Point_Array := (16#2F83E#,16#5448#);
   S2F83F : aliased constant Code_Point_Array := (16#2F83F#,16#5468#);
   S2F840 : aliased constant Code_Point_Array := (16#2F840#,16#54A2#);
   S2F841 : aliased constant Code_Point_Array := (16#2F841#,16#54F6#);
   S2F842 : aliased constant Code_Point_Array := (16#2F842#,16#5510#);
   S2F843 : aliased constant Code_Point_Array := (16#2F843#,16#5553#);
   S2F844 : aliased constant Code_Point_Array := (16#2F844#,16#5563#);
   S2F845 : aliased constant Code_Point_Array := (16#2F845#,16#5584#);
   S2F846 : aliased constant Code_Point_Array := (16#2F846#,16#5584#);
   S2F847 : aliased constant Code_Point_Array := (16#2F847#,16#5599#);
   S2F848 : aliased constant Code_Point_Array := (16#2F848#,16#55AB#);
   S2F849 : aliased constant Code_Point_Array := (16#2F849#,16#55B3#);
   S2F84A : aliased constant Code_Point_Array := (16#2F84A#,16#55C2#);
   S2F84B : aliased constant Code_Point_Array := (16#2F84B#,16#5716#);
   S2F84C : aliased constant Code_Point_Array := (16#2F84C#,16#5606#);
   S2F84D : aliased constant Code_Point_Array := (16#2F84D#,16#5717#);
   S2F84E : aliased constant Code_Point_Array := (16#2F84E#,16#5651#);
   S2F84F : aliased constant Code_Point_Array := (16#2F84F#,16#5674#);
   S2F850 : aliased constant Code_Point_Array := (16#2F850#,16#5207#);
   S2F851 : aliased constant Code_Point_Array := (16#2F851#,16#58EE#);
   S2F852 : aliased constant Code_Point_Array := (16#2F852#,16#57CE#);
   S2F853 : aliased constant Code_Point_Array := (16#2F853#,16#57F4#);
   S2F854 : aliased constant Code_Point_Array := (16#2F854#,16#580D#);
   S2F855 : aliased constant Code_Point_Array := (16#2F855#,16#578B#);
   S2F856 : aliased constant Code_Point_Array := (16#2F856#,16#5832#);
   S2F857 : aliased constant Code_Point_Array := (16#2F857#,16#5831#);
   S2F858 : aliased constant Code_Point_Array := (16#2F858#,16#58AC#);
   S2F859 : aliased constant Code_Point_Array := (16#2F859#,16#214E4#);
   S2F85A : aliased constant Code_Point_Array := (16#2F85A#,16#58F2#);
   S2F85B : aliased constant Code_Point_Array := (16#2F85B#,16#58F7#);
   S2F85C : aliased constant Code_Point_Array := (16#2F85C#,16#5906#);
   S2F85D : aliased constant Code_Point_Array := (16#2F85D#,16#591A#);
   S2F85E : aliased constant Code_Point_Array := (16#2F85E#,16#5922#);
   S2F85F : aliased constant Code_Point_Array := (16#2F85F#,16#5962#);
   S2F860 : aliased constant Code_Point_Array := (16#2F860#,16#216A8#);
   S2F861 : aliased constant Code_Point_Array := (16#2F861#,16#216EA#);
   S2F862 : aliased constant Code_Point_Array := (16#2F862#,16#59EC#);
   S2F863 : aliased constant Code_Point_Array := (16#2F863#,16#5A1B#);
   S2F864 : aliased constant Code_Point_Array := (16#2F864#,16#5A27#);
   S2F865 : aliased constant Code_Point_Array := (16#2F865#,16#59D8#);
   S2F866 : aliased constant Code_Point_Array := (16#2F866#,16#5A66#);
   S2F867 : aliased constant Code_Point_Array := (16#2F867#,16#36EE#);
   S2F868 : aliased constant Code_Point_Array := (16#2F868#,16#36FC#);
   S2F869 : aliased constant Code_Point_Array := (16#2F869#,16#5B08#);
   S2F86A : aliased constant Code_Point_Array := (16#2F86A#,16#5B3E#);
   S2F86B : aliased constant Code_Point_Array := (16#2F86B#,16#5B3E#);
   S2F86C : aliased constant Code_Point_Array := (16#2F86C#,16#219C8#);
   S2F86D : aliased constant Code_Point_Array := (16#2F86D#,16#5BC3#);
   S2F86E : aliased constant Code_Point_Array := (16#2F86E#,16#5BD8#);
   S2F86F : aliased constant Code_Point_Array := (16#2F86F#,16#5BE7#);
   S2F870 : aliased constant Code_Point_Array := (16#2F870#,16#5BF3#);
   S2F871 : aliased constant Code_Point_Array := (16#2F871#,16#21B18#);
   S2F872 : aliased constant Code_Point_Array := (16#2F872#,16#5BFF#);
   S2F873 : aliased constant Code_Point_Array := (16#2F873#,16#5C06#);
   S2F874 : aliased constant Code_Point_Array := (16#2F874#,16#5F53#);
   S2F875 : aliased constant Code_Point_Array := (16#2F875#,16#5C22#);
   S2F876 : aliased constant Code_Point_Array := (16#2F876#,16#3781#);
   S2F877 : aliased constant Code_Point_Array := (16#2F877#,16#5C60#);
   S2F878 : aliased constant Code_Point_Array := (16#2F878#,16#5C6E#);
   S2F879 : aliased constant Code_Point_Array := (16#2F879#,16#5CC0#);
   S2F87A : aliased constant Code_Point_Array := (16#2F87A#,16#5C8D#);
   S2F87B : aliased constant Code_Point_Array := (16#2F87B#,16#21DE4#);
   S2F87C : aliased constant Code_Point_Array := (16#2F87C#,16#5D43#);
   S2F87D : aliased constant Code_Point_Array := (16#2F87D#,16#21DE6#);
   S2F87E : aliased constant Code_Point_Array := (16#2F87E#,16#5D6E#);
   S2F87F : aliased constant Code_Point_Array := (16#2F87F#,16#5D6B#);
   S2F880 : aliased constant Code_Point_Array := (16#2F880#,16#5D7C#);
   S2F881 : aliased constant Code_Point_Array := (16#2F881#,16#5DE1#);
   S2F882 : aliased constant Code_Point_Array := (16#2F882#,16#5DE2#);
   S2F883 : aliased constant Code_Point_Array := (16#2F883#,16#382F#);
   S2F884 : aliased constant Code_Point_Array := (16#2F884#,16#5DFD#);
   S2F885 : aliased constant Code_Point_Array := (16#2F885#,16#5E28#);
   S2F886 : aliased constant Code_Point_Array := (16#2F886#,16#5E3D#);
   S2F887 : aliased constant Code_Point_Array := (16#2F887#,16#5E69#);
   S2F888 : aliased constant Code_Point_Array := (16#2F888#,16#3862#);
   S2F889 : aliased constant Code_Point_Array := (16#2F889#,16#22183#);
   S2F88A : aliased constant Code_Point_Array := (16#2F88A#,16#387C#);
   S2F88B : aliased constant Code_Point_Array := (16#2F88B#,16#5EB0#);
   S2F88C : aliased constant Code_Point_Array := (16#2F88C#,16#5EB3#);
   S2F88D : aliased constant Code_Point_Array := (16#2F88D#,16#5EB6#);
   S2F88E : aliased constant Code_Point_Array := (16#2F88E#,16#5ECA#);
   S2F88F : aliased constant Code_Point_Array := (16#2F88F#,16#2A392#);
   S2F890 : aliased constant Code_Point_Array := (16#2F890#,16#5EFE#);
   S2F891 : aliased constant Code_Point_Array := (16#2F891#,16#22331#);
   S2F892 : aliased constant Code_Point_Array := (16#2F892#,16#22331#);
   S2F893 : aliased constant Code_Point_Array := (16#2F893#,16#8201#);
   S2F894 : aliased constant Code_Point_Array := (16#2F894#,16#5F22#);
   S2F895 : aliased constant Code_Point_Array := (16#2F895#,16#5F22#);
   S2F896 : aliased constant Code_Point_Array := (16#2F896#,16#38C7#);
   S2F897 : aliased constant Code_Point_Array := (16#2F897#,16#232B8#);
   S2F898 : aliased constant Code_Point_Array := (16#2F898#,16#261DA#);
   S2F899 : aliased constant Code_Point_Array := (16#2F899#,16#5F62#);
   S2F89A : aliased constant Code_Point_Array := (16#2F89A#,16#5F6B#);
   S2F89B : aliased constant Code_Point_Array := (16#2F89B#,16#38E3#);
   S2F89C : aliased constant Code_Point_Array := (16#2F89C#,16#5F9A#);
   S2F89D : aliased constant Code_Point_Array := (16#2F89D#,16#5FCD#);
   S2F89E : aliased constant Code_Point_Array := (16#2F89E#,16#5FD7#);
   S2F89F : aliased constant Code_Point_Array := (16#2F89F#,16#5FF9#);
   S2F8A0 : aliased constant Code_Point_Array := (16#2F8A0#,16#6081#);
   S2F8A1 : aliased constant Code_Point_Array := (16#2F8A1#,16#393A#);
   S2F8A2 : aliased constant Code_Point_Array := (16#2F8A2#,16#391C#);
   S2F8A3 : aliased constant Code_Point_Array := (16#2F8A3#,16#6094#);
   S2F8A4 : aliased constant Code_Point_Array := (16#2F8A4#,16#226D4#);
   S2F8A5 : aliased constant Code_Point_Array := (16#2F8A5#,16#60C7#);
   S2F8A6 : aliased constant Code_Point_Array := (16#2F8A6#,16#6148#);
   S2F8A7 : aliased constant Code_Point_Array := (16#2F8A7#,16#614C#);
   S2F8A8 : aliased constant Code_Point_Array := (16#2F8A8#,16#614E#);
   S2F8A9 : aliased constant Code_Point_Array := (16#2F8A9#,16#614C#);
   S2F8AA : aliased constant Code_Point_Array := (16#2F8AA#,16#617A#);
   S2F8AB : aliased constant Code_Point_Array := (16#2F8AB#,16#618E#);
   S2F8AC : aliased constant Code_Point_Array := (16#2F8AC#,16#61B2#);
   S2F8AD : aliased constant Code_Point_Array := (16#2F8AD#,16#61A4#);
   S2F8AE : aliased constant Code_Point_Array := (16#2F8AE#,16#61AF#);
   S2F8AF : aliased constant Code_Point_Array := (16#2F8AF#,16#61DE#);
   S2F8B0 : aliased constant Code_Point_Array := (16#2F8B0#,16#61F2#);
   S2F8B1 : aliased constant Code_Point_Array := (16#2F8B1#,16#61F6#);
   S2F8B2 : aliased constant Code_Point_Array := (16#2F8B2#,16#6210#);
   S2F8B3 : aliased constant Code_Point_Array := (16#2F8B3#,16#621B#);
   S2F8B4 : aliased constant Code_Point_Array := (16#2F8B4#,16#625D#);
   S2F8B5 : aliased constant Code_Point_Array := (16#2F8B5#,16#62B1#);
   S2F8B6 : aliased constant Code_Point_Array := (16#2F8B6#,16#62D4#);
   S2F8B7 : aliased constant Code_Point_Array := (16#2F8B7#,16#6350#);
   S2F8B8 : aliased constant Code_Point_Array := (16#2F8B8#,16#22B0C#);
   S2F8B9 : aliased constant Code_Point_Array := (16#2F8B9#,16#633D#);
   S2F8BA : aliased constant Code_Point_Array := (16#2F8BA#,16#62FC#);
   S2F8BB : aliased constant Code_Point_Array := (16#2F8BB#,16#6368#);
   S2F8BC : aliased constant Code_Point_Array := (16#2F8BC#,16#6383#);
   S2F8BD : aliased constant Code_Point_Array := (16#2F8BD#,16#63E4#);
   S2F8BE : aliased constant Code_Point_Array := (16#2F8BE#,16#22BF1#);
   S2F8BF : aliased constant Code_Point_Array := (16#2F8BF#,16#6422#);
   S2F8C0 : aliased constant Code_Point_Array := (16#2F8C0#,16#63C5#);
   S2F8C1 : aliased constant Code_Point_Array := (16#2F8C1#,16#63A9#);
   S2F8C2 : aliased constant Code_Point_Array := (16#2F8C2#,16#3A2E#);
   S2F8C3 : aliased constant Code_Point_Array := (16#2F8C3#,16#6469#);
   S2F8C4 : aliased constant Code_Point_Array := (16#2F8C4#,16#647E#);
   S2F8C5 : aliased constant Code_Point_Array := (16#2F8C5#,16#649D#);
   S2F8C6 : aliased constant Code_Point_Array := (16#2F8C6#,16#6477#);
   S2F8C7 : aliased constant Code_Point_Array := (16#2F8C7#,16#3A6C#);
   S2F8C8 : aliased constant Code_Point_Array := (16#2F8C8#,16#654F#);
   S2F8C9 : aliased constant Code_Point_Array := (16#2F8C9#,16#656C#);
   S2F8CA : aliased constant Code_Point_Array := (16#2F8CA#,16#2300A#);
   S2F8CB : aliased constant Code_Point_Array := (16#2F8CB#,16#65E3#);
   S2F8CC : aliased constant Code_Point_Array := (16#2F8CC#,16#66F8#);
   S2F8CD : aliased constant Code_Point_Array := (16#2F8CD#,16#6649#);
   S2F8CE : aliased constant Code_Point_Array := (16#2F8CE#,16#3B19#);
   S2F8CF : aliased constant Code_Point_Array := (16#2F8CF#,16#6691#);
   S2F8D0 : aliased constant Code_Point_Array := (16#2F8D0#,16#3B08#);
   S2F8D1 : aliased constant Code_Point_Array := (16#2F8D1#,16#3AE4#);
   S2F8D2 : aliased constant Code_Point_Array := (16#2F8D2#,16#5192#);
   S2F8D3 : aliased constant Code_Point_Array := (16#2F8D3#,16#5195#);
   S2F8D4 : aliased constant Code_Point_Array := (16#2F8D4#,16#6700#);
   S2F8D5 : aliased constant Code_Point_Array := (16#2F8D5#,16#669C#);
   S2F8D6 : aliased constant Code_Point_Array := (16#2F8D6#,16#80AD#);
   S2F8D7 : aliased constant Code_Point_Array := (16#2F8D7#,16#43D9#);
   S2F8D8 : aliased constant Code_Point_Array := (16#2F8D8#,16#6717#);
   S2F8D9 : aliased constant Code_Point_Array := (16#2F8D9#,16#671B#);
   S2F8DA : aliased constant Code_Point_Array := (16#2F8DA#,16#6721#);
   S2F8DB : aliased constant Code_Point_Array := (16#2F8DB#,16#675E#);
   S2F8DC : aliased constant Code_Point_Array := (16#2F8DC#,16#6753#);
   S2F8DD : aliased constant Code_Point_Array := (16#2F8DD#,16#233C3#);
   S2F8DE : aliased constant Code_Point_Array := (16#2F8DE#,16#3B49#);
   S2F8DF : aliased constant Code_Point_Array := (16#2F8DF#,16#67FA#);
   S2F8E0 : aliased constant Code_Point_Array := (16#2F8E0#,16#6785#);
   S2F8E1 : aliased constant Code_Point_Array := (16#2F8E1#,16#6852#);
   S2F8E2 : aliased constant Code_Point_Array := (16#2F8E2#,16#6885#);
   S2F8E3 : aliased constant Code_Point_Array := (16#2F8E3#,16#2346D#);
   S2F8E4 : aliased constant Code_Point_Array := (16#2F8E4#,16#688E#);
   S2F8E5 : aliased constant Code_Point_Array := (16#2F8E5#,16#681F#);
   S2F8E6 : aliased constant Code_Point_Array := (16#2F8E6#,16#6914#);
   S2F8E7 : aliased constant Code_Point_Array := (16#2F8E7#,16#3B9D#);
   S2F8E8 : aliased constant Code_Point_Array := (16#2F8E8#,16#6942#);
   S2F8E9 : aliased constant Code_Point_Array := (16#2F8E9#,16#69A3#);
   S2F8EA : aliased constant Code_Point_Array := (16#2F8EA#,16#69EA#);
   S2F8EB : aliased constant Code_Point_Array := (16#2F8EB#,16#6AA8#);
   S2F8EC : aliased constant Code_Point_Array := (16#2F8EC#,16#236A3#);
   S2F8ED : aliased constant Code_Point_Array := (16#2F8ED#,16#6ADB#);
   S2F8EE : aliased constant Code_Point_Array := (16#2F8EE#,16#3C18#);
   S2F8EF : aliased constant Code_Point_Array := (16#2F8EF#,16#6B21#);
   S2F8F0 : aliased constant Code_Point_Array := (16#2F8F0#,16#238A7#);
   S2F8F1 : aliased constant Code_Point_Array := (16#2F8F1#,16#6B54#);
   S2F8F2 : aliased constant Code_Point_Array := (16#2F8F2#,16#3C4E#);
   S2F8F3 : aliased constant Code_Point_Array := (16#2F8F3#,16#6B72#);
   S2F8F4 : aliased constant Code_Point_Array := (16#2F8F4#,16#6B9F#);
   S2F8F5 : aliased constant Code_Point_Array := (16#2F8F5#,16#6BBA#);
   S2F8F6 : aliased constant Code_Point_Array := (16#2F8F6#,16#6BBB#);
   S2F8F7 : aliased constant Code_Point_Array := (16#2F8F7#,16#23A8D#);
   S2F8F8 : aliased constant Code_Point_Array := (16#2F8F8#,16#21D0B#);
   S2F8F9 : aliased constant Code_Point_Array := (16#2F8F9#,16#23AFA#);
   S2F8FA : aliased constant Code_Point_Array := (16#2F8FA#,16#6C4E#);
   S2F8FB : aliased constant Code_Point_Array := (16#2F8FB#,16#23CBC#);
   S2F8FC : aliased constant Code_Point_Array := (16#2F8FC#,16#6CBF#);
   S2F8FD : aliased constant Code_Point_Array := (16#2F8FD#,16#6CCD#);
   S2F8FE : aliased constant Code_Point_Array := (16#2F8FE#,16#6C67#);
   S2F8FF : aliased constant Code_Point_Array := (16#2F8FF#,16#6D16#);
   S2F900 : aliased constant Code_Point_Array := (16#2F900#,16#6D3E#);
   S2F901 : aliased constant Code_Point_Array := (16#2F901#,16#6D77#);
   S2F902 : aliased constant Code_Point_Array := (16#2F902#,16#6D41#);
   S2F903 : aliased constant Code_Point_Array := (16#2F903#,16#6D69#);
   S2F904 : aliased constant Code_Point_Array := (16#2F904#,16#6D78#);
   S2F905 : aliased constant Code_Point_Array := (16#2F905#,16#6D85#);
   S2F906 : aliased constant Code_Point_Array := (16#2F906#,16#23D1E#);
   S2F907 : aliased constant Code_Point_Array := (16#2F907#,16#6D34#);
   S2F908 : aliased constant Code_Point_Array := (16#2F908#,16#6E2F#);
   S2F909 : aliased constant Code_Point_Array := (16#2F909#,16#6E6E#);
   S2F90A : aliased constant Code_Point_Array := (16#2F90A#,16#3D33#);
   S2F90B : aliased constant Code_Point_Array := (16#2F90B#,16#6ECB#);
   S2F90C : aliased constant Code_Point_Array := (16#2F90C#,16#6EC7#);
   S2F90D : aliased constant Code_Point_Array := (16#2F90D#,16#23ED1#);
   S2F90E : aliased constant Code_Point_Array := (16#2F90E#,16#6DF9#);
   S2F90F : aliased constant Code_Point_Array := (16#2F90F#,16#6F6E#);
   S2F910 : aliased constant Code_Point_Array := (16#2F910#,16#23F5E#);
   S2F911 : aliased constant Code_Point_Array := (16#2F911#,16#23F8E#);
   S2F912 : aliased constant Code_Point_Array := (16#2F912#,16#6FC6#);
   S2F913 : aliased constant Code_Point_Array := (16#2F913#,16#7039#);
   S2F914 : aliased constant Code_Point_Array := (16#2F914#,16#701E#);
   S2F915 : aliased constant Code_Point_Array := (16#2F915#,16#701B#);
   S2F916 : aliased constant Code_Point_Array := (16#2F916#,16#3D96#);
   S2F917 : aliased constant Code_Point_Array := (16#2F917#,16#704A#);
   S2F918 : aliased constant Code_Point_Array := (16#2F918#,16#707D#);
   S2F919 : aliased constant Code_Point_Array := (16#2F919#,16#7077#);
   S2F91A : aliased constant Code_Point_Array := (16#2F91A#,16#70AD#);
   S2F91B : aliased constant Code_Point_Array := (16#2F91B#,16#20525#);
   S2F91C : aliased constant Code_Point_Array := (16#2F91C#,16#7145#);
   S2F91D : aliased constant Code_Point_Array := (16#2F91D#,16#24263#);
   S2F91E : aliased constant Code_Point_Array := (16#2F91E#,16#719C#);
   S2F91F : aliased constant Code_Point_Array := (16#2F91F#,16#243AB#);
   S2F920 : aliased constant Code_Point_Array := (16#2F920#,16#7228#);
   S2F921 : aliased constant Code_Point_Array := (16#2F921#,16#7235#);
   S2F922 : aliased constant Code_Point_Array := (16#2F922#,16#7250#);
   S2F923 : aliased constant Code_Point_Array := (16#2F923#,16#24608#);
   S2F924 : aliased constant Code_Point_Array := (16#2F924#,16#7280#);
   S2F925 : aliased constant Code_Point_Array := (16#2F925#,16#7295#);
   S2F926 : aliased constant Code_Point_Array := (16#2F926#,16#24735#);
   S2F927 : aliased constant Code_Point_Array := (16#2F927#,16#24814#);
   S2F928 : aliased constant Code_Point_Array := (16#2F928#,16#737A#);
   S2F929 : aliased constant Code_Point_Array := (16#2F929#,16#738B#);
   S2F92A : aliased constant Code_Point_Array := (16#2F92A#,16#3EAC#);
   S2F92B : aliased constant Code_Point_Array := (16#2F92B#,16#73A5#);
   S2F92C : aliased constant Code_Point_Array := (16#2F92C#,16#3EB8#);
   S2F92D : aliased constant Code_Point_Array := (16#2F92D#,16#3EB8#);
   S2F92E : aliased constant Code_Point_Array := (16#2F92E#,16#7447#);
   S2F92F : aliased constant Code_Point_Array := (16#2F92F#,16#745C#);
   S2F930 : aliased constant Code_Point_Array := (16#2F930#,16#7471#);
   S2F931 : aliased constant Code_Point_Array := (16#2F931#,16#7485#);
   S2F932 : aliased constant Code_Point_Array := (16#2F932#,16#74CA#);
   S2F933 : aliased constant Code_Point_Array := (16#2F933#,16#3F1B#);
   S2F934 : aliased constant Code_Point_Array := (16#2F934#,16#7524#);
   S2F935 : aliased constant Code_Point_Array := (16#2F935#,16#24C36#);
   S2F936 : aliased constant Code_Point_Array := (16#2F936#,16#753E#);
   S2F937 : aliased constant Code_Point_Array := (16#2F937#,16#24C92#);
   S2F938 : aliased constant Code_Point_Array := (16#2F938#,16#7570#);
   S2F939 : aliased constant Code_Point_Array := (16#2F939#,16#2219F#);
   S2F93A : aliased constant Code_Point_Array := (16#2F93A#,16#7610#);
   S2F93B : aliased constant Code_Point_Array := (16#2F93B#,16#24FA1#);
   S2F93C : aliased constant Code_Point_Array := (16#2F93C#,16#24FB8#);
   S2F93D : aliased constant Code_Point_Array := (16#2F93D#,16#25044#);
   S2F93E : aliased constant Code_Point_Array := (16#2F93E#,16#3FFC#);
   S2F93F : aliased constant Code_Point_Array := (16#2F93F#,16#4008#);
   S2F940 : aliased constant Code_Point_Array := (16#2F940#,16#76F4#);
   S2F941 : aliased constant Code_Point_Array := (16#2F941#,16#250F3#);
   S2F942 : aliased constant Code_Point_Array := (16#2F942#,16#250F2#);
   S2F943 : aliased constant Code_Point_Array := (16#2F943#,16#25119#);
   S2F944 : aliased constant Code_Point_Array := (16#2F944#,16#25133#);
   S2F945 : aliased constant Code_Point_Array := (16#2F945#,16#771E#);
   S2F946 : aliased constant Code_Point_Array := (16#2F946#,16#771F#);
   S2F947 : aliased constant Code_Point_Array := (16#2F947#,16#771F#);
   S2F948 : aliased constant Code_Point_Array := (16#2F948#,16#774A#);
   S2F949 : aliased constant Code_Point_Array := (16#2F949#,16#4039#);
   S2F94A : aliased constant Code_Point_Array := (16#2F94A#,16#778B#);
   S2F94B : aliased constant Code_Point_Array := (16#2F94B#,16#4046#);
   S2F94C : aliased constant Code_Point_Array := (16#2F94C#,16#4096#);
   S2F94D : aliased constant Code_Point_Array := (16#2F94D#,16#2541D#);
   S2F94E : aliased constant Code_Point_Array := (16#2F94E#,16#784E#);
   S2F94F : aliased constant Code_Point_Array := (16#2F94F#,16#788C#);
   S2F950 : aliased constant Code_Point_Array := (16#2F950#,16#78CC#);
   S2F951 : aliased constant Code_Point_Array := (16#2F951#,16#40E3#);
   S2F952 : aliased constant Code_Point_Array := (16#2F952#,16#25626#);
   S2F953 : aliased constant Code_Point_Array := (16#2F953#,16#7956#);
   S2F954 : aliased constant Code_Point_Array := (16#2F954#,16#2569A#);
   S2F955 : aliased constant Code_Point_Array := (16#2F955#,16#256C5#);
   S2F956 : aliased constant Code_Point_Array := (16#2F956#,16#798F#);
   S2F957 : aliased constant Code_Point_Array := (16#2F957#,16#79EB#);
   S2F958 : aliased constant Code_Point_Array := (16#2F958#,16#412F#);
   S2F959 : aliased constant Code_Point_Array := (16#2F959#,16#7A40#);
   S2F95A : aliased constant Code_Point_Array := (16#2F95A#,16#7A4A#);
   S2F95B : aliased constant Code_Point_Array := (16#2F95B#,16#7A4F#);
   S2F95C : aliased constant Code_Point_Array := (16#2F95C#,16#2597C#);
   S2F95D : aliased constant Code_Point_Array := (16#2F95D#,16#25AA7#);
   S2F95E : aliased constant Code_Point_Array := (16#2F95E#,16#25AA7#);
   S2F95F : aliased constant Code_Point_Array := (16#2F95F#,16#7AEE#);
   S2F960 : aliased constant Code_Point_Array := (16#2F960#,16#4202#);
   S2F961 : aliased constant Code_Point_Array := (16#2F961#,16#25BAB#);
   S2F962 : aliased constant Code_Point_Array := (16#2F962#,16#7BC6#);
   S2F963 : aliased constant Code_Point_Array := (16#2F963#,16#7BC9#);
   S2F964 : aliased constant Code_Point_Array := (16#2F964#,16#4227#);
   S2F965 : aliased constant Code_Point_Array := (16#2F965#,16#25C80#);
   S2F966 : aliased constant Code_Point_Array := (16#2F966#,16#7CD2#);
   S2F967 : aliased constant Code_Point_Array := (16#2F967#,16#42A0#);
   S2F968 : aliased constant Code_Point_Array := (16#2F968#,16#7CE8#);
   S2F969 : aliased constant Code_Point_Array := (16#2F969#,16#7CE3#);
   S2F96A : aliased constant Code_Point_Array := (16#2F96A#,16#7D00#);
   S2F96B : aliased constant Code_Point_Array := (16#2F96B#,16#25F86#);
   S2F96C : aliased constant Code_Point_Array := (16#2F96C#,16#7D63#);
   S2F96D : aliased constant Code_Point_Array := (16#2F96D#,16#4301#);
   S2F96E : aliased constant Code_Point_Array := (16#2F96E#,16#7DC7#);
   S2F96F : aliased constant Code_Point_Array := (16#2F96F#,16#7E02#);
   S2F970 : aliased constant Code_Point_Array := (16#2F970#,16#7E45#);
   S2F971 : aliased constant Code_Point_Array := (16#2F971#,16#4334#);
   S2F972 : aliased constant Code_Point_Array := (16#2F972#,16#26228#);
   S2F973 : aliased constant Code_Point_Array := (16#2F973#,16#26247#);
   S2F974 : aliased constant Code_Point_Array := (16#2F974#,16#4359#);
   S2F975 : aliased constant Code_Point_Array := (16#2F975#,16#262D9#);
   S2F976 : aliased constant Code_Point_Array := (16#2F976#,16#7F7A#);
   S2F977 : aliased constant Code_Point_Array := (16#2F977#,16#2633E#);
   S2F978 : aliased constant Code_Point_Array := (16#2F978#,16#7F95#);
   S2F979 : aliased constant Code_Point_Array := (16#2F979#,16#7FFA#);
   S2F97A : aliased constant Code_Point_Array := (16#2F97A#,16#8005#);
   S2F97B : aliased constant Code_Point_Array := (16#2F97B#,16#264DA#);
   S2F97C : aliased constant Code_Point_Array := (16#2F97C#,16#26523#);
   S2F97D : aliased constant Code_Point_Array := (16#2F97D#,16#8060#);
   S2F97E : aliased constant Code_Point_Array := (16#2F97E#,16#265A8#);
   S2F97F : aliased constant Code_Point_Array := (16#2F97F#,16#8070#);
   S2F980 : aliased constant Code_Point_Array := (16#2F980#,16#2335F#);
   S2F981 : aliased constant Code_Point_Array := (16#2F981#,16#43D5#);
   S2F982 : aliased constant Code_Point_Array := (16#2F982#,16#80B2#);
   S2F983 : aliased constant Code_Point_Array := (16#2F983#,16#8103#);
   S2F984 : aliased constant Code_Point_Array := (16#2F984#,16#440B#);
   S2F985 : aliased constant Code_Point_Array := (16#2F985#,16#813E#);
   S2F986 : aliased constant Code_Point_Array := (16#2F986#,16#5AB5#);
   S2F987 : aliased constant Code_Point_Array := (16#2F987#,16#267A7#);
   S2F988 : aliased constant Code_Point_Array := (16#2F988#,16#267B5#);
   S2F989 : aliased constant Code_Point_Array := (16#2F989#,16#23393#);
   S2F98A : aliased constant Code_Point_Array := (16#2F98A#,16#2339C#);
   S2F98B : aliased constant Code_Point_Array := (16#2F98B#,16#8201#);
   S2F98C : aliased constant Code_Point_Array := (16#2F98C#,16#8204#);
   S2F98D : aliased constant Code_Point_Array := (16#2F98D#,16#8F9E#);
   S2F98E : aliased constant Code_Point_Array := (16#2F98E#,16#446B#);
   S2F98F : aliased constant Code_Point_Array := (16#2F98F#,16#8291#);
   S2F990 : aliased constant Code_Point_Array := (16#2F990#,16#828B#);
   S2F991 : aliased constant Code_Point_Array := (16#2F991#,16#829D#);
   S2F992 : aliased constant Code_Point_Array := (16#2F992#,16#52B3#);
   S2F993 : aliased constant Code_Point_Array := (16#2F993#,16#82B1#);
   S2F994 : aliased constant Code_Point_Array := (16#2F994#,16#82B3#);
   S2F995 : aliased constant Code_Point_Array := (16#2F995#,16#82BD#);
   S2F996 : aliased constant Code_Point_Array := (16#2F996#,16#82E6#);
   S2F997 : aliased constant Code_Point_Array := (16#2F997#,16#26B3C#);
   S2F998 : aliased constant Code_Point_Array := (16#2F998#,16#82E5#);
   S2F999 : aliased constant Code_Point_Array := (16#2F999#,16#831D#);
   S2F99A : aliased constant Code_Point_Array := (16#2F99A#,16#8363#);
   S2F99B : aliased constant Code_Point_Array := (16#2F99B#,16#83AD#);
   S2F99C : aliased constant Code_Point_Array := (16#2F99C#,16#8323#);
   S2F99D : aliased constant Code_Point_Array := (16#2F99D#,16#83BD#);
   S2F99E : aliased constant Code_Point_Array := (16#2F99E#,16#83E7#);
   S2F99F : aliased constant Code_Point_Array := (16#2F99F#,16#8457#);
   S2F9A0 : aliased constant Code_Point_Array := (16#2F9A0#,16#8353#);
   S2F9A1 : aliased constant Code_Point_Array := (16#2F9A1#,16#83CA#);
   S2F9A2 : aliased constant Code_Point_Array := (16#2F9A2#,16#83CC#);
   S2F9A3 : aliased constant Code_Point_Array := (16#2F9A3#,16#83DC#);
   S2F9A4 : aliased constant Code_Point_Array := (16#2F9A4#,16#26C36#);
   S2F9A5 : aliased constant Code_Point_Array := (16#2F9A5#,16#26D6B#);
   S2F9A6 : aliased constant Code_Point_Array := (16#2F9A6#,16#26CD5#);
   S2F9A7 : aliased constant Code_Point_Array := (16#2F9A7#,16#452B#);
   S2F9A8 : aliased constant Code_Point_Array := (16#2F9A8#,16#84F1#);
   S2F9A9 : aliased constant Code_Point_Array := (16#2F9A9#,16#84F3#);
   S2F9AA : aliased constant Code_Point_Array := (16#2F9AA#,16#8516#);
   S2F9AB : aliased constant Code_Point_Array := (16#2F9AB#,16#273CA#);
   S2F9AC : aliased constant Code_Point_Array := (16#2F9AC#,16#8564#);
   S2F9AD : aliased constant Code_Point_Array := (16#2F9AD#,16#26F2C#);
   S2F9AE : aliased constant Code_Point_Array := (16#2F9AE#,16#455D#);
   S2F9AF : aliased constant Code_Point_Array := (16#2F9AF#,16#4561#);
   S2F9B0 : aliased constant Code_Point_Array := (16#2F9B0#,16#26FB1#);
   S2F9B1 : aliased constant Code_Point_Array := (16#2F9B1#,16#270D2#);
   S2F9B2 : aliased constant Code_Point_Array := (16#2F9B2#,16#456B#);
   S2F9B3 : aliased constant Code_Point_Array := (16#2F9B3#,16#8650#);
   S2F9B4 : aliased constant Code_Point_Array := (16#2F9B4#,16#865C#);
   S2F9B5 : aliased constant Code_Point_Array := (16#2F9B5#,16#8667#);
   S2F9B6 : aliased constant Code_Point_Array := (16#2F9B6#,16#8669#);
   S2F9B7 : aliased constant Code_Point_Array := (16#2F9B7#,16#86A9#);
   S2F9B8 : aliased constant Code_Point_Array := (16#2F9B8#,16#8688#);
   S2F9B9 : aliased constant Code_Point_Array := (16#2F9B9#,16#870E#);
   S2F9BA : aliased constant Code_Point_Array := (16#2F9BA#,16#86E2#);
   S2F9BB : aliased constant Code_Point_Array := (16#2F9BB#,16#8779#);
   S2F9BC : aliased constant Code_Point_Array := (16#2F9BC#,16#8728#);
   S2F9BD : aliased constant Code_Point_Array := (16#2F9BD#,16#876B#);
   S2F9BE : aliased constant Code_Point_Array := (16#2F9BE#,16#8786#);
   S2F9BF : aliased constant Code_Point_Array := (16#2F9BF#,16#45D7#);
   S2F9C0 : aliased constant Code_Point_Array := (16#2F9C0#,16#87E1#);
   S2F9C1 : aliased constant Code_Point_Array := (16#2F9C1#,16#8801#);
   S2F9C2 : aliased constant Code_Point_Array := (16#2F9C2#,16#45F9#);
   S2F9C3 : aliased constant Code_Point_Array := (16#2F9C3#,16#8860#);
   S2F9C4 : aliased constant Code_Point_Array := (16#2F9C4#,16#8863#);
   S2F9C5 : aliased constant Code_Point_Array := (16#2F9C5#,16#27667#);
   S2F9C6 : aliased constant Code_Point_Array := (16#2F9C6#,16#88D7#);
   S2F9C7 : aliased constant Code_Point_Array := (16#2F9C7#,16#88DE#);
   S2F9C8 : aliased constant Code_Point_Array := (16#2F9C8#,16#4635#);
   S2F9C9 : aliased constant Code_Point_Array := (16#2F9C9#,16#88FA#);
   S2F9CA : aliased constant Code_Point_Array := (16#2F9CA#,16#34BB#);
   S2F9CB : aliased constant Code_Point_Array := (16#2F9CB#,16#278AE#);
   S2F9CC : aliased constant Code_Point_Array := (16#2F9CC#,16#27966#);
   S2F9CD : aliased constant Code_Point_Array := (16#2F9CD#,16#46BE#);
   S2F9CE : aliased constant Code_Point_Array := (16#2F9CE#,16#46C7#);
   S2F9CF : aliased constant Code_Point_Array := (16#2F9CF#,16#8AA0#);
   S2F9D0 : aliased constant Code_Point_Array := (16#2F9D0#,16#8AED#);
   S2F9D1 : aliased constant Code_Point_Array := (16#2F9D1#,16#8B8A#);
   S2F9D2 : aliased constant Code_Point_Array := (16#2F9D2#,16#8C55#);
   S2F9D3 : aliased constant Code_Point_Array := (16#2F9D3#,16#27CA8#);
   S2F9D4 : aliased constant Code_Point_Array := (16#2F9D4#,16#8CAB#);
   S2F9D5 : aliased constant Code_Point_Array := (16#2F9D5#,16#8CC1#);
   S2F9D6 : aliased constant Code_Point_Array := (16#2F9D6#,16#8D1B#);
   S2F9D7 : aliased constant Code_Point_Array := (16#2F9D7#,16#8D77#);
   S2F9D8 : aliased constant Code_Point_Array := (16#2F9D8#,16#27F2F#);
   S2F9D9 : aliased constant Code_Point_Array := (16#2F9D9#,16#20804#);
   S2F9DA : aliased constant Code_Point_Array := (16#2F9DA#,16#8DCB#);
   S2F9DB : aliased constant Code_Point_Array := (16#2F9DB#,16#8DBC#);
   S2F9DC : aliased constant Code_Point_Array := (16#2F9DC#,16#8DF0#);
   S2F9DD : aliased constant Code_Point_Array := (16#2F9DD#,16#208DE#);
   S2F9DE : aliased constant Code_Point_Array := (16#2F9DE#,16#8ED4#);
   S2F9DF : aliased constant Code_Point_Array := (16#2F9DF#,16#8F38#);
   S2F9E0 : aliased constant Code_Point_Array := (16#2F9E0#,16#285D2#);
   S2F9E1 : aliased constant Code_Point_Array := (16#2F9E1#,16#285ED#);
   S2F9E2 : aliased constant Code_Point_Array := (16#2F9E2#,16#9094#);
   S2F9E3 : aliased constant Code_Point_Array := (16#2F9E3#,16#90F1#);
   S2F9E4 : aliased constant Code_Point_Array := (16#2F9E4#,16#9111#);
   S2F9E5 : aliased constant Code_Point_Array := (16#2F9E5#,16#2872E#);
   S2F9E6 : aliased constant Code_Point_Array := (16#2F9E6#,16#911B#);
   S2F9E7 : aliased constant Code_Point_Array := (16#2F9E7#,16#9238#);
   S2F9E8 : aliased constant Code_Point_Array := (16#2F9E8#,16#92D7#);
   S2F9E9 : aliased constant Code_Point_Array := (16#2F9E9#,16#92D8#);
   S2F9EA : aliased constant Code_Point_Array := (16#2F9EA#,16#927C#);
   S2F9EB : aliased constant Code_Point_Array := (16#2F9EB#,16#93F9#);
   S2F9EC : aliased constant Code_Point_Array := (16#2F9EC#,16#9415#);
   S2F9ED : aliased constant Code_Point_Array := (16#2F9ED#,16#28BFA#);
   S2F9EE : aliased constant Code_Point_Array := (16#2F9EE#,16#958B#);
   S2F9EF : aliased constant Code_Point_Array := (16#2F9EF#,16#4995#);
   S2F9F0 : aliased constant Code_Point_Array := (16#2F9F0#,16#95B7#);
   S2F9F1 : aliased constant Code_Point_Array := (16#2F9F1#,16#28D77#);
   S2F9F2 : aliased constant Code_Point_Array := (16#2F9F2#,16#49E6#);
   S2F9F3 : aliased constant Code_Point_Array := (16#2F9F3#,16#96C3#);
   S2F9F4 : aliased constant Code_Point_Array := (16#2F9F4#,16#5DB2#);
   S2F9F5 : aliased constant Code_Point_Array := (16#2F9F5#,16#9723#);
   S2F9F6 : aliased constant Code_Point_Array := (16#2F9F6#,16#29145#);
   S2F9F7 : aliased constant Code_Point_Array := (16#2F9F7#,16#2921A#);
   S2F9F8 : aliased constant Code_Point_Array := (16#2F9F8#,16#4A6E#);
   S2F9F9 : aliased constant Code_Point_Array := (16#2F9F9#,16#4A76#);
   S2F9FA : aliased constant Code_Point_Array := (16#2F9FA#,16#97E0#);
   S2F9FB : aliased constant Code_Point_Array := (16#2F9FB#,16#2940A#);
   S2F9FC : aliased constant Code_Point_Array := (16#2F9FC#,16#4AB2#);
   S2F9FD : aliased constant Code_Point_Array := (16#2F9FD#,16#29496#);
   S2F9FE : aliased constant Code_Point_Array := (16#2F9FE#,16#980B#);
   S2F9FF : aliased constant Code_Point_Array := (16#2F9FF#,16#980B#);
   S2FA00 : aliased constant Code_Point_Array := (16#2FA00#,16#9829#);
   S2FA01 : aliased constant Code_Point_Array := (16#2FA01#,16#295B6#);
   S2FA02 : aliased constant Code_Point_Array := (16#2FA02#,16#98E2#);
   S2FA03 : aliased constant Code_Point_Array := (16#2FA03#,16#4B33#);
   S2FA04 : aliased constant Code_Point_Array := (16#2FA04#,16#9929#);
   S2FA05 : aliased constant Code_Point_Array := (16#2FA05#,16#99A7#);
   S2FA06 : aliased constant Code_Point_Array := (16#2FA06#,16#99C2#);
   S2FA07 : aliased constant Code_Point_Array := (16#2FA07#,16#99FE#);
   S2FA08 : aliased constant Code_Point_Array := (16#2FA08#,16#4BCE#);
   S2FA09 : aliased constant Code_Point_Array := (16#2FA09#,16#29B30#);
   S2FA0A : aliased constant Code_Point_Array := (16#2FA0A#,16#9B12#);
   S2FA0B : aliased constant Code_Point_Array := (16#2FA0B#,16#9C40#);
   S2FA0C : aliased constant Code_Point_Array := (16#2FA0C#,16#9CFD#);
   S2FA0D : aliased constant Code_Point_Array := (16#2FA0D#,16#4CCE#);
   S2FA0E : aliased constant Code_Point_Array := (16#2FA0E#,16#4CED#);
   S2FA0F : aliased constant Code_Point_Array := (16#2FA0F#,16#9D67#);
   S2FA10 : aliased constant Code_Point_Array := (16#2FA10#,16#2A0CE#);
   S2FA11 : aliased constant Code_Point_Array := (16#2FA11#,16#4CF8#);
   S2FA12 : aliased constant Code_Point_Array := (16#2FA12#,16#2A105#);
   S2FA13 : aliased constant Code_Point_Array := (16#2FA13#,16#2A20E#);
   S2FA14 : aliased constant Code_Point_Array := (16#2FA14#,16#2A291#);
   S2FA15 : aliased constant Code_Point_Array := (16#2FA15#,16#9EBB#);
   S2FA16 : aliased constant Code_Point_Array := (16#2FA16#,16#4D56#);
   S2FA17 : aliased constant Code_Point_Array := (16#2FA17#,16#9EF9#);
   S2FA18 : aliased constant Code_Point_Array := (16#2FA18#,16#9EFE#);
   S2FA19 : aliased constant Code_Point_Array := (16#2FA19#,16#9F05#);
   S2FA1A : aliased constant Code_Point_Array := (16#2FA1A#,16#9F0F#);
   S2FA1B : aliased constant Code_Point_Array := (16#2FA1B#,16#9F16#);
   S2FA1C : aliased constant Code_Point_Array := (16#2FA1C#,16#9F3B#);
   S2FA1D : aliased constant Code_Point_Array := (16#2FA1D#,16#2A600#);
   Mapping : constant Normalization_Map :=
   (  SA0'Access,SA8'Access,SAA'Access,SAF'Access,SB2'Access,
      SB3'Access,SB4'Access,SB5'Access,SB8'Access,SB9'Access,
      SBA'Access,SBC'Access,SBD'Access,SBE'Access,SC0'Access,
      SC1'Access,SC2'Access,SC3'Access,SC4'Access,SC5'Access,
      SC7'Access,SC8'Access,SC9'Access,SCA'Access,SCB'Access,
      SCC'Access,SCD'Access,SCE'Access,SCF'Access,SD1'Access,
      SD2'Access,SD3'Access,SD4'Access,SD5'Access,SD6'Access,
      SD9'Access,SDA'Access,SDB'Access,SDC'Access,SDD'Access,
      SE0'Access,SE1'Access,SE2'Access,SE3'Access,SE4'Access,
      SE5'Access,SE7'Access,SE8'Access,SE9'Access,SEA'Access,
      SEB'Access,SEC'Access,SED'Access,SEE'Access,SEF'Access,
      SF1'Access,SF2'Access,SF3'Access,SF4'Access,SF5'Access,
      SF6'Access,SF9'Access,SFA'Access,SFB'Access,SFC'Access,
      SFD'Access,SFF'Access,S100'Access,S101'Access,S102'Access,
      S103'Access,S104'Access,S105'Access,S106'Access,S107'Access,
      S108'Access,S109'Access,S10A'Access,S10B'Access,S10C'Access,
      S10D'Access,S10E'Access,S10F'Access,S112'Access,S113'Access,
      S114'Access,S115'Access,S116'Access,S117'Access,S118'Access,
      S119'Access,S11A'Access,S11B'Access,S11C'Access,S11D'Access,
      S11E'Access,S11F'Access,S120'Access,S121'Access,S122'Access,
      S123'Access,S124'Access,S125'Access,S128'Access,S129'Access,
      S12A'Access,S12B'Access,S12C'Access,S12D'Access,S12E'Access,
      S12F'Access,S130'Access,S132'Access,S133'Access,S134'Access,
      S135'Access,S136'Access,S137'Access,S139'Access,S13A'Access,
      S13B'Access,S13C'Access,S13D'Access,S13E'Access,S13F'Access,
      S140'Access,S143'Access,S144'Access,S145'Access,S146'Access,
      S147'Access,S148'Access,S149'Access,S14C'Access,S14D'Access,
      S14E'Access,S14F'Access,S150'Access,S151'Access,S154'Access,
      S155'Access,S156'Access,S157'Access,S158'Access,S159'Access,
      S15A'Access,S15B'Access,S15C'Access,S15D'Access,S15E'Access,
      S15F'Access,S160'Access,S161'Access,S162'Access,S163'Access,
      S164'Access,S165'Access,S168'Access,S169'Access,S16A'Access,
      S16B'Access,S16C'Access,S16D'Access,S16E'Access,S16F'Access,
      S170'Access,S171'Access,S172'Access,S173'Access,S174'Access,
      S175'Access,S176'Access,S177'Access,S178'Access,S179'Access,
      S17A'Access,S17B'Access,S17C'Access,S17D'Access,S17E'Access,
      S17F'Access,S1A0'Access,S1A1'Access,S1AF'Access,S1B0'Access,
      S1C4'Access,S1C5'Access,S1C6'Access,S1C7'Access,S1C8'Access,
      S1C9'Access,S1CA'Access,S1CB'Access,S1CC'Access,S1CD'Access,
      S1CE'Access,S1CF'Access,S1D0'Access,S1D1'Access,S1D2'Access,
      S1D3'Access,S1D4'Access,S1D5'Access,S1D6'Access,S1D7'Access,
      S1D8'Access,S1D9'Access,S1DA'Access,S1DB'Access,S1DC'Access,
      S1DE'Access,S1DF'Access,S1E0'Access,S1E1'Access,S1E2'Access,
      S1E3'Access,S1E6'Access,S1E7'Access,S1E8'Access,S1E9'Access,
      S1EA'Access,S1EB'Access,S1EC'Access,S1ED'Access,S1EE'Access,
      S1EF'Access,S1F0'Access,S1F1'Access,S1F2'Access,S1F3'Access,
      S1F4'Access,S1F5'Access,S1F8'Access,S1F9'Access,S1FA'Access,
      S1FB'Access,S1FC'Access,S1FD'Access,S1FE'Access,S1FF'Access,
      S200'Access,S201'Access,S202'Access,S203'Access,S204'Access,
      S205'Access,S206'Access,S207'Access,S208'Access,S209'Access,
      S20A'Access,S20B'Access,S20C'Access,S20D'Access,S20E'Access,
      S20F'Access,S210'Access,S211'Access,S212'Access,S213'Access,
      S214'Access,S215'Access,S216'Access,S217'Access,S218'Access,
      S219'Access,S21A'Access,S21B'Access,S21E'Access,S21F'Access,
      S226'Access,S227'Access,S228'Access,S229'Access,S22A'Access,
      S22B'Access,S22C'Access,S22D'Access,S22E'Access,S22F'Access,
      S230'Access,S231'Access,S232'Access,S233'Access,S2B0'Access,
      S2B1'Access,S2B2'Access,S2B3'Access,S2B4'Access,S2B5'Access,
      S2B6'Access,S2B7'Access,S2B8'Access,S2D8'Access,S2D9'Access,
      S2DA'Access,S2DB'Access,S2DC'Access,S2DD'Access,S2E0'Access,
      S2E1'Access,S2E2'Access,S2E3'Access,S2E4'Access,S340'Access,
      S341'Access,S343'Access,S344'Access,S374'Access,S37A'Access,
      S37E'Access,S384'Access,S385'Access,S386'Access,S387'Access,
      S388'Access,S389'Access,S38A'Access,S38C'Access,S38E'Access,
      S38F'Access,S390'Access,S3AA'Access,S3AB'Access,S3AC'Access,
      S3AD'Access,S3AE'Access,S3AF'Access,S3B0'Access,S3CA'Access,
      S3CB'Access,S3CC'Access,S3CD'Access,S3CE'Access,S3D0'Access,
      S3D1'Access,S3D2'Access,S3D3'Access,S3D4'Access,S3D5'Access,
      S3D6'Access,S3F0'Access,S3F1'Access,S3F2'Access,S3F4'Access,
      S3F5'Access,S3F9'Access,S400'Access,S401'Access,S403'Access,
      S407'Access,S40C'Access,S40D'Access,S40E'Access,S419'Access,
      S439'Access,S450'Access,S451'Access,S453'Access,S457'Access,
      S45C'Access,S45D'Access,S45E'Access,S476'Access,S477'Access,
      S4C1'Access,S4C2'Access,S4D0'Access,S4D1'Access,S4D2'Access,
      S4D3'Access,S4D6'Access,S4D7'Access,S4DA'Access,S4DB'Access,
      S4DC'Access,S4DD'Access,S4DE'Access,S4DF'Access,S4E2'Access,
      S4E3'Access,S4E4'Access,S4E5'Access,S4E6'Access,S4E7'Access,
      S4EA'Access,S4EB'Access,S4EC'Access,S4ED'Access,S4EE'Access,
      S4EF'Access,S4F0'Access,S4F1'Access,S4F2'Access,S4F3'Access,
      S4F4'Access,S4F5'Access,S4F8'Access,S4F9'Access,S587'Access,
      S622'Access,S623'Access,S624'Access,S625'Access,S626'Access,
      S675'Access,S676'Access,S677'Access,S678'Access,S6C0'Access,
      S6C2'Access,S6D3'Access,S929'Access,S931'Access,S934'Access,
      S958'Access,S959'Access,S95A'Access,S95B'Access,S95C'Access,
      S95D'Access,S95E'Access,S95F'Access,S9CB'Access,S9CC'Access,
      S9DC'Access,S9DD'Access,S9DF'Access,SA33'Access,SA36'Access,
      SA59'Access,SA5A'Access,SA5B'Access,SA5E'Access,SB48'Access,
      SB4B'Access,SB4C'Access,SB5C'Access,SB5D'Access,SB94'Access,
      SBCA'Access,SBCB'Access,SBCC'Access,SC48'Access,SCC0'Access,
      SCC7'Access,SCC8'Access,SCCA'Access,SCCB'Access,SD4A'Access,
      SD4B'Access,SD4C'Access,SDDA'Access,SDDC'Access,SDDD'Access,
      SDDE'Access,SE33'Access,SEB3'Access,SEDC'Access,SEDD'Access,
      SF0C'Access,SF43'Access,SF4D'Access,SF52'Access,SF57'Access,
      SF5C'Access,SF69'Access,SF73'Access,SF75'Access,SF76'Access,
      SF77'Access,SF78'Access,SF79'Access,SF81'Access,SF93'Access,
      SF9D'Access,SFA2'Access,SFA7'Access,SFAC'Access,SFB9'Access,
      S1026'Access,S10FC'Access,S1B06'Access,S1B08'Access,S1B0A'Access,
      S1B0C'Access,S1B0E'Access,S1B12'Access,S1B3B'Access,S1B3D'Access,
      S1B40'Access,S1B41'Access,S1B43'Access,S1D2C'Access,S1D2D'Access,
      S1D2E'Access,S1D30'Access,S1D31'Access,S1D32'Access,S1D33'Access,
      S1D34'Access,S1D35'Access,S1D36'Access,S1D37'Access,S1D38'Access,
      S1D39'Access,S1D3A'Access,S1D3C'Access,S1D3D'Access,S1D3E'Access,
      S1D3F'Access,S1D40'Access,S1D41'Access,S1D42'Access,S1D43'Access,
      S1D44'Access,S1D45'Access,S1D46'Access,S1D47'Access,S1D48'Access,
      S1D49'Access,S1D4A'Access,S1D4B'Access,S1D4C'Access,S1D4D'Access,
      S1D4F'Access,S1D50'Access,S1D51'Access,S1D52'Access,S1D53'Access,
      S1D54'Access,S1D55'Access,S1D56'Access,S1D57'Access,S1D58'Access,
      S1D59'Access,S1D5A'Access,S1D5B'Access,S1D5C'Access,S1D5D'Access,
      S1D5E'Access,S1D5F'Access,S1D60'Access,S1D61'Access,S1D62'Access,
      S1D63'Access,S1D64'Access,S1D65'Access,S1D66'Access,S1D67'Access,
      S1D68'Access,S1D69'Access,S1D6A'Access,S1D78'Access,S1D9B'Access,
      S1D9C'Access,S1D9D'Access,S1D9E'Access,S1D9F'Access,S1DA0'Access,
      S1DA1'Access,S1DA2'Access,S1DA3'Access,S1DA4'Access,S1DA5'Access,
      S1DA6'Access,S1DA7'Access,S1DA8'Access,S1DA9'Access,S1DAA'Access,
      S1DAB'Access,S1DAC'Access,S1DAD'Access,S1DAE'Access,S1DAF'Access,
      S1DB0'Access,S1DB1'Access,S1DB2'Access,S1DB3'Access,S1DB4'Access,
      S1DB5'Access,S1DB6'Access,S1DB7'Access,S1DB8'Access,S1DB9'Access,
      S1DBA'Access,S1DBB'Access,S1DBC'Access,S1DBD'Access,S1DBE'Access,
      S1DBF'Access,S1E00'Access,S1E01'Access,S1E02'Access,S1E03'Access,
      S1E04'Access,S1E05'Access,S1E06'Access,S1E07'Access,S1E08'Access,
      S1E09'Access,S1E0A'Access,S1E0B'Access,S1E0C'Access,S1E0D'Access,
      S1E0E'Access,S1E0F'Access,S1E10'Access,S1E11'Access,S1E12'Access,
      S1E13'Access,S1E14'Access,S1E15'Access,S1E16'Access,S1E17'Access,
      S1E18'Access,S1E19'Access,S1E1A'Access,S1E1B'Access,S1E1C'Access,
      S1E1D'Access,S1E1E'Access,S1E1F'Access,S1E20'Access,S1E21'Access,
      S1E22'Access,S1E23'Access,S1E24'Access,S1E25'Access,S1E26'Access,
      S1E27'Access,S1E28'Access,S1E29'Access,S1E2A'Access,S1E2B'Access,
      S1E2C'Access,S1E2D'Access,S1E2E'Access,S1E2F'Access,S1E30'Access,
      S1E31'Access,S1E32'Access,S1E33'Access,S1E34'Access,S1E35'Access,
      S1E36'Access,S1E37'Access,S1E38'Access,S1E39'Access,S1E3A'Access,
      S1E3B'Access,S1E3C'Access,S1E3D'Access,S1E3E'Access,S1E3F'Access,
      S1E40'Access,S1E41'Access,S1E42'Access,S1E43'Access,S1E44'Access,
      S1E45'Access,S1E46'Access,S1E47'Access,S1E48'Access,S1E49'Access,
      S1E4A'Access,S1E4B'Access,S1E4C'Access,S1E4D'Access,S1E4E'Access,
      S1E4F'Access,S1E50'Access,S1E51'Access,S1E52'Access,S1E53'Access,
      S1E54'Access,S1E55'Access,S1E56'Access,S1E57'Access,S1E58'Access,
      S1E59'Access,S1E5A'Access,S1E5B'Access,S1E5C'Access,S1E5D'Access,
      S1E5E'Access,S1E5F'Access,S1E60'Access,S1E61'Access,S1E62'Access,
      S1E63'Access,S1E64'Access,S1E65'Access,S1E66'Access,S1E67'Access,
      S1E68'Access,S1E69'Access,S1E6A'Access,S1E6B'Access,S1E6C'Access,
      S1E6D'Access,S1E6E'Access,S1E6F'Access,S1E70'Access,S1E71'Access,
      S1E72'Access,S1E73'Access,S1E74'Access,S1E75'Access,S1E76'Access,
      S1E77'Access,S1E78'Access,S1E79'Access,S1E7A'Access,S1E7B'Access,
      S1E7C'Access,S1E7D'Access,S1E7E'Access,S1E7F'Access,S1E80'Access,
      S1E81'Access,S1E82'Access,S1E83'Access,S1E84'Access,S1E85'Access,
      S1E86'Access,S1E87'Access,S1E88'Access,S1E89'Access,S1E8A'Access,
      S1E8B'Access,S1E8C'Access,S1E8D'Access,S1E8E'Access,S1E8F'Access,
      S1E90'Access,S1E91'Access,S1E92'Access,S1E93'Access,S1E94'Access,
      S1E95'Access,S1E96'Access,S1E97'Access,S1E98'Access,S1E99'Access,
      S1E9A'Access,S1E9B'Access,S1EA0'Access,S1EA1'Access,S1EA2'Access,
      S1EA3'Access,S1EA4'Access,S1EA5'Access,S1EA6'Access,S1EA7'Access,
      S1EA8'Access,S1EA9'Access,S1EAA'Access,S1EAB'Access,S1EAC'Access,
      S1EAD'Access,S1EAE'Access,S1EAF'Access,S1EB0'Access,S1EB1'Access,
      S1EB2'Access,S1EB3'Access,S1EB4'Access,S1EB5'Access,S1EB6'Access,
      S1EB7'Access,S1EB8'Access,S1EB9'Access,S1EBA'Access,S1EBB'Access,
      S1EBC'Access,S1EBD'Access,S1EBE'Access,S1EBF'Access,S1EC0'Access,
      S1EC1'Access,S1EC2'Access,S1EC3'Access,S1EC4'Access,S1EC5'Access,
      S1EC6'Access,S1EC7'Access,S1EC8'Access,S1EC9'Access,S1ECA'Access,
      S1ECB'Access,S1ECC'Access,S1ECD'Access,S1ECE'Access,S1ECF'Access,
      S1ED0'Access,S1ED1'Access,S1ED2'Access,S1ED3'Access,S1ED4'Access,
      S1ED5'Access,S1ED6'Access,S1ED7'Access,S1ED8'Access,S1ED9'Access,
      S1EDA'Access,S1EDB'Access,S1EDC'Access,S1EDD'Access,S1EDE'Access,
      S1EDF'Access,S1EE0'Access,S1EE1'Access,S1EE2'Access,S1EE3'Access,
      S1EE4'Access,S1EE5'Access,S1EE6'Access,S1EE7'Access,S1EE8'Access,
      S1EE9'Access,S1EEA'Access,S1EEB'Access,S1EEC'Access,S1EED'Access,
      S1EEE'Access,S1EEF'Access,S1EF0'Access,S1EF1'Access,S1EF2'Access,
      S1EF3'Access,S1EF4'Access,S1EF5'Access,S1EF6'Access,S1EF7'Access,
      S1EF8'Access,S1EF9'Access,S1F00'Access,S1F01'Access,S1F02'Access,
      S1F03'Access,S1F04'Access,S1F05'Access,S1F06'Access,S1F07'Access,
      S1F08'Access,S1F09'Access,S1F0A'Access,S1F0B'Access,S1F0C'Access,
      S1F0D'Access,S1F0E'Access,S1F0F'Access,S1F10'Access,S1F11'Access,
      S1F12'Access,S1F13'Access,S1F14'Access,S1F15'Access,S1F18'Access,
      S1F19'Access,S1F1A'Access,S1F1B'Access,S1F1C'Access,S1F1D'Access,
      S1F20'Access,S1F21'Access,S1F22'Access,S1F23'Access,S1F24'Access,
      S1F25'Access,S1F26'Access,S1F27'Access,S1F28'Access,S1F29'Access,
      S1F2A'Access,S1F2B'Access,S1F2C'Access,S1F2D'Access,S1F2E'Access,
      S1F2F'Access,S1F30'Access,S1F31'Access,S1F32'Access,S1F33'Access,
      S1F34'Access,S1F35'Access,S1F36'Access,S1F37'Access,S1F38'Access,
      S1F39'Access,S1F3A'Access,S1F3B'Access,S1F3C'Access,S1F3D'Access,
      S1F3E'Access,S1F3F'Access,S1F40'Access,S1F41'Access,S1F42'Access,
      S1F43'Access,S1F44'Access,S1F45'Access,S1F48'Access,S1F49'Access,
      S1F4A'Access,S1F4B'Access,S1F4C'Access,S1F4D'Access,S1F50'Access,
      S1F51'Access,S1F52'Access,S1F53'Access,S1F54'Access,S1F55'Access,
      S1F56'Access,S1F57'Access,S1F59'Access,S1F5B'Access,S1F5D'Access,
      S1F5F'Access,S1F60'Access,S1F61'Access,S1F62'Access,S1F63'Access,
      S1F64'Access,S1F65'Access,S1F66'Access,S1F67'Access,S1F68'Access,
      S1F69'Access,S1F6A'Access,S1F6B'Access,S1F6C'Access,S1F6D'Access,
      S1F6E'Access,S1F6F'Access,S1F70'Access,S1F71'Access,S1F72'Access,
      S1F73'Access,S1F74'Access,S1F75'Access,S1F76'Access,S1F77'Access,
      S1F78'Access,S1F79'Access,S1F7A'Access,S1F7B'Access,S1F7C'Access,
      S1F7D'Access,S1F80'Access,S1F81'Access,S1F82'Access,S1F83'Access,
      S1F84'Access,S1F85'Access,S1F86'Access,S1F87'Access,S1F88'Access,
      S1F89'Access,S1F8A'Access,S1F8B'Access,S1F8C'Access,S1F8D'Access,
      S1F8E'Access,S1F8F'Access,S1F90'Access,S1F91'Access,S1F92'Access,
      S1F93'Access,S1F94'Access,S1F95'Access,S1F96'Access,S1F97'Access,
      S1F98'Access,S1F99'Access,S1F9A'Access,S1F9B'Access,S1F9C'Access,
      S1F9D'Access,S1F9E'Access,S1F9F'Access,S1FA0'Access,S1FA1'Access,
      S1FA2'Access,S1FA3'Access,S1FA4'Access,S1FA5'Access,S1FA6'Access,
      S1FA7'Access,S1FA8'Access,S1FA9'Access,S1FAA'Access,S1FAB'Access,
      S1FAC'Access,S1FAD'Access,S1FAE'Access,S1FAF'Access,S1FB0'Access,
      S1FB1'Access,S1FB2'Access,S1FB3'Access,S1FB4'Access,S1FB6'Access,
      S1FB7'Access,S1FB8'Access,S1FB9'Access,S1FBA'Access,S1FBB'Access,
      S1FBC'Access,S1FBD'Access,S1FBE'Access,S1FBF'Access,S1FC0'Access,
      S1FC1'Access,S1FC2'Access,S1FC3'Access,S1FC4'Access,S1FC6'Access,
      S1FC7'Access,S1FC8'Access,S1FC9'Access,S1FCA'Access,S1FCB'Access,
      S1FCC'Access,S1FCD'Access,S1FCE'Access,S1FCF'Access,S1FD0'Access,
      S1FD1'Access,S1FD2'Access,S1FD3'Access,S1FD6'Access,S1FD7'Access,
      S1FD8'Access,S1FD9'Access,S1FDA'Access,S1FDB'Access,S1FDD'Access,
      S1FDE'Access,S1FDF'Access,S1FE0'Access,S1FE1'Access,S1FE2'Access,
      S1FE3'Access,S1FE4'Access,S1FE5'Access,S1FE6'Access,S1FE7'Access,
      S1FE8'Access,S1FE9'Access,S1FEA'Access,S1FEB'Access,S1FEC'Access,
      S1FED'Access,S1FEE'Access,S1FEF'Access,S1FF2'Access,S1FF3'Access,
      S1FF4'Access,S1FF6'Access,S1FF7'Access,S1FF8'Access,S1FF9'Access,
      S1FFA'Access,S1FFB'Access,S1FFC'Access,S1FFD'Access,S1FFE'Access,
      S2000'Access,S2001'Access,S2002'Access,S2003'Access,S2004'Access,
      S2005'Access,S2006'Access,S2007'Access,S2008'Access,S2009'Access,
      S200A'Access,S2011'Access,S2017'Access,S2024'Access,S2025'Access,
      S2026'Access,S202F'Access,S2033'Access,S2034'Access,S2036'Access,
      S2037'Access,S203C'Access,S203E'Access,S2047'Access,S2048'Access,
      S2049'Access,S2057'Access,S205F'Access,S2070'Access,S2071'Access,
      S2074'Access,S2075'Access,S2076'Access,S2077'Access,S2078'Access,
      S2079'Access,S207A'Access,S207B'Access,S207C'Access,S207D'Access,
      S207E'Access,S207F'Access,S2080'Access,S2081'Access,S2082'Access,
      S2083'Access,S2084'Access,S2085'Access,S2086'Access,S2087'Access,
      S2088'Access,S2089'Access,S208A'Access,S208B'Access,S208C'Access,
      S208D'Access,S208E'Access,S2090'Access,S2091'Access,S2092'Access,
      S2093'Access,S2094'Access,S2095'Access,S2096'Access,S2097'Access,
      S2098'Access,S2099'Access,S209A'Access,S209B'Access,S209C'Access,
      S20A8'Access,S2100'Access,S2101'Access,S2102'Access,S2103'Access,
      S2105'Access,S2106'Access,S2107'Access,S2109'Access,S210A'Access,
      S210B'Access,S210C'Access,S210D'Access,S210E'Access,S210F'Access,
      S2110'Access,S2111'Access,S2112'Access,S2113'Access,S2115'Access,
      S2116'Access,S2119'Access,S211A'Access,S211B'Access,S211C'Access,
      S211D'Access,S2120'Access,S2121'Access,S2122'Access,S2124'Access,
      S2126'Access,S2128'Access,S212A'Access,S212B'Access,S212C'Access,
      S212D'Access,S212F'Access,S2130'Access,S2131'Access,S2133'Access,
      S2134'Access,S2135'Access,S2136'Access,S2137'Access,S2138'Access,
      S2139'Access,S213B'Access,S213C'Access,S213D'Access,S213E'Access,
      S213F'Access,S2140'Access,S2145'Access,S2146'Access,S2147'Access,
      S2148'Access,S2149'Access,S2150'Access,S2151'Access,S2152'Access,
      S2153'Access,S2154'Access,S2155'Access,S2156'Access,S2157'Access,
      S2158'Access,S2159'Access,S215A'Access,S215B'Access,S215C'Access,
      S215D'Access,S215E'Access,S215F'Access,S2160'Access,S2161'Access,
      S2162'Access,S2163'Access,S2164'Access,S2165'Access,S2166'Access,
      S2167'Access,S2168'Access,S2169'Access,S216A'Access,S216B'Access,
      S216C'Access,S216D'Access,S216E'Access,S216F'Access,S2170'Access,
      S2171'Access,S2172'Access,S2173'Access,S2174'Access,S2175'Access,
      S2176'Access,S2177'Access,S2178'Access,S2179'Access,S217A'Access,
      S217B'Access,S217C'Access,S217D'Access,S217E'Access,S217F'Access,
      S2189'Access,S219A'Access,S219B'Access,S21AE'Access,S21CD'Access,
      S21CE'Access,S21CF'Access,S2204'Access,S2209'Access,S220C'Access,
      S2224'Access,S2226'Access,S222C'Access,S222D'Access,S222F'Access,
      S2230'Access,S2241'Access,S2244'Access,S2247'Access,S2249'Access,
      S2260'Access,S2262'Access,S226D'Access,S226E'Access,S226F'Access,
      S2270'Access,S2271'Access,S2274'Access,S2275'Access,S2278'Access,
      S2279'Access,S2280'Access,S2281'Access,S2284'Access,S2285'Access,
      S2288'Access,S2289'Access,S22AC'Access,S22AD'Access,S22AE'Access,
      S22AF'Access,S22E0'Access,S22E1'Access,S22E2'Access,S22E3'Access,
      S22EA'Access,S22EB'Access,S22EC'Access,S22ED'Access,S2329'Access,
      S232A'Access,S2460'Access,S2461'Access,S2462'Access,S2463'Access,
      S2464'Access,S2465'Access,S2466'Access,S2467'Access,S2468'Access,
      S2469'Access,S246A'Access,S246B'Access,S246C'Access,S246D'Access,
      S246E'Access,S246F'Access,S2470'Access,S2471'Access,S2472'Access,
      S2473'Access,S2474'Access,S2475'Access,S2476'Access,S2477'Access,
      S2478'Access,S2479'Access,S247A'Access,S247B'Access,S247C'Access,
      S247D'Access,S247E'Access,S247F'Access,S2480'Access,S2481'Access,
      S2482'Access,S2483'Access,S2484'Access,S2485'Access,S2486'Access,
      S2487'Access,S2488'Access,S2489'Access,S248A'Access,S248B'Access,
      S248C'Access,S248D'Access,S248E'Access,S248F'Access,S2490'Access,
      S2491'Access,S2492'Access,S2493'Access,S2494'Access,S2495'Access,
      S2496'Access,S2497'Access,S2498'Access,S2499'Access,S249A'Access,
      S249B'Access,S249C'Access,S249D'Access,S249E'Access,S249F'Access,
      S24A0'Access,S24A1'Access,S24A2'Access,S24A3'Access,S24A4'Access,
      S24A5'Access,S24A6'Access,S24A7'Access,S24A8'Access,S24A9'Access,
      S24AA'Access,S24AB'Access,S24AC'Access,S24AD'Access,S24AE'Access,
      S24AF'Access,S24B0'Access,S24B1'Access,S24B2'Access,S24B3'Access,
      S24B4'Access,S24B5'Access,S24B6'Access,S24B7'Access,S24B8'Access,
      S24B9'Access,S24BA'Access,S24BB'Access,S24BC'Access,S24BD'Access,
      S24BE'Access,S24BF'Access,S24C0'Access,S24C1'Access,S24C2'Access,
      S24C3'Access,S24C4'Access,S24C5'Access,S24C6'Access,S24C7'Access,
      S24C8'Access,S24C9'Access,S24CA'Access,S24CB'Access,S24CC'Access,
      S24CD'Access,S24CE'Access,S24CF'Access,S24D0'Access,S24D1'Access,
      S24D2'Access,S24D3'Access,S24D4'Access,S24D5'Access,S24D6'Access,
      S24D7'Access,S24D8'Access,S24D9'Access,S24DA'Access,S24DB'Access,
      S24DC'Access,S24DD'Access,S24DE'Access,S24DF'Access,S24E0'Access,
      S24E1'Access,S24E2'Access,S24E3'Access,S24E4'Access,S24E5'Access,
      S24E6'Access,S24E7'Access,S24E8'Access,S24E9'Access,S24EA'Access,
      S2A0C'Access,S2A74'Access,S2A75'Access,S2A76'Access,S2ADC'Access,
      S2C7C'Access,S2C7D'Access,S2D6F'Access,S2E9F'Access,S2EF3'Access,
      S2F00'Access,S2F01'Access,S2F02'Access,S2F03'Access,S2F04'Access,
      S2F05'Access,S2F06'Access,S2F07'Access,S2F08'Access,S2F09'Access,
      S2F0A'Access,S2F0B'Access,S2F0C'Access,S2F0D'Access,S2F0E'Access,
      S2F0F'Access,S2F10'Access,S2F11'Access,S2F12'Access,S2F13'Access,
      S2F14'Access,S2F15'Access,S2F16'Access,S2F17'Access,S2F18'Access,
      S2F19'Access,S2F1A'Access,S2F1B'Access,S2F1C'Access,S2F1D'Access,
      S2F1E'Access,S2F1F'Access,S2F20'Access,S2F21'Access,S2F22'Access,
      S2F23'Access,S2F24'Access,S2F25'Access,S2F26'Access,S2F27'Access,
      S2F28'Access,S2F29'Access,S2F2A'Access,S2F2B'Access,S2F2C'Access,
      S2F2D'Access,S2F2E'Access,S2F2F'Access,S2F30'Access,S2F31'Access,
      S2F32'Access,S2F33'Access,S2F34'Access,S2F35'Access,S2F36'Access,
      S2F37'Access,S2F38'Access,S2F39'Access,S2F3A'Access,S2F3B'Access,
      S2F3C'Access,S2F3D'Access,S2F3E'Access,S2F3F'Access,S2F40'Access,
      S2F41'Access,S2F42'Access,S2F43'Access,S2F44'Access,S2F45'Access,
      S2F46'Access,S2F47'Access,S2F48'Access,S2F49'Access,S2F4A'Access,
      S2F4B'Access,S2F4C'Access,S2F4D'Access,S2F4E'Access,S2F4F'Access,
      S2F50'Access,S2F51'Access,S2F52'Access,S2F53'Access,S2F54'Access,
      S2F55'Access,S2F56'Access,S2F57'Access,S2F58'Access,S2F59'Access,
      S2F5A'Access,S2F5B'Access,S2F5C'Access,S2F5D'Access,S2F5E'Access,
      S2F5F'Access,S2F60'Access,S2F61'Access,S2F62'Access,S2F63'Access,
      S2F64'Access,S2F65'Access,S2F66'Access,S2F67'Access,S2F68'Access,
      S2F69'Access,S2F6A'Access,S2F6B'Access,S2F6C'Access,S2F6D'Access,
      S2F6E'Access,S2F6F'Access,S2F70'Access,S2F71'Access,S2F72'Access,
      S2F73'Access,S2F74'Access,S2F75'Access,S2F76'Access,S2F77'Access,
      S2F78'Access,S2F79'Access,S2F7A'Access,S2F7B'Access,S2F7C'Access,
      S2F7D'Access,S2F7E'Access,S2F7F'Access,S2F80'Access,S2F81'Access,
      S2F82'Access,S2F83'Access,S2F84'Access,S2F85'Access,S2F86'Access,
      S2F87'Access,S2F88'Access,S2F89'Access,S2F8A'Access,S2F8B'Access,
      S2F8C'Access,S2F8D'Access,S2F8E'Access,S2F8F'Access,S2F90'Access,
      S2F91'Access,S2F92'Access,S2F93'Access,S2F94'Access,S2F95'Access,
      S2F96'Access,S2F97'Access,S2F98'Access,S2F99'Access,S2F9A'Access,
      S2F9B'Access,S2F9C'Access,S2F9D'Access,S2F9E'Access,S2F9F'Access,
      S2FA0'Access,S2FA1'Access,S2FA2'Access,S2FA3'Access,S2FA4'Access,
      S2FA5'Access,S2FA6'Access,S2FA7'Access,S2FA8'Access,S2FA9'Access,
      S2FAA'Access,S2FAB'Access,S2FAC'Access,S2FAD'Access,S2FAE'Access,
      S2FAF'Access,S2FB0'Access,S2FB1'Access,S2FB2'Access,S2FB3'Access,
      S2FB4'Access,S2FB5'Access,S2FB6'Access,S2FB7'Access,S2FB8'Access,
      S2FB9'Access,S2FBA'Access,S2FBB'Access,S2FBC'Access,S2FBD'Access,
      S2FBE'Access,S2FBF'Access,S2FC0'Access,S2FC1'Access,S2FC2'Access,
      S2FC3'Access,S2FC4'Access,S2FC5'Access,S2FC6'Access,S2FC7'Access,
      S2FC8'Access,S2FC9'Access,S2FCA'Access,S2FCB'Access,S2FCC'Access,
      S2FCD'Access,S2FCE'Access,S2FCF'Access,S2FD0'Access,S2FD1'Access,
      S2FD2'Access,S2FD3'Access,S2FD4'Access,S2FD5'Access,S3000'Access,
      S3036'Access,S3038'Access,S3039'Access,S303A'Access,S304C'Access,
      S304E'Access,S3050'Access,S3052'Access,S3054'Access,S3056'Access,
      S3058'Access,S305A'Access,S305C'Access,S305E'Access,S3060'Access,
      S3062'Access,S3065'Access,S3067'Access,S3069'Access,S3070'Access,
      S3071'Access,S3073'Access,S3074'Access,S3076'Access,S3077'Access,
      S3079'Access,S307A'Access,S307C'Access,S307D'Access,S3094'Access,
      S309B'Access,S309C'Access,S309E'Access,S309F'Access,S30AC'Access,
      S30AE'Access,S30B0'Access,S30B2'Access,S30B4'Access,S30B6'Access,
      S30B8'Access,S30BA'Access,S30BC'Access,S30BE'Access,S30C0'Access,
      S30C2'Access,S30C5'Access,S30C7'Access,S30C9'Access,S30D0'Access,
      S30D1'Access,S30D3'Access,S30D4'Access,S30D6'Access,S30D7'Access,
      S30D9'Access,S30DA'Access,S30DC'Access,S30DD'Access,S30F4'Access,
      S30F7'Access,S30F8'Access,S30F9'Access,S30FA'Access,S30FE'Access,
      S30FF'Access,S3131'Access,S3132'Access,S3133'Access,S3134'Access,
      S3135'Access,S3136'Access,S3137'Access,S3138'Access,S3139'Access,
      S313A'Access,S313B'Access,S313C'Access,S313D'Access,S313E'Access,
      S313F'Access,S3140'Access,S3141'Access,S3142'Access,S3143'Access,
      S3144'Access,S3145'Access,S3146'Access,S3147'Access,S3148'Access,
      S3149'Access,S314A'Access,S314B'Access,S314C'Access,S314D'Access,
      S314E'Access,S314F'Access,S3150'Access,S3151'Access,S3152'Access,
      S3153'Access,S3154'Access,S3155'Access,S3156'Access,S3157'Access,
      S3158'Access,S3159'Access,S315A'Access,S315B'Access,S315C'Access,
      S315D'Access,S315E'Access,S315F'Access,S3160'Access,S3161'Access,
      S3162'Access,S3163'Access,S3164'Access,S3165'Access,S3166'Access,
      S3167'Access,S3168'Access,S3169'Access,S316A'Access,S316B'Access,
      S316C'Access,S316D'Access,S316E'Access,S316F'Access,S3170'Access,
      S3171'Access,S3172'Access,S3173'Access,S3174'Access,S3175'Access,
      S3176'Access,S3177'Access,S3178'Access,S3179'Access,S317A'Access,
      S317B'Access,S317C'Access,S317D'Access,S317E'Access,S317F'Access,
      S3180'Access,S3181'Access,S3182'Access,S3183'Access,S3184'Access,
      S3185'Access,S3186'Access,S3187'Access,S3188'Access,S3189'Access,
      S318A'Access,S318B'Access,S318C'Access,S318D'Access,S318E'Access,
      S3192'Access,S3193'Access,S3194'Access,S3195'Access,S3196'Access,
      S3197'Access,S3198'Access,S3199'Access,S319A'Access,S319B'Access,
      S319C'Access,S319D'Access,S319E'Access,S319F'Access,S3200'Access,
      S3201'Access,S3202'Access,S3203'Access,S3204'Access,S3205'Access,
      S3206'Access,S3207'Access,S3208'Access,S3209'Access,S320A'Access,
      S320B'Access,S320C'Access,S320D'Access,S320E'Access,S320F'Access,
      S3210'Access,S3211'Access,S3212'Access,S3213'Access,S3214'Access,
      S3215'Access,S3216'Access,S3217'Access,S3218'Access,S3219'Access,
      S321A'Access,S321B'Access,S321C'Access,S321D'Access,S321E'Access,
      S3220'Access,S3221'Access,S3222'Access,S3223'Access,S3224'Access,
      S3225'Access,S3226'Access,S3227'Access,S3228'Access,S3229'Access,
      S322A'Access,S322B'Access,S322C'Access,S322D'Access,S322E'Access,
      S322F'Access,S3230'Access,S3231'Access,S3232'Access,S3233'Access,
      S3234'Access,S3235'Access,S3236'Access,S3237'Access,S3238'Access,
      S3239'Access,S323A'Access,S323B'Access,S323C'Access,S323D'Access,
      S323E'Access,S323F'Access,S3240'Access,S3241'Access,S3242'Access,
      S3243'Access,S3244'Access,S3245'Access,S3246'Access,S3247'Access,
      S3250'Access,S3251'Access,S3252'Access,S3253'Access,S3254'Access,
      S3255'Access,S3256'Access,S3257'Access,S3258'Access,S3259'Access,
      S325A'Access,S325B'Access,S325C'Access,S325D'Access,S325E'Access,
      S325F'Access,S3260'Access,S3261'Access,S3262'Access,S3263'Access,
      S3264'Access,S3265'Access,S3266'Access,S3267'Access,S3268'Access,
      S3269'Access,S326A'Access,S326B'Access,S326C'Access,S326D'Access,
      S326E'Access,S326F'Access,S3270'Access,S3271'Access,S3272'Access,
      S3273'Access,S3274'Access,S3275'Access,S3276'Access,S3277'Access,
      S3278'Access,S3279'Access,S327A'Access,S327B'Access,S327C'Access,
      S327D'Access,S327E'Access,S3280'Access,S3281'Access,S3282'Access,
      S3283'Access,S3284'Access,S3285'Access,S3286'Access,S3287'Access,
      S3288'Access,S3289'Access,S328A'Access,S328B'Access,S328C'Access,
      S328D'Access,S328E'Access,S328F'Access,S3290'Access,S3291'Access,
      S3292'Access,S3293'Access,S3294'Access,S3295'Access,S3296'Access,
      S3297'Access,S3298'Access,S3299'Access,S329A'Access,S329B'Access,
      S329C'Access,S329D'Access,S329E'Access,S329F'Access,S32A0'Access,
      S32A1'Access,S32A2'Access,S32A3'Access,S32A4'Access,S32A5'Access,
      S32A6'Access,S32A7'Access,S32A8'Access,S32A9'Access,S32AA'Access,
      S32AB'Access,S32AC'Access,S32AD'Access,S32AE'Access,S32AF'Access,
      S32B0'Access,S32B1'Access,S32B2'Access,S32B3'Access,S32B4'Access,
      S32B5'Access,S32B6'Access,S32B7'Access,S32B8'Access,S32B9'Access,
      S32BA'Access,S32BB'Access,S32BC'Access,S32BD'Access,S32BE'Access,
      S32BF'Access,S32C0'Access,S32C1'Access,S32C2'Access,S32C3'Access,
      S32C4'Access,S32C5'Access,S32C6'Access,S32C7'Access,S32C8'Access,
      S32C9'Access,S32CA'Access,S32CB'Access,S32CC'Access,S32CD'Access,
      S32CE'Access,S32CF'Access,S32D0'Access,S32D1'Access,S32D2'Access,
      S32D3'Access,S32D4'Access,S32D5'Access,S32D6'Access,S32D7'Access,
      S32D8'Access,S32D9'Access,S32DA'Access,S32DB'Access,S32DC'Access,
      S32DD'Access,S32DE'Access,S32DF'Access,S32E0'Access,S32E1'Access,
      S32E2'Access,S32E3'Access,S32E4'Access,S32E5'Access,S32E6'Access,
      S32E7'Access,S32E8'Access,S32E9'Access,S32EA'Access,S32EB'Access,
      S32EC'Access,S32ED'Access,S32EE'Access,S32EF'Access,S32F0'Access,
      S32F1'Access,S32F2'Access,S32F3'Access,S32F4'Access,S32F5'Access,
      S32F6'Access,S32F7'Access,S32F8'Access,S32F9'Access,S32FA'Access,
      S32FB'Access,S32FC'Access,S32FD'Access,S32FE'Access,S32FF'Access,
      S3300'Access,S3301'Access,S3302'Access,S3303'Access,S3304'Access,
      S3305'Access,S3306'Access,S3307'Access,S3308'Access,S3309'Access,
      S330A'Access,S330B'Access,S330C'Access,S330D'Access,S330E'Access,
      S330F'Access,S3310'Access,S3311'Access,S3312'Access,S3313'Access,
      S3314'Access,S3315'Access,S3316'Access,S3317'Access,S3318'Access,
      S3319'Access,S331A'Access,S331B'Access,S331C'Access,S331D'Access,
      S331E'Access,S331F'Access,S3320'Access,S3321'Access,S3322'Access,
      S3323'Access,S3324'Access,S3325'Access,S3326'Access,S3327'Access,
      S3328'Access,S3329'Access,S332A'Access,S332B'Access,S332C'Access,
      S332D'Access,S332E'Access,S332F'Access,S3330'Access,S3331'Access,
      S3332'Access,S3333'Access,S3334'Access,S3335'Access,S3336'Access,
      S3337'Access,S3338'Access,S3339'Access,S333A'Access,S333B'Access,
      S333C'Access,S333D'Access,S333E'Access,S333F'Access,S3340'Access,
      S3341'Access,S3342'Access,S3343'Access,S3344'Access,S3345'Access,
      S3346'Access,S3347'Access,S3348'Access,S3349'Access,S334A'Access,
      S334B'Access,S334C'Access,S334D'Access,S334E'Access,S334F'Access,
      S3350'Access,S3351'Access,S3352'Access,S3353'Access,S3354'Access,
      S3355'Access,S3356'Access,S3357'Access,S3358'Access,S3359'Access,
      S335A'Access,S335B'Access,S335C'Access,S335D'Access,S335E'Access,
      S335F'Access,S3360'Access,S3361'Access,S3362'Access,S3363'Access,
      S3364'Access,S3365'Access,S3366'Access,S3367'Access,S3368'Access,
      S3369'Access,S336A'Access,S336B'Access,S336C'Access,S336D'Access,
      S336E'Access,S336F'Access,S3370'Access,S3371'Access,S3372'Access,
      S3373'Access,S3374'Access,S3375'Access,S3376'Access,S3377'Access,
      S3378'Access,S3379'Access,S337A'Access,S337B'Access,S337C'Access,
      S337D'Access,S337E'Access,S337F'Access,S3380'Access,S3381'Access,
      S3382'Access,S3383'Access,S3384'Access,S3385'Access,S3386'Access,
      S3387'Access,S3388'Access,S3389'Access,S338A'Access,S338B'Access,
      S338C'Access,S338D'Access,S338E'Access,S338F'Access,S3390'Access,
      S3391'Access,S3392'Access,S3393'Access,S3394'Access,S3395'Access,
      S3396'Access,S3397'Access,S3398'Access,S3399'Access,S339A'Access,
      S339B'Access,S339C'Access,S339D'Access,S339E'Access,S339F'Access,
      S33A0'Access,S33A1'Access,S33A2'Access,S33A3'Access,S33A4'Access,
      S33A5'Access,S33A6'Access,S33A7'Access,S33A8'Access,S33A9'Access,
      S33AA'Access,S33AB'Access,S33AC'Access,S33AD'Access,S33AE'Access,
      S33AF'Access,S33B0'Access,S33B1'Access,S33B2'Access,S33B3'Access,
      S33B4'Access,S33B5'Access,S33B6'Access,S33B7'Access,S33B8'Access,
      S33B9'Access,S33BA'Access,S33BB'Access,S33BC'Access,S33BD'Access,
      S33BE'Access,S33BF'Access,S33C0'Access,S33C1'Access,S33C2'Access,
      S33C3'Access,S33C4'Access,S33C5'Access,S33C6'Access,S33C7'Access,
      S33C8'Access,S33C9'Access,S33CA'Access,S33CB'Access,S33CC'Access,
      S33CD'Access,S33CE'Access,S33CF'Access,S33D0'Access,S33D1'Access,
      S33D2'Access,S33D3'Access,S33D4'Access,S33D5'Access,S33D6'Access,
      S33D7'Access,S33D8'Access,S33D9'Access,S33DA'Access,S33DB'Access,
      S33DC'Access,S33DD'Access,S33DE'Access,S33DF'Access,S33E0'Access,
      S33E1'Access,S33E2'Access,S33E3'Access,S33E4'Access,S33E5'Access,
      S33E6'Access,S33E7'Access,S33E8'Access,S33E9'Access,S33EA'Access,
      S33EB'Access,S33EC'Access,S33ED'Access,S33EE'Access,S33EF'Access,
      S33F0'Access,S33F1'Access,S33F2'Access,S33F3'Access,S33F4'Access,
      S33F5'Access,S33F6'Access,S33F7'Access,S33F8'Access,S33F9'Access,
      S33FA'Access,S33FB'Access,S33FC'Access,S33FD'Access,S33FE'Access,
      S33FF'Access,SA69C'Access,SA69D'Access,SA770'Access,SA7F2'Access,
      SA7F3'Access,SA7F4'Access,SA7F8'Access,SA7F9'Access,SAB5C'Access,
      SAB5D'Access,SAB5E'Access,SAB5F'Access,SAB69'Access,SF900'Access,
      SF901'Access,SF902'Access,SF903'Access,SF904'Access,SF905'Access,
      SF906'Access,SF907'Access,SF908'Access,SF909'Access,SF90A'Access,
      SF90B'Access,SF90C'Access,SF90D'Access,SF90E'Access,SF90F'Access,
      SF910'Access,SF911'Access,SF912'Access,SF913'Access,SF914'Access,
      SF915'Access,SF916'Access,SF917'Access,SF918'Access,SF919'Access,
      SF91A'Access,SF91B'Access,SF91C'Access,SF91D'Access,SF91E'Access,
      SF91F'Access,SF920'Access,SF921'Access,SF922'Access,SF923'Access,
      SF924'Access,SF925'Access,SF926'Access,SF927'Access,SF928'Access,
      SF929'Access,SF92A'Access,SF92B'Access,SF92C'Access,SF92D'Access,
      SF92E'Access,SF92F'Access,SF930'Access,SF931'Access,SF932'Access,
      SF933'Access,SF934'Access,SF935'Access,SF936'Access,SF937'Access,
      SF938'Access,SF939'Access,SF93A'Access,SF93B'Access,SF93C'Access,
      SF93D'Access,SF93E'Access,SF93F'Access,SF940'Access,SF941'Access,
      SF942'Access,SF943'Access,SF944'Access,SF945'Access,SF946'Access,
      SF947'Access,SF948'Access,SF949'Access,SF94A'Access,SF94B'Access,
      SF94C'Access,SF94D'Access,SF94E'Access,SF94F'Access,SF950'Access,
      SF951'Access,SF952'Access,SF953'Access,SF954'Access,SF955'Access,
      SF956'Access,SF957'Access,SF958'Access,SF959'Access,SF95A'Access,
      SF95B'Access,SF95C'Access,SF95D'Access,SF95E'Access,SF95F'Access,
      SF960'Access,SF961'Access,SF962'Access,SF963'Access,SF964'Access,
      SF965'Access,SF966'Access,SF967'Access,SF968'Access,SF969'Access,
      SF96A'Access,SF96B'Access,SF96C'Access,SF96D'Access,SF96E'Access,
      SF96F'Access,SF970'Access,SF971'Access,SF972'Access,SF973'Access,
      SF974'Access,SF975'Access,SF976'Access,SF977'Access,SF978'Access,
      SF979'Access,SF97A'Access,SF97B'Access,SF97C'Access,SF97D'Access,
      SF97E'Access,SF97F'Access,SF980'Access,SF981'Access,SF982'Access,
      SF983'Access,SF984'Access,SF985'Access,SF986'Access,SF987'Access,
      SF988'Access,SF989'Access,SF98A'Access,SF98B'Access,SF98C'Access,
      SF98D'Access,SF98E'Access,SF98F'Access,SF990'Access,SF991'Access,
      SF992'Access,SF993'Access,SF994'Access,SF995'Access,SF996'Access,
      SF997'Access,SF998'Access,SF999'Access,SF99A'Access,SF99B'Access,
      SF99C'Access,SF99D'Access,SF99E'Access,SF99F'Access,SF9A0'Access,
      SF9A1'Access,SF9A2'Access,SF9A3'Access,SF9A4'Access,SF9A5'Access,
      SF9A6'Access,SF9A7'Access,SF9A8'Access,SF9A9'Access,SF9AA'Access,
      SF9AB'Access,SF9AC'Access,SF9AD'Access,SF9AE'Access,SF9AF'Access,
      SF9B0'Access,SF9B1'Access,SF9B2'Access,SF9B3'Access,SF9B4'Access,
      SF9B5'Access,SF9B6'Access,SF9B7'Access,SF9B8'Access,SF9B9'Access,
      SF9BA'Access,SF9BB'Access,SF9BC'Access,SF9BD'Access,SF9BE'Access,
      SF9BF'Access,SF9C0'Access,SF9C1'Access,SF9C2'Access,SF9C3'Access,
      SF9C4'Access,SF9C5'Access,SF9C6'Access,SF9C7'Access,SF9C8'Access,
      SF9C9'Access,SF9CA'Access,SF9CB'Access,SF9CC'Access,SF9CD'Access,
      SF9CE'Access,SF9CF'Access,SF9D0'Access,SF9D1'Access,SF9D2'Access,
      SF9D3'Access,SF9D4'Access,SF9D5'Access,SF9D6'Access,SF9D7'Access,
      SF9D8'Access,SF9D9'Access,SF9DA'Access,SF9DB'Access,SF9DC'Access,
      SF9DD'Access,SF9DE'Access,SF9DF'Access,SF9E0'Access,SF9E1'Access,
      SF9E2'Access,SF9E3'Access,SF9E4'Access,SF9E5'Access,SF9E6'Access,
      SF9E7'Access,SF9E8'Access,SF9E9'Access,SF9EA'Access,SF9EB'Access,
      SF9EC'Access,SF9ED'Access,SF9EE'Access,SF9EF'Access,SF9F0'Access,
      SF9F1'Access,SF9F2'Access,SF9F3'Access,SF9F4'Access,SF9F5'Access,
      SF9F6'Access,SF9F7'Access,SF9F8'Access,SF9F9'Access,SF9FA'Access,
      SF9FB'Access,SF9FC'Access,SF9FD'Access,SF9FE'Access,SF9FF'Access,
      SFA00'Access,SFA01'Access,SFA02'Access,SFA03'Access,SFA04'Access,
      SFA05'Access,SFA06'Access,SFA07'Access,SFA08'Access,SFA09'Access,
      SFA0A'Access,SFA0B'Access,SFA0C'Access,SFA0D'Access,SFA10'Access,
      SFA12'Access,SFA15'Access,SFA16'Access,SFA17'Access,SFA18'Access,
      SFA19'Access,SFA1A'Access,SFA1B'Access,SFA1C'Access,SFA1D'Access,
      SFA1E'Access,SFA20'Access,SFA22'Access,SFA25'Access,SFA26'Access,
      SFA2A'Access,SFA2B'Access,SFA2C'Access,SFA2D'Access,SFA2E'Access,
      SFA2F'Access,SFA30'Access,SFA31'Access,SFA32'Access,SFA33'Access,
      SFA34'Access,SFA35'Access,SFA36'Access,SFA37'Access,SFA38'Access,
      SFA39'Access,SFA3A'Access,SFA3B'Access,SFA3C'Access,SFA3D'Access,
      SFA3E'Access,SFA3F'Access,SFA40'Access,SFA41'Access,SFA42'Access,
      SFA43'Access,SFA44'Access,SFA45'Access,SFA46'Access,SFA47'Access,
      SFA48'Access,SFA49'Access,SFA4A'Access,SFA4B'Access,SFA4C'Access,
      SFA4D'Access,SFA4E'Access,SFA4F'Access,SFA50'Access,SFA51'Access,
      SFA52'Access,SFA53'Access,SFA54'Access,SFA55'Access,SFA56'Access,
      SFA57'Access,SFA58'Access,SFA59'Access,SFA5A'Access,SFA5B'Access,
      SFA5C'Access,SFA5D'Access,SFA5E'Access,SFA5F'Access,SFA60'Access,
      SFA61'Access,SFA62'Access,SFA63'Access,SFA64'Access,SFA65'Access,
      SFA66'Access,SFA67'Access,SFA68'Access,SFA69'Access,SFA6A'Access,
      SFA6B'Access,SFA6C'Access,SFA6D'Access,SFA70'Access,SFA71'Access,
      SFA72'Access,SFA73'Access,SFA74'Access,SFA75'Access,SFA76'Access,
      SFA77'Access,SFA78'Access,SFA79'Access,SFA7A'Access,SFA7B'Access,
      SFA7C'Access,SFA7D'Access,SFA7E'Access,SFA7F'Access,SFA80'Access,
      SFA81'Access,SFA82'Access,SFA83'Access,SFA84'Access,SFA85'Access,
      SFA86'Access,SFA87'Access,SFA88'Access,SFA89'Access,SFA8A'Access,
      SFA8B'Access,SFA8C'Access,SFA8D'Access,SFA8E'Access,SFA8F'Access,
      SFA90'Access,SFA91'Access,SFA92'Access,SFA93'Access,SFA94'Access,
      SFA95'Access,SFA96'Access,SFA97'Access,SFA98'Access,SFA99'Access,
      SFA9A'Access,SFA9B'Access,SFA9C'Access,SFA9D'Access,SFA9E'Access,
      SFA9F'Access,SFAA0'Access,SFAA1'Access,SFAA2'Access,SFAA3'Access,
      SFAA4'Access,SFAA5'Access,SFAA6'Access,SFAA7'Access,SFAA8'Access,
      SFAA9'Access,SFAAA'Access,SFAAB'Access,SFAAC'Access,SFAAD'Access,
      SFAAE'Access,SFAAF'Access,SFAB0'Access,SFAB1'Access,SFAB2'Access,
      SFAB3'Access,SFAB4'Access,SFAB5'Access,SFAB6'Access,SFAB7'Access,
      SFAB8'Access,SFAB9'Access,SFABA'Access,SFABB'Access,SFABC'Access,
      SFABD'Access,SFABE'Access,SFABF'Access,SFAC0'Access,SFAC1'Access,
      SFAC2'Access,SFAC3'Access,SFAC4'Access,SFAC5'Access,SFAC6'Access,
      SFAC7'Access,SFAC8'Access,SFAC9'Access,SFACA'Access,SFACB'Access,
      SFACC'Access,SFACD'Access,SFACE'Access,SFACF'Access,SFAD0'Access,
      SFAD1'Access,SFAD2'Access,SFAD3'Access,SFAD4'Access,SFAD5'Access,
      SFAD6'Access,SFAD7'Access,SFAD8'Access,SFAD9'Access,SFB00'Access,
      SFB01'Access,SFB02'Access,SFB03'Access,SFB04'Access,SFB05'Access,
      SFB06'Access,SFB13'Access,SFB14'Access,SFB15'Access,SFB16'Access,
      SFB17'Access,SFB1D'Access,SFB1F'Access,SFB20'Access,SFB21'Access,
      SFB22'Access,SFB23'Access,SFB24'Access,SFB25'Access,SFB26'Access,
      SFB27'Access,SFB28'Access,SFB29'Access,SFB2A'Access,SFB2B'Access,
      SFB2C'Access,SFB2D'Access,SFB2E'Access,SFB2F'Access,SFB30'Access,
      SFB31'Access,SFB32'Access,SFB33'Access,SFB34'Access,SFB35'Access,
      SFB36'Access,SFB38'Access,SFB39'Access,SFB3A'Access,SFB3B'Access,
      SFB3C'Access,SFB3E'Access,SFB40'Access,SFB41'Access,SFB43'Access,
      SFB44'Access,SFB46'Access,SFB47'Access,SFB48'Access,SFB49'Access,
      SFB4A'Access,SFB4B'Access,SFB4C'Access,SFB4D'Access,SFB4E'Access,
      SFB4F'Access,SFB50'Access,SFB51'Access,SFB52'Access,SFB53'Access,
      SFB54'Access,SFB55'Access,SFB56'Access,SFB57'Access,SFB58'Access,
      SFB59'Access,SFB5A'Access,SFB5B'Access,SFB5C'Access,SFB5D'Access,
      SFB5E'Access,SFB5F'Access,SFB60'Access,SFB61'Access,SFB62'Access,
      SFB63'Access,SFB64'Access,SFB65'Access,SFB66'Access,SFB67'Access,
      SFB68'Access,SFB69'Access,SFB6A'Access,SFB6B'Access,SFB6C'Access,
      SFB6D'Access,SFB6E'Access,SFB6F'Access,SFB70'Access,SFB71'Access,
      SFB72'Access,SFB73'Access,SFB74'Access,SFB75'Access,SFB76'Access,
      SFB77'Access,SFB78'Access,SFB79'Access,SFB7A'Access,SFB7B'Access,
      SFB7C'Access,SFB7D'Access,SFB7E'Access,SFB7F'Access,SFB80'Access,
      SFB81'Access,SFB82'Access,SFB83'Access,SFB84'Access,SFB85'Access,
      SFB86'Access,SFB87'Access,SFB88'Access,SFB89'Access,SFB8A'Access,
      SFB8B'Access,SFB8C'Access,SFB8D'Access,SFB8E'Access,SFB8F'Access,
      SFB90'Access,SFB91'Access,SFB92'Access,SFB93'Access,SFB94'Access,
      SFB95'Access,SFB96'Access,SFB97'Access,SFB98'Access,SFB99'Access,
      SFB9A'Access,SFB9B'Access,SFB9C'Access,SFB9D'Access,SFB9E'Access,
      SFB9F'Access,SFBA0'Access,SFBA1'Access,SFBA2'Access,SFBA3'Access,
      SFBA4'Access,SFBA5'Access,SFBA6'Access,SFBA7'Access,SFBA8'Access,
      SFBA9'Access,SFBAA'Access,SFBAB'Access,SFBAC'Access,SFBAD'Access,
      SFBAE'Access,SFBAF'Access,SFBB0'Access,SFBB1'Access,SFBD3'Access,
      SFBD4'Access,SFBD5'Access,SFBD6'Access,SFBD7'Access,SFBD8'Access,
      SFBD9'Access,SFBDA'Access,SFBDB'Access,SFBDC'Access,SFBDD'Access,
      SFBDE'Access,SFBDF'Access,SFBE0'Access,SFBE1'Access,SFBE2'Access,
      SFBE3'Access,SFBE4'Access,SFBE5'Access,SFBE6'Access,SFBE7'Access,
      SFBE8'Access,SFBE9'Access,SFBEA'Access,SFBEB'Access,SFBEC'Access,
      SFBED'Access,SFBEE'Access,SFBEF'Access,SFBF0'Access,SFBF1'Access,
      SFBF2'Access,SFBF3'Access,SFBF4'Access,SFBF5'Access,SFBF6'Access,
      SFBF7'Access,SFBF8'Access,SFBF9'Access,SFBFA'Access,SFBFB'Access,
      SFBFC'Access,SFBFD'Access,SFBFE'Access,SFBFF'Access,SFC00'Access,
      SFC01'Access,SFC02'Access,SFC03'Access,SFC04'Access,SFC05'Access,
      SFC06'Access,SFC07'Access,SFC08'Access,SFC09'Access,SFC0A'Access,
      SFC0B'Access,SFC0C'Access,SFC0D'Access,SFC0E'Access,SFC0F'Access,
      SFC10'Access,SFC11'Access,SFC12'Access,SFC13'Access,SFC14'Access,
      SFC15'Access,SFC16'Access,SFC17'Access,SFC18'Access,SFC19'Access,
      SFC1A'Access,SFC1B'Access,SFC1C'Access,SFC1D'Access,SFC1E'Access,
      SFC1F'Access,SFC20'Access,SFC21'Access,SFC22'Access,SFC23'Access,
      SFC24'Access,SFC25'Access,SFC26'Access,SFC27'Access,SFC28'Access,
      SFC29'Access,SFC2A'Access,SFC2B'Access,SFC2C'Access,SFC2D'Access,
      SFC2E'Access,SFC2F'Access,SFC30'Access,SFC31'Access,SFC32'Access,
      SFC33'Access,SFC34'Access,SFC35'Access,SFC36'Access,SFC37'Access,
      SFC38'Access,SFC39'Access,SFC3A'Access,SFC3B'Access,SFC3C'Access,
      SFC3D'Access,SFC3E'Access,SFC3F'Access,SFC40'Access,SFC41'Access,
      SFC42'Access,SFC43'Access,SFC44'Access,SFC45'Access,SFC46'Access,
      SFC47'Access,SFC48'Access,SFC49'Access,SFC4A'Access,SFC4B'Access,
      SFC4C'Access,SFC4D'Access,SFC4E'Access,SFC4F'Access,SFC50'Access,
      SFC51'Access,SFC52'Access,SFC53'Access,SFC54'Access,SFC55'Access,
      SFC56'Access,SFC57'Access,SFC58'Access,SFC59'Access,SFC5A'Access,
      SFC5B'Access,SFC5C'Access,SFC5D'Access,SFC5E'Access,SFC5F'Access,
      SFC60'Access,SFC61'Access,SFC62'Access,SFC63'Access,SFC64'Access,
      SFC65'Access,SFC66'Access,SFC67'Access,SFC68'Access,SFC69'Access,
      SFC6A'Access,SFC6B'Access,SFC6C'Access,SFC6D'Access,SFC6E'Access,
      SFC6F'Access,SFC70'Access,SFC71'Access,SFC72'Access,SFC73'Access,
      SFC74'Access,SFC75'Access,SFC76'Access,SFC77'Access,SFC78'Access,
      SFC79'Access,SFC7A'Access,SFC7B'Access,SFC7C'Access,SFC7D'Access,
      SFC7E'Access,SFC7F'Access,SFC80'Access,SFC81'Access,SFC82'Access,
      SFC83'Access,SFC84'Access,SFC85'Access,SFC86'Access,SFC87'Access,
      SFC88'Access,SFC89'Access,SFC8A'Access,SFC8B'Access,SFC8C'Access,
      SFC8D'Access,SFC8E'Access,SFC8F'Access,SFC90'Access,SFC91'Access,
      SFC92'Access,SFC93'Access,SFC94'Access,SFC95'Access,SFC96'Access,
      SFC97'Access,SFC98'Access,SFC99'Access,SFC9A'Access,SFC9B'Access,
      SFC9C'Access,SFC9D'Access,SFC9E'Access,SFC9F'Access,SFCA0'Access,
      SFCA1'Access,SFCA2'Access,SFCA3'Access,SFCA4'Access,SFCA5'Access,
      SFCA6'Access,SFCA7'Access,SFCA8'Access,SFCA9'Access,SFCAA'Access,
      SFCAB'Access,SFCAC'Access,SFCAD'Access,SFCAE'Access,SFCAF'Access,
      SFCB0'Access,SFCB1'Access,SFCB2'Access,SFCB3'Access,SFCB4'Access,
      SFCB5'Access,SFCB6'Access,SFCB7'Access,SFCB8'Access,SFCB9'Access,
      SFCBA'Access,SFCBB'Access,SFCBC'Access,SFCBD'Access,SFCBE'Access,
      SFCBF'Access,SFCC0'Access,SFCC1'Access,SFCC2'Access,SFCC3'Access,
      SFCC4'Access,SFCC5'Access,SFCC6'Access,SFCC7'Access,SFCC8'Access,
      SFCC9'Access,SFCCA'Access,SFCCB'Access,SFCCC'Access,SFCCD'Access,
      SFCCE'Access,SFCCF'Access,SFCD0'Access,SFCD1'Access,SFCD2'Access,
      SFCD3'Access,SFCD4'Access,SFCD5'Access,SFCD6'Access,SFCD7'Access,
      SFCD8'Access,SFCD9'Access,SFCDA'Access,SFCDB'Access,SFCDC'Access,
      SFCDD'Access,SFCDE'Access,SFCDF'Access,SFCE0'Access,SFCE1'Access,
      SFCE2'Access,SFCE3'Access,SFCE4'Access,SFCE5'Access,SFCE6'Access,
      SFCE7'Access,SFCE8'Access,SFCE9'Access,SFCEA'Access,SFCEB'Access,
      SFCEC'Access,SFCED'Access,SFCEE'Access,SFCEF'Access,SFCF0'Access,
      SFCF1'Access,SFCF2'Access,SFCF3'Access,SFCF4'Access,SFCF5'Access,
      SFCF6'Access,SFCF7'Access,SFCF8'Access,SFCF9'Access,SFCFA'Access,
      SFCFB'Access,SFCFC'Access,SFCFD'Access,SFCFE'Access,SFCFF'Access,
      SFD00'Access,SFD01'Access,SFD02'Access,SFD03'Access,SFD04'Access,
      SFD05'Access,SFD06'Access,SFD07'Access,SFD08'Access,SFD09'Access,
      SFD0A'Access,SFD0B'Access,SFD0C'Access,SFD0D'Access,SFD0E'Access,
      SFD0F'Access,SFD10'Access,SFD11'Access,SFD12'Access,SFD13'Access,
      SFD14'Access,SFD15'Access,SFD16'Access,SFD17'Access,SFD18'Access,
      SFD19'Access,SFD1A'Access,SFD1B'Access,SFD1C'Access,SFD1D'Access,
      SFD1E'Access,SFD1F'Access,SFD20'Access,SFD21'Access,SFD22'Access,
      SFD23'Access,SFD24'Access,SFD25'Access,SFD26'Access,SFD27'Access,
      SFD28'Access,SFD29'Access,SFD2A'Access,SFD2B'Access,SFD2C'Access,
      SFD2D'Access,SFD2E'Access,SFD2F'Access,SFD30'Access,SFD31'Access,
      SFD32'Access,SFD33'Access,SFD34'Access,SFD35'Access,SFD36'Access,
      SFD37'Access,SFD38'Access,SFD39'Access,SFD3A'Access,SFD3B'Access,
      SFD3C'Access,SFD3D'Access,SFD50'Access,SFD51'Access,SFD52'Access,
      SFD53'Access,SFD54'Access,SFD55'Access,SFD56'Access,SFD57'Access,
      SFD58'Access,SFD59'Access,SFD5A'Access,SFD5B'Access,SFD5C'Access,
      SFD5D'Access,SFD5E'Access,SFD5F'Access,SFD60'Access,SFD61'Access,
      SFD62'Access,SFD63'Access,SFD64'Access,SFD65'Access,SFD66'Access,
      SFD67'Access,SFD68'Access,SFD69'Access,SFD6A'Access,SFD6B'Access,
      SFD6C'Access,SFD6D'Access,SFD6E'Access,SFD6F'Access,SFD70'Access,
      SFD71'Access,SFD72'Access,SFD73'Access,SFD74'Access,SFD75'Access,
      SFD76'Access,SFD77'Access,SFD78'Access,SFD79'Access,SFD7A'Access,
      SFD7B'Access,SFD7C'Access,SFD7D'Access,SFD7E'Access,SFD7F'Access,
      SFD80'Access,SFD81'Access,SFD82'Access,SFD83'Access,SFD84'Access,
      SFD85'Access,SFD86'Access,SFD87'Access,SFD88'Access,SFD89'Access,
      SFD8A'Access,SFD8B'Access,SFD8C'Access,SFD8D'Access,SFD8E'Access,
      SFD8F'Access,SFD92'Access,SFD93'Access,SFD94'Access,SFD95'Access,
      SFD96'Access,SFD97'Access,SFD98'Access,SFD99'Access,SFD9A'Access,
      SFD9B'Access,SFD9C'Access,SFD9D'Access,SFD9E'Access,SFD9F'Access,
      SFDA0'Access,SFDA1'Access,SFDA2'Access,SFDA3'Access,SFDA4'Access,
      SFDA5'Access,SFDA6'Access,SFDA7'Access,SFDA8'Access,SFDA9'Access,
      SFDAA'Access,SFDAB'Access,SFDAC'Access,SFDAD'Access,SFDAE'Access,
      SFDAF'Access,SFDB0'Access,SFDB1'Access,SFDB2'Access,SFDB3'Access,
      SFDB4'Access,SFDB5'Access,SFDB6'Access,SFDB7'Access,SFDB8'Access,
      SFDB9'Access,SFDBA'Access,SFDBB'Access,SFDBC'Access,SFDBD'Access,
      SFDBE'Access,SFDBF'Access,SFDC0'Access,SFDC1'Access,SFDC2'Access,
      SFDC3'Access,SFDC4'Access,SFDC5'Access,SFDC6'Access,SFDC7'Access,
      SFDF0'Access,SFDF1'Access,SFDF2'Access,SFDF3'Access,SFDF4'Access,
      SFDF5'Access,SFDF6'Access,SFDF7'Access,SFDF8'Access,SFDF9'Access,
      SFDFA'Access,SFDFB'Access,SFDFC'Access,SFE10'Access,SFE11'Access,
      SFE12'Access,SFE13'Access,SFE14'Access,SFE15'Access,SFE16'Access,
      SFE17'Access,SFE18'Access,SFE19'Access,SFE30'Access,SFE31'Access,
      SFE32'Access,SFE33'Access,SFE34'Access,SFE35'Access,SFE36'Access,
      SFE37'Access,SFE38'Access,SFE39'Access,SFE3A'Access,SFE3B'Access,
      SFE3C'Access,SFE3D'Access,SFE3E'Access,SFE3F'Access,SFE40'Access,
      SFE41'Access,SFE42'Access,SFE43'Access,SFE44'Access,SFE47'Access,
      SFE48'Access,SFE49'Access,SFE4A'Access,SFE4B'Access,SFE4C'Access,
      SFE4D'Access,SFE4E'Access,SFE4F'Access,SFE50'Access,SFE51'Access,
      SFE52'Access,SFE54'Access,SFE55'Access,SFE56'Access,SFE57'Access,
      SFE58'Access,SFE59'Access,SFE5A'Access,SFE5B'Access,SFE5C'Access,
      SFE5D'Access,SFE5E'Access,SFE5F'Access,SFE60'Access,SFE61'Access,
      SFE62'Access,SFE63'Access,SFE64'Access,SFE65'Access,SFE66'Access,
      SFE68'Access,SFE69'Access,SFE6A'Access,SFE6B'Access,SFE70'Access,
      SFE71'Access,SFE72'Access,SFE74'Access,SFE76'Access,SFE77'Access,
      SFE78'Access,SFE79'Access,SFE7A'Access,SFE7B'Access,SFE7C'Access,
      SFE7D'Access,SFE7E'Access,SFE7F'Access,SFE80'Access,SFE81'Access,
      SFE82'Access,SFE83'Access,SFE84'Access,SFE85'Access,SFE86'Access,
      SFE87'Access,SFE88'Access,SFE89'Access,SFE8A'Access,SFE8B'Access,
      SFE8C'Access,SFE8D'Access,SFE8E'Access,SFE8F'Access,SFE90'Access,
      SFE91'Access,SFE92'Access,SFE93'Access,SFE94'Access,SFE95'Access,
      SFE96'Access,SFE97'Access,SFE98'Access,SFE99'Access,SFE9A'Access,
      SFE9B'Access,SFE9C'Access,SFE9D'Access,SFE9E'Access,SFE9F'Access,
      SFEA0'Access,SFEA1'Access,SFEA2'Access,SFEA3'Access,SFEA4'Access,
      SFEA5'Access,SFEA6'Access,SFEA7'Access,SFEA8'Access,SFEA9'Access,
      SFEAA'Access,SFEAB'Access,SFEAC'Access,SFEAD'Access,SFEAE'Access,
      SFEAF'Access,SFEB0'Access,SFEB1'Access,SFEB2'Access,SFEB3'Access,
      SFEB4'Access,SFEB5'Access,SFEB6'Access,SFEB7'Access,SFEB8'Access,
      SFEB9'Access,SFEBA'Access,SFEBB'Access,SFEBC'Access,SFEBD'Access,
      SFEBE'Access,SFEBF'Access,SFEC0'Access,SFEC1'Access,SFEC2'Access,
      SFEC3'Access,SFEC4'Access,SFEC5'Access,SFEC6'Access,SFEC7'Access,
      SFEC8'Access,SFEC9'Access,SFECA'Access,SFECB'Access,SFECC'Access,
      SFECD'Access,SFECE'Access,SFECF'Access,SFED0'Access,SFED1'Access,
      SFED2'Access,SFED3'Access,SFED4'Access,SFED5'Access,SFED6'Access,
      SFED7'Access,SFED8'Access,SFED9'Access,SFEDA'Access,SFEDB'Access,
      SFEDC'Access,SFEDD'Access,SFEDE'Access,SFEDF'Access,SFEE0'Access,
      SFEE1'Access,SFEE2'Access,SFEE3'Access,SFEE4'Access,SFEE5'Access,
      SFEE6'Access,SFEE7'Access,SFEE8'Access,SFEE9'Access,SFEEA'Access,
      SFEEB'Access,SFEEC'Access,SFEED'Access,SFEEE'Access,SFEEF'Access,
      SFEF0'Access,SFEF1'Access,SFEF2'Access,SFEF3'Access,SFEF4'Access,
      SFEF5'Access,SFEF6'Access,SFEF7'Access,SFEF8'Access,SFEF9'Access,
      SFEFA'Access,SFEFB'Access,SFEFC'Access,SFF01'Access,SFF02'Access,
      SFF03'Access,SFF04'Access,SFF05'Access,SFF06'Access,SFF07'Access,
      SFF08'Access,SFF09'Access,SFF0A'Access,SFF0B'Access,SFF0C'Access,
      SFF0D'Access,SFF0E'Access,SFF0F'Access,SFF10'Access,SFF11'Access,
      SFF12'Access,SFF13'Access,SFF14'Access,SFF15'Access,SFF16'Access,
      SFF17'Access,SFF18'Access,SFF19'Access,SFF1A'Access,SFF1B'Access,
      SFF1C'Access,SFF1D'Access,SFF1E'Access,SFF1F'Access,SFF20'Access,
      SFF21'Access,SFF22'Access,SFF23'Access,SFF24'Access,SFF25'Access,
      SFF26'Access,SFF27'Access,SFF28'Access,SFF29'Access,SFF2A'Access,
      SFF2B'Access,SFF2C'Access,SFF2D'Access,SFF2E'Access,SFF2F'Access,
      SFF30'Access,SFF31'Access,SFF32'Access,SFF33'Access,SFF34'Access,
      SFF35'Access,SFF36'Access,SFF37'Access,SFF38'Access,SFF39'Access,
      SFF3A'Access,SFF3B'Access,SFF3C'Access,SFF3D'Access,SFF3E'Access,
      SFF3F'Access,SFF40'Access,SFF41'Access,SFF42'Access,SFF43'Access,
      SFF44'Access,SFF45'Access,SFF46'Access,SFF47'Access,SFF48'Access,
      SFF49'Access,SFF4A'Access,SFF4B'Access,SFF4C'Access,SFF4D'Access,
      SFF4E'Access,SFF4F'Access,SFF50'Access,SFF51'Access,SFF52'Access,
      SFF53'Access,SFF54'Access,SFF55'Access,SFF56'Access,SFF57'Access,
      SFF58'Access,SFF59'Access,SFF5A'Access,SFF5B'Access,SFF5C'Access,
      SFF5D'Access,SFF5E'Access,SFF5F'Access,SFF60'Access,SFF61'Access,
      SFF62'Access,SFF63'Access,SFF64'Access,SFF65'Access,SFF66'Access,
      SFF67'Access,SFF68'Access,SFF69'Access,SFF6A'Access,SFF6B'Access,
      SFF6C'Access,SFF6D'Access,SFF6E'Access,SFF6F'Access,SFF70'Access,
      SFF71'Access,SFF72'Access,SFF73'Access,SFF74'Access,SFF75'Access,
      SFF76'Access,SFF77'Access,SFF78'Access,SFF79'Access,SFF7A'Access,
      SFF7B'Access,SFF7C'Access,SFF7D'Access,SFF7E'Access,SFF7F'Access,
      SFF80'Access,SFF81'Access,SFF82'Access,SFF83'Access,SFF84'Access,
      SFF85'Access,SFF86'Access,SFF87'Access,SFF88'Access,SFF89'Access,
      SFF8A'Access,SFF8B'Access,SFF8C'Access,SFF8D'Access,SFF8E'Access,
      SFF8F'Access,SFF90'Access,SFF91'Access,SFF92'Access,SFF93'Access,
      SFF94'Access,SFF95'Access,SFF96'Access,SFF97'Access,SFF98'Access,
      SFF99'Access,SFF9A'Access,SFF9B'Access,SFF9C'Access,SFF9D'Access,
      SFF9E'Access,SFF9F'Access,SFFA0'Access,SFFA1'Access,SFFA2'Access,
      SFFA3'Access,SFFA4'Access,SFFA5'Access,SFFA6'Access,SFFA7'Access,
      SFFA8'Access,SFFA9'Access,SFFAA'Access,SFFAB'Access,SFFAC'Access,
      SFFAD'Access,SFFAE'Access,SFFAF'Access,SFFB0'Access,SFFB1'Access,
      SFFB2'Access,SFFB3'Access,SFFB4'Access,SFFB5'Access,SFFB6'Access,
      SFFB7'Access,SFFB8'Access,SFFB9'Access,SFFBA'Access,SFFBB'Access,
      SFFBC'Access,SFFBD'Access,SFFBE'Access,SFFC2'Access,SFFC3'Access,
      SFFC4'Access,SFFC5'Access,SFFC6'Access,SFFC7'Access,SFFCA'Access,
      SFFCB'Access,SFFCC'Access,SFFCD'Access,SFFCE'Access,SFFCF'Access,
      SFFD2'Access,SFFD3'Access,SFFD4'Access,SFFD5'Access,SFFD6'Access,
      SFFD7'Access,SFFDA'Access,SFFDB'Access,SFFDC'Access,SFFE0'Access,
      SFFE1'Access,SFFE2'Access,SFFE3'Access,SFFE4'Access,SFFE5'Access,
      SFFE6'Access,SFFE8'Access,SFFE9'Access,SFFEA'Access,SFFEB'Access,
      SFFEC'Access,SFFED'Access,SFFEE'Access,S10781'Access,S10782'Access,
      S10783'Access,S10784'Access,S10785'Access,S10787'Access,S10788'Access,
      S10789'Access,S1078A'Access,S1078B'Access,S1078C'Access,S1078D'Access,
      S1078E'Access,S1078F'Access,S10790'Access,S10791'Access,S10792'Access,
      S10793'Access,S10794'Access,S10795'Access,S10796'Access,S10797'Access,
      S10798'Access,S10799'Access,S1079A'Access,S1079B'Access,S1079C'Access,
      S1079D'Access,S1079E'Access,S1079F'Access,S107A0'Access,S107A1'Access,
      S107A2'Access,S107A3'Access,S107A4'Access,S107A5'Access,S107A6'Access,
      S107A7'Access,S107A8'Access,S107A9'Access,S107AA'Access,S107AB'Access,
      S107AC'Access,S107AD'Access,S107AE'Access,S107AF'Access,S107B0'Access,
      S107B2'Access,S107B3'Access,S107B4'Access,S107B5'Access,S107B6'Access,
      S107B7'Access,S107B8'Access,S107B9'Access,S107BA'Access,S1109A'Access,
      S1109C'Access,S110AB'Access,S1112E'Access,S1112F'Access,S1134B'Access,
      S1134C'Access,S114BB'Access,S114BC'Access,S114BE'Access,S115BA'Access,
      S115BB'Access,S11938'Access,S1D15E'Access,S1D15F'Access,S1D160'Access,
      S1D161'Access,S1D162'Access,S1D163'Access,S1D164'Access,S1D1BB'Access,
      S1D1BC'Access,S1D1BD'Access,S1D1BE'Access,S1D1BF'Access,S1D1C0'Access,
      S1D400'Access,S1D401'Access,S1D402'Access,S1D403'Access,S1D404'Access,
      S1D405'Access,S1D406'Access,S1D407'Access,S1D408'Access,S1D409'Access,
      S1D40A'Access,S1D40B'Access,S1D40C'Access,S1D40D'Access,S1D40E'Access,
      S1D40F'Access,S1D410'Access,S1D411'Access,S1D412'Access,S1D413'Access,
      S1D414'Access,S1D415'Access,S1D416'Access,S1D417'Access,S1D418'Access,
      S1D419'Access,S1D41A'Access,S1D41B'Access,S1D41C'Access,S1D41D'Access,
      S1D41E'Access,S1D41F'Access,S1D420'Access,S1D421'Access,S1D422'Access,
      S1D423'Access,S1D424'Access,S1D425'Access,S1D426'Access,S1D427'Access,
      S1D428'Access,S1D429'Access,S1D42A'Access,S1D42B'Access,S1D42C'Access,
      S1D42D'Access,S1D42E'Access,S1D42F'Access,S1D430'Access,S1D431'Access,
      S1D432'Access,S1D433'Access,S1D434'Access,S1D435'Access,S1D436'Access,
      S1D437'Access,S1D438'Access,S1D439'Access,S1D43A'Access,S1D43B'Access,
      S1D43C'Access,S1D43D'Access,S1D43E'Access,S1D43F'Access,S1D440'Access,
      S1D441'Access,S1D442'Access,S1D443'Access,S1D444'Access,S1D445'Access,
      S1D446'Access,S1D447'Access,S1D448'Access,S1D449'Access,S1D44A'Access,
      S1D44B'Access,S1D44C'Access,S1D44D'Access,S1D44E'Access,S1D44F'Access,
      S1D450'Access,S1D451'Access,S1D452'Access,S1D453'Access,S1D454'Access,
      S1D456'Access,S1D457'Access,S1D458'Access,S1D459'Access,S1D45A'Access,
      S1D45B'Access,S1D45C'Access,S1D45D'Access,S1D45E'Access,S1D45F'Access,
      S1D460'Access,S1D461'Access,S1D462'Access,S1D463'Access,S1D464'Access,
      S1D465'Access,S1D466'Access,S1D467'Access,S1D468'Access,S1D469'Access,
      S1D46A'Access,S1D46B'Access,S1D46C'Access,S1D46D'Access,S1D46E'Access,
      S1D46F'Access,S1D470'Access,S1D471'Access,S1D472'Access,S1D473'Access,
      S1D474'Access,S1D475'Access,S1D476'Access,S1D477'Access,S1D478'Access,
      S1D479'Access,S1D47A'Access,S1D47B'Access,S1D47C'Access,S1D47D'Access,
      S1D47E'Access,S1D47F'Access,S1D480'Access,S1D481'Access,S1D482'Access,
      S1D483'Access,S1D484'Access,S1D485'Access,S1D486'Access,S1D487'Access,
      S1D488'Access,S1D489'Access,S1D48A'Access,S1D48B'Access,S1D48C'Access,
      S1D48D'Access,S1D48E'Access,S1D48F'Access,S1D490'Access,S1D491'Access,
      S1D492'Access,S1D493'Access,S1D494'Access,S1D495'Access,S1D496'Access,
      S1D497'Access,S1D498'Access,S1D499'Access,S1D49A'Access,S1D49B'Access,
      S1D49C'Access,S1D49E'Access,S1D49F'Access,S1D4A2'Access,S1D4A5'Access,
      S1D4A6'Access,S1D4A9'Access,S1D4AA'Access,S1D4AB'Access,S1D4AC'Access,
      S1D4AE'Access,S1D4AF'Access,S1D4B0'Access,S1D4B1'Access,S1D4B2'Access,
      S1D4B3'Access,S1D4B4'Access,S1D4B5'Access,S1D4B6'Access,S1D4B7'Access,
      S1D4B8'Access,S1D4B9'Access,S1D4BB'Access,S1D4BD'Access,S1D4BE'Access,
      S1D4BF'Access,S1D4C0'Access,S1D4C1'Access,S1D4C2'Access,S1D4C3'Access,
      S1D4C5'Access,S1D4C6'Access,S1D4C7'Access,S1D4C8'Access,S1D4C9'Access,
      S1D4CA'Access,S1D4CB'Access,S1D4CC'Access,S1D4CD'Access,S1D4CE'Access,
      S1D4CF'Access,S1D4D0'Access,S1D4D1'Access,S1D4D2'Access,S1D4D3'Access,
      S1D4D4'Access,S1D4D5'Access,S1D4D6'Access,S1D4D7'Access,S1D4D8'Access,
      S1D4D9'Access,S1D4DA'Access,S1D4DB'Access,S1D4DC'Access,S1D4DD'Access,
      S1D4DE'Access,S1D4DF'Access,S1D4E0'Access,S1D4E1'Access,S1D4E2'Access,
      S1D4E3'Access,S1D4E4'Access,S1D4E5'Access,S1D4E6'Access,S1D4E7'Access,
      S1D4E8'Access,S1D4E9'Access,S1D4EA'Access,S1D4EB'Access,S1D4EC'Access,
      S1D4ED'Access,S1D4EE'Access,S1D4EF'Access,S1D4F0'Access,S1D4F1'Access,
      S1D4F2'Access,S1D4F3'Access,S1D4F4'Access,S1D4F5'Access,S1D4F6'Access,
      S1D4F7'Access,S1D4F8'Access,S1D4F9'Access,S1D4FA'Access,S1D4FB'Access,
      S1D4FC'Access,S1D4FD'Access,S1D4FE'Access,S1D4FF'Access,S1D500'Access,
      S1D501'Access,S1D502'Access,S1D503'Access,S1D504'Access,S1D505'Access,
      S1D507'Access,S1D508'Access,S1D509'Access,S1D50A'Access,S1D50D'Access,
      S1D50E'Access,S1D50F'Access,S1D510'Access,S1D511'Access,S1D512'Access,
      S1D513'Access,S1D514'Access,S1D516'Access,S1D517'Access,S1D518'Access,
      S1D519'Access,S1D51A'Access,S1D51B'Access,S1D51C'Access,S1D51E'Access,
      S1D51F'Access,S1D520'Access,S1D521'Access,S1D522'Access,S1D523'Access,
      S1D524'Access,S1D525'Access,S1D526'Access,S1D527'Access,S1D528'Access,
      S1D529'Access,S1D52A'Access,S1D52B'Access,S1D52C'Access,S1D52D'Access,
      S1D52E'Access,S1D52F'Access,S1D530'Access,S1D531'Access,S1D532'Access,
      S1D533'Access,S1D534'Access,S1D535'Access,S1D536'Access,S1D537'Access,
      S1D538'Access,S1D539'Access,S1D53B'Access,S1D53C'Access,S1D53D'Access,
      S1D53E'Access,S1D540'Access,S1D541'Access,S1D542'Access,S1D543'Access,
      S1D544'Access,S1D546'Access,S1D54A'Access,S1D54B'Access,S1D54C'Access,
      S1D54D'Access,S1D54E'Access,S1D54F'Access,S1D550'Access,S1D552'Access,
      S1D553'Access,S1D554'Access,S1D555'Access,S1D556'Access,S1D557'Access,
      S1D558'Access,S1D559'Access,S1D55A'Access,S1D55B'Access,S1D55C'Access,
      S1D55D'Access,S1D55E'Access,S1D55F'Access,S1D560'Access,S1D561'Access,
      S1D562'Access,S1D563'Access,S1D564'Access,S1D565'Access,S1D566'Access,
      S1D567'Access,S1D568'Access,S1D569'Access,S1D56A'Access,S1D56B'Access,
      S1D56C'Access,S1D56D'Access,S1D56E'Access,S1D56F'Access,S1D570'Access,
      S1D571'Access,S1D572'Access,S1D573'Access,S1D574'Access,S1D575'Access,
      S1D576'Access,S1D577'Access,S1D578'Access,S1D579'Access,S1D57A'Access,
      S1D57B'Access,S1D57C'Access,S1D57D'Access,S1D57E'Access,S1D57F'Access,
      S1D580'Access,S1D581'Access,S1D582'Access,S1D583'Access,S1D584'Access,
      S1D585'Access,S1D586'Access,S1D587'Access,S1D588'Access,S1D589'Access,
      S1D58A'Access,S1D58B'Access,S1D58C'Access,S1D58D'Access,S1D58E'Access,
      S1D58F'Access,S1D590'Access,S1D591'Access,S1D592'Access,S1D593'Access,
      S1D594'Access,S1D595'Access,S1D596'Access,S1D597'Access,S1D598'Access,
      S1D599'Access,S1D59A'Access,S1D59B'Access,S1D59C'Access,S1D59D'Access,
      S1D59E'Access,S1D59F'Access,S1D5A0'Access,S1D5A1'Access,S1D5A2'Access,
      S1D5A3'Access,S1D5A4'Access,S1D5A5'Access,S1D5A6'Access,S1D5A7'Access,
      S1D5A8'Access,S1D5A9'Access,S1D5AA'Access,S1D5AB'Access,S1D5AC'Access,
      S1D5AD'Access,S1D5AE'Access,S1D5AF'Access,S1D5B0'Access,S1D5B1'Access,
      S1D5B2'Access,S1D5B3'Access,S1D5B4'Access,S1D5B5'Access,S1D5B6'Access,
      S1D5B7'Access,S1D5B8'Access,S1D5B9'Access,S1D5BA'Access,S1D5BB'Access,
      S1D5BC'Access,S1D5BD'Access,S1D5BE'Access,S1D5BF'Access,S1D5C0'Access,
      S1D5C1'Access,S1D5C2'Access,S1D5C3'Access,S1D5C4'Access,S1D5C5'Access,
      S1D5C6'Access,S1D5C7'Access,S1D5C8'Access,S1D5C9'Access,S1D5CA'Access,
      S1D5CB'Access,S1D5CC'Access,S1D5CD'Access,S1D5CE'Access,S1D5CF'Access,
      S1D5D0'Access,S1D5D1'Access,S1D5D2'Access,S1D5D3'Access,S1D5D4'Access,
      S1D5D5'Access,S1D5D6'Access,S1D5D7'Access,S1D5D8'Access,S1D5D9'Access,
      S1D5DA'Access,S1D5DB'Access,S1D5DC'Access,S1D5DD'Access,S1D5DE'Access,
      S1D5DF'Access,S1D5E0'Access,S1D5E1'Access,S1D5E2'Access,S1D5E3'Access,
      S1D5E4'Access,S1D5E5'Access,S1D5E6'Access,S1D5E7'Access,S1D5E8'Access,
      S1D5E9'Access,S1D5EA'Access,S1D5EB'Access,S1D5EC'Access,S1D5ED'Access,
      S1D5EE'Access,S1D5EF'Access,S1D5F0'Access,S1D5F1'Access,S1D5F2'Access,
      S1D5F3'Access,S1D5F4'Access,S1D5F5'Access,S1D5F6'Access,S1D5F7'Access,
      S1D5F8'Access,S1D5F9'Access,S1D5FA'Access,S1D5FB'Access,S1D5FC'Access,
      S1D5FD'Access,S1D5FE'Access,S1D5FF'Access,S1D600'Access,S1D601'Access,
      S1D602'Access,S1D603'Access,S1D604'Access,S1D605'Access,S1D606'Access,
      S1D607'Access,S1D608'Access,S1D609'Access,S1D60A'Access,S1D60B'Access,
      S1D60C'Access,S1D60D'Access,S1D60E'Access,S1D60F'Access,S1D610'Access,
      S1D611'Access,S1D612'Access,S1D613'Access,S1D614'Access,S1D615'Access,
      S1D616'Access,S1D617'Access,S1D618'Access,S1D619'Access,S1D61A'Access,
      S1D61B'Access,S1D61C'Access,S1D61D'Access,S1D61E'Access,S1D61F'Access,
      S1D620'Access,S1D621'Access,S1D622'Access,S1D623'Access,S1D624'Access,
      S1D625'Access,S1D626'Access,S1D627'Access,S1D628'Access,S1D629'Access,
      S1D62A'Access,S1D62B'Access,S1D62C'Access,S1D62D'Access,S1D62E'Access,
      S1D62F'Access,S1D630'Access,S1D631'Access,S1D632'Access,S1D633'Access,
      S1D634'Access,S1D635'Access,S1D636'Access,S1D637'Access,S1D638'Access,
      S1D639'Access,S1D63A'Access,S1D63B'Access,S1D63C'Access,S1D63D'Access,
      S1D63E'Access,S1D63F'Access,S1D640'Access,S1D641'Access,S1D642'Access,
      S1D643'Access,S1D644'Access,S1D645'Access,S1D646'Access,S1D647'Access,
      S1D648'Access,S1D649'Access,S1D64A'Access,S1D64B'Access,S1D64C'Access,
      S1D64D'Access,S1D64E'Access,S1D64F'Access,S1D650'Access,S1D651'Access,
      S1D652'Access,S1D653'Access,S1D654'Access,S1D655'Access,S1D656'Access,
      S1D657'Access,S1D658'Access,S1D659'Access,S1D65A'Access,S1D65B'Access,
      S1D65C'Access,S1D65D'Access,S1D65E'Access,S1D65F'Access,S1D660'Access,
      S1D661'Access,S1D662'Access,S1D663'Access,S1D664'Access,S1D665'Access,
      S1D666'Access,S1D667'Access,S1D668'Access,S1D669'Access,S1D66A'Access,
      S1D66B'Access,S1D66C'Access,S1D66D'Access,S1D66E'Access,S1D66F'Access,
      S1D670'Access,S1D671'Access,S1D672'Access,S1D673'Access,S1D674'Access,
      S1D675'Access,S1D676'Access,S1D677'Access,S1D678'Access,S1D679'Access,
      S1D67A'Access,S1D67B'Access,S1D67C'Access,S1D67D'Access,S1D67E'Access,
      S1D67F'Access,S1D680'Access,S1D681'Access,S1D682'Access,S1D683'Access,
      S1D684'Access,S1D685'Access,S1D686'Access,S1D687'Access,S1D688'Access,
      S1D689'Access,S1D68A'Access,S1D68B'Access,S1D68C'Access,S1D68D'Access,
      S1D68E'Access,S1D68F'Access,S1D690'Access,S1D691'Access,S1D692'Access,
      S1D693'Access,S1D694'Access,S1D695'Access,S1D696'Access,S1D697'Access,
      S1D698'Access,S1D699'Access,S1D69A'Access,S1D69B'Access,S1D69C'Access,
      S1D69D'Access,S1D69E'Access,S1D69F'Access,S1D6A0'Access,S1D6A1'Access,
      S1D6A2'Access,S1D6A3'Access,S1D6A4'Access,S1D6A5'Access,S1D6A8'Access,
      S1D6A9'Access,S1D6AA'Access,S1D6AB'Access,S1D6AC'Access,S1D6AD'Access,
      S1D6AE'Access,S1D6AF'Access,S1D6B0'Access,S1D6B1'Access,S1D6B2'Access,
      S1D6B3'Access,S1D6B4'Access,S1D6B5'Access,S1D6B6'Access,S1D6B7'Access,
      S1D6B8'Access,S1D6B9'Access,S1D6BA'Access,S1D6BB'Access,S1D6BC'Access,
      S1D6BD'Access,S1D6BE'Access,S1D6BF'Access,S1D6C0'Access,S1D6C1'Access,
      S1D6C2'Access,S1D6C3'Access,S1D6C4'Access,S1D6C5'Access,S1D6C6'Access,
      S1D6C7'Access,S1D6C8'Access,S1D6C9'Access,S1D6CA'Access,S1D6CB'Access,
      S1D6CC'Access,S1D6CD'Access,S1D6CE'Access,S1D6CF'Access,S1D6D0'Access,
      S1D6D1'Access,S1D6D2'Access,S1D6D3'Access,S1D6D4'Access,S1D6D5'Access,
      S1D6D6'Access,S1D6D7'Access,S1D6D8'Access,S1D6D9'Access,S1D6DA'Access,
      S1D6DB'Access,S1D6DC'Access,S1D6DD'Access,S1D6DE'Access,S1D6DF'Access,
      S1D6E0'Access,S1D6E1'Access,S1D6E2'Access,S1D6E3'Access,S1D6E4'Access,
      S1D6E5'Access,S1D6E6'Access,S1D6E7'Access,S1D6E8'Access,S1D6E9'Access,
      S1D6EA'Access,S1D6EB'Access,S1D6EC'Access,S1D6ED'Access,S1D6EE'Access,
      S1D6EF'Access,S1D6F0'Access,S1D6F1'Access,S1D6F2'Access,S1D6F3'Access,
      S1D6F4'Access,S1D6F5'Access,S1D6F6'Access,S1D6F7'Access,S1D6F8'Access,
      S1D6F9'Access,S1D6FA'Access,S1D6FB'Access,S1D6FC'Access,S1D6FD'Access,
      S1D6FE'Access,S1D6FF'Access,S1D700'Access,S1D701'Access,S1D702'Access,
      S1D703'Access,S1D704'Access,S1D705'Access,S1D706'Access,S1D707'Access,
      S1D708'Access,S1D709'Access,S1D70A'Access,S1D70B'Access,S1D70C'Access,
      S1D70D'Access,S1D70E'Access,S1D70F'Access,S1D710'Access,S1D711'Access,
      S1D712'Access,S1D713'Access,S1D714'Access,S1D715'Access,S1D716'Access,
      S1D717'Access,S1D718'Access,S1D719'Access,S1D71A'Access,S1D71B'Access,
      S1D71C'Access,S1D71D'Access,S1D71E'Access,S1D71F'Access,S1D720'Access,
      S1D721'Access,S1D722'Access,S1D723'Access,S1D724'Access,S1D725'Access,
      S1D726'Access,S1D727'Access,S1D728'Access,S1D729'Access,S1D72A'Access,
      S1D72B'Access,S1D72C'Access,S1D72D'Access,S1D72E'Access,S1D72F'Access,
      S1D730'Access,S1D731'Access,S1D732'Access,S1D733'Access,S1D734'Access,
      S1D735'Access,S1D736'Access,S1D737'Access,S1D738'Access,S1D739'Access,
      S1D73A'Access,S1D73B'Access,S1D73C'Access,S1D73D'Access,S1D73E'Access,
      S1D73F'Access,S1D740'Access,S1D741'Access,S1D742'Access,S1D743'Access,
      S1D744'Access,S1D745'Access,S1D746'Access,S1D747'Access,S1D748'Access,
      S1D749'Access,S1D74A'Access,S1D74B'Access,S1D74C'Access,S1D74D'Access,
      S1D74E'Access,S1D74F'Access,S1D750'Access,S1D751'Access,S1D752'Access,
      S1D753'Access,S1D754'Access,S1D755'Access,S1D756'Access,S1D757'Access,
      S1D758'Access,S1D759'Access,S1D75A'Access,S1D75B'Access,S1D75C'Access,
      S1D75D'Access,S1D75E'Access,S1D75F'Access,S1D760'Access,S1D761'Access,
      S1D762'Access,S1D763'Access,S1D764'Access,S1D765'Access,S1D766'Access,
      S1D767'Access,S1D768'Access,S1D769'Access,S1D76A'Access,S1D76B'Access,
      S1D76C'Access,S1D76D'Access,S1D76E'Access,S1D76F'Access,S1D770'Access,
      S1D771'Access,S1D772'Access,S1D773'Access,S1D774'Access,S1D775'Access,
      S1D776'Access,S1D777'Access,S1D778'Access,S1D779'Access,S1D77A'Access,
      S1D77B'Access,S1D77C'Access,S1D77D'Access,S1D77E'Access,S1D77F'Access,
      S1D780'Access,S1D781'Access,S1D782'Access,S1D783'Access,S1D784'Access,
      S1D785'Access,S1D786'Access,S1D787'Access,S1D788'Access,S1D789'Access,
      S1D78A'Access,S1D78B'Access,S1D78C'Access,S1D78D'Access,S1D78E'Access,
      S1D78F'Access,S1D790'Access,S1D791'Access,S1D792'Access,S1D793'Access,
      S1D794'Access,S1D795'Access,S1D796'Access,S1D797'Access,S1D798'Access,
      S1D799'Access,S1D79A'Access,S1D79B'Access,S1D79C'Access,S1D79D'Access,
      S1D79E'Access,S1D79F'Access,S1D7A0'Access,S1D7A1'Access,S1D7A2'Access,
      S1D7A3'Access,S1D7A4'Access,S1D7A5'Access,S1D7A6'Access,S1D7A7'Access,
      S1D7A8'Access,S1D7A9'Access,S1D7AA'Access,S1D7AB'Access,S1D7AC'Access,
      S1D7AD'Access,S1D7AE'Access,S1D7AF'Access,S1D7B0'Access,S1D7B1'Access,
      S1D7B2'Access,S1D7B3'Access,S1D7B4'Access,S1D7B5'Access,S1D7B6'Access,
      S1D7B7'Access,S1D7B8'Access,S1D7B9'Access,S1D7BA'Access,S1D7BB'Access,
      S1D7BC'Access,S1D7BD'Access,S1D7BE'Access,S1D7BF'Access,S1D7C0'Access,
      S1D7C1'Access,S1D7C2'Access,S1D7C3'Access,S1D7C4'Access,S1D7C5'Access,
      S1D7C6'Access,S1D7C7'Access,S1D7C8'Access,S1D7C9'Access,S1D7CA'Access,
      S1D7CB'Access,S1D7CE'Access,S1D7CF'Access,S1D7D0'Access,S1D7D1'Access,
      S1D7D2'Access,S1D7D3'Access,S1D7D4'Access,S1D7D5'Access,S1D7D6'Access,
      S1D7D7'Access,S1D7D8'Access,S1D7D9'Access,S1D7DA'Access,S1D7DB'Access,
      S1D7DC'Access,S1D7DD'Access,S1D7DE'Access,S1D7DF'Access,S1D7E0'Access,
      S1D7E1'Access,S1D7E2'Access,S1D7E3'Access,S1D7E4'Access,S1D7E5'Access,
      S1D7E6'Access,S1D7E7'Access,S1D7E8'Access,S1D7E9'Access,S1D7EA'Access,
      S1D7EB'Access,S1D7EC'Access,S1D7ED'Access,S1D7EE'Access,S1D7EF'Access,
      S1D7F0'Access,S1D7F1'Access,S1D7F2'Access,S1D7F3'Access,S1D7F4'Access,
      S1D7F5'Access,S1D7F6'Access,S1D7F7'Access,S1D7F8'Access,S1D7F9'Access,
      S1D7FA'Access,S1D7FB'Access,S1D7FC'Access,S1D7FD'Access,S1D7FE'Access,
      S1D7FF'Access,S1E030'Access,S1E031'Access,S1E032'Access,S1E033'Access,
      S1E034'Access,S1E035'Access,S1E036'Access,S1E037'Access,S1E038'Access,
      S1E039'Access,S1E03A'Access,S1E03B'Access,S1E03C'Access,S1E03D'Access,
      S1E03E'Access,S1E03F'Access,S1E040'Access,S1E041'Access,S1E042'Access,
      S1E043'Access,S1E044'Access,S1E045'Access,S1E046'Access,S1E047'Access,
      S1E048'Access,S1E049'Access,S1E04A'Access,S1E04B'Access,S1E04C'Access,
      S1E04D'Access,S1E04E'Access,S1E04F'Access,S1E050'Access,S1E051'Access,
      S1E052'Access,S1E053'Access,S1E054'Access,S1E055'Access,S1E056'Access,
      S1E057'Access,S1E058'Access,S1E059'Access,S1E05A'Access,S1E05B'Access,
      S1E05C'Access,S1E05D'Access,S1E05E'Access,S1E05F'Access,S1E060'Access,
      S1E061'Access,S1E062'Access,S1E063'Access,S1E064'Access,S1E065'Access,
      S1E066'Access,S1E067'Access,S1E068'Access,S1E069'Access,S1E06A'Access,
      S1E06B'Access,S1E06C'Access,S1E06D'Access,S1EE00'Access,S1EE01'Access,
      S1EE02'Access,S1EE03'Access,S1EE05'Access,S1EE06'Access,S1EE07'Access,
      S1EE08'Access,S1EE09'Access,S1EE0A'Access,S1EE0B'Access,S1EE0C'Access,
      S1EE0D'Access,S1EE0E'Access,S1EE0F'Access,S1EE10'Access,S1EE11'Access,
      S1EE12'Access,S1EE13'Access,S1EE14'Access,S1EE15'Access,S1EE16'Access,
      S1EE17'Access,S1EE18'Access,S1EE19'Access,S1EE1A'Access,S1EE1B'Access,
      S1EE1C'Access,S1EE1D'Access,S1EE1E'Access,S1EE1F'Access,S1EE21'Access,
      S1EE22'Access,S1EE24'Access,S1EE27'Access,S1EE29'Access,S1EE2A'Access,
      S1EE2B'Access,S1EE2C'Access,S1EE2D'Access,S1EE2E'Access,S1EE2F'Access,
      S1EE30'Access,S1EE31'Access,S1EE32'Access,S1EE34'Access,S1EE35'Access,
      S1EE36'Access,S1EE37'Access,S1EE39'Access,S1EE3B'Access,S1EE42'Access,
      S1EE47'Access,S1EE49'Access,S1EE4B'Access,S1EE4D'Access,S1EE4E'Access,
      S1EE4F'Access,S1EE51'Access,S1EE52'Access,S1EE54'Access,S1EE57'Access,
      S1EE59'Access,S1EE5B'Access,S1EE5D'Access,S1EE5F'Access,S1EE61'Access,
      S1EE62'Access,S1EE64'Access,S1EE67'Access,S1EE68'Access,S1EE69'Access,
      S1EE6A'Access,S1EE6C'Access,S1EE6D'Access,S1EE6E'Access,S1EE6F'Access,
      S1EE70'Access,S1EE71'Access,S1EE72'Access,S1EE74'Access,S1EE75'Access,
      S1EE76'Access,S1EE77'Access,S1EE79'Access,S1EE7A'Access,S1EE7B'Access,
      S1EE7C'Access,S1EE7E'Access,S1EE80'Access,S1EE81'Access,S1EE82'Access,
      S1EE83'Access,S1EE84'Access,S1EE85'Access,S1EE86'Access,S1EE87'Access,
      S1EE88'Access,S1EE89'Access,S1EE8B'Access,S1EE8C'Access,S1EE8D'Access,
      S1EE8E'Access,S1EE8F'Access,S1EE90'Access,S1EE91'Access,S1EE92'Access,
      S1EE93'Access,S1EE94'Access,S1EE95'Access,S1EE96'Access,S1EE97'Access,
      S1EE98'Access,S1EE99'Access,S1EE9A'Access,S1EE9B'Access,S1EEA1'Access,
      S1EEA2'Access,S1EEA3'Access,S1EEA5'Access,S1EEA6'Access,S1EEA7'Access,
      S1EEA8'Access,S1EEA9'Access,S1EEAB'Access,S1EEAC'Access,S1EEAD'Access,
      S1EEAE'Access,S1EEAF'Access,S1EEB0'Access,S1EEB1'Access,S1EEB2'Access,
      S1EEB3'Access,S1EEB4'Access,S1EEB5'Access,S1EEB6'Access,S1EEB7'Access,
      S1EEB8'Access,S1EEB9'Access,S1EEBA'Access,S1EEBB'Access,S1F100'Access,
      S1F101'Access,S1F102'Access,S1F103'Access,S1F104'Access,S1F105'Access,
      S1F106'Access,S1F107'Access,S1F108'Access,S1F109'Access,S1F10A'Access,
      S1F110'Access,S1F111'Access,S1F112'Access,S1F113'Access,S1F114'Access,
      S1F115'Access,S1F116'Access,S1F117'Access,S1F118'Access,S1F119'Access,
      S1F11A'Access,S1F11B'Access,S1F11C'Access,S1F11D'Access,S1F11E'Access,
      S1F11F'Access,S1F120'Access,S1F121'Access,S1F122'Access,S1F123'Access,
      S1F124'Access,S1F125'Access,S1F126'Access,S1F127'Access,S1F128'Access,
      S1F129'Access,S1F12A'Access,S1F12B'Access,S1F12C'Access,S1F12D'Access,
      S1F12E'Access,S1F130'Access,S1F131'Access,S1F132'Access,S1F133'Access,
      S1F134'Access,S1F135'Access,S1F136'Access,S1F137'Access,S1F138'Access,
      S1F139'Access,S1F13A'Access,S1F13B'Access,S1F13C'Access,S1F13D'Access,
      S1F13E'Access,S1F13F'Access,S1F140'Access,S1F141'Access,S1F142'Access,
      S1F143'Access,S1F144'Access,S1F145'Access,S1F146'Access,S1F147'Access,
      S1F148'Access,S1F149'Access,S1F14A'Access,S1F14B'Access,S1F14C'Access,
      S1F14D'Access,S1F14E'Access,S1F14F'Access,S1F16A'Access,S1F16B'Access,
      S1F16C'Access,S1F190'Access,S1F200'Access,S1F201'Access,S1F202'Access,
      S1F210'Access,S1F211'Access,S1F212'Access,S1F213'Access,S1F214'Access,
      S1F215'Access,S1F216'Access,S1F217'Access,S1F218'Access,S1F219'Access,
      S1F21A'Access,S1F21B'Access,S1F21C'Access,S1F21D'Access,S1F21E'Access,
      S1F21F'Access,S1F220'Access,S1F221'Access,S1F222'Access,S1F223'Access,
      S1F224'Access,S1F225'Access,S1F226'Access,S1F227'Access,S1F228'Access,
      S1F229'Access,S1F22A'Access,S1F22B'Access,S1F22C'Access,S1F22D'Access,
      S1F22E'Access,S1F22F'Access,S1F230'Access,S1F231'Access,S1F232'Access,
      S1F233'Access,S1F234'Access,S1F235'Access,S1F236'Access,S1F237'Access,
      S1F238'Access,S1F239'Access,S1F23A'Access,S1F23B'Access,S1F240'Access,
      S1F241'Access,S1F242'Access,S1F243'Access,S1F244'Access,S1F245'Access,
      S1F246'Access,S1F247'Access,S1F248'Access,S1F250'Access,S1F251'Access,
      S1FBF0'Access,S1FBF1'Access,S1FBF2'Access,S1FBF3'Access,S1FBF4'Access,
      S1FBF5'Access,S1FBF6'Access,S1FBF7'Access,S1FBF8'Access,S1FBF9'Access,
      S2F800'Access,S2F801'Access,S2F802'Access,S2F803'Access,S2F804'Access,
      S2F805'Access,S2F806'Access,S2F807'Access,S2F808'Access,S2F809'Access,
      S2F80A'Access,S2F80B'Access,S2F80C'Access,S2F80D'Access,S2F80E'Access,
      S2F80F'Access,S2F810'Access,S2F811'Access,S2F812'Access,S2F813'Access,
      S2F814'Access,S2F815'Access,S2F816'Access,S2F817'Access,S2F818'Access,
      S2F819'Access,S2F81A'Access,S2F81B'Access,S2F81C'Access,S2F81D'Access,
      S2F81E'Access,S2F81F'Access,S2F820'Access,S2F821'Access,S2F822'Access,
      S2F823'Access,S2F824'Access,S2F825'Access,S2F826'Access,S2F827'Access,
      S2F828'Access,S2F829'Access,S2F82A'Access,S2F82B'Access,S2F82C'Access,
      S2F82D'Access,S2F82E'Access,S2F82F'Access,S2F830'Access,S2F831'Access,
      S2F832'Access,S2F833'Access,S2F834'Access,S2F835'Access,S2F836'Access,
      S2F837'Access,S2F838'Access,S2F839'Access,S2F83A'Access,S2F83B'Access,
      S2F83C'Access,S2F83D'Access,S2F83E'Access,S2F83F'Access,S2F840'Access,
      S2F841'Access,S2F842'Access,S2F843'Access,S2F844'Access,S2F845'Access,
      S2F846'Access,S2F847'Access,S2F848'Access,S2F849'Access,S2F84A'Access,
      S2F84B'Access,S2F84C'Access,S2F84D'Access,S2F84E'Access,S2F84F'Access,
      S2F850'Access,S2F851'Access,S2F852'Access,S2F853'Access,S2F854'Access,
      S2F855'Access,S2F856'Access,S2F857'Access,S2F858'Access,S2F859'Access,
      S2F85A'Access,S2F85B'Access,S2F85C'Access,S2F85D'Access,S2F85E'Access,
      S2F85F'Access,S2F860'Access,S2F861'Access,S2F862'Access,S2F863'Access,
      S2F864'Access,S2F865'Access,S2F866'Access,S2F867'Access,S2F868'Access,
      S2F869'Access,S2F86A'Access,S2F86B'Access,S2F86C'Access,S2F86D'Access,
      S2F86E'Access,S2F86F'Access,S2F870'Access,S2F871'Access,S2F872'Access,
      S2F873'Access,S2F874'Access,S2F875'Access,S2F876'Access,S2F877'Access,
      S2F878'Access,S2F879'Access,S2F87A'Access,S2F87B'Access,S2F87C'Access,
      S2F87D'Access,S2F87E'Access,S2F87F'Access,S2F880'Access,S2F881'Access,
      S2F882'Access,S2F883'Access,S2F884'Access,S2F885'Access,S2F886'Access,
      S2F887'Access,S2F888'Access,S2F889'Access,S2F88A'Access,S2F88B'Access,
      S2F88C'Access,S2F88D'Access,S2F88E'Access,S2F88F'Access,S2F890'Access,
      S2F891'Access,S2F892'Access,S2F893'Access,S2F894'Access,S2F895'Access,
      S2F896'Access,S2F897'Access,S2F898'Access,S2F899'Access,S2F89A'Access,
      S2F89B'Access,S2F89C'Access,S2F89D'Access,S2F89E'Access,S2F89F'Access,
      S2F8A0'Access,S2F8A1'Access,S2F8A2'Access,S2F8A3'Access,S2F8A4'Access,
      S2F8A5'Access,S2F8A6'Access,S2F8A7'Access,S2F8A8'Access,S2F8A9'Access,
      S2F8AA'Access,S2F8AB'Access,S2F8AC'Access,S2F8AD'Access,S2F8AE'Access,
      S2F8AF'Access,S2F8B0'Access,S2F8B1'Access,S2F8B2'Access,S2F8B3'Access,
      S2F8B4'Access,S2F8B5'Access,S2F8B6'Access,S2F8B7'Access,S2F8B8'Access,
      S2F8B9'Access,S2F8BA'Access,S2F8BB'Access,S2F8BC'Access,S2F8BD'Access,
      S2F8BE'Access,S2F8BF'Access,S2F8C0'Access,S2F8C1'Access,S2F8C2'Access,
      S2F8C3'Access,S2F8C4'Access,S2F8C5'Access,S2F8C6'Access,S2F8C7'Access,
      S2F8C8'Access,S2F8C9'Access,S2F8CA'Access,S2F8CB'Access,S2F8CC'Access,
      S2F8CD'Access,S2F8CE'Access,S2F8CF'Access,S2F8D0'Access,S2F8D1'Access,
      S2F8D2'Access,S2F8D3'Access,S2F8D4'Access,S2F8D5'Access,S2F8D6'Access,
      S2F8D7'Access,S2F8D8'Access,S2F8D9'Access,S2F8DA'Access,S2F8DB'Access,
      S2F8DC'Access,S2F8DD'Access,S2F8DE'Access,S2F8DF'Access,S2F8E0'Access,
      S2F8E1'Access,S2F8E2'Access,S2F8E3'Access,S2F8E4'Access,S2F8E5'Access,
      S2F8E6'Access,S2F8E7'Access,S2F8E8'Access,S2F8E9'Access,S2F8EA'Access,
      S2F8EB'Access,S2F8EC'Access,S2F8ED'Access,S2F8EE'Access,S2F8EF'Access,
      S2F8F0'Access,S2F8F1'Access,S2F8F2'Access,S2F8F3'Access,S2F8F4'Access,
      S2F8F5'Access,S2F8F6'Access,S2F8F7'Access,S2F8F8'Access,S2F8F9'Access,
      S2F8FA'Access,S2F8FB'Access,S2F8FC'Access,S2F8FD'Access,S2F8FE'Access,
      S2F8FF'Access,S2F900'Access,S2F901'Access,S2F902'Access,S2F903'Access,
      S2F904'Access,S2F905'Access,S2F906'Access,S2F907'Access,S2F908'Access,
      S2F909'Access,S2F90A'Access,S2F90B'Access,S2F90C'Access,S2F90D'Access,
      S2F90E'Access,S2F90F'Access,S2F910'Access,S2F911'Access,S2F912'Access,
      S2F913'Access,S2F914'Access,S2F915'Access,S2F916'Access,S2F917'Access,
      S2F918'Access,S2F919'Access,S2F91A'Access,S2F91B'Access,S2F91C'Access,
      S2F91D'Access,S2F91E'Access,S2F91F'Access,S2F920'Access,S2F921'Access,
      S2F922'Access,S2F923'Access,S2F924'Access,S2F925'Access,S2F926'Access,
      S2F927'Access,S2F928'Access,S2F929'Access,S2F92A'Access,S2F92B'Access,
      S2F92C'Access,S2F92D'Access,S2F92E'Access,S2F92F'Access,S2F930'Access,
      S2F931'Access,S2F932'Access,S2F933'Access,S2F934'Access,S2F935'Access,
      S2F936'Access,S2F937'Access,S2F938'Access,S2F939'Access,S2F93A'Access,
      S2F93B'Access,S2F93C'Access,S2F93D'Access,S2F93E'Access,S2F93F'Access,
      S2F940'Access,S2F941'Access,S2F942'Access,S2F943'Access,S2F944'Access,
      S2F945'Access,S2F946'Access,S2F947'Access,S2F948'Access,S2F949'Access,
      S2F94A'Access,S2F94B'Access,S2F94C'Access,S2F94D'Access,S2F94E'Access,
      S2F94F'Access,S2F950'Access,S2F951'Access,S2F952'Access,S2F953'Access,
      S2F954'Access,S2F955'Access,S2F956'Access,S2F957'Access,S2F958'Access,
      S2F959'Access,S2F95A'Access,S2F95B'Access,S2F95C'Access,S2F95D'Access,
      S2F95E'Access,S2F95F'Access,S2F960'Access,S2F961'Access,S2F962'Access,
      S2F963'Access,S2F964'Access,S2F965'Access,S2F966'Access,S2F967'Access,
      S2F968'Access,S2F969'Access,S2F96A'Access,S2F96B'Access,S2F96C'Access,
      S2F96D'Access,S2F96E'Access,S2F96F'Access,S2F970'Access,S2F971'Access,
      S2F972'Access,S2F973'Access,S2F974'Access,S2F975'Access,S2F976'Access,
      S2F977'Access,S2F978'Access,S2F979'Access,S2F97A'Access,S2F97B'Access,
      S2F97C'Access,S2F97D'Access,S2F97E'Access,S2F97F'Access,S2F980'Access,
      S2F981'Access,S2F982'Access,S2F983'Access,S2F984'Access,S2F985'Access,
      S2F986'Access,S2F987'Access,S2F988'Access,S2F989'Access,S2F98A'Access,
      S2F98B'Access,S2F98C'Access,S2F98D'Access,S2F98E'Access,S2F98F'Access,
      S2F990'Access,S2F991'Access,S2F992'Access,S2F993'Access,S2F994'Access,
      S2F995'Access,S2F996'Access,S2F997'Access,S2F998'Access,S2F999'Access,
      S2F99A'Access,S2F99B'Access,S2F99C'Access,S2F99D'Access,S2F99E'Access,
      S2F99F'Access,S2F9A0'Access,S2F9A1'Access,S2F9A2'Access,S2F9A3'Access,
      S2F9A4'Access,S2F9A5'Access,S2F9A6'Access,S2F9A7'Access,S2F9A8'Access,
      S2F9A9'Access,S2F9AA'Access,S2F9AB'Access,S2F9AC'Access,S2F9AD'Access,
      S2F9AE'Access,S2F9AF'Access,S2F9B0'Access,S2F9B1'Access,S2F9B2'Access,
      S2F9B3'Access,S2F9B4'Access,S2F9B5'Access,S2F9B6'Access,S2F9B7'Access,
      S2F9B8'Access,S2F9B9'Access,S2F9BA'Access,S2F9BB'Access,S2F9BC'Access,
      S2F9BD'Access,S2F9BE'Access,S2F9BF'Access,S2F9C0'Access,S2F9C1'Access,
      S2F9C2'Access,S2F9C3'Access,S2F9C4'Access,S2F9C5'Access,S2F9C6'Access,
      S2F9C7'Access,S2F9C8'Access,S2F9C9'Access,S2F9CA'Access,S2F9CB'Access,
      S2F9CC'Access,S2F9CD'Access,S2F9CE'Access,S2F9CF'Access,S2F9D0'Access,
      S2F9D1'Access,S2F9D2'Access,S2F9D3'Access,S2F9D4'Access,S2F9D5'Access,
      S2F9D6'Access,S2F9D7'Access,S2F9D8'Access,S2F9D9'Access,S2F9DA'Access,
      S2F9DB'Access,S2F9DC'Access,S2F9DD'Access,S2F9DE'Access,S2F9DF'Access,
      S2F9E0'Access,S2F9E1'Access,S2F9E2'Access,S2F9E3'Access,S2F9E4'Access,
      S2F9E5'Access,S2F9E6'Access,S2F9E7'Access,S2F9E8'Access,S2F9E9'Access,
      S2F9EA'Access,S2F9EB'Access,S2F9EC'Access,S2F9ED'Access,S2F9EE'Access,
      S2F9EF'Access,S2F9F0'Access,S2F9F1'Access,S2F9F2'Access,S2F9F3'Access,
      S2F9F4'Access,S2F9F5'Access,S2F9F6'Access,S2F9F7'Access,S2F9F8'Access,
      S2F9F9'Access,S2F9FA'Access,S2F9FB'Access,S2F9FC'Access,S2F9FD'Access,
      S2F9FE'Access,S2F9FF'Access,S2FA00'Access,S2FA01'Access,S2FA02'Access,
      S2FA03'Access,S2FA04'Access,S2FA05'Access,S2FA06'Access,S2FA07'Access,
      S2FA08'Access,S2FA09'Access,S2FA0A'Access,S2FA0B'Access,S2FA0C'Access,
      S2FA0D'Access,S2FA0E'Access,S2FA0F'Access,S2FA10'Access,S2FA11'Access,
      S2FA12'Access,S2FA13'Access,S2FA14'Access,S2FA15'Access,S2FA16'Access,
      S2FA17'Access,S2FA18'Access,S2FA19'Access,S2FA1A'Access,S2FA1B'Access,
      S2FA1C'Access,S2FA1D'Access
   );
   type Class_Index is range 1..922;
   type Pair is record
      Code  : UTF8_Code_Point;
      Class : Canonical_Combining_Class;
   end record;
   type Class_Array is array (Class_Index) of Pair;
   Classes : constant Class_Array :=
   (  (16#300#,230),(16#301#,230),(16#302#,230),(16#303#,230),(16#304#,230),
      (16#305#,230),(16#306#,230),(16#307#,230),(16#308#,230),(16#309#,230),
      (16#30A#,230),(16#30B#,230),(16#30C#,230),(16#30D#,230),(16#30E#,230),
      (16#30F#,230),(16#310#,230),(16#311#,230),(16#312#,230),(16#313#,230),
      (16#314#,230),(16#315#,232),(16#316#,220),(16#317#,220),(16#318#,220),
      (16#319#,220),(16#31A#,232),(16#31B#,216),(16#31C#,220),(16#31D#,220),
      (16#31E#,220),(16#31F#,220),(16#320#,220),(16#321#,202),(16#322#,202),
      (16#323#,220),(16#324#,220),(16#325#,220),(16#326#,220),(16#327#,202),
      (16#328#,202),(16#329#,220),(16#32A#,220),(16#32B#,220),(16#32C#,220),
      (16#32D#,220),(16#32E#,220),(16#32F#,220),(16#330#,220),(16#331#,220),
      (16#332#,220),(16#333#,220),(16#334#,1),(16#335#,1),(16#336#,1),
      (16#337#,1),(16#338#,1),(16#339#,220),(16#33A#,220),(16#33B#,220),
      (16#33C#,220),(16#33D#,230),(16#33E#,230),(16#33F#,230),(16#340#,230),
      (16#341#,230),(16#342#,230),(16#343#,230),(16#344#,230),(16#345#,240),
      (16#346#,230),(16#347#,220),(16#348#,220),(16#349#,220),(16#34A#,230),
      (16#34B#,230),(16#34C#,230),(16#34D#,220),(16#34E#,220),(16#350#,230),
      (16#351#,230),(16#352#,230),(16#353#,220),(16#354#,220),(16#355#,220),
      (16#356#,220),(16#357#,230),(16#358#,232),(16#359#,220),(16#35A#,220),
      (16#35B#,230),(16#35C#,233),(16#35D#,234),(16#35E#,234),(16#35F#,233),
      (16#360#,234),(16#361#,234),(16#362#,233),(16#363#,230),(16#364#,230),
      (16#365#,230),(16#366#,230),(16#367#,230),(16#368#,230),(16#369#,230),
      (16#36A#,230),(16#36B#,230),(16#36C#,230),(16#36D#,230),(16#36E#,230),
      (16#36F#,230),(16#483#,230),(16#484#,230),(16#485#,230),(16#486#,230),
      (16#487#,230),(16#591#,220),(16#592#,230),(16#593#,230),(16#594#,230),
      (16#595#,230),(16#596#,220),(16#597#,230),(16#598#,230),(16#599#,230),
      (16#59A#,222),(16#59B#,220),(16#59C#,230),(16#59D#,230),(16#59E#,230),
      (16#59F#,230),(16#5A0#,230),(16#5A1#,230),(16#5A2#,220),(16#5A3#,220),
      (16#5A4#,220),(16#5A5#,220),(16#5A6#,220),(16#5A7#,220),(16#5A8#,230),
      (16#5A9#,230),(16#5AA#,220),(16#5AB#,230),(16#5AC#,230),(16#5AD#,222),
      (16#5AE#,228),(16#5AF#,230),(16#5B0#,10),(16#5B1#,11),(16#5B2#,12),
      (16#5B3#,13),(16#5B4#,14),(16#5B5#,15),(16#5B6#,16),(16#5B7#,17),
      (16#5B8#,18),(16#5B9#,19),(16#5BA#,19),(16#5BB#,20),(16#5BC#,21),
      (16#5BD#,22),(16#5BF#,23),(16#5C1#,24),(16#5C2#,25),(16#5C4#,230),
      (16#5C5#,220),(16#5C7#,18),(16#610#,230),(16#611#,230),(16#612#,230),
      (16#613#,230),(16#614#,230),(16#615#,230),(16#616#,230),(16#617#,230),
      (16#618#,30),(16#619#,31),(16#61A#,32),(16#64B#,27),(16#64C#,28),
      (16#64D#,29),(16#64E#,30),(16#64F#,31),(16#650#,32),(16#651#,33),
      (16#652#,34),(16#653#,230),(16#654#,230),(16#655#,220),(16#656#,220),
      (16#657#,230),(16#658#,230),(16#659#,230),(16#65A#,230),(16#65B#,230),
      (16#65C#,220),(16#65D#,230),(16#65E#,230),(16#65F#,220),(16#670#,35),
      (16#6D6#,230),(16#6D7#,230),(16#6D8#,230),(16#6D9#,230),(16#6DA#,230),
      (16#6DB#,230),(16#6DC#,230),(16#6DF#,230),(16#6E0#,230),(16#6E1#,230),
      (16#6E2#,230),(16#6E3#,220),(16#6E4#,230),(16#6E7#,230),(16#6E8#,230),
      (16#6EA#,220),(16#6EB#,230),(16#6EC#,230),(16#6ED#,220),(16#711#,36),
      (16#730#,230),(16#731#,220),(16#732#,230),(16#733#,230),(16#734#,220),
      (16#735#,230),(16#736#,230),(16#737#,220),(16#738#,220),(16#739#,220),
      (16#73A#,230),(16#73B#,220),(16#73C#,220),(16#73D#,230),(16#73E#,220),
      (16#73F#,230),(16#740#,230),(16#741#,230),(16#742#,220),(16#743#,230),
      (16#744#,220),(16#745#,230),(16#746#,220),(16#747#,230),(16#748#,220),
      (16#749#,230),(16#74A#,230),(16#7EB#,230),(16#7EC#,230),(16#7ED#,230),
      (16#7EE#,230),(16#7EF#,230),(16#7F0#,230),(16#7F1#,230),(16#7F2#,220),
      (16#7F3#,230),(16#7FD#,220),(16#816#,230),(16#817#,230),(16#818#,230),
      (16#819#,230),(16#81B#,230),(16#81C#,230),(16#81D#,230),(16#81E#,230),
      (16#81F#,230),(16#820#,230),(16#821#,230),(16#822#,230),(16#823#,230),
      (16#825#,230),(16#826#,230),(16#827#,230),(16#829#,230),(16#82A#,230),
      (16#82B#,230),(16#82C#,230),(16#82D#,230),(16#859#,220),(16#85A#,220),
      (16#85B#,220),(16#898#,230),(16#899#,220),(16#89A#,220),(16#89B#,220),
      (16#89C#,230),(16#89D#,230),(16#89E#,230),(16#89F#,230),(16#8CA#,230),
      (16#8CB#,230),(16#8CC#,230),(16#8CD#,230),(16#8CE#,230),(16#8CF#,220),
      (16#8D0#,220),(16#8D1#,220),(16#8D2#,220),(16#8D3#,220),(16#8D4#,230),
      (16#8D5#,230),(16#8D6#,230),(16#8D7#,230),(16#8D8#,230),(16#8D9#,230),
      (16#8DA#,230),(16#8DB#,230),(16#8DC#,230),(16#8DD#,230),(16#8DE#,230),
      (16#8DF#,230),(16#8E0#,230),(16#8E1#,230),(16#8E3#,220),(16#8E4#,230),
      (16#8E5#,230),(16#8E6#,220),(16#8E7#,230),(16#8E8#,230),(16#8E9#,220),
      (16#8EA#,230),(16#8EB#,230),(16#8EC#,230),(16#8ED#,220),(16#8EE#,220),
      (16#8EF#,220),(16#8F0#,27),(16#8F1#,28),(16#8F2#,29),(16#8F3#,230),
      (16#8F4#,230),(16#8F5#,230),(16#8F6#,220),(16#8F7#,230),(16#8F8#,230),
      (16#8F9#,220),(16#8FA#,220),(16#8FB#,230),(16#8FC#,230),(16#8FD#,230),
      (16#8FE#,230),(16#8FF#,230),(16#93C#,7),(16#94D#,9),(16#951#,230),
      (16#952#,220),(16#953#,230),(16#954#,230),(16#9BC#,7),(16#9CD#,9),
      (16#9FE#,230),(16#A3C#,7),(16#A4D#,9),(16#ABC#,7),(16#ACD#,9),
      (16#B3C#,7),(16#B4D#,9),(16#BCD#,9),(16#C3C#,7),(16#C4D#,9),
      (16#C55#,84),(16#C56#,91),(16#CBC#,7),(16#CCD#,9),(16#D3B#,9),
      (16#D3C#,9),(16#D4D#,9),(16#DCA#,9),(16#E38#,103),(16#E39#,103),
      (16#E3A#,9),(16#E48#,107),(16#E49#,107),(16#E4A#,107),(16#E4B#,107),
      (16#EB8#,118),(16#EB9#,118),(16#EBA#,9),(16#EC8#,122),(16#EC9#,122),
      (16#ECA#,122),(16#ECB#,122),(16#F18#,220),(16#F19#,220),(16#F35#,220),
      (16#F37#,220),(16#F39#,216),(16#F71#,129),(16#F72#,130),(16#F74#,132),
      (16#F7A#,130),(16#F7B#,130),(16#F7C#,130),(16#F7D#,130),(16#F80#,130),
      (16#F82#,230),(16#F83#,230),(16#F84#,9),(16#F86#,230),(16#F87#,230),
      (16#FC6#,220),(16#1037#,7),(16#1039#,9),(16#103A#,9),(16#108D#,220),
      (16#135D#,230),(16#135E#,230),(16#135F#,230),(16#1714#,9),(16#1715#,9),
      (16#1734#,9),(16#17D2#,9),(16#17DD#,230),(16#18A9#,228),(16#1939#,222),
      (16#193A#,230),(16#193B#,220),(16#1A17#,230),(16#1A18#,220),(16#1A60#,9),
      (16#1A75#,230),(16#1A76#,230),(16#1A77#,230),(16#1A78#,230),(16#1A79#,230),
      (16#1A7A#,230),(16#1A7B#,230),(16#1A7C#,230),(16#1A7F#,220),(16#1AB0#,230),
      (16#1AB1#,230),(16#1AB2#,230),(16#1AB3#,230),(16#1AB4#,230),(16#1AB5#,220),
      (16#1AB6#,220),(16#1AB7#,220),(16#1AB8#,220),(16#1AB9#,220),(16#1ABA#,220),
      (16#1ABB#,230),(16#1ABC#,230),(16#1ABD#,220),(16#1ABF#,220),(16#1AC0#,220),
      (16#1AC1#,230),(16#1AC2#,230),(16#1AC3#,220),(16#1AC4#,220),(16#1AC5#,230),
      (16#1AC6#,230),(16#1AC7#,230),(16#1AC8#,230),(16#1AC9#,230),(16#1ACA#,220),
      (16#1ACB#,230),(16#1ACC#,230),(16#1ACD#,230),(16#1ACE#,230),(16#1B34#,7),
      (16#1B44#,9),(16#1B6B#,230),(16#1B6C#,220),(16#1B6D#,230),(16#1B6E#,230),
      (16#1B6F#,230),(16#1B70#,230),(16#1B71#,230),(16#1B72#,230),(16#1B73#,230),
      (16#1BAA#,9),(16#1BAB#,9),(16#1BE6#,7),(16#1BF2#,9),(16#1BF3#,9),
      (16#1C37#,7),(16#1CD0#,230),(16#1CD1#,230),(16#1CD2#,230),(16#1CD4#,1),
      (16#1CD5#,220),(16#1CD6#,220),(16#1CD7#,220),(16#1CD8#,220),(16#1CD9#,220),
      (16#1CDA#,230),(16#1CDB#,230),(16#1CDC#,220),(16#1CDD#,220),(16#1CDE#,220),
      (16#1CDF#,220),(16#1CE0#,230),(16#1CE2#,1),(16#1CE3#,1),(16#1CE4#,1),
      (16#1CE5#,1),(16#1CE6#,1),(16#1CE7#,1),(16#1CE8#,1),(16#1CED#,220),
      (16#1CF4#,230),(16#1CF8#,230),(16#1CF9#,230),(16#1DC0#,230),(16#1DC1#,230),
      (16#1DC2#,220),(16#1DC3#,230),(16#1DC4#,230),(16#1DC5#,230),(16#1DC6#,230),
      (16#1DC7#,230),(16#1DC8#,230),(16#1DC9#,230),(16#1DCA#,220),(16#1DCB#,230),
      (16#1DCC#,230),(16#1DCD#,234),(16#1DCE#,214),(16#1DCF#,220),(16#1DD0#,202),
      (16#1DD1#,230),(16#1DD2#,230),(16#1DD3#,230),(16#1DD4#,230),(16#1DD5#,230),
      (16#1DD6#,230),(16#1DD7#,230),(16#1DD8#,230),(16#1DD9#,230),(16#1DDA#,230),
      (16#1DDB#,230),(16#1DDC#,230),(16#1DDD#,230),(16#1DDE#,230),(16#1DDF#,230),
      (16#1DE0#,230),(16#1DE1#,230),(16#1DE2#,230),(16#1DE3#,230),(16#1DE4#,230),
      (16#1DE5#,230),(16#1DE6#,230),(16#1DE7#,230),(16#1DE8#,230),(16#1DE9#,230),
      (16#1DEA#,230),(16#1DEB#,230),(16#1DEC#,230),(16#1DED#,230),(16#1DEE#,230),
      (16#1DEF#,230),(16#1DF0#,230),(16#1DF1#,230),(16#1DF2#,230),(16#1DF3#,230),
      (16#1DF4#,230),(16#1DF5#,230),(16#1DF6#,232),(16#1DF7#,228),(16#1DF8#,228),
      (16#1DF9#,220),(16#1DFA#,218),(16#1DFB#,230),(16#1DFC#,233),(16#1DFD#,220),
      (16#1DFE#,230),(16#1DFF#,220),(16#20D0#,230),(16#20D1#,230),(16#20D2#,1),
      (16#20D3#,1),(16#20D4#,230),(16#20D5#,230),(16#20D6#,230),(16#20D7#,230),
      (16#20D8#,1),(16#20D9#,1),(16#20DA#,1),(16#20DB#,230),(16#20DC#,230),
      (16#20E1#,230),(16#20E5#,1),(16#20E6#,1),(16#20E7#,230),(16#20E8#,220),
      (16#20E9#,230),(16#20EA#,1),(16#20EB#,1),(16#20EC#,220),(16#20ED#,220),
      (16#20EE#,220),(16#20EF#,220),(16#20F0#,230),(16#2CEF#,230),(16#2CF0#,230),
      (16#2CF1#,230),(16#2D7F#,9),(16#2DE0#,230),(16#2DE1#,230),(16#2DE2#,230),
      (16#2DE3#,230),(16#2DE4#,230),(16#2DE5#,230),(16#2DE6#,230),(16#2DE7#,230),
      (16#2DE8#,230),(16#2DE9#,230),(16#2DEA#,230),(16#2DEB#,230),(16#2DEC#,230),
      (16#2DED#,230),(16#2DEE#,230),(16#2DEF#,230),(16#2DF0#,230),(16#2DF1#,230),
      (16#2DF2#,230),(16#2DF3#,230),(16#2DF4#,230),(16#2DF5#,230),(16#2DF6#,230),
      (16#2DF7#,230),(16#2DF8#,230),(16#2DF9#,230),(16#2DFA#,230),(16#2DFB#,230),
      (16#2DFC#,230),(16#2DFD#,230),(16#2DFE#,230),(16#2DFF#,230),(16#302A#,218),
      (16#302B#,228),(16#302C#,232),(16#302D#,222),(16#302E#,224),(16#302F#,224),
      (16#3099#,8),(16#309A#,8),(16#A66F#,230),(16#A674#,230),(16#A675#,230),
      (16#A676#,230),(16#A677#,230),(16#A678#,230),(16#A679#,230),(16#A67A#,230),
      (16#A67B#,230),(16#A67C#,230),(16#A67D#,230),(16#A69E#,230),(16#A69F#,230),
      (16#A6F0#,230),(16#A6F1#,230),(16#A806#,9),(16#A82C#,9),(16#A8C4#,9),
      (16#A8E0#,230),(16#A8E1#,230),(16#A8E2#,230),(16#A8E3#,230),(16#A8E4#,230),
      (16#A8E5#,230),(16#A8E6#,230),(16#A8E7#,230),(16#A8E8#,230),(16#A8E9#,230),
      (16#A8EA#,230),(16#A8EB#,230),(16#A8EC#,230),(16#A8ED#,230),(16#A8EE#,230),
      (16#A8EF#,230),(16#A8F0#,230),(16#A8F1#,230),(16#A92B#,220),(16#A92C#,220),
      (16#A92D#,220),(16#A953#,9),(16#A9B3#,7),(16#A9C0#,9),(16#AAB0#,230),
      (16#AAB2#,230),(16#AAB3#,230),(16#AAB4#,220),(16#AAB7#,230),(16#AAB8#,230),
      (16#AABE#,230),(16#AABF#,230),(16#AAC1#,230),(16#AAF6#,9),(16#ABED#,9),
      (16#FB1E#,26),(16#FE20#,230),(16#FE21#,230),(16#FE22#,230),(16#FE23#,230),
      (16#FE24#,230),(16#FE25#,230),(16#FE26#,230),(16#FE27#,220),(16#FE28#,220),
      (16#FE29#,220),(16#FE2A#,220),(16#FE2B#,220),(16#FE2C#,220),(16#FE2D#,220),
      (16#FE2E#,230),(16#FE2F#,230),(16#101FD#,220),(16#102E0#,220),(16#10376#,230),
      (16#10377#,230),(16#10378#,230),(16#10379#,230),(16#1037A#,230),(16#10A0D#,220),
      (16#10A0F#,230),(16#10A38#,230),(16#10A39#,1),(16#10A3A#,220),(16#10A3F#,9),
      (16#10AE5#,230),(16#10AE6#,220),(16#10D24#,230),(16#10D25#,230),(16#10D26#,230),
      (16#10D27#,230),(16#10EAB#,230),(16#10EAC#,230),(16#10EFD#,220),(16#10EFE#,220),
      (16#10EFF#,220),(16#10F46#,220),(16#10F47#,220),(16#10F48#,230),(16#10F49#,230),
      (16#10F4A#,230),(16#10F4B#,220),(16#10F4C#,230),(16#10F4D#,220),(16#10F4E#,220),
      (16#10F4F#,220),(16#10F50#,220),(16#10F82#,230),(16#10F83#,220),(16#10F84#,230),
      (16#10F85#,220),(16#11046#,9),(16#11070#,9),(16#1107F#,9),(16#110B9#,9),
      (16#110BA#,7),(16#11100#,230),(16#11101#,230),(16#11102#,230),(16#11133#,9),
      (16#11134#,9),(16#11173#,7),(16#111C0#,9),(16#111CA#,7),(16#11235#,9),
      (16#11236#,7),(16#112E9#,7),(16#112EA#,9),(16#1133B#,7),(16#1133C#,7),
      (16#1134D#,9),(16#11366#,230),(16#11367#,230),(16#11368#,230),(16#11369#,230),
      (16#1136A#,230),(16#1136B#,230),(16#1136C#,230),(16#11370#,230),(16#11371#,230),
      (16#11372#,230),(16#11373#,230),(16#11374#,230),(16#11442#,9),(16#11446#,7),
      (16#1145E#,230),(16#114C2#,9),(16#114C3#,7),(16#115BF#,9),(16#115C0#,7),
      (16#1163F#,9),(16#116B6#,9),(16#116B7#,7),(16#1172B#,9),(16#11839#,9),
      (16#1183A#,7),(16#1193D#,9),(16#1193E#,9),(16#11943#,7),(16#119E0#,9),
      (16#11A34#,9),(16#11A47#,9),(16#11A99#,9),(16#11C3F#,9),(16#11D42#,7),
      (16#11D44#,9),(16#11D45#,9),(16#11D97#,9),(16#11F41#,9),(16#11F42#,9),
      (16#16AF0#,1),(16#16AF1#,1),(16#16AF2#,1),(16#16AF3#,1),(16#16AF4#,1),
      (16#16B30#,230),(16#16B31#,230),(16#16B32#,230),(16#16B33#,230),(16#16B34#,230),
      (16#16B35#,230),(16#16B36#,230),(16#16FF0#,6),(16#16FF1#,6),(16#1BC9E#,1),
      (16#1D165#,216),(16#1D166#,216),(16#1D167#,1),(16#1D168#,1),(16#1D169#,1),
      (16#1D16D#,226),(16#1D16E#,216),(16#1D16F#,216),(16#1D170#,216),(16#1D171#,216),
      (16#1D172#,216),(16#1D17B#,220),(16#1D17C#,220),(16#1D17D#,220),(16#1D17E#,220),
      (16#1D17F#,220),(16#1D180#,220),(16#1D181#,220),(16#1D182#,220),(16#1D185#,230),
      (16#1D186#,230),(16#1D187#,230),(16#1D188#,230),(16#1D189#,230),(16#1D18A#,220),
      (16#1D18B#,220),(16#1D1AA#,230),(16#1D1AB#,230),(16#1D1AC#,230),(16#1D1AD#,230),
      (16#1D242#,230),(16#1D243#,230),(16#1D244#,230),(16#1E000#,230),(16#1E001#,230),
      (16#1E002#,230),(16#1E003#,230),(16#1E004#,230),(16#1E005#,230),(16#1E006#,230),
      (16#1E008#,230),(16#1E009#,230),(16#1E00A#,230),(16#1E00B#,230),(16#1E00C#,230),
      (16#1E00D#,230),(16#1E00E#,230),(16#1E00F#,230),(16#1E010#,230),(16#1E011#,230),
      (16#1E012#,230),(16#1E013#,230),(16#1E014#,230),(16#1E015#,230),(16#1E016#,230),
      (16#1E017#,230),(16#1E018#,230),(16#1E01B#,230),(16#1E01C#,230),(16#1E01D#,230),
      (16#1E01E#,230),(16#1E01F#,230),(16#1E020#,230),(16#1E021#,230),(16#1E023#,230),
      (16#1E024#,230),(16#1E026#,230),(16#1E027#,230),(16#1E028#,230),(16#1E029#,230),
      (16#1E02A#,230),(16#1E08F#,230),(16#1E130#,230),(16#1E131#,230),(16#1E132#,230),
      (16#1E133#,230),(16#1E134#,230),(16#1E135#,230),(16#1E136#,230),(16#1E2AE#,230),
      (16#1E2EC#,230),(16#1E2ED#,230),(16#1E2EE#,230),(16#1E2EF#,230),(16#1E4EC#,232),
      (16#1E4ED#,232),(16#1E4EE#,220),(16#1E4EF#,230),(16#1E8D0#,220),(16#1E8D1#,220),
      (16#1E8D2#,220),(16#1E8D3#,220),(16#1E8D4#,220),(16#1E8D5#,220),(16#1E8D6#,220),
      (16#1E944#,230),(16#1E945#,230),(16#1E946#,230),(16#1E947#,230),(16#1E948#,230),
      (16#1E949#,230),(16#1E94A#,7)
   );
   type NFD_Index is range 1..1026;
   type NFD_Array is array (NFD_Index) of Code_Point_Array_Ptr;
   NFD_List : constant NFD_Array :=
   (  S226E'Access,S2260'Access,S226F'Access,SC0'Access,SC1'Access,
      SC2'Access,SC3'Access,S100'Access,S102'Access,S226'Access,
      SC4'Access,S1EA2'Access,SC5'Access,S1CD'Access,S200'Access,
      S202'Access,S1EA0'Access,S1E00'Access,S104'Access,S1E02'Access,
      S1E04'Access,S1E06'Access,S106'Access,S108'Access,S10A'Access,
      S10C'Access,SC7'Access,S1E0A'Access,S10E'Access,S1E0C'Access,
      S1E10'Access,S1E12'Access,S1E0E'Access,SC8'Access,SC9'Access,
      SCA'Access,S1EBC'Access,S112'Access,S114'Access,S116'Access,
      SCB'Access,S1EBA'Access,S11A'Access,S204'Access,S206'Access,
      S1EB8'Access,S228'Access,S118'Access,S1E18'Access,S1E1A'Access,
      S1E1E'Access,S1F4'Access,S11C'Access,S1E20'Access,S11E'Access,
      S120'Access,S1E6'Access,S122'Access,S124'Access,S1E22'Access,
      S1E26'Access,S21E'Access,S1E24'Access,S1E28'Access,S1E2A'Access,
      SCC'Access,SCD'Access,SCE'Access,S128'Access,S12A'Access,
      S12C'Access,S130'Access,SCF'Access,S1EC8'Access,S1CF'Access,
      S208'Access,S20A'Access,S1ECA'Access,S12E'Access,S1E2C'Access,
      S134'Access,S1E30'Access,S1E8'Access,S1E32'Access,S136'Access,
      S1E34'Access,S139'Access,S13D'Access,S1E36'Access,S13B'Access,
      S1E3C'Access,S1E3A'Access,S1E3E'Access,S1E40'Access,S1E42'Access,
      S1F8'Access,S143'Access,SD1'Access,S1E44'Access,S147'Access,
      S1E46'Access,S145'Access,S1E4A'Access,S1E48'Access,SD2'Access,
      SD3'Access,SD4'Access,SD5'Access,S14C'Access,S14E'Access,
      S22E'Access,SD6'Access,S1ECE'Access,S150'Access,S1D1'Access,
      S20C'Access,S20E'Access,S1A0'Access,S1ECC'Access,S1EA'Access,
      S1E54'Access,S1E56'Access,S154'Access,S1E58'Access,S158'Access,
      S210'Access,S212'Access,S1E5A'Access,S156'Access,S1E5E'Access,
      S15A'Access,S15C'Access,S1E60'Access,S160'Access,S1E62'Access,
      S218'Access,S15E'Access,S1E6A'Access,S164'Access,S1E6C'Access,
      S21A'Access,S162'Access,S1E70'Access,S1E6E'Access,SD9'Access,
      SDA'Access,SDB'Access,S168'Access,S16A'Access,S16C'Access,
      SDC'Access,S1EE6'Access,S16E'Access,S170'Access,S1D3'Access,
      S214'Access,S216'Access,S1AF'Access,S1EE4'Access,S1E72'Access,
      S172'Access,S1E76'Access,S1E74'Access,S1E7C'Access,S1E7E'Access,
      S1E80'Access,S1E82'Access,S174'Access,S1E86'Access,S1E84'Access,
      S1E88'Access,S1E8A'Access,S1E8C'Access,S1EF2'Access,SDD'Access,
      S176'Access,S1EF8'Access,S232'Access,S1E8E'Access,S178'Access,
      S1EF6'Access,S1EF4'Access,S179'Access,S1E90'Access,S17B'Access,
      S17D'Access,S1E92'Access,S1E94'Access,SE0'Access,SE1'Access,
      SE2'Access,SE3'Access,S101'Access,S103'Access,S227'Access,
      SE4'Access,S1EA3'Access,SE5'Access,S1CE'Access,S201'Access,
      S203'Access,S1EA1'Access,S1E01'Access,S105'Access,S1E03'Access,
      S1E05'Access,S1E07'Access,S107'Access,S109'Access,S10B'Access,
      S10D'Access,SE7'Access,S1E0B'Access,S10F'Access,S1E0D'Access,
      S1E11'Access,S1E13'Access,S1E0F'Access,SE8'Access,SE9'Access,
      SEA'Access,S1EBD'Access,S113'Access,S115'Access,S117'Access,
      SEB'Access,S1EBB'Access,S11B'Access,S205'Access,S207'Access,
      S1EB9'Access,S229'Access,S119'Access,S1E19'Access,S1E1B'Access,
      S1E1F'Access,S1F5'Access,S11D'Access,S1E21'Access,S11F'Access,
      S121'Access,S1E7'Access,S123'Access,S125'Access,S1E23'Access,
      S1E27'Access,S21F'Access,S1E25'Access,S1E29'Access,S1E2B'Access,
      S1E96'Access,SEC'Access,SED'Access,SEE'Access,S129'Access,
      S12B'Access,S12D'Access,SEF'Access,S1EC9'Access,S1D0'Access,
      S209'Access,S20B'Access,S1ECB'Access,S12F'Access,S1E2D'Access,
      S135'Access,S1F0'Access,S1E31'Access,S1E9'Access,S1E33'Access,
      S137'Access,S1E35'Access,S13A'Access,S13E'Access,S1E37'Access,
      S13C'Access,S1E3D'Access,S1E3B'Access,S1E3F'Access,S1E41'Access,
      S1E43'Access,S1F9'Access,S144'Access,SF1'Access,S1E45'Access,
      S148'Access,S1E47'Access,S146'Access,S1E4B'Access,S1E49'Access,
      SF2'Access,SF3'Access,SF4'Access,SF5'Access,S14D'Access,
      S14F'Access,S22F'Access,SF6'Access,S1ECF'Access,S151'Access,
      S1D2'Access,S20D'Access,S20F'Access,S1A1'Access,S1ECD'Access,
      S1EB'Access,S1E55'Access,S1E57'Access,S155'Access,S1E59'Access,
      S159'Access,S211'Access,S213'Access,S1E5B'Access,S157'Access,
      S1E5F'Access,S15B'Access,S15D'Access,S1E61'Access,S161'Access,
      S1E63'Access,S219'Access,S15F'Access,S1E6B'Access,S1E97'Access,
      S165'Access,S1E6D'Access,S21B'Access,S163'Access,S1E71'Access,
      S1E6F'Access,SF9'Access,SFA'Access,SFB'Access,S169'Access,
      S16B'Access,S16D'Access,SFC'Access,S1EE7'Access,S16F'Access,
      S171'Access,S1D4'Access,S215'Access,S217'Access,S1B0'Access,
      S1EE5'Access,S1E73'Access,S173'Access,S1E77'Access,S1E75'Access,
      S1E7D'Access,S1E7F'Access,S1E81'Access,S1E83'Access,S175'Access,
      S1E87'Access,S1E85'Access,S1E98'Access,S1E89'Access,S1E8B'Access,
      S1E8D'Access,S1EF3'Access,SFD'Access,S177'Access,S1EF9'Access,
      S233'Access,S1E8F'Access,SFF'Access,S1EF7'Access,S1E99'Access,
      S1EF5'Access,S17A'Access,S1E91'Access,S17C'Access,S17E'Access,
      S1E93'Access,S1E95'Access,S1FED'Access,S385'Access,S1FC1'Access,
      S1EA6'Access,S1EA4'Access,S1EAA'Access,S1EA8'Access,S1DE'Access,
      S1FA'Access,S1FC'Access,S1E2'Access,S1E08'Access,S1EC0'Access,
      S1EBE'Access,S1EC4'Access,S1EC2'Access,S1E2E'Access,S1ED2'Access,
      S1ED0'Access,S1ED6'Access,S1ED4'Access,S1E4C'Access,S22C'Access,
      S1E4E'Access,S22A'Access,S1FE'Access,S1DB'Access,S1D7'Access,
      S1D5'Access,S1D9'Access,S1EA7'Access,S1EA5'Access,S1EAB'Access,
      S1EA9'Access,S1DF'Access,S1FB'Access,S1FD'Access,S1E3'Access,
      S1E09'Access,S1EC1'Access,S1EBF'Access,S1EC5'Access,S1EC3'Access,
      S1E2F'Access,S1ED3'Access,S1ED1'Access,S1ED7'Access,S1ED5'Access,
      S1E4D'Access,S22D'Access,S1E4F'Access,S22B'Access,S1FF'Access,
      S1DC'Access,S1D8'Access,S1D6'Access,S1DA'Access,S1EB0'Access,
      S1EAE'Access,S1EB4'Access,S1EB2'Access,S1EB1'Access,S1EAF'Access,
      S1EB5'Access,S1EB3'Access,S1E14'Access,S1E16'Access,S1E15'Access,
      S1E17'Access,S1E50'Access,S1E52'Access,S1E51'Access,S1E53'Access,
      S1E64'Access,S1E65'Access,S1E66'Access,S1E67'Access,S1E78'Access,
      S1E79'Access,S1E7A'Access,S1E7B'Access,S1E9B'Access,S1EDC'Access,
      S1EDA'Access,S1EE0'Access,S1EDE'Access,S1EE2'Access,S1EDD'Access,
      S1EDB'Access,S1EE1'Access,S1EDF'Access,S1EE3'Access,S1EEA'Access,
      S1EE8'Access,S1EEE'Access,S1EEC'Access,S1EF0'Access,S1EEB'Access,
      S1EE9'Access,S1EEF'Access,S1EED'Access,S1EF1'Access,S1EE'Access,
      S1EC'Access,S1ED'Access,S1E0'Access,S1E1'Access,S1E1C'Access,
      S1E1D'Access,S230'Access,S231'Access,S1EF'Access,S344'Access,
      S1FBA'Access,S386'Access,S1FB9'Access,S1FB8'Access,S1F08'Access,
      S1F09'Access,S1FBC'Access,S1FC8'Access,S388'Access,S1F18'Access,
      S1F19'Access,S1FCA'Access,S389'Access,S1F28'Access,S1F29'Access,
      S1FCC'Access,S1FDA'Access,S38A'Access,S1FD9'Access,S1FD8'Access,
      S3AA'Access,S1F38'Access,S1F39'Access,S1FF8'Access,S38C'Access,
      S1F48'Access,S1F49'Access,S1FEC'Access,S1FEA'Access,S38E'Access,
      S1FE9'Access,S1FE8'Access,S3AB'Access,S1F59'Access,S1FFA'Access,
      S38F'Access,S1F68'Access,S1F69'Access,S1FFC'Access,S1FB4'Access,
      S1FC4'Access,S1F70'Access,S3AC'Access,S1FB1'Access,S1FB0'Access,
      S1F00'Access,S1F01'Access,S1FB6'Access,S1FB3'Access,S1F72'Access,
      S3AD'Access,S1F10'Access,S1F11'Access,S1F74'Access,S3AE'Access,
      S1F20'Access,S1F21'Access,S1FC6'Access,S1FC3'Access,S1F76'Access,
      S3AF'Access,S1FD1'Access,S1FD0'Access,S3CA'Access,S1F30'Access,
      S1F31'Access,S1FD6'Access,S1F78'Access,S3CC'Access,S1F40'Access,
      S1F41'Access,S1FE4'Access,S1FE5'Access,S1F7A'Access,S3CD'Access,
      S1FE1'Access,S1FE0'Access,S3CB'Access,S1F50'Access,S1F51'Access,
      S1FE6'Access,S1F7C'Access,S3CE'Access,S1F60'Access,S1F61'Access,
      S1FF6'Access,S1FF3'Access,S1FD2'Access,S390'Access,S1FD7'Access,
      S1FE2'Access,S3B0'Access,S1FE7'Access,S1FF4'Access,S3D3'Access,
      S3D4'Access,S407'Access,S4D0'Access,S4D2'Access,S403'Access,
      S400'Access,S4D6'Access,S401'Access,S4C1'Access,S4DC'Access,
      S4DE'Access,S40D'Access,S4E2'Access,S419'Access,S4E4'Access,
      S40C'Access,S4E6'Access,S4EE'Access,S40E'Access,S4F0'Access,
      S4F2'Access,S4F4'Access,S4F8'Access,S4EC'Access,S4D1'Access,
      S4D3'Access,S453'Access,S450'Access,S4D7'Access,S451'Access,
      S4C2'Access,S4DD'Access,S4DF'Access,S45D'Access,S4E3'Access,
      S439'Access,S4E5'Access,S45C'Access,S4E7'Access,S4EF'Access,
      S45E'Access,S4F1'Access,S4F3'Access,S4F5'Access,S4F9'Access,
      S4ED'Access,S457'Access,S476'Access,S477'Access,S4DA'Access,
      S4DB'Access,S4EA'Access,S4EB'Access,SFB2E'Access,SFB2F'Access,
      SFB30'Access,SFB31'Access,SFB4C'Access,SFB32'Access,SFB33'Access,
      SFB34'Access,SFB4B'Access,SFB35'Access,SFB36'Access,SFB38'Access,
      SFB1D'Access,SFB39'Access,SFB3A'Access,SFB3B'Access,SFB4D'Access,
      SFB3C'Access,SFB3E'Access,SFB40'Access,SFB41'Access,SFB43'Access,
      SFB44'Access,SFB4E'Access,SFB46'Access,SFB47'Access,SFB48'Access,
      SFB49'Access,SFB2A'Access,SFB2B'Access,SFB4A'Access,SFB1F'Access,
      S622'Access,S623'Access,S625'Access,S624'Access,S626'Access,
      S6C2'Access,S6D3'Access,S6C0'Access,S958'Access,S959'Access,
      S95A'Access,S95B'Access,S95C'Access,S95D'Access,S929'Access,
      S95E'Access,S95F'Access,S931'Access,S934'Access,S9DC'Access,
      S9DD'Access,S9DF'Access,S9CB'Access,S9CC'Access,SA59'Access,
      SA5A'Access,SA5B'Access,SA5E'Access,SA33'Access,SA36'Access,
      SB5C'Access,SB5D'Access,SB4B'Access,SB48'Access,SB4C'Access,
      SB94'Access,SBCA'Access,SBCC'Access,SBCB'Access,SC48'Access,
      SCC0'Access,SCCA'Access,SCC7'Access,SCC8'Access,SCCB'Access,
      SD4A'Access,SD4C'Access,SD4B'Access,SDDA'Access,SDDC'Access,
      SDDE'Access,SDDD'Access,SF69'Access,SF43'Access,SF4D'Access,
      SF52'Access,SF57'Access,SF5C'Access,SF73'Access,SF75'Access,
      SF81'Access,SFB9'Access,SF93'Access,SF9D'Access,SFA2'Access,
      SFA7'Access,SFAC'Access,SF76'Access,SF78'Access,S1026'Access,
      S1B06'Access,S1B08'Access,S1B0A'Access,S1B0C'Access,S1B0E'Access,
      S1B12'Access,S1B3B'Access,S1B3D'Access,S1B40'Access,S1B41'Access,
      S1B43'Access,S1E38'Access,S1E39'Access,S1E5C'Access,S1E5D'Access,
      S1E68'Access,S1E69'Access,S1EAC'Access,S1EB6'Access,S1EAD'Access,
      S1EB7'Access,S1EC6'Access,S1EC7'Access,S1ED8'Access,S1ED9'Access,
      S1F02'Access,S1F04'Access,S1F06'Access,S1F80'Access,S1F03'Access,
      S1F05'Access,S1F07'Access,S1F81'Access,S1F82'Access,S1F83'Access,
      S1F84'Access,S1F85'Access,S1F86'Access,S1F87'Access,S1F0A'Access,
      S1F0C'Access,S1F0E'Access,S1F88'Access,S1F0B'Access,S1F0D'Access,
      S1F0F'Access,S1F89'Access,S1F8A'Access,S1F8B'Access,S1F8C'Access,
      S1F8D'Access,S1F8E'Access,S1F8F'Access,S1F12'Access,S1F14'Access,
      S1F13'Access,S1F15'Access,S1F1A'Access,S1F1C'Access,S1F1B'Access,
      S1F1D'Access,S1F22'Access,S1F24'Access,S1F26'Access,S1F90'Access,
      S1F23'Access,S1F25'Access,S1F27'Access,S1F91'Access,S1F92'Access,
      S1F93'Access,S1F94'Access,S1F95'Access,S1F96'Access,S1F97'Access,
      S1F2A'Access,S1F2C'Access,S1F2E'Access,S1F98'Access,S1F2B'Access,
      S1F2D'Access,S1F2F'Access,S1F99'Access,S1F9A'Access,S1F9B'Access,
      S1F9C'Access,S1F9D'Access,S1F9E'Access,S1F9F'Access,S1F32'Access,
      S1F34'Access,S1F36'Access,S1F33'Access,S1F35'Access,S1F37'Access,
      S1F3A'Access,S1F3C'Access,S1F3E'Access,S1F3B'Access,S1F3D'Access,
      S1F3F'Access,S1F42'Access,S1F44'Access,S1F43'Access,S1F45'Access,
      S1F4A'Access,S1F4C'Access,S1F4B'Access,S1F4D'Access,S1F52'Access,
      S1F54'Access,S1F56'Access,S1F53'Access,S1F55'Access,S1F57'Access,
      S1F5B'Access,S1F5D'Access,S1F5F'Access,S1F62'Access,S1F64'Access,
      S1F66'Access,S1FA0'Access,S1F63'Access,S1F65'Access,S1F67'Access,
      S1FA1'Access,S1FA2'Access,S1FA3'Access,S1FA4'Access,S1FA5'Access,
      S1FA6'Access,S1FA7'Access,S1F6A'Access,S1F6C'Access,S1F6E'Access,
      S1FA8'Access,S1F6B'Access,S1F6D'Access,S1F6F'Access,S1FA9'Access,
      S1FAA'Access,S1FAB'Access,S1FAC'Access,S1FAD'Access,S1FAE'Access,
      S1FAF'Access,S1FB2'Access,S1FC2'Access,S1FF2'Access,S1FB7'Access,
      S1FCD'Access,S1FCE'Access,S1FCF'Access,S1FC7'Access,S1FF7'Access,
      S1FDD'Access,S1FDE'Access,S1FDF'Access,S219A'Access,S219B'Access,
      S21AE'Access,S21CD'Access,S21CF'Access,S21CE'Access,S2204'Access,
      S2209'Access,S220C'Access,S2224'Access,S2226'Access,S2241'Access,
      S2244'Access,S2247'Access,S2249'Access,S226D'Access,S2262'Access,
      S2270'Access,S2271'Access,S2274'Access,S2275'Access,S2278'Access,
      S2279'Access,S2280'Access,S2281'Access,S22E0'Access,S22E1'Access,
      S2284'Access,S2285'Access,S2288'Access,S2289'Access,S22E2'Access,
      S22E3'Access,S22AC'Access,S22AD'Access,S22AE'Access,S22AF'Access,
      S22EA'Access,S22EB'Access,S22EC'Access,S22ED'Access,S2ADC'Access,
      S3094'Access,S304C'Access,S304E'Access,S3050'Access,S3052'Access,
      S3054'Access,S3056'Access,S3058'Access,S305A'Access,S305C'Access,
      S305E'Access,S3060'Access,S3062'Access,S3065'Access,S3067'Access,
      S3069'Access,S3070'Access,S3071'Access,S3073'Access,S3074'Access,
      S3076'Access,S3077'Access,S3079'Access,S307A'Access,S307C'Access,
      S307D'Access,S309E'Access,S30F4'Access,S30AC'Access,S30AE'Access,
      S30B0'Access,S30B2'Access,S30B4'Access,S30B6'Access,S30B8'Access,
      S30BA'Access,S30BC'Access,S30BE'Access,S30C0'Access,S30C2'Access,
      S30C5'Access,S30C7'Access,S30C9'Access,S30D0'Access,S30D1'Access,
      S30D3'Access,S30D4'Access,S30D6'Access,S30D7'Access,S30D9'Access,
      S30DA'Access,S30DC'Access,S30DD'Access,S30F7'Access,S30F8'Access,
      S30F9'Access,S30FA'Access,S30FE'Access,SFB2C'Access,SFB2D'Access,
      S1109A'Access,S1109C'Access,S110AB'Access,S1112E'Access,S1112F'Access,
      S1134B'Access,S1134C'Access,S114BC'Access,S114BB'Access,S114BE'Access,
      S115BA'Access,S115BB'Access,S11938'Access,S1D15E'Access,S1D15F'Access,
      S1D160'Access,S1D161'Access,S1D162'Access,S1D163'Access,S1D164'Access,
      S1D1BB'Access,S1D1BC'Access,S1D1BD'Access,S1D1BF'Access,S1D1BE'Access,
      S1D1C0'Access
   );
   --  type NFKD_Index is range 1..817;
   --  type NFKD_Array is array (NFKD_Index) of Code_Point_Array_Ptr;
   --  NFKD_List : constant NFKD_Array :=
   --  (
   --  );

   function Find (Value : UTF8_Code_Point)
      return Code_Point_Array_Ptr is
      From    : Normalization_Map_Size := Mapping'First;
      To      : Normalization_Map_Size := Mapping'Last;
      This    : Normalization_Map_Size;
      Current : Code_Point_Array_Ptr;
   begin
      loop
         This := (From + To) / 2;
         Current := Mapping (This);
         if Current (1) > Value then
            exit when This = From;
            To := This - 1;
         elsif Current (1) < Value then
            exit when This = To;
            From := This + 1;
         else
            return Current;
         end if;
      end loop;
      return null;
   end Find;

   --  function Dump (Code : Code_Point) return String is
   --     type Integer_64 is range -2**63..2**63 - 1;
   --     package Edit is new Strings_Edit.Integer_Edit (Integer_64);
   --  begin
   --     return Edit.Image (Integer_64 (Code), Base => 16);
   --  end Dump;
   --
   --  function Dump (S : Code_Point_Array) return String is
   --     Result  : String (1..120);
   --     Pointer : Integer := 1;
   --  begin
   --     for I in S'Range loop
   --        if Pointer > 1 then
   --           Put (Result, Pointer, ",");
   --        end if;
   --        Put (Result, Pointer, Dump (S (I)));
   --     end loop;
   --     return Result (1..Pointer - 1);
   --  end Dump;
   --
   --  function Dump (S : UTF8_Code_Point_Array) return String is
   --     Result  : String (1..120);
   --     Pointer : Integer := 1;
   --  begin
   --     for I in S'Range loop
   --        if Pointer > 1 then
   --           Put (Result, Pointer, ",");
   --        end if;
   --        Put (Result, Pointer, Dump (Code_Point (S (I))));
   --     end loop;
   --     return Result (1..Pointer - 1);
   --  end Dump;
   --
   --  function Dump (S : String) return String is
   --     Result : String (1..120);
   --     Code   : Strings_Edit.UTF8.UTF8_Code_Point;
   --     From   : Integer := S'First;
   --     To     : Integer := Result'First;
   --  begin
   --     while From <= S'Last loop
   --        Get (S, From, Code);
   --        Put (Result, To, Dump (Code) & " ");
   --     end loop;
   --     return Result (1..To - 1);
   --  end Dump;

   function Compare
            (  Left  : Code_Point_Array;
               Right : UTF8_Code_Point_Array
            )  return Precedence is
      J : Integer := Right'First;
   begin
      for I in Left'Range loop
         if J > Right'Last then
            return Greater;
         end if;
         declare
            L : constant Code_Point := Left (I) and Mask;
            R : constant UTF8_Code_Point := Right (J);
         begin
            if L < R then
               return Less;
            elsif L > R then
               return Greater;
            end if;
         end;
         J := J + 1;
      end loop;
      return Equal;
   end Compare;

   function Find_NFC (Value : UTF8_Code_Point_Array)
      return Code_Point_Array_Ptr is
   begin
      if Value'Length < 2 then
         return null;
      end if;
      declare
         From    : NFD_Index := NFD_List'First;
         To      : NFD_Index := NFD_List'Last;
         This    : NFD_Index;
         Current : Code_Point_Array_Ptr;
      begin
         loop
            This := (From + To) / 2;
            Current := NFD_List (This);
            case Compare (Current (2..Current'Last), Value) is
               when Greater =>
                  exit when This = From;
                  To := This - 1;
               when Less =>
                  exit when This = To;
                  From := This + 1;
               when Equal =>
                  for I in This + 1..NFD_List'Last loop
                     declare
                        Next : constant
                               Code_Point_Array_Ptr := NFD_List (I);
                     begin
                        exit when Next'Length >  Value'Length
                          or else Next'Length <= Current'Length
                          or else Compare (Next (2..Next'Last), Value)
                               /= Equal;
                        Current := Next;
                     end;
                  end loop;
                  return Current;
            end case;
         end loop;
      end;
      return null;
   end Find_NFC;

   function Combining_Class
            (  Code : UTF8_Code_Point
            )  return Canonical_Combining_Class is
      From    : Class_Index := Classes'First;
      To      : Class_Index := Classes'Last;
      This    : Class_Index;
      Current : Pair;
   begin
      loop
         This := (From + To) / 2;
         Current := Classes (This);
         if Current.Code > Code then
            exit when This = From;
            To := This - 1;
         elsif Current.Code < Code then
            exit when This = To;
            From := This + 1;
         else
            return Current.Class;
         end if;
      end loop;
      return 0;
   end Combining_Class;

   function Compare_Decomposed
            (  Left, Right : UTF8_Code_Point;
               Map         : Unicode_Mapping;
               Form        : Normalization_D_Form := NFD
            )  return Precedence is
   begin
      if Left = Right then
         return Equal;
      end if;
      declare
         L : UTF8_Code_Point_Array (1..Buffer_Size);
         R : UTF8_Code_Point_Array (1..Buffer_Size);
         I : Integer := L'First;
         J : Integer := R'First;
      begin
         Decompose (L, I, Left,  Form);
         Decompose (R, J, Right, Form);
         return Compare (L (1..I - 1), R (1..J - 1), Map);
      end;
   end Compare_Decomposed;

   function Compare_Decomposed
            (  Left, Right : UTF8_Code_Point;
               Form        : Normalization_D_Form := NFD
            )  return Precedence is
   begin
      if Left = Right then
         return Equal;
      end if;
      declare
         L : UTF8_Code_Point_Array (1..Buffer_Size);
         R : UTF8_Code_Point_Array (1..Buffer_Size);
         I : Integer := L'First;
         J : Integer := R'First;
      begin
         Decompose (L, I, Left,  Form);
         Decompose (R, J, Right, Form);
         return Compare (L (1..I - 1), R (1..J - 1));
      end;
   end Compare_Decomposed;

   function Compare_Decomposed
            (  Left, Right : String;
               Map         : Unicode_Mapping;
               Form        : Normalization_D_Form := NFD
            )  return Precedence is
      L : UTF8_Code_Point_Array (1..Buffer_Size);
      R : UTF8_Code_Point_Array (1..Buffer_Size);
      I : Integer := Left'First;
      J : Integer := Right'First;
      Code    : UTF8_Code_Point;
      Index   : Integer := 1;
      L_Index : Integer := 1;
      R_Index : Integer := 1;
   begin
      loop
         while I <= Left'Last loop -- Until first anchored code point
            Index := I;
            Get (Left, Index, Code);
            exit when L_Index > 1 and then Combining_Class (Code) = 0;
            Decompose (L, L_Index, Code, Form);
            I := Index;
         end loop;
         while J <= Right'Last loop -- Until first anchored code point
            Index := J;
            Get (Right, Index, Code);
            exit when R_Index > 1 and then Combining_Class (Code) = 0;
            Decompose (R, R_Index, Code, Form);
            J := Index;
         end loop;
         case Compare (L (1..L_Index - 1), R (1..R_Index - 1), Map) is
            when Less =>
               return Less;
            when Greater =>
               return Greater;
            when Equal =>
               if I > Left'Last and then J > Right'Last then
                  return Equal;
               end if;
               L_Index := 1;
               R_Index := 1;
         end case;
      end loop;
   exception
      when Layout_Error => -- Too many diacritic marks
         raise Constraint_Error;
   end Compare_Decomposed;

   function Compare_Decomposed
            (  Left, Right : String;
               Form        : Normalization_D_Form := NFD
            )  return Precedence is
      L : UTF8_Code_Point_Array (1..Buffer_Size);
      R : UTF8_Code_Point_Array (1..Buffer_Size);
      I : Integer := Left'First;
      J : Integer := Right'First;
      Code    : UTF8_Code_Point;
      Index   : Integer := 1;
      L_Index : Integer := 1;
      R_Index : Integer := 1;
   begin
      loop
         while I <= Left'Last loop -- Until first anchored code point
            Index := I;
            Get (Left, Index, Code);
            exit when L_Index > 1 and then Combining_Class (Code) = 0;
            Decompose (L, L_Index, Code, Form);
            I := Index;
         end loop;
         while J <= Right'Last loop -- Until first anchored code point
            Index := J;
            Get (Right, Index, Code);
            exit when R_Index > 1 and then Combining_Class (Code) = 0;
            Decompose (R, R_Index, Code, Form);
            J := Index;
         end loop;
         case Compare (L (1..L_Index - 1), R (1..R_Index - 1)) is
            when Less =>
               return Less;
            when Greater =>
               return Greater;
            when Equal =>
               if I > Left'Last and then J > Right'Last then
                  return Equal;
               end if;
               L_Index := 1;
               R_Index := 1;
         end case;
      end loop;
   exception
      when Layout_Error => -- Too many diacritic marks
         raise Constraint_Error;
   end Compare_Decomposed;

   procedure Compose
             (  Destination : in out String;
                Pointer     : in out Integer;
                Source      : String
             )  is
      Buffer : UTF8_Code_Point_Array (1..Buffer_Size);
      This   : UTF8_Code_Point;
      From   : Integer := Source'First;
      To     : Integer := Pointer;
      Index  : Integer := Buffer'First;
   begin
      while From <= Source'Last loop
         Get (Source, From, This);
         if Index > Buffer'First then
            if Category (This) not in Mark then
               Compose (Buffer (Buffer'First..Index - 1), Index);
               for Item in Buffer'First..Index - 1 loop
                  Put (Destination, To, Buffer (Item));
               end loop;
               Index := Buffer'First;
            end if;
         elsif Index > Buffer'Last then
            raise Constraint_Error;
         end if;
         Buffer (Index) := This;
         Index := Index + 1;
      end loop;
      if Index > Buffer'First then
         Compose (Buffer (Buffer'First..Index - 1), Index);
         for Item in Buffer'First..Index - 1 loop
            Put (Destination, To, Buffer (Item));
         end loop;
      end if;
      Pointer := To;
   end Compose;

   procedure Compose
             (  Text    : in out UTF8_Code_Point_Array;
                Pointer : out Integer
             )  is
      From : Integer := Text'First;
      To   : Integer := From;
      This : Code_Point_Array_Ptr;

      procedure Collapse is
      begin
         if This = null then
            Text (To) := Text (From);
            From := From + 1;
            To   := To   + 1;
         else
            From := From + This'Length - 2; -- Remove points
            Text (From) := This (1);
         end if;
      end Collapse;
   begin
      while From <= Text'Last loop
         loop
            This := Find_NFC (Text (From..Text'Last));
            Collapse;
            exit when This = null;
         end loop;
      end loop;
      Pointer := To;
   end Compose;

   procedure Decompose
             (  Destination : in out UTF8_Code_Point_Array;
                Pointer     : in out Integer;
                Code        : UTF8_Code_Point;
                Form        : Normalization_D_Form := NFD
             )  is
      procedure Add (Code : UTF8_Code_Point) is
      begin
         if Pointer > Destination'Last then
            raise Layout_Error; -- No room for output
         elsif Pointer = Destination'First then
            Destination (Pointer) := Code;
            Pointer := Pointer + 1;
            return;
         end if;
         declare
            Class : constant Canonical_Combining_Class :=
                             Combining_Class (Code);
            Index : Integer := Pointer;
         begin
            if Class = 0 then
               Destination (Pointer) := Code;
               Pointer := Pointer + 1;
               return;
            end if;
            while Index > Destination'First loop
               exit when Combining_Class (Destination (Index - 1))
                      <= Class;
               Index := Index - 1;
            end loop;
            if Index /= Pointer then
               Destination (Index + 1..Pointer) :=
                  Destination (Index..Pointer - 1);
            end if;
            Destination (Index) := Code;
            Pointer := Pointer + 1;
         end;
      end Add;
   begin
      if Pointer not in Destination'Range then
         raise Layout_Error;
      end if;
      declare
         This : constant Code_Point_Array_Ptr := Find (Code);
      begin
         if This = null then
            Add (Code);
         else
            declare
               Sequence : Code_Point_Array renames This.all;
            begin
               if Sequence (Sequence'First + 1) >= Compatibility then
                  if Form = NFD then
                     Add (Code);
                  else
                     Decompose
                     (  Destination,
                        Pointer,
                        Sequence (Sequence'First + 1) mod Compatibility,
                        Form
                     );
                     for Index in Sequence'First + 2..Sequence'Last loop
                        Add (Sequence (Index));
                     end loop;
                  end if;
               else
                  Decompose
                  (  Destination,
                     Pointer,
                     Sequence (Sequence'First + 1) mod Compatibility,
                     Form
                  );
                  for Index in Sequence'First + 2..Sequence'Last loop
                     Add (Sequence (Index));
                  end loop;
               end if;
            end;
         end if;
      end;
   end Decompose;

   procedure Decompose
             (  Destination : in out String;
                Pointer     : in out Integer;
                Code        : UTF8_Code_Point;
                Form        : Normalization_D_Form := NFD
             )  is
      Position : Integer := Pointer;

      procedure Add (Code : UTF8_Code_Point) is
         Representation : constant String := Image (Code);
      begin
         if Destination'Last - Position < Representation'Length - 1 then
            raise Layout_Error; -- No room for output
         elsif Position = Destination'First then
            Put (Destination, Position, Representation);
            return;
         end if;
         declare
            Class : constant Canonical_Combining_Class :=
                             Combining_Class (Code);
            Index : Integer := Position;
         begin
            if Class = 0 then
               Put (Destination, Position, Representation);
               return;
            end if;
            while Index > Destination'First loop
               declare
                  Location : Integer := Index;
                  Left     : UTF8_Code_Point;
               begin
                  Get_Backwards (Destination, Location, Left);
                  exit when Combining_Class (Left) <= Class;
                  Index := Location;
               end;
            end loop;
            if Index /= Position then
               Destination
               (  Index + Representation'Length
               .. Representation'Length + Position - 1
               )  := Destination (Index..Position - 1);
            end if;
            Destination (Index..Index + Representation'Length - 1) :=
               Representation;
            Position := Position + Representation'Length;
         end;
      end Add;
   begin
      if Position not in Destination'Range then
         raise Layout_Error;
      end if;
      declare
         This : constant Code_Point_Array_Ptr := Find (Code);
      begin
         if This = null then
            Add (Code);
         else
            declare
               Sequence : Code_Point_Array renames This.all;
            begin
               if Sequence (Sequence'First + 1) >= Compatibility then
                  if Form = NFD then
                     Add (Code);
                  else
                     Decompose
                     (  Destination,
                        Position,
                        Sequence (Sequence'First + 1) mod Compatibility,
                        Form
                     );
                     for Index in Sequence'First + 2..Sequence'Last loop
                        Add (Sequence (Index));
                     end loop;
                  end if;
               else
                  Decompose
                  (  Destination,
                     Position,
                     Sequence (Sequence'First + 1),
                     Form
                  );
                  for Index in Sequence'First + 2..Sequence'Last loop
                     Add (Sequence (Index));
                  end loop;
               end if;
            end;
         end if;
      end;
      Pointer := Position;
   end Decompose;

   function Is_Normalized
            (  Code : UTF8_Code_Point;
               Form : Normalization_D_Form := NFD
            )  return Boolean is
      This : constant Code_Point_Array_Ptr := Find (Code);
   begin
      if This = null then
         return True;
      else
         declare
            Sequence : Code_Point_Array renames This.all;
         begin
            if Sequence (Sequence'First + 1) >= Compatibility then
               if Form = NFD then
                  return True;
               else
                  return False;
               end if;
            else
               return False;
            end if;
         end;
      end if;
   end Is_Normalized;

   function Is_Normalized
            (  Source : String;
               Form   : Normalization_D_Form := NFD
            )  return Boolean is
      This     : UTF8_Code_Point;
      Pointer  : Integer := Source'First;
      Current  : Canonical_Combining_Class := 0;
      Previous : Canonical_Combining_Class := 0;
   begin
      while Pointer <= Source'Last loop
         Get (Source, Pointer, This);
         if not Is_Normalized (This, Form) then
            return False;
         end if;
         Current := Combining_Class (This);
         if Current > 0 and then Current <= Previous then
            return False;
         end if;
         Previous := Current;
      end loop;
      return True;
   end Is_Normalized;

   function Is_Normalized_Prefix
            (  Prefix     : String;
               Source     : String;
               Pointer    : access Integer;
               Map        : Unicode_Mapping;
               Form       : Normalization_C_Form := NFC;
               Normalized : Normalized_Type      := None
            )  return Boolean is
      L : UTF8_Code_Point_Array (1..Buffer_Size);
      R : UTF8_Code_Point_Array (1..Buffer_Size);
      I : Integer := Prefix'First;
      J : Integer := Pointer.all;
      As      : Normalization_D_Form;
      Code    : UTF8_Code_Point;
      Index   : Integer         := 1;
      L_Index : aliased Integer := 1;
      R_Index : aliased Integer := 1;
   begin
      if Normalized = Both then
         return Is_Prefix (Prefix, Source, Map);
      elsif J < Source'First then
         raise Layout_Error;
      elsif J > Source'Last then
         if J - 1 > Source'Last then
            raise Layout_Error;
         else
            return Prefix'Length = 0;
         end if;
      elsif Prefix'Length = 0 then
         return True;
      end if;
      case Form is
         when NFC =>
            As := NFD;
         when NFKC =>
            As := NFKD;
      end case;
      loop
         while I <= Prefix'Last loop -- Until first anchored code point
            Index := I;
            Get (Prefix, Index, Code);
            exit when L_Index > 1 and then Combining_Class (Code) = 0;
            if Normalized not in First..Both then
               Decompose (L, L_Index, Code, As);
            elsif L_Index > L'Last then
               raise Constraint_Error;
            else
               L (L_Index) := Code;
               L_Index := L_Index + 1;
            end if;
            I := Index;
         end loop;
         if Normalized not in First..Both then
            Compose (L (1..L_Index - 1), L_Index);
         end if;
         while J <= Source'Last loop -- Until first anchored code point
            Index := J;
            Get (Source, Index, Code);
            exit when R_Index > 1 and then Combining_Class (Code) = 0;
            if Normalized not in Both..Second then
               Decompose (R, R_Index, Code, As);
            end if;
            J := Index;
         end loop;
         if Normalized not in Both..Second then
            Compose (R (1..R_Index - 1), R_Index);
         elsif R_Index > R'Last then
            raise Constraint_Error;
         else
            R (R_Index) := Code;
            R_Index := R_Index + 1;
         end if;
         case Compare (L (1..L_Index - 1), R (1..R_Index - 1), Map) is
            when Less | Greater =>
               return False;
            when Equal =>
               if I > Prefix'Last then
                  Pointer.all := J;
                  return True;
               elsif J > Source'Last then
                  return False;
               end if;
               L_Index := 1;
               R_Index := 1;
         end case;
      end loop;
   exception
      when Layout_Error => -- Too many diacritic marks
         raise Constraint_Error;
   end Is_Normalized_Prefix;

   function Is_Normalized_Prefix
            (  Prefix     : String;
               Source     : String;
               Map        : Unicode_Mapping;
               Form       : Normalization_C_Form := NFC;
               Normalized : Normalized_Type      := None
            )  return Boolean is
      Pointer : aliased Integer := Source'First;
   begin
      return Is_Normalized_Prefix
             (  Prefix,
                Source,
                Pointer'Access,
                Map,
                Form,
                Normalized
             );
   end Is_Normalized_Prefix;

   function Is_Normalized_Prefix
            (  Prefix     : String;
               Source     : String;
               Pointer    : access Integer;
               Form       : Normalization_C_Form := NFC;
               Normalized : Normalized_Type      := None
            )  return Boolean is
      L : UTF8_Code_Point_Array (1..Buffer_Size);
      R : UTF8_Code_Point_Array (1..Buffer_Size);
      I : Integer := Prefix'First;
      J : Integer := Pointer.all;
      As      : Normalization_D_Form;
      Code    : UTF8_Code_Point;
      Index   : Integer := 1;
      L_Index : aliased Integer := 1;
      R_Index : aliased Integer := 1;
   begin
      if Normalized = Both then
         return Is_Prefix (Prefix, Source);
      elsif J < Source'First then
         raise Layout_Error;
      elsif J > Source'Last then
         if J - 1 > Source'Last then
            raise Layout_Error;
         else
            return Prefix'Length = 0;
         end if;
      elsif Prefix'Length = 0 then
         return True;
      end if;
      case Form is
         when NFC =>
            As := NFD;
         when NFKC =>
            As := NFKD;
      end case;
      loop
         while I <= Prefix'Last loop -- Until first anchored code point
            Index := I;
            Get (Prefix, Index, Code);
            exit when L_Index > 1 and then Combining_Class (Code) = 0;
            if Normalized not in First..Both then
               Decompose (L, L_Index, Code, As);
            elsif L_Index > L'Last then
               raise Constraint_Error;
            else
               L (L_Index) := Code;
               L_Index := L_Index + 1;
            end if;
            I := Index;
         end loop;
         if Normalized not in First..Both then
            Compose (L (1..L_Index - 1), L_Index);
         end if;
         while J <= Source'Last loop -- Until first anchored code point
            Index := J;
            Get (Source, Index, Code);
            exit when R_Index > 1 and then Combining_Class (Code) = 0;
            if Normalized not in Both..Second then
               Decompose (R, R_Index, Code, As);
            end if;
            J := Index;
         end loop;
         if Normalized not in Both..Second then
            Compose (R (1..R_Index - 1), R_Index);
         elsif R_Index > R'Last then
            raise Constraint_Error;
         else
            R (R_Index) := Code;
            R_Index := R_Index + 1;
         end if;
         case Compare (L (1..L_Index - 1), R (1..R_Index - 1)) is
            when Less | Greater =>
               return False;
            when Equal =>
               if I > Prefix'Last then
                  Pointer.all := J;
                  return True;
               elsif J > Source'Last then
                  return False;
               end if;
               L_Index := 1;
               R_Index := 1;
         end case;
      end loop;
   exception
      when Layout_Error => -- Too many diacritic marks
         raise Constraint_Error;
   end Is_Normalized_Prefix;

   function Is_Normalized_Prefix
            (  Prefix     : String;
               Source     : String;
               Form       : Normalization_C_Form := NFC;
               Normalized : Normalized_Type      := None
            )  return Boolean is
      Pointer : aliased Integer := Source'First;
   begin
      return Is_Normalized_Prefix
             (  Prefix,
                Source,
                Pointer'Access,
                Form,
                Normalized
             );
   end Is_Normalized_Prefix;

   function Normalize
            (  Code : UTF8_Code_Point;
               Form : Normalization_Form := NFD
            )  return UTF8_Code_Point_Array is
      Result  : UTF8_Code_Point_Array (1..Buffer_Size);
      Pointer : Integer := Result'First;
   begin
      case Form is
         when NFD =>
            Decompose (Result, Pointer, Code, NFD);
         when NFKD =>
            Decompose (Result, Pointer, Code, NFKD);
         when NFC =>
            Decompose (Result, Pointer, Code, NFD);
            Compose (Result (Result'First..Pointer - 1), Pointer);
         when NFKC =>
            Decompose (Result, Pointer, Code, NFKD);
            Compose (Result (Result'First..Pointer - 1), Pointer);
      end case;
      return Result (Result'First..Pointer - 1);
   end Normalize;

   function Normalize
            (  Code : UTF8_Code_Point;
               Form : Normalization_Form := NFD
            )  return String is
   begin
      return Image (Normalize (Code, Form));
   end Normalize;

   procedure Normalize
             (  Destination : in out String;
                Pointer     : in out Integer;
                Source      : String;
                Form        : Normalization_Form := NFD
             )  is
      From : Integer := Source'First;
      To   : Integer := Pointer;
      This : UTF8_Code_Point;
      As   : Normalization_D_Form;
   begin
      if Pointer < Destination'First then
         raise Layout_Error;
      elsif Pointer > Destination'Last then
         if Pointer - 1 > Destination'Last then
            raise Layout_Error;
         end if;
      end if;
      case Form is
         when NFD | NFKD =>
            As := Form;
         when NFC =>
            As := NFD;
         when NFKC =>
            As := NFKD;
      end case;
      case Form is
         when NFD | NFKD =>
            while From <= Source'Last loop
               Get (Source, From, This);
               Decompose (Destination, To, This, As);
            end loop;
         when NFC | NFKC =>
            declare
               Buffer : UTF8_Code_Point_Array (1..Buffer_Size);
               Index  : Integer := Buffer'First;
            begin
               while From <= Source'Last loop
                  Get (Source, From, This);
                  if Index > Buffer'First and then
                     Combining_Class (This) = 0
                  then  -- Flush the buffer
                     Compose (Buffer (Buffer'First..Index - 1), Index);
                     for Item in Buffer'First..Index - 1 loop
                        Put (Destination, To, Buffer (Item));
                     end loop;
                     Index := Buffer'First;
                  end if;
                  Decompose (Buffer, Index, This, As);
               end loop;
               if Index > Buffer'First then -- Flush the buffer
                  Compose (Buffer (Buffer'First..Index - 1), Index);
                  for Item in Buffer'First..Index - 1 loop
                     Put (Destination, To, Buffer (Item));
                  end loop;
               end if;
            exception
               when Layout_Error =>
                  raise Constraint_Error;
            end;
      end case;
      Pointer := To;
   end Normalize;

   function Normalize
            (  Source : String;
               Form   : Normalization_Form := NFD
            )  return String is
      Size : Natural := Source'Length * 2 + 1;
   begin
      loop
         declare
            Result  : String (1..Size);
            Pointer : Integer := Result'First;
         begin
            Normalize (Result, Pointer, Source, Form);
            return Result (Result'First..Pointer - 1);
         exception
            when Layout_Error =>
               Size := Size * 2;
         end;
      end loop;
   end Normalize;

end Strings_Edit.UTF8.Normalization;
