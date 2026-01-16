--                                                                    --
--  package Measures_UTF8_Edit      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2005       --
--                                                                    --
--                                Last revision :  13:42 19 Mar 2011  --
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
--
-- This  package  provides  edit  facilities for the type Measure. It is
-- similar to Measures_Edit but uses UTF-8 strings.
--
-- The package Float_Measures_UTF8_Edit is a non-generic version of  the
-- package based on the type Float.
--
with Measures_Irregular;
with Measures_Universal_Edit;
with Strings_Edit.Float_Edit;

generic
   with package Irregular_Measures is new Measures_Irregular (<>);
   with package Float_Edit is
      new Strings_Edit.Float_Edit
            (Irregular_Measures.Measures_Of.Number);
package Measures_UTF8_Edit is
   package Float_Edit_Of renames Float_Edit;
   package Irregular_Measures_Of renames Irregular_Measures;
   package Measures_Of renames Irregular_Measures.Measures_Of;
   use Measures_Of;
--
-- Get -- Get a measure from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--
-- This procedure gets a measure from the string Source. The  process
-- starts  from  the  Source  (Pointer)  position.  After  successful
-- completion  Pointer  is  advanced  to  the  position following the
-- measure. The parameter Value accepts the measure.
--
-- The measure syntax:
--
--    <measure> ::= (<measure>)
--    <measure> ::= <measure> [<dyadic-operation>] <measure>
--    <measure> ::= <prefix-operation> <measure>
--    <measure> ::= <measure> <postfix-operation>
--    <measure> ::= <number>
--    <measure> ::= <unit>
--    <dyadic-operation>  ::= ** | ^ | * | · | / | + | - | and
--    <prefix-operation>  ::= + | -
--    <postfix-operation> ::= <superscript-number>
--
-- Here  <unit>  is  a  name  denoting a measurement unit, such as foot.
-- Multiplication has higher priority than division.  Hence:  m/s*kg  is
-- interpreted as m/(s*kg).
--
-- Examples :
--
--    34.5 * mm
--    65·km/h         -- 65 * km/h
--    65km/h          -- same
--    K and 273.15    -- degree Celsius
--    yd^2            -- square yard
--    lb·yd²/s²       -- using superscript
--
-- The  variant  Get_UTF8  functions  lile Get with Latin1 = True but
-- uses UTF-8 encoding. It also supports upper-case omega for Ohm.
--
-- Exceptions :
--
--    Constraint_Error - Numeric error in unit expression
--    Data_Error       - Syntax error
--    End_Error        - There is no measure in the string
--    Layout_Error     - Pointer not in Source'First..Source'Last+1
--    Unit_Error       - Illegal unit expression (like m/°C)
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure
             );
--
-- Get -- Get measure as a scaled value
--
--    Source    - The string to be processed
--    Pointer   - The current position in the string
--    Value     - The result
--    Mode      - The code set
--
-- This is an advanced variant of Get with additional information  about
-- canonic representation returned. The canonic representation is:
--
--    <numeral> [<scale>]
--
-- Where <numeral>  is  a  plain  number  and  scale  is  a  dimemsional
-- multiplicand. For example:
--
--                      Numeral      Scale
--      -12.4 km/h        -12.4      km/h
--           3 / s            3      1/s
--     10 * 4 feet           10      4 feet
--    (2**2 + 1) m     2**2 + 1      m
--       2 / 3 / 4            2      1 / 3 / 4
--
-- The follwing are examples of non-canonic representations:
--
--    km/h   (1 m)*s
--
-- Exceptions :
--
--    Constraint_Error - Numeric error in unit expression
--    Data_Error       - Syntax error
--    End_Error        - There is no measure in the string
--    Layout_Error     - Pointer not in Source'First..Source'Last+1
--    Unit_Error       - Illegal unit expression (like m/°C)
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Scaled
             );
--
-- Get_Unit -- Get a measure unit from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--
-- This  procedure is a variant of Get restricted to only measure units.
-- It  does  not  recognize  numbers and unit operations. The only valid
-- input are units like foot or meter. Irregular units and units with SI
-- prefixes are recognized as well. So km is legal.
--
-- Exceptions :
--
--    Constraint_Error - Numeric error in unit expression
--    Data_Error       - Syntax error
--    End_Error        - There is no measure in the string
--    Unit_Error       - Illegal unit expression (like m/°C)
--
   procedure Get_Unit
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure
             );
--
-- Value -- String to measure conversion
--
--    Source - The string to be processed
--
-- This function gets the measure from  the  string  Source.  The  whole
-- Source  string  should be matched. Otherwise the exception Data_Error
-- is propagated. For the measure syntax see Get.
--
-- Returns :
--
--    The value
--
-- Exceptions:
--
--    Constraint_Error - Numeric error in unit expression
--    Data_Error       - Syntax error
--    End_Error        - There is no measure in the string
--    Unit_Error       - Illegal unit expression (like m/°C)
--
   function Value (Source : String) return Measure;
--
-- Put -- Put a measure into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The measure to be put
--    Derived     - Derived SI units use flag
--    RelSmall    - Relative precision of the output
--    AbsSmall    - Absolute one
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This  procedure  places  the measure specified by the parameter Value
-- into  the  output  string Destination. The string is written starting
-- from Destination (Pointer). The parameter Derived if true, allows use
-- of derived SI units (such as N, F and etc.). The parameters  RelSmall
-- and AbsSmall  specify  the  precision  of  the  numeric  output  (see
-- Strings_Edit.Float_Edit  for  further  information). After successful
-- completion Pointer is advanced to the first character  following  the
-- output or to Destination'Last + 1.
--
-- A measure is output in one of the following forms:
--
--     <gain> · <unit> and <offset>
--     <unit> and <offset>             -- Gain is 1.0
--     <gain> · <unit>                 -- Offset is 0.0
--     <unit>                          -- Gain is 1.0, Offset is 0.0
--     <gain> and <offset>             -- Shifted unitless
--     <gain>                          -- Unitless
--
-- When the parameter Field is not zero then Justify specifies alignment
-- and Fill is the character used for filling.  When  Field  is  greater
-- than Destination'Last - Pointer + 1,  the  latter  is  used  instead.
-- After  successful  completion  Pointer  is  advanced  to  the   first
-- character following the output or to Destination'Last + 1.
--
-- Exceptions:
--
--    Layout_Error -- Pointer is not in Destination'Range or there is no
--                    room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Measure;
                Derived     : Boolean  := True;
                RelSmall    : Positive := Strings_Edit.MaxSmall;
                AbsSmall    : Integer  :=-Strings_Edit.MaxSmall;
                Field       : Natural  := 0;
                Justify     : Strings_Edit.Alignment :=
                                 Strings_Edit.Left;
                Fill        : Character := ' '
             );
--
-- Image -- Measure to string conversion
--
--    Value       - The value to be converted
--    Derived     - Derived SI units use flag
--    RelSmall    - Relative precision of the output
--    AbsSmall    - Absolute one
--
-- This procedure converts the parameter Value to string. The parameters
-- Derived, RelSmall and AbsSmall have same meaning as in Put (see).
--
-- Returns :
--
--	 The result string
--
   function Image
            (  Value    : Measure;
               Derived  : Boolean := True;
               RelSmall : Positive := Strings_Edit.MaxSmall;
               AbsSmall : Integer  :=-Strings_Edit.MaxSmall
            )  return String;
--
-- Universal_Edit -- Instantiation of Measures_Universal_Edit
--
   package Universal_Edit is
      new Measures_Universal_Edit
          (  Irregular_Measures => Irregular_Measures,
             Float_Edit         => Float_Edit
          );

end Measures_UTF8_Edit;
