--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Measures_Generic_Edit                      Luebeck            --
--  Interface                                      Spring, 2005       --
--                                                                    --
--                                Last revision :  09:59 09 Apr 2016  --
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
-- This  package  provides  edit  facilities for the type Measure in the
-- encoding  of  choice.  The package Float_Measures_Universal_Edit is a
-- non-generic  version  of  the  package  based  on the type Float. The
-- formal generic parameters are:
--
-- (o)  Irregular_Measures  is  an  instance  of Measures_Irregular. The
--      type Measure is taken from there;
-- (o)  Float_Edit  is  an  instance of Strings_Edit.Float_Edit based on
--      the same floating-point type as Irregular_Measures;
-- (o)  Get_Superscript is a procedure used to input unit powers;
-- (o)  Get_Image is a function to convert a unit to string.
--
with Units;         use Units;
with Strings_Edit;  use Strings_Edit;

with Measures_Irregular;
with Strings_Edit.Float_Edit;

generic
   with package Irregular_Measures is new Measures_Irregular (<>);
   with package Float_Edit is
      new Strings_Edit.Float_Edit
          (  Irregular_Measures.Derived_Measures.Measures.Number
          );
   with procedure Get_Superscript
                  (  Source  : String;
                     Pointer : in out Integer;
                     Number  : out Integer;
                     Mode    : Code_Set
                  );
   with function Image
                 (  Value : Unit;
                    Mode  : Code_Set
                 )  return String;
package Measures_Generic_Edit is
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
--    Mode    - The code set
--
-- This  procedure  gets  a  measure from the string Source. The process
-- starts  from  the  Source  (Pointer)   position.   After   successful
-- completion Pointer is advanced to the position following the measure.
-- The parameter Value accepts the measure.
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
-- The variant UTF8 functions like Get  with  Latin1  =  True  but  uses
-- UTF-encoding. It also supports upper-case omega for Ohm, Kelving sign
-- etc.
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
                Value   : out Measure;
                Mode    : Code_Set
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
                Value   : out Scaled;
                Mode    : Code_Set
             );
--
-- Get_Unit -- Get a measure unit from the string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Mode    - The code set
--
-- This procedure is a restricted variant of Get. It also gets a measure
-- from the string Source, but the input is restricted to  only  measure
-- units, such as foor, meter etc. Irregular units  and  units  with  SI
-- prefixes are recognized.
--
-- Exceptions :
--
--    Data_Error       - Syntax error
--    End_Error        - There is no measure in the string
--    Layout_Error     - Pointer not in Source'First..Source'Last+1
--
   procedure Get_Unit
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure;
                Mode    : Code_Set
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
--    Constraint_Error - The number is not in First..Last
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--
   function Value (Source : String; Mode : Code_Set) return Measure;
--
-- Put -- Put a measure into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The measure to be put
--    Mode        - The code set
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
-- Strings_Edit.Float_Edit  for  further  information).  The  parameters
-- Field,  Justify and Fill are described in Strings_Edit.
--
-- When the parameter Field is not zero then Justify specifies alignment
-- and Fill is the character used for filling.  When  Field  is  greater
-- than Destination'Last - Pointer + 1,  the  latter  is  used  instead.
-- After  successful  completion  Pointer  is  advanced  to  the   first
-- character following the output or to Destination'Last + 1.
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
-- Exceptions:
--
--    Layout_Error -- Pointer is not in Destination'Range or there
--                    is no room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Measure;
                Mode        : Code_Set;
                Derived     : Boolean   := True;
                RelSmall    : Positive  := MaxSmall;
                AbsSmall    : Integer   :=-MaxSmall;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Image -- Measure to string conversion
--
--    Value    - The value to be converted
--    Mode     - The code set
--    Derived  - Derived SI units use flag
--    RelSmall - Relative precision of the output
--    AbsSmall - Absolute one
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
               Mode     : Code_Set;
               Derived  : Boolean  := True;
               RelSmall : Positive := MaxSmall;
               AbsSmall : Integer  :=-MaxSmall
            )  return String;

end Measures_Generic_Edit;
