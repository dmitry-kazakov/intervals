--                                                                    --
--  package Units                   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  11:50 30 May 2014  --
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
-- This package defines the type Unit.  A value of Unit corresponds to a
-- SI unit. The package itself is rather useless.  It  is  used  by  the
-- package Measures, where the type Measure is  defined.  The  following
-- operations are defined for the type:
--
-- (o) Power, right operand is an integer;
-- (o) Multipication
-- (o) Division
-- (o) Square root (Sqrt)
--
-- The exponent part of any base SI component has range  -8..7.  If  the
-- compiler supports 64-bit modular arithmetic the range can be enlarged
-- up to -512..511, just by defining the type UnitPower as  Unsigned_64.
-- The  exception  Constraint_Error  is propagated when the result of an
-- operation is illegal. The function Sqrt  raises  Constraint_Error  if
-- any of components has odd power.
--
-- The child package Units.Base defines base SI units. The child package
-- Units.Constants defines some of derived SI units.
--
-- Some  words about the design. I didn't make Unit private because then
-- it  could  not  be  used  as  a discriminant of the type Measure (see
-- package Measures). To have Measure discriminated  seems  to  be  very
-- important. It allows to define subtypes of Measure such as:
--
--    subtype Speed is Measure (Velocity);  -- Unit is fixed
--
-- For  the  same reason and for performance sake too, Unit is a modular
-- number.
--
-- The package three:
--
--    Units
--      |_ Units.Base
--      |_ Units.Constants
--      |_ Units.Edit
--      |_ Units.UTF8_Edit
--
with Interfaces;

package Units is
   pragma Pure (Units);
   --
   -- Here we define the type used to represent units. It  should  be  a
   -- modular type with a defined operation Shift_Right.
   --
   subtype UnitPower is Interfaces.Unsigned_32;
   type Unit is new UnitPower;

   function "**" (Left : Unit; Right : Integer) return Unit;
   function "*"  (Left, Right : Unit) return Unit;
   function "/"  (Left, Right : Unit) return Unit;
   function Sqrt (X : Unit) return Unit;
   pragma Inline ("**", "*", "/", Sqrt);
   --
   -- The following operations are disallowed.
   --
   function "abs" (      Right : Unit) return Unit is abstract;
   function "and" (Left, Right : Unit) return Unit is abstract;
   function "mod" (Left, Right : Unit) return Unit is abstract;
   function "not" (      Right : Unit) return Unit is abstract;
   function "or"  (Left, Right : Unit) return Unit is abstract;
   function "rem" (Left, Right : Unit) return Unit is abstract;
   function "xor" (Left, Right : Unit) return Unit is abstract;

   function "+" (Right : Unit) return Unit is abstract;
   function "-" (Right : Unit) return Unit is abstract;

   function "+" (Left, Right : Unit) return Unit is abstract;
   function "-" (Left, Right : Unit) return Unit is abstract;

   Unit_Error : exception;
--
-- Code_Set -- Supported I/O code sets
--
--    ASCII_Set  - Plain ASCII (7-bit)
--    Latin1_Set - Latin-1 (8-bit)
--    UTF8_Set   - UTF-8 encoding
--
   type Code_Set is (ASCII_Set, Latin1_Set, UTF8_Set);
--
-- Split -- Integral unit value into components
--
--    SI          - The unit value
--    Current ... - The powers of the corresponding base unit
--
   procedure Split
             (  SI           : Unit;
                Current      : out Natural;
                Luminescence : out Natural;
                Temperature  : out Natural;
                Mass         : out Natural;
                Length       : out Natural;
                Quantity     : out Natural;
                Time         : out Natural
             );
private
   PowerBits    : constant := (UnitPower'Size - 1) / 7;
   Current      : constant := 2**(PowerBits*0);
   Luminescence : constant := 2**(PowerBits*1);
   Temperature  : constant := 2**(PowerBits*2);
   Mass         : constant := 2**(PowerBits*3);
   Length       : constant := 2**(PowerBits*4);
   Quantity     : constant := 2**(PowerBits*5);
   Time         : constant := 2**(PowerBits*6);
   Unitless     : constant := 0;
end Units;
