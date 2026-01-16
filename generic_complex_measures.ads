--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Complex_Measures                    Luebeck            --
--  Interface                                      Spring, 2023       --
--                                                                    --
--                                Last revision :  10:39 22 Apr 2023  --
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

with Units;  use Units;

with Ada.Numerics.Generic_Complex_Types;
with Measures;
with Units.Base;

generic
   with package Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (<>);
   with package Real_Measures is new Measures (Complex_Types.Real);
package Generic_Complex_Measures is
   pragma Preelaborate (Generic_Complex_Measures);
   use Complex_Types;
   use Real_Measures;

   subtype Number is Real;
   type Complex_Measure (SI : Unit := Units.Base.Unitless) is record
      Gain   : Complex;
      Offset : Number'Base := 0.0;
   end record;
   --
   -- Unary operations
   --
   function "abs" (Right : Complex_Measure) return Measure;
   function "+"   (Right : Complex_Measure) return Complex_Measure;
   function "-"   (Right : Complex_Measure) return Complex_Measure;
   --
   -- Exponentation
   --
   function "**"
            (  Left  : Complex_Measure;
               Right : Integer
            )  return Complex_Measure;
   --
   -- Multiplication
   --
   function "*"
            (  Left  : Number'Base;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "*"
            (  Left  : Imaginary;
               Right : Measure
            )  return Complex_Measure;
   function "*"
            (  Left  : Imaginary;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "*"
            (  Left  : Complex;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "*"
            (  Left  : Measure;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "*"
            (  Left  : Complex_Measure;
               Right : Number'Base
            )  return Complex_Measure;
   function "*"
            (  Left  : Complex_Measure;
               Right : Imaginary
            )  return Complex_Measure;
   function "*"
            (  Left  : Complex_Measure;
               Right : Complex
            )  return Complex_Measure;
   function "*"
            (  Left  : Complex_Measure;
               Right : Measure
            )  return Complex_Measure;
   function "*" (Left, Right : Complex_Measure) return Complex_Measure;
   --
   -- Division
   --
   function "/"
            (  Left  : Number'Base;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "/"
            (  Left  : Imaginary;
               Right : Measure
            )  return Complex_Measure;
   function "/"
            (  Left  : Imaginary;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "/"
            (  Left  : Complex;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "/"
            (  Left  : Measure;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "/"
            (  Left  : Complex_Measure;
               Right : Number'Base
            )  return Complex_Measure;
   function "/"
            (  Left  : Measure;
               Right : Imaginary
            )  return Complex_Measure;
   function "/"
            (  Left  : Complex_Measure;
               Right : Imaginary
            )  return Complex_Measure;
   function "/"
            (  Left  : Complex_Measure;
               Right : Complex
            )  return Complex_Measure;
   function "/"
            (  Left  : Complex_Measure;
               Right : Measure
            )  return Complex_Measure;
   function "/" (Left, Right : Complex_Measure) return Complex_Measure;
   --
   -- Scale shift (destructive)
   --
   function "and"
            (  Left  : Complex_Measure;
               Right : Number'Base
            )  return Complex_Measure;
   --
   -- Addition
   --
   function "+"
            (  Left  : Complex_Measure;
               Right : Measure
            ) return Complex_Measure;
   function "+"
            (  Left  : Measure;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "+" (Left, Right : Complex_Measure) return Complex_Measure;
   --
   -- Subtraction
   --
   function "-"
            (  Left  : Complex_Measure;
               Right : Measure
            )  return Complex_Measure;
   function "-"
            (  Left  : Measure;
               Right : Complex_Measure
            )  return Complex_Measure;
   function "-" (Left, Right : Complex_Measure) return Complex_Measure;
   --
   -- Equality
   --
   function "=" (Left, Right : Complex_Measure) return Boolean;
   --
   -- Re -- Real and imaginary parts
   --
   function Re (X : Complex_Measure) return Measure;
   function Im (X : Complex_Measure) return Measure;

   procedure Set_Re (X : in out Complex_Measure; Re : Measure);
   procedure Set_Im (X : in out Complex_Measure; Im : Measure);

   function Compose_From_Cartesian (Re, Im : Measure)
      return Complex_Measure;
   function Compose_From_Cartesian (Re : Measure)
      return Complex_Measure;
   --
   -- Polar coordinates
   --
   function Modulus (X : Complex_Measure) return Measure renames "abs";

   function Argument (X : Complex_Measure) return Number'Base;
   function Argument
            (  X     : Complex_Measure;
               Cycle : Number'Base
            )  return Number'Base;

   function Compose_From_Polar
            (  Modulus  : Measure;
               Argument : Number'Base
            )  return Complex_Measure;
   function Compose_From_Polar
            (  Modulus  : Measure;
               Argument : Number'Base;
               Cycle    : Number'Base
            )  return Complex_Measure;
   --
   -- Conjugate -- Exchange real and imaginary parts
   --
   function Conjugate (X : Complex_Measure) return Complex_Measure;
   --
   -- Get_Value -- Get SI value
   --
   --    Value - The complex measure
   --
   -- Returns :
   --
   --    SI equivalent of Value in numeric form
   --
   function Get_Value (Value : Complex_Measure) return Complex;
   --
   -- Get_Value_As -- Get value measured in non-SI units
   --
   --    Value - The complex measure of the value
   --    Scale - The complex measure of the result
   --
   -- This function is used to get the value in units other than SI. For
   -- instance:
   --
   --    GetValueAs (T, Celsius)  -- Temperature in Celsius degrees
   --
   -- Returns :
   --
   --    Scale equivalent of Value in numeric form
   --
   -- Exceptions :
   --
   --    Unit_Error -- Value and Scale have different units
   --
   function Get_Value_As
            (  Value : Complex_Measure;
               Scale : Measure
            )  return Complex;
   --
   -- Get_Unit -- Get unit
   --
   --    Value  - The complex measure
   --
   -- Returns :
   --
   --    SI component
   --
   function Get_Unit (Value : Complex_Measure) return Unit;
   --
   -- Convert -- Complex measure conversion
   --
   --    Value  - The complex measure of the value
   --    Scale  - The measure of the result
   --
   -- This  function is used  to convert  the complex  measure  Value to
   -- measurement units specified  by the parameter Scale.  When offsets
   -- of Value and Scale are same this is the null operation.
   --
   -- Returns :
   --
   --    Scale equivalent of Value
   --
   -- Exceptions :
   --
   --    Unit_Error -- Item and Scale have different units
   --
   function Convert
            (  Value : Complex_Measure;
               Scale : Measure
            )  return Complex_Measure;
   --
   -- Normalize -- Shift removing
   --
   --    Value  - The measure
   --
   -- This function is used to convert  the  measure  Value  to  to  its
   -- unshifted equivalent. The result has the field Gain = Value.Gain +
   -- Value.Offset and zero Offset.
   --
   -- Returns :
   --
   --    Unshifted equivalent of Value
   --
   function Normalize (Value : Complex_Measure) return Complex_Measure;
   --
   -- Shift -- Non-destructive shift
   --
   --    Value  - The measure
   --    Shift  - Shift
   --
   -- This function is used to convert  the complex measure Value to its
   -- shifted  equivalent.  The result has the field Gain = Value.Gain -
   -- Shift and the field Offset = Value.Offset + Shift.
   --
   -- Returns :
   --
   --    Shifted equivalent of Value
   --
   function Shift
            (  Value : Complex_Measure;
               Shift : Number'Base
            )  return Complex_Measure;
   --
   -- To_Measure -- Convert a number to the corresponding measure
   --
   --    Value - To be converted
   --
   -- Returns :
   --
   --    The dimensionless complex measure corresponding to the value
   --
   function To_Measure (Value : Complex) return Complex_Measure;

private
   pragma Inline (Get_Value);
   pragma Inline (Get_Value_As);
   pragma Inline (Get_Unit);
   pragma Inline (Convert);
   pragma Inline (Im);
   pragma Inline (Normalize);
   pragma Inline (Re);
   pragma Inline (Shift);
   pragma Inline (To_Measure);

end Generic_Complex_Measures;
