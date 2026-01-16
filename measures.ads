--                                                                    --
--  package Measures                Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2000       --
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
--
-- This package provides the type Measure. A value  of  Measure  denotes
-- some quantity of a physical unit. Measures can be added,  subtracted,
-- multiplied  and divided. The package is generic. The parameter Number
-- specifies the floating-point type used for Measure.
--
-- The type Measure is a discriminated record type. The discriminant  SI
-- has the type Unit (see units.ads). Unconstrained instances of Measure
-- can be declared as:
--
--    Entity : Measure;  -- May hold value of any measurement unit
--       . . .
--    Entity := 5.0 * A / s;  -- Set value 5 A/s
--
-- It is possible however, to create  constrained  subtypes  of  Measure
-- capable  to  hold  values  of  only  a specific measurement unit. For
-- instance:
--
--    subtype Speed is Measure (Velocity);
--       . . .
--    Car_Speed : Speed;  -- Only velocities are allowed
--       . . .
--    Car_Speed := 10.0 * km / h;  -- OK
--    Car_Speed := A;              -- Illegal, Constraint_Error
--
-- The  type  Measure  has  two components: Gain and Offset. To get a SI
-- value of a measure one should sum Gain and Offset. This is  what  the
-- function GetValue actually does. For instance:
--
--    Meter : Measure := (Length, Gain=>1.0,    Offset=>0.0);
--    Foot  : Measure := (Length, Gain=>0.3048, Offset=>0.0);
--
-- Shifted  measurement units, like degree Celsius can be obtained using
-- the fieid Offset:
--
--    Celsius : Measure := (Temperature, Gain=>1.0, Offset=>273.15);
--
-- Actually,  Offset  should  also  be  a   discriminant   of   Measure.
-- Unfortunately  it is illegal in Ada. Measures with same unit and same
-- offset  form  a  set  closed  relatively  addition,  subtraction  and
-- multiplication to a number. Therefore:
--
--    5.0 * Celsius        -- Legal, 5 C = 278.15 K
--    Celsius + Celsius    -- Legal, 2 C = 275.15 K
--    Celsius * Celsius    -- Illegal - Unit_Error is propagated
--
-- Addition and subtraction are only legal when units  and  offsets  are
-- same.
--
--    Kelvin : Measure := (Temperature, Gain=>1.0);
--
--    Celsius + Kelvin     -- Illegal - Unit_Error is propagated
--    Celsius * Kelvin     -- Illegal - Unit_Error is propagated
--
-- The result offset should be explicitly specified:
--
--    Convert (0.0 * Celsius, Kelvin) + 0.0 * Kelvin   -- OK, +273.15 K
--    0.0 * Celsius + Convert (0.0 * Kelvin, Celsius)  -- OK, -273.15 C
--
-- Measures with zero Offset can be multiplied and divided:
--
--    Meter * Foot         -- 0.3048 square meters
--    Meter + Foot         -- 1.3048 m
--    Meter ** 2           -- 1 square meter
--
-- The  package  Float_Measures  is a non-generic version of the package
-- Measures based on the type Float.
--
-- The package three:
--
--    Measures
--    Measures_Derived     -- Derived SI units as constants of Measure
--    Measures_Irregular   -- Irregular units
--    Measures_Edit        -- Measure <-> String conversions
--
-- Non-generic versions (based on Float):
--
--    Float_Measure
--    Float_Derived_Measures
--    Float_Irregular_Measures
--    Float_Measures_Edit
--
-- Implementation specification packages (library level instantiations)
--
--    PrefixTable, UnitTable
--
with Units;      use Units;
with Units.Base;

generic
   type Number is digits <>;
package Measures is
   pragma Preelaborate (Measures);

   type Measure (SI : Unit := Units.Base.Unitless) is record
      Gain   : Number;
      Offset : Number'Base := 0.0;
   end record;

   subtype Dimensionless is Measure (Units.Base.Unitless);
   --
   -- Unary operations
   --
   function "abs" (Right : Measure) return Measure;
   function "+"   (Right : Measure) return Measure;
   function "-"   (Right : Measure) return Measure;
   --
   -- Exponentation
   --
   function "**" (Left : Measure; Right : Integer) return Measure;
   --
   -- Multiplication
   --
   function "*" (Left, Right : Measure) return Measure;
   function "*" (Left : Number'Base; Right : Measure) return Measure;
   function "*" (Left : Measure; Right : Number'Base) return Measure;
   --
   -- Division
   --
   function "/" (Left, Right : Measure) return Measure;
   function "/" (Left : Number'Base; Right : Measure) return Measure;
   function "/" (Left : Measure; Right : Number'Base) return Measure;
   --
   -- Scale shift (destructive)
   --
   function "and" (Left : Measure; Right : Number'Base) return Measure;
   --
   -- Addition and subtraction
   --
   function "+"  (Left, Right : Measure) return Measure;
   function "-"  (Left, Right : Measure) return Measure;
   pragma Inline ("abs", "and", "**", "*", "/", "+", "-");
   --
   -- Comparisons
   --
   function ">"  (Left, Right : Measure) return Boolean;
   function "<"  (Left, Right : Measure) return Boolean;
   function "="  (Left, Right : Measure) return Boolean;
   function ">=" (Left, Right : Measure) return Boolean;
   function "<=" (Left, Right : Measure) return Boolean;
   pragma Inline (">", "<", "=", ">=", "<=");
   --
   -- Get_Value -- Get SI value
   --
   --    Value  - The measure
   --
   -- Returns :
   --
   --    SI equivalent of Value in numeric form
   --
   function Get_Value (Value : Measure) return Number;
   pragma Inline (Get_Value);
   --
   -- Get_Value_As -- Get value measured in non-SI units
   --
   --    Value  - The measure of the value
   --    Scale  - The measure of the result
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
   function Get_Value_As (Value, Scale : Measure) return Number;
   pragma Inline (Get_Value_As);
   --
   -- Get_Unit -- Get unit
   --
   --    Value  - The measure
   --
   -- Returns :
   --
   --    SI component
   --
   function Get_Unit (Value : Measure) return Unit;
   pragma Inline (Get_Unit);
   --
   -- Convert -- Measure conversion
   --
   --    Value  - The measure of the value
   --    Scale  - The measure of the result
   --
   -- This function is used to convert the measure Value to  measurement
   -- units specified by the parameter Scale. When offsets of Value  and
   -- Scale are same this is the null operation.
   --
   --    Convert (T, Celsius)  -- Temperature in Celsius degrees
   --
   -- Returns :
   --
   --    Scale equivalent of Value
   --
   -- Exceptions :
   --
   --    Unit_Error -- Item and Scale have different units
   --
   function Convert (Value, Scale : Measure) return Measure;
   pragma Inline (Convert);
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
   function Normalize (Value : Measure) return Measure;
   pragma Inline (Normalize);
   --
   -- Shift -- Non-destructive shift
   --
   --    Value  - The measure
   --    Shift  - Shift
   --
   -- This function is used to convert the measure  Value to its shifted
   -- equivalent. The result has the field Gain = Value.Gain - Shift and
   -- the field Offset = Value.Offset + Shift.
   --
   -- Returns :
   --
   --    Shifted equivalent of Value
   --
   function Shift (Value : Measure; Shift : Number'Base) return Measure;
   pragma Inline (Shift);
   --
   -- To_Measure -- Convert a number to the corresponding measure
   --
   --    Value - To be converted
   --
   -- Returns :
   --
   --    The dimensionless measure corresponding to the value
   --
   function To_Measure (Value : Number) return Measure;
   pragma Inline (To_Measure);
   --
   -- Base SI measurement units
   --
   A   : aliased constant Measure := (Units.Base.Current,      1.0, 0.0);
   K   : aliased constant Measure := (Units.Base.Temperature,  1.0, 0.0);
   cd  : aliased constant Measure := (Units.Base.Luminescence, 1.0, 0.0);
   kg  : aliased constant Measure := (Units.Base.Mass,         1.0, 0.0);
   m   : aliased constant Measure := (Units.Base.Length,       1.0, 0.0);
   mol : aliased constant Measure := (Units.Base.Quantity,     1.0, 0.0);
   s   : aliased constant Measure := (Units.Base.Time,         1.0, 0.0);
   --
   -- Unitless SI things
   --
   Np  : aliased constant Measure := (Units.Base.Unitless, 1.0, 0.0);
   rad : aliased constant Measure := (Units.Base.Unitless, 1.0, 0.0);
   sr  : aliased constant Measure := (Units.Base.Unitless, 1.0, 0.0);
--
-- Scaled -- Scaled value
--
-- The discriminant Format determines the value representation;
--
--    Scaler  - A scalar numeral
--    Numeric - A scalar numeral multiplied by a numeric scale.
--    Canonic - A scalar numeral multiplied by a dimensioned scale
--    Jumbled - Others
--
-- The full value is the product of Numeral and Scale.  When  Format  is
-- Scalar, the component Scale is 1  SI.  When  Format  is  Numeric  the
-- component  Scale  is  dimensionless.  When  Format  is  Jumbled   the
-- component Numeral is 1.0.
--
   type Value_Format is (Scalar, Numeric, Canonic, Jumbled);
   type Scaled (Format : Value_Format := Canonic) is record
      Numeral : Number'Base;
      Scale   : Measure;
   end record;

end Measures;
