--                                                                    --
--  package Intervals.Measures      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2005       --
--                                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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
--  This  package   provides   interval   arithmetic   for   dimensioned
--  floating-point  numbers.  No  overflow  checks  made explicitly. The
--  package is generic, the formal parameters are:
--
--  (o)  Float_Intervals - Plain intervals with floating-point bounds
--  (o)  Float_Measures  - A floating-point type of dimensioned values
--
with Units;  use Units;

with Intervals.Floats;
with Measures;
with Units.Base;

generic
   with package Float_Intervals is new Intervals.Floats (<>);
   use Float_Intervals;
   with package Float_Measures is new Standard.Measures (Number);
package Intervals.Measures is
   package Float_Intervals_Of renames Float_Intervals;
   package Float_Measures_Of  renames Float_Measures;

   type Interval_Measure (SI : Unit := Units.Base.Unitless) is record
      From   : Number;
      To     : Number;
      Offset : Number'Base;
   end record;

   use Float_Measures_Of;
--
-- Convert -- Measure conversion
--
--    Value - The interval of measurements to convert
--    Scale - The measure of the result
--
-- This  function  is  used to convert the interval measure Value to the
-- measurement units specified by the parameter Scale. When  offsets  of
-- Value and Scale are same this is the null operation.
--
--    Convert (T, Celsius)  -- Temperature range in Celsius degrees
--
-- Returns :
--
--    Scale equivalent of Value
--
-- Exceptions :
--
--    Unit_Error - Item and Scale have different units
--
   function Convert (Value : Interval_Measure; Scale : Measure)
      return Interval_Measure;
--
-- Distance -- Interval distance
--
--    Left  - The first argument
--    Right - The second argument
--
-- Distance can be used to compare intervals as follows:
--
--    if Distance (X, Y) <= Limit then
--       -- X and Y are close enough
--    else
--       -- They are not
--    end if;
--
-- Which is an equivalent to rather heavyweighted:
--
--    case abs (X - Y) <= (Limit, Limit) is
--       when True =>
--          -- X and Y are close enough
--       when False | Uncertain =>
--          -- They are not
--    end case;
--
-- Returns :
--
--    max(|a-c|,|b-d|)
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function Distance (Left, Right : Interval_Measure)
      return Measure;
   function Distance (Left : Measure; Right : Interval_Measure)
      return Measure;
   function Distance (Left : Interval_Measure; Right : Measure)
      return Measure;
--
-- From -- The interval's lower bound
--
--    Left - The argument
--
-- Returns :
--
--    The lower bound of the interval
--
   function From (Left : Interval_Measure) return Measure;
--
-- Get_Value -- Get SI value
--
--    Value  - The interval measure
--
-- Returns :
--
--    SI equivalent of Value as a plain interval
--
   function Get_Value (Value : Interval_Measure) return Interval;
--
-- Get_Value_As -- Get value measured in non-SI units
--
--    Value  - The measure of the value
--    Scale  - The measure of the result
--
-- This  function  is  used to get the value in units other than SI. For
-- instance:
--
--    Get_Value_As (T, Celsius)  -- Temperatures range in Celsius degrees
--
-- Returns :
--
--    Scale equivalent of Value in as a plain interval
--
-- Exceptions :
--
--    Unit_Error -- Value and Scale have different units
--
   function Get_Value_As (Value : Interval_Measure; Scale : Measure)
      return Interval;
--
-- Get_Unit -- Get unit
--
--    Value  - The measure
--
-- Returns :
--
--    SI component
--
   function Get_Unit (Value : Interval_Measure) return Unit;
--
-- Is_In -- Check if an interval or a measure belongs to other interval
--
--    Left  - The measure or interval
--    Right - The interval
--
-- Returns :
--
--    True if Left is in Right
--
   function Is_In (Left, Right : Interval_Measure) return Boolean;
   function Is_In (Left : Measure; Right : Interval_Measure)
      return Boolean;
--
-- Is_Negative -- Check if all measures within interval are negative
--
--    Left - The argument
--
-- Note that a non-negative  interval  is  not  necessarily  a  positive
-- interval.
--
-- Returns :
--
--    True if the interval is negative
--
   function Is_Negative (Left : Interval_Measure) return Boolean;
--
-- Is_Positive -- Check if all Measures within interval are positive
--
--    Left - The argument
--
-- Note that a non-positive  interval  is  not  necessarily  a  negative
-- interval.
--
-- Returns :
--
--    True if the interval is positive
--
   function Is_Positive (Left : Interval_Measure) return Boolean;
--
-- Length -- Get the interval length
--
--    Left - The argument
--
-- When the interval contains only one measure then its length is defined
-- as zero. Thus the number of measures in the interval is Length + 1.
--
-- Returns :
--
--    The interval length = b - a
--
   function Length (Left : Interval_Measure) return Measure;
--
-- Normalize -- Shift removing
--
--    Value - The interval of measurements
--
-- This  function  is  used  to  convert  Value  to  to  its   unshifted
-- equivalent.
--
-- Returns :
--
--    Unshifted equivalent of Value
--
   function Normalize (Value : Interval_Measure)
      return Interval_Measure;
--
-- Shift -- Non-destructive shift
--
--    Value  - The interval of measurements
--    Shift  - Shift
--
-- This  function  is  used  to  convert  the  Value  to  to its shifted
-- equivalent.
--
-- Returns :
--
--    Shifted equivalent of Value
--
   function Shift (Value : Interval_Measure; Shift : Number'Base)
      return Interval_Measure;
--
-- To -- The interval's upper bound
--
--    Left - The argument
--
-- Returns :
--
--    The upper bound of the interval
--
   function To (Left : Interval_Measure) return Measure;
--
-- To_Interval_Measure -- Get interval from bounds
--
--    Left    - The lower bound
--  [ Right ] - The upper bound
--
-- Returns :
--
--    [Left, Right]
--
-- Exceptions :
--
--    Constraint_Error - Left is not less or equal to Right
--    Unit_Error       - Incompatible units
--
   function To_Interval_Measure (Left, Right : Measure)
      return Interval_Measure;
   function To_Interval_Measure (Left, Right : Number)
      return Interval_Measure;
   function To_Interval_Measure (Left : Measure)
      return Interval_Measure;
   function To_Interval_Measure (Left : Number)
      return Interval_Measure;
   function To_Interval_Measure (Left : Interval)
      return Interval_Measure;
--
-- abs -- Absolute value
--
--    Left - The argument
--
-- The operation is defined as |[a,b]| = [min(|a|,|b|), max(|a|,|b|)].
--
-- Returns :
--
--    Absolute value
--
   function "abs" (Left : Interval_Measure) return Interval_Measure;
--
-- + -- Unary plus
--
--    Left - The operand
--
-- Returns :
--
--    Left
--
   function "+" (Left : Interval_Measure) return Interval_Measure;
--
-- - -- Unary minus
--
--    Left - The operand
--
-- Returns :
--
--    [-b,-a]
--
   function "-" (Left : Interval_Measure) return Interval_Measure;
--
-- & -- Check if intervals intersect
--
--    Left  - The argument
--    Right - The argument
--
-- Differently shifted arguments are allowed.
--
-- Returns :
--
--    True if the intersection of intervals is not empty
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function "&" (Left, Right : Interval_Measure) return Boolean;
--
-- + -- Addition
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either both arguments are intervals or one of them is a measure.  The
-- result  is always an interval. It is defined as [a,b] + [c,d] = [a+c,
-- b+d].
--
-- Returns :
--
--      Left + Right
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function "+" (Left, Right : Interval_Measure)
      return Interval_Measure;
   function "+" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure;
   function "+" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure;
--
-- - -- Substraction
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either both arguments are intervals or one of them is a measure.  The
-- result  is always an interval. It is defined as [a,b] - [c,d] = [a-d,
-- b-c].
--
-- Returns :
--
--    Left - Right
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function "-" (Left, Right : Interval_Measure)
      return Interval_Measure;
   function "-" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure;
   function "-" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure;
--
-- * -- Multiplication
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either both arguments are intervals or one of them is a measure.  The
-- result is always an interval, except for the case when  a  dimensioed
-- number is multiplied to a plain interval. The operation is defined as
-- [a, b] * [c, d] = [min (a*c,a*d,b*c,b*d), max (a*c,a*d,b*c,b*d)].
--
-- Returns :
--
--    Left * Right
--
-- Exceptions :
--
--    Constraint_Error - Unit power overflow
--    Unit_Error       - Incompatible units of wrong unit of the result
--
   function "*" (Left, Right : Interval_Measure)
      return Interval_Measure;
   function "*" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure;
   function "*" (Left : Interval_Measure; Right : Interval)
      return Interval_Measure;
   function "*" (Left : Interval_Measure; Right : Number'Base)
      return Interval_Measure;
   function "*" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure;
   function "*" (Left : Interval; Right : Interval_Measure)
      return Interval_Measure;
   function "*" (Left : Number'Base; Right : Interval_Measure)
      return Interval_Measure;
   function "*" (Left : Measure; Right : Interval)
      return Interval_Measure;
   function "*" (Left : Interval; Right : Measure)
      return Interval_Measure;
--
-- / -- Division
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either both arguments are intervals or one of them is a measure.  The
-- result is always an interval, except for the case when  a  dimensioed
-- number and a plain interval are divided by each other. It is  defined
-- as [a, b] / [c, d] = [a, b] * [1/d, 1/c]. The result is undefined  if
-- the divisor contains 0. In this case Constraint_Error is propagated.
--
-- Returns :
--
--    Left / Right
--
-- Exceptions :
--
--    Constraint_Error - Zero divide, unit power overflow
--    Unit_Error       - Incompatible units
--
   function "/" (Left, Right : Interval_Measure) return Interval_Measure;
   function "/" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure;
   function "/" (Left : Interval_Measure; Right : Interval)
      return Interval_Measure;
   function "/" (Left : Interval_Measure; Right : Number'Base)
      return Interval_Measure;
   function "/" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure;
   function "/" (Left : Interval; Right : Interval_Measure)
      return Interval_Measure;
   function "/" (Left : Number'Base; Right : Interval_Measure)
      return Interval_Measure;
   function "/" (Left : Measure; Right : Interval)
      return Interval_Measure;
   function "/" (Left : Interval; Right : Measure)
      return Interval_Measure;
--
-- ** -- Power
--
--    Left  - The first operand
--    Right - The second operand (natural)
--
-- Returns :
--
--    Left ** Right
--
-- Exceptions :
--
--    Constraint_Error - Unit power overflow
--
   function "**" (Left : Interval_Measure; Right : Natural)
      return Interval_Measure;
--
-- >, >=, <, <= -- Comparisons
--
--    Left  - The first operand
--    Right - The second operand
--
-- Comparisons are allowed for differently shifted arguments. One of the
-- arguments can be measure or plain interval.
--
-- Returns :
--
--    The result of comparison
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function ">" (Left, Right : Interval_Measure)
      return Logical;
   function ">" (Left : Interval_Measure; Right : Measure)
      return Logical;
   function ">" (Left : Measure; Right : Interval_Measure)
      return Logical;
   function ">=" (Left, Right : Interval_Measure)
      return Logical;
   function ">=" (Left : Interval_Measure; Right : Measure)
      return Logical;
   function ">=" (Left : Measure; Right : Interval_Measure)
      return Logical;
   function "<=" (Left, Right : Interval_Measure)
      return Logical;
   function "<=" (Left : Interval_Measure; Right : Measure)
      return Logical;
   function "<=" (Left : Measure; Right : Interval_Measure)
      return Logical;
   function "<"  (Left, Right : Interval_Measure)
      return Logical;
   function "<"  (Left : Interval_Measure; Right : Measure)
      return Logical;
   function "<"  (Left : Measure; Right : Interval_Measure)
      return Logical;

private
   pragma Inline (Convert);
   pragma Inline (Distance);
   pragma Inline (From);
   pragma Inline (Get_Value);
   pragma Inline (Get_Value_As);
   pragma Inline (Get_Unit);
   pragma Inline (Is_In);
   pragma Inline (Is_Negative);
   pragma Inline (Is_Positive);
   pragma Inline (Length);
   pragma Inline (Normalize);
   pragma Inline (Shift);
   pragma Inline (To);
   pragma Inline (To_Interval_Measure);
   pragma Inline ("abs");
   pragma Inline ("+", "-", "&", "*", "/", "**");
   pragma Inline (">", ">=", "<", "<=");

end Intervals.Measures;
