--                                                                    --
--  package Intervals.Floats        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
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
--  This   package   provides  interval  arithmetic  for  floating-point
--  numbers. No overflow checks made explicitly. The package is generic,
--  the formal parameter is the type used for the bounds:
--
--      Number - A floating-point type
--
--  This implementation supports rounding control, which means that  the
--  result of any inexact operation (an  interval)  contains  the  exact
--  result.   A   discussion   concerning   implementation  of  interval
--  arithmetic  can  be  found  in INTRODUCTION TO INTERVAL COMPUTATION,
--  G.Alefeld,   J.Herzberger;   Academic   Press,   1983.  Equality  is
--  implemented as in set  theory,  because  an  alternative  definition
--  ([a,b]=[c,d] <=> for any x from [a,b], y from [c,d]; x=y) is  rather
--  useless. But other relational  operators  are  implemented  in  this
--  sense. If @ is a relational operator defined on numbers, then: [a,b]
--  @  [c,d] is True if for any x from [a,b], y from [c,d]; x @ y. It is
--  False  if  for  any  x  from  [a,b],  y  from  [c,d]; not x@y. It is
--  Uncertain otherwise.
--
generic
   type Number is digits <>;
package Intervals.Floats is
   subtype Number_Of is Number;
   type Interval is record
      From : Number;
      To   : Number;
   end record;
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
   function Distance (Left, Right : Interval) return Number;
   function Distance (Left : Number; Right : Interval) return Number;
   function Distance (Left : Interval; Right : Number) return Number;
--
-- From -- The interval's lower bound
--
--    Left - The argument
--
-- Returns :
--
--    The lower bound of the interval
--
   function From (Left : Interval) return Number;
--
-- Is_In -- Check if an interval or a number belongs to other interval
--
--    Left  - The number or interval
--    Right - The interval
--
-- Returns :
--
--    True if Left is in Right
--
   function Is_In (Left, Right : Interval) return Boolean;
   function Is_In (Left : Number; Right : Interval) return Boolean;
--
-- Is_Negative -- Check if all numbers within interval are negative
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
   function Is_Negative (Left : Interval) return Boolean;
--
-- Is_Positive -- Check if all numbers within interval are positive
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
   function Is_Positive (Left : Interval) return Boolean;
--
-- Length -- Get the interval length
--
--    Left - The argument
--
-- When the interval contains only one number then its length is defined
-- as zero. Thus the number of numbers in the interval is Length + 1.
--
-- Returns :
--
--    The interval length = b - a
--
   function Length (Left : Interval) return Number;
--
-- To -- The interval's upper bound
--
--    Left - The argument
--
-- Returns :
--
--    The upper bound of the interval
--
   function To (Left : Interval) return Number;
--
-- To_Interval -- Get interval corresponding to a number
--
--    Left - The argument
--
-- Returns :
--
--    [Left, Left]
--
   function To_Interval (Left : Number) return Interval;
--
-- To_Interval -- Get interval from two bounds
--
--    Left  - The lower bound
--    Right - The upper bound
--
-- Returns :
--
--    [Left, Right]
--
-- Exceptions :
--
--    Constraint_Error - Left is not less or equal to Right
--
   function To_Interval (Left, Right : Number) return Interval;
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
   function "abs" (Left : Interval) return Interval;
--
-- + -- Unary plus
--
--    Left - The operand
--
-- Returns :
--
--    Left
--
   function "+" (Left : Interval) return Interval;
--
-- - -- Unary minus
--
--    Left - The operand
--
-- Returns :
--
--    [-b,-a]
--
   function "-" (Left : Interval) return Interval;
--
-- & -- Check if intervals intersect
--
--    Left  - The argument
--    Right - The argument
--
-- Returns :
--
--    True if the intersection of intervals is not empty
--
   function "&" (Left, Right : Interval) return Boolean;
--
-- + -- Addition
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both  arguments are intervals or one of them is a number. The
-- result is always an interval. It  is  defined  as  [a,b]  +  [c,d]  =
-- [a+c, b+d].
--
-- Returns :
--
--    Left + Right
--
   function "+" (Left, Right : Interval) return Interval;
   function "+" (Left : Interval; Right : Number) return Interval;
   function "+" (Left : Number;  Right : Interval) return Interval;
--
-- - -- Substraction
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both  arguments are intervals or one of them is a number. The
-- result is always an interval. It  is  defined  as  [a,b]  -  [c,d]  =
-- [a-d, b-c].
--
-- Returns :
--
--    Left - Right
--
   function "-" (Left, Right : Interval) return Interval;
   function "-" (Left : Interval; Right : Number) return Interval;
   function "-" (Left : Number; Right : Interval) return Interval;
--
-- * -- Multiplication
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both  arguments are intervals or one of them is a number. The
-- result is always an interval. The operation is  defined  as  [a, b] *
-- [c, d] = [min (a*c,a*d,b*c,b*d), max (a*c,a*d,b*c,b*d)].
--
-- Returns :
--
--    Left * Right
--
   function "*" (Left : Interval; Right : Number) return Interval;
   function "*" (Left : Number; Right : Interval) return Interval;
   function "*" (Left, Right : Interval) return Interval;
--
-- / -- Division
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both  arguments are intervals or one of them is a number. The
-- result  is always an interval. It is defined as [a, b] / [c, d] = [a,
-- b]  *  [1/d, 1/c]. The result is undefined if the divisor contains 0.
-- In this case Constraint_Error  is  propagated.  Because  division  of
-- integer  numbers  is  unprecise,  rounding is made to ensure that the
-- result interval covers all possible outcomes.
--
-- Returns :
--
--    Left / Right
--
-- Exceptions :
--
--    Constraint_Error - Zero divide
--
   function "/" (Left : Interval; Right : Number) return Interval;
   function "/" (Left : Number; Right : Interval) return Interval;
   function "/" (Left, Right : Interval) return Interval;
--
-- ** -- Power
--
--    Left  - The first operand
--    Right - The second operand
--
-- Exponentiation [a,b]**k is defined as follows:
--
-- 1. [a,b] contains 0   [a,b]**k =
--    1.1. k=0              [0.0, 1.0]
--    1.2. k=2n             [0.0, max (a**k, b**k)]
--    1.3. k=2n+1           [a**k, b**k]
-- 2. [a,b] > 0
--    2.1. k=0              [1.0, 1.0]
--    2.2. k>0              [a**k, b**k]
-- 3. [a,b] < 0
--    3.1. k=0              [1.0, 1.0]
--    3.2. k=2n             [b**k, a**k]
--    3.3. k=2n+1           [a**k, b**k]
--
-- Returns :
--
--    Left ** Right
--
   function "**" (Left : Interval; Right : Natural) return Interval;
--
-- >, >=, <, <= -- Comparisons
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both  arguments are intervals or one of them is a number. The
-- arguments can be incomparable, in which case Uncertain is the result.
-- Comparisons for intervals are defined as follows. If @ is one of  {>,
-- >=,  <, <=}, then [a,b] @ [c,d] is true when for any x from [a,b] and
-- any  y from [c,d], x @ y is also true. It is false if for these x and
-- y, x @ y is false. Otherwise, intervals are incomparable.  Note  that
-- for  intervals (x <= y) is not (x < y) V (x = y). Note also that like
-- in the case of floating-point numbers  equality  =  of  intervals  is
-- rather  meanigless.  One  can  use  Distance to compare intervals for
-- equality tests.
--
-- Returns :
--
--    The result of comparison
--
   function ">"  (Left, Right : Interval) return Logical;
   function ">"  (Left : Interval; Right : Number) return Logical;
   function ">"  (Left : Number;   Right : Interval) return Logical;
   function ">=" (Left, Right : Interval) return Logical;
   function ">=" (Left : Interval; Right : Number) return Logical;
   function ">=" (Left : Number;   Right : Interval) return Logical;
   function "<=" (Left, Right : Interval) return Logical;
   function "<=" (Left : Interval; Right : Number) return Logical;
   function "<=" (Left : Number;   Right : Interval) return Logical;
   function "<"  (Left, Right : Interval) return Logical;
   function "<"  (Left : Interval; Right : Number) return Logical;
   function "<"  (Left : Number;   Right : Interval) return Logical;

private
   pragma Inline (Distance);
   pragma Inline (From);
   pragma Inline (Is_In);
   pragma Inline (Is_Negative);
   pragma Inline (Is_Positive);
   pragma Inline (Length);
   pragma Inline (To);
   pragma Inline (To_Interval);
   pragma Inline ("abs");
   pragma Inline ("+", "-", "&", "*", "/", "**");
   pragma Inline (">", ">=", "<", "<=");

end Intervals.Floats;
