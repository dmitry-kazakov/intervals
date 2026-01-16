--                                                                    --
--  package Intervals.Integers      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body Intervals.Integers is
--
-- Div_Low -- Division with truncation
--
--      Left  - The first operand
--      Right - The second operand
--
-- Returns :
--
--      Result <= Left / Right
--
   function Div_Low (Left, Right : Number) return Number;
   pragma Inline (Div_Low);
--
-- Div_Low -- Division with rounding
--
--      Left  - The first operand
--      Right - The second operand
--
-- Returns :
--
--      Result >= Left / Right
--
   function Div_High (Left, Right : Number) return Number;
   pragma Inline (Div_High);

   function Distance (Left, Right : Interval) return Number is
   begin
      return
         Number'Max
         (  abs (Left.From - Right.From),
            abs (Left.To   - Right.To)
         );
   end Distance;

   function Distance (Left : Number; Right : Interval) return Number is
   begin
      return
         Number'Max
         (  abs (Left - Right.From),
            abs (Left - Right.To)
         );
   end Distance;

   function Distance (Left : Interval; Right : Number) return Number is
   begin
      return
         Number'Max
         (  abs (Left.From - Right),
            abs (Left.To   - Right)
         );
   end Distance;

   function From (Left : Interval) return Number is
   begin
      return Left.From;
   end From;

   function Is_In (Left, Right : Interval) return Boolean is
   begin
      return Left.From >= Right.From and then Left.To <= Right.To;
   end Is_In;

   function Is_In (Left : Number; Right : Interval) return Boolean is
   begin
      return Left >= Right.From and then Left <= Right.To;
   end Is_In;

   function Is_Negative (Left : Interval) return Boolean is
   begin
      return Left.To < 0;
   end Is_Negative;

   function Is_Positive (Left : Interval) return Boolean is
   begin
      return Left.From > 0;
   end Is_Positive;

   function Length (Left : Interval) return Number is
   begin
      return Left.To - Left.From;
   end Length;

   function To (Left : Interval) return Number is
   begin
      return Left.To;
   end To;

   function To_Interval (Left : Number) return Interval is
   begin
      return (Left, Left);
   end To_Interval;

   function To_Interval (Left, Right : Number) return Interval is
   begin
      if Left > Right then
         raise Constraint_Error;
      else
         return (Left, Right);
      end if;
   end To_Interval;

   function "abs" (Left : Interval) return Interval is
      From : constant Number := abs Left.From;
      To   : constant Number := abs Left.To;
   begin
      if From <= To then
         return (From, To);
      else
         return (To, From);
      end if;
   end "abs";

   function "+" (Left : Interval) return Interval is
   begin
      return Left;
   end "+";

   function "-" (Left : Interval) return Interval is
   begin
      return (-Left.To, -Left.From);
   end "-";

   function "&" (Left, Right : Interval) return Boolean is
   begin
      return Left.To >= Right.From and then Left.From <= Right.To;
   end "&";

   function "+" (Left, Right : Interval) return Interval is
   begin
      return (Left.From + Right.From, Left.To + Right.To);
   end "+";

   function "+" (Left : Interval; Right : Number) return Interval is
   begin
      return (Left.From + Right, Left.To + Right);      
   end "+";

   function "+" (Left : Number; Right : Interval) return Interval is
   begin
      return (Right.From + Left, Right.To + Left);      
   end "+";

   function "-" (Left, Right : Interval) return Interval is
   begin
      return (Left.From - Right.To, Left.To - Right.From);
   end "-";

   function "-" (Left : Interval; Right : Number)  return Interval is
   begin
      return (Left.From - Right, Left.To - Right);      
   end "-";

   function "-" (Left : Number; Right : Interval) return Interval is
   begin
      return (Left - Right.To, Left - Right.From);      
   end "-";

   function "*" (Left, Right : Interval) return Interval is
   begin
      if Left.From > 0 then
         if Right.From > 0 then   -- +,+ -> [ac, bd]
            return (Left.From * Right.From, Left.To * Right.To);
         elsif Right.To < 0 then  -- +,- -> [bc, ad]
            return (Left.To * Right.From, Left.From * Right.To);
         else                     -- +,0 -> [bc, bd]
            return (Left.To * Right.From, Left.To * Right.To);
         end if;
      elsif Left.To < 0 then
         if Right.From > 0 then   -- -,+ -> [ad, bc]
            return (Left.From * Right.To, Left.To * Right.From);
         elsif Right.To < 0 then  -- -,- -> [bd, ac]
            return (Left.To * Right.To, Left.From * Right.From);
         else                     -- -,0 -> [ad, ac]
            return (Left.From * Right.To, Left.From * Right.From);
         end if;
      else
         if Right.From > 0 then   -- 0,+ -> [ad, bd]
            return (Left.From * Right.To, Left.To * Right.To);
         elsif Right.To < 0 then  -- 0,- -> [bc, ac]
            return (Left.To * Right.From, Left.From * Right.From);
         else                     -- 0,0 -> [min(ad,bc),max(ac,bd)]
            return
            (  Number'Min (Left.From * Right.To, Left.To * Right.From),
               Number'Max (Left.From * Right.From, Left.To * Right.To)
            );
         end if;
      end if;
   end "*";

   function "*" (Left : Interval; Right : Number)  return Interval is
   begin
      if Right >= 0 then
         return (Left.From * Right, Left.To * Right);
      else
         return (Left.To * Right, Left.From * Right);
      end if;
   end "*";

   function "*" (Left : Number; Right : Interval) return Interval is
   begin
      if Left >= 0 then
         return (Left * Right.From, Left * Right.To);
      else
         return (Left * Right.To, Left * Right.From);
      end if;
   end "*";

   function "**" (Left : Interval; Right : Natural) return Interval is
   begin
      if Left.From <= 0 and then Left.To >= 0 then
         if Right = 0 then
            return (0, 1);
         elsif Right mod 2 = 0 then
            return
            (  0,
               Number'Max (Left.From ** Right, Left.To ** Right)
            );
         else
            return (Left.From ** Right, Left.To ** Right);
         end if;
      else
         if Right = 0 then
            return (1, 1);
         elsif Left.To < 0 and then Right mod 2 = 0 then
            return (Left.To ** Right, Left.From ** Right);
         else
            return (Left.From ** Right, Left.To ** Right);
         end if;
      end if;
   end "**";

   function Div_Low (Left, Right : Number) return Number is
      Result : constant Number := Left / Right;
   begin
      if (Left > 0 xor Right > 0) and then Result * Right /= Left then
         return Result - 1;
      else
         return Result;
      end if;
   end Div_Low;

   function Div_High (Left, Right : Number) return Number is
      Result : constant Number := Left / Right;
   begin
      if (Left < 0 xor Right > 0) and then Result * Right /= Left then
         return Result + 1;
      else
         return Result;
      end if;
   end Div_High;

   function "/" (Left, Right : Interval) return Interval is
   begin
      if Left.From > 0 then
         if Right.From > 0 then   -- +,+ -> [a/d, b/c]
            return
            (  Div_Low  (Left.From, Right.To),
               Div_High (Left.To,   Right.From)
            );
         elsif Right.To < 0 then  -- +,- -> [b/d, a/c]
            return
            (  Div_Low  (Left.To,   Right.To),
               Div_High (Left.From, Right.From)
            );
         end if;
      elsif Left.To < 0 then
         if Right.From > 0 then   -- -,+ -> [a/c,b/d]
            return
            (  Div_Low  (Left.From, Right.From),
               Div_High (Left.To,   Right.To)
            );
         elsif Right.To < 0 then  -- -,- -> [b/c, a/d]
            return
            (  Div_Low  (Left.To,  Right.From),
               Div_High (Left.From, Right.To)
            );
         end if;
      else
         if Right.From > 0 then   -- 0,+ -> [a/c, b/c]
            return
            (  Div_Low  (Left.From, Right.From),
               Div_High (Left.To,   Right.From)
            );
         elsif Right.To < 0 then  -- 0,- -> [b/d, a/d]
            return
            (  Div_Low  (Left.To,   Right.To),
               Div_High (Left.From, Right.To)
            );
         end if;
      end if;
      raise Constraint_Error;
   end "/";

   function "/" (Left : Interval; Right : Number) return Interval is
   begin
      if Right >= 0 then
         return
         (  Div_Low  (Left.From, Right),
            Div_High (Left.To,   Right)
         );
      else
         return
         (  Div_Low  (Left.To, Right),
            Div_High (Left.From, Right)
         );
      end if;
   end "/";

   function "/" (Left : Number; Right : Interval) return Interval is
   begin
      if 0 > Right.To or else 0 < Right.From then
         if Left >= 0 then
            return
            (  Div_Low  (Left, Right.To),
               Div_High (Left, Right.From)
            );
         else
            return
            (  Div_Low  (Left, Right.From),
               Div_High (Left, Right.To)
            );
         end if;
      end if;
      raise Constraint_Error;
   end "/";

   function ">" (Left : Interval; Right : Number) return Logical is
   begin
      if Left.From > Right then
         return True;
      elsif Left.To <= Right then
         return False;
      else
         return Uncertain;
      end if;
   end ">";

   function ">" (Left : Number; Right : Interval) return Logical is
   begin
      if Left > Right.To then
         return True;
      elsif Left <= Right.From then
         return False;
      else
         return Uncertain;
      end if;
   end ">";

   function ">" (Left, Right : Interval) return Logical is
   begin
      if Left.From > Right.To then
         return True;
      elsif Left.To <= Right.From then
         return False;
      else
         return Uncertain;
      end if;
   end ">";

   function ">=" (Left : Interval; Right : Number) return Logical is
   begin
      if Left.From >= Right then
         return True;
      elsif Left.To < Right then
         return False;
      else
         return Uncertain;
      end if;
   end ">=";

   function ">=" (Left : Number; Right : Interval) return Logical is
   begin
      if Left >= Right.To then
         return True;
      elsif Left < Right.From then
         return False;
      else
         return Uncertain;
      end if;
   end ">=";

   function ">=" (Left, Right : Interval) return Logical is
   begin
      if Left.From >= Right.To then
         return True;
      elsif Left.To < Right.From then
         return False;
      else
         return Uncertain;
      end if;
   end ">=";

   function "<=" (Left : Interval; Right : Number) return Logical is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Number;  Right : Interval) return Logical is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left, Right : Interval) return Logical is
   begin
      return Right >= Left;
   end "<=";

   function "<"(Left : Interval; Right : Number) return Logical is
   begin
      return Right > Left;
   end "<";

   function "<" (Left : Number;  Right : Interval) return Logical is
   begin
      return Right > Left;
   end "<";

   function "<" (Left, Right : Interval) return Logical is
   begin
      return Right > Left;
   end "<";

end Intervals.Integers;
