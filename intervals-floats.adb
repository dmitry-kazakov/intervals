--                                                                    --
--  package Intervals.Floats        Copyright (c)  Dmitry A. Kazakov  --
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

package body Intervals.Floats is
--
-- Diff -- Upper bound of a difference
--
--    Left   - The first argument
--    Right  - The second argument
--
-- Returns :
--
--    The upper bound of |Left-Right|
--
   function Diff (Left, Right : Number'Base) return Number'Base;
   pragma Inline (Diff);
--
-- Inf - Lower bound of the result
--
--      Left  - An unprecise result of some operation
--
-- Returns :
--
--      The lower bound of the precise result
--
   function Inf (Left : Number'Base) return Number'Base;
   pragma Inline (Inf);
--
-- Sup -- Upper bound of the result
--
--      Left  - An unprecise result of some operation
--
-- Returns :
--
--      The lower bound of the precise result
--
   function Sup (Left : Number'Base) return Number'Base;
   pragma Inline (Sup);

   function Diff (Left, Right : Number'Base) return Number'Base is
   begin
      if Left > Right then
         return Sup (Left - Right);
      elsif Left < Right then
         return Sup (Right - Left);
      else
         return 0.0;
      end if;
   end Diff;

   function Inf (Left : Number'Base) return Number'Base is
   begin
      if Left < 0.0 then
         return Number'Base'Adjacent (Left, Number'Base'First);
      else
         if Number'Machine_Rounds then
            return Number'Base'Adjacent (Left, 0.0);
         else
            return Left;
         end if;
      end if;
   end Inf;

   function Sup (Left : Number'Base) return Number'Base is
   begin
      if Left > 0.0 then
         return Number'Base'Adjacent (Left, Number'Base'Last);
      else
         if Number'Base'Machine_Rounds then
            return Number'Base'Adjacent (Left, 0.0);
         else
            return Left;
         end if;
      end if;
   end Sup;

   function Distance (Left, Right : Interval) return Number is
   begin
      return
         Number'Base'Max
         (  Diff (Left.From, Right.From),
            Diff (Left.To,   Right.To)
         );
   end Distance;

   function Distance (Left : Number; Right : Interval) return Number is
   begin
      return
         Number'Base'Max
         (  Diff (Left, Right.From),
            Diff (Left, Right.To)
         );
   end Distance;

   function Distance (Left : Interval; Right : Number) return Number is
   begin
      return
         Number'Base'Max
         (  Diff (Left.From, Right),
            Diff (Left.To,   Right)
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
      return Left.To < 0.0;
   end Is_Negative;

   function Is_Positive (Left : Interval) return Boolean is
   begin
      return Left.From > 0.0;
   end Is_Positive;

   function Length (Left : Interval) return Number is
   begin
      return Sup (Left.To - Left.From);
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
   begin
      if Left.From >= 0.0 then
         return Left;
      elsif Left.To <= 0.0 then
         return -Left;
      else
         return (0.0, Number'Base'Max (Left.To, Sup (-Left.From)));
      end if;
   end "abs";

   function "+" (Left : Interval) return Interval is
   begin
      return Left;
   end "+";

   function "-" (Left : Interval) return Interval is
   begin
      return (Inf (-Left.To), Sup (-Left.From));
   end "-";

   function "&" (Left, Right : Interval) return Boolean is
   begin
      return Left.To >= Right.From and then Left.From <= Right.To;
   end "&";

   function "+" (Left, Right : Interval) return Interval is
   begin
      return (Inf (Left.From + Right.From), Sup (Left.To + Right.To));
   end "+";

   function "+" (Left : Interval; Right : Number) return Interval is
   begin
      return (Inf (Left.From + Right), Sup (Left.To + Right));
   end "+";

   function "+" (Left : Number; Right : Interval) return Interval is
   begin
      return (Inf (Right.From + Left), Sup (Right.To + Left));
   end "+";

   function "-" (Left, Right : Interval) return Interval is
   begin
      return (Inf (Left.From - Right.To), Sup (Left.To - Right.From));
   end "-";

   function "-" (Left : Interval; Right : Number) return Interval is
   begin
      return (Inf (Left.From - Right), Sup (Left.To - Right));
   end "-";

   function "-" (Left : Number; Right : Interval) return Interval is
   begin
      return (Inf (Left - Right.To), Sup (Left - Right.From));
   end "-";

   function "*" (Left, Right : Interval) return Interval is
   begin
      if Left.From > 0.0 then
         if Right.From > 0.0 then   -- +,+ -> [ac, bd]
            return
            (  Inf (Left.From * Right.From),
               Sup (  Left.To * Right.To)
            );
         elsif Right.To < 0.0 then  -- +,- -> [bc, ad]
            return
            (  Inf (  Left.To * Right.From),
               Sup (Left.From * Right.To)
            );
         else                      -- +,0 -> [bc, bd]
            return
            (  Inf (Left.To * Right.From),
               Sup (Left.To * Right.To)
            );
         end if;
      elsif Left.To < 0.0 then
         if Right.From > 0.0 then   -- -,+ -> [ad, bc]
            return
            (  Inf (Left.From * Right.To),
               Sup (  Left.To * Right.From)
            );
         elsif Right.To < 0.0 then  -- -,- -> [bd, ac]
            return
            (  Inf (  Left.To * Right.To),
               Sup (Left.From * Right.From)
            );
         else                      -- -,0 -> [ad, ac]
            return
            (  Inf (Left.From * Right.To),
               Sup (Left.From * Right.From)
            );
         end if;
      else
         if Right.From > 0.0 then   -- 0,+ -> [ad, bd]
            return
            (  Inf (Left.From * Right.To),
               Sup (Left.To * Right.To)
            );
         elsif Right.To < 0.0 then  -- 0,- -> [bc, ac]
            return
            (  Inf (Left.To   * Right.From),
               Sup (Left.From * Right.From)
            );
         else                      -- 0,0 -> [min(ad,bc),max(ac,bd)]
            return
            (  Inf
               (  Number'Min
                  (  Left.From * Right.To,
                     Left.To   * Right.From
               )  ),
               Sup
               (  Number'Max
                  (  Left.From * Right.From,
                     Left.To   * Right.To
            )  )  );
         end if;
      end if;
   end "*";

   function "*" (Left : Interval; Right : Number)  return Interval is
   begin
      if Right >= 0.0 then
         return (Inf (Left.From * Right), Sup (Left.To * Right));
      else
         return (Inf (Left.To * Right), Sup (Left.From * Right));
      end if;
   end "*";

   function "*" (Left : Number; Right : Interval) return Interval is
   begin
      if Left >= 0.0 then
         return (Inf (Left * Right.From), Sup (Left * Right.To));
      else
         return (Inf (Left * Right.To), Sup (Left * Right.From));
      end if;
   end "*";

   function "/" (Left, Right : Interval) return Interval is
   begin
      if Left.From > 0.0 then
         if Right.From > 0.0 then   -- +,+ -> [a/d, b/c]
            return
            (  Inf (Left.From / Right.To),
               Sup (Left.To   / Right.From)
            );
         elsif Right.To < 0.0 then  -- +,- -> [b/d, a/c]
            return
            (  Inf (Left.To   / Right.To),
               Sup (Left.From / Right.From)
            );
         end if;
      elsif Left.To < 0.0 then
         if Right.From > 0.0 then   -- -,+ -> [a/c,b/d]
            return
            (  Inf (Left.From / Right.From),
               Sup (Left.To   / Right.To)
            );
         elsif Right.To < 0.0 then  -- -,- -> [b/c, a/d]
            return
            (  Inf (Left.To   / Right.From),
               Sup (Left.From / Right.To)
            );
         end if;
      else
         if Right.From > 0.0 then   -- 0,+ -> [a/c, b/c]
            return
            (  Inf (Left.From / Right.From),
               Sup (Left.To    / Right.From)
            );
         elsif Right.To < 0.0 then  -- 0,- -> [b/d, a/d]
            return
            (  Inf (Left.To   / Right.To),
               Sup (Left.From / Right.To)
            );
         end if;
      end if;
      raise Constraint_Error;
   end "/";

   function "/" (Left : Interval; Right : Number) return Interval is
   begin
      if Right >= 0.0 then
         return
         (  Inf (Left.From / Right),
            Sup (Left.To   / Right)
         );
      else
         return
         (  Inf (Left.To   / Right),
            Sup (Left.From / Right)
         );
      end if;
   end "/";

   function "/" (Left : Number; Right : Interval) return Interval is
   begin
      if 0.0 > Right.To or else 0.0 < Right.From then
         if Left >= 0.0 then
            return
            (  Inf (Left / Right.To),
               Sup (Left / Right.From)
            );
         else
            return
            (  Inf (Left / Right.From),
               Sup (Left / Right.To)
            );
         end if;
      end if;
      raise Constraint_Error;
   end "/";

   function Sup_Pow (Left : Number'Base; Right : Positive)
      return Number'Base is
   begin
      if Right = 1 then
         return Left;
      end if;
      declare
         Exponent : Integer     := Right;
         Power    : Number'Base := Left;
         Result   : Number'Base := 1.0;
      begin
         loop
            if 0 /= Exponent mod 2 then
               Result := Sup (Result * Power);
            end if;
            Exponent := Exponent / 2;
            exit when Exponent = 0;
            Power := Sup (Power * Power);
         end loop;
         return Result;
      end;
   end Sup_Pow;

   function Inf_Pow (Left : Number'Base; Right : Positive)
      return Number'Base is
   begin
      if Right = 1 then
         return Left;
      end if;
      declare
         Exponent : Integer     := Right;
         Power    : Number'Base := Left;
         Result   : Number'Base := 1.0;
      begin
         loop
            if 0 /= Exponent mod 2 then
               Result := Inf (Result * Power);
            end if;
            Exponent := Exponent / 2;
            exit when Exponent = 0;
            Power := Inf (Power * Power);
         end loop;
         return Result;
      end;
   end Inf_Pow;

   function "**" (Left : Interval; Right : Natural) return Interval is
   begin
      if Left.From <= 0.0 and then Left.To >= 0.0 then
         if Right = 0 then
            return (0.0, 1.0);
         elsif Right mod 2 = 0 then
            return
            (  0.0,
               Number'Max
               (  Sup_Pow (Left.From, Right),
                  Sup_Pow (Left.To,   Right)
            )  );
         else
            return
            (  Inf_Pow (Left.From, Right),
               Sup_Pow (Left.To,   Right)
            );
         end if;
      else
         if Right = 0 then
            return (1.0, 1.0);
         elsif Left.To < 0.0 and then Right mod 2 = 0 then
            return
            (  Inf_Pow (Left.To,   Right),
               Sup_Pow (Left.From, Right)
            );
         else
            return
            (  Inf_Pow (Left.From, Right),
               Sup_Pow (Left.To,   Right)
            );
         end if;
      end if;
   end "**";

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

   function "<=" (Left : Number; Right : Interval) return Logical is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left, Right : Interval) return Logical is
   begin
      return Right >= Left;
   end "<=";

   function "<" (Left : Interval; Right : Number) return Logical is
   begin
      return Right > Left;
   end "<";

   function "<" (Left : Number; Right : Interval) return Logical is
   begin
      return Right > Left;
   end "<";

   function "<" (Left, Right : Interval) return Logical is
   begin
      return Right > Left;
   end "<";

end Intervals.Floats;
