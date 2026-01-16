--                                                                    --
--  package Intervals.Measures      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body Intervals.Measures is
   use Units.Base;

   function Create (SI : Unit; Gain : Interval; Offset : Number)
      return Interval_Measure is
      pragma Inline (Create);
   begin
      return
      (  SI     => SI,
         From   => Gain.From,
         To     => Gain.To,
         Offset => Offset
      );
   end Create;

   function Convert (Value : Interval_Measure; Scale : Measure)
      return Interval_Measure is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      else
         declare
            Result : constant Interval :=
               ((Value.From, Value.To) + Value.Offset) - Scale.Offset;
         begin
            return
            (  SI     => Value.SI,
               From   => Result.From,
               To     => Result.To,
               Offset => Scale.Offset
            );
         end;
      end if;
   end Convert;

   function Distance (Left, Right : Interval_Measure) return Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
            (  Left.SI,
               Distance ((Left.From, Left.To), (Right.From, Right.To)),
               Left.Offset
            );
      end if;
   end Distance;

   function Distance (Left : Measure; Right : Interval_Measure)
      return Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  Left.SI,
            Distance (Left.Gain, (Right.From, Right.To)),
            Left.Offset
         );
      end if;
   end Distance;

   function Distance (Left : Interval_Measure; Right : Measure)
      return Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  Left.SI,
            Distance ((Left.From, Left.To), Right.Gain),
            Left.Offset
         );
      end if;
   end Distance;

   function From (Left : Interval_Measure) return Measure is
   begin
      return (Left.SI, Left.From, Left.Offset);
   end From;

   function Get_Value (Value : Interval_Measure) return Interval is
   begin
      return (Value.From, Value.To) + Value.Offset;
   end Get_Value;

   function Get_Value_As (Value : Interval_Measure; Scale : Measure)
      return Interval is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      else
         return
         (  (((Value.From, Value.To) + Value.Offset) - Scale.Offset)
         /  Scale.Gain
         );
      end if;
   end Get_Value_As;

   function Get_Unit (Value : Interval_Measure) return Unit is
   begin
      return Value.SI;
   end Get_Unit;

   function Is_In (Left, Right : Interval_Measure) return Boolean is
   begin
      if Left.SI /= Right.SI then
         return False;
      else
         return
            Is_In
            (  ((Left.From,  Left.To ) + Left.Offset) - Right.Offset,
               (Right.From, Right.To)
            );
      end if;
   end Is_In;

   function Is_In (Left : Measure; Right : Interval_Measure)
      return Boolean is
   begin
      if Left.SI /= Right.SI then
         return False;
      else
         return
            Is_In
            (  ((Left.Gain,  Left.Gain) + Left.Offset) - Right.Offset,
               (Right.From, Right.To)
            );
      end if;
   end Is_In;

   function Is_Negative (Left : Interval_Measure) return Boolean is
   begin
      return Left.To < 0.0;
   end Is_Negative;

   function Is_Positive (Left : Interval_Measure) return Boolean is
   begin
      return Left.From > 0.0;
   end Is_Positive;

   function Length (Left : Interval_Measure) return Measure is
   begin
      return
      (  Left.SI,
         Float_Intervals.Length ((Left.From, Left.To)),
	 Left.Offset
      );
   end Length;

   function Normalize (Value : Interval_Measure)
      return Interval_Measure is
      Result : constant Interval :=
                  (Value.From, Value.To) + Value.Offset;
   begin
      return
      (  SI     => Value.SI,
         From   => Result.From,
         To     => Result.To,
         Offset => 0.0
      );
   end Normalize;

   function Shift (Value : Interval_Measure; Shift : Number'Base)
      return Interval_Measure is
      Result : constant Interval := (Value.From, Value.To) - Shift;
   begin
      return
      (  SI     => Value.SI,
         From   => Result.From,
         To     => Result.To,
         Offset => Value.Offset + Shift
      );
   end Shift;

   function To (Left : Interval_Measure) return Measure is
   begin
      return (Left.SI, Left.To, Left.Offset);
   end To;

   function To_Interval_Measure (Left, Right : Measure)
      return Interval_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      elsif Left.Gain > Right.Gain then
         raise Constraint_Error;
      else
         return
         (  SI     => Left.SI,
            From   => Left.Gain,
            To     => Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end To_Interval_Measure;

   function To_Interval_Measure (Left, Right : Number)
      return Interval_Measure is
   begin
      if Left > Right then
         raise Constraint_Error;
      else
         return
         (  SI     => Unitless,
            From   => Left,
            To     => Right,
            Offset => 0.0
         );
      end if;
   end To_Interval_Measure;

   function To_Interval_Measure (Left : Number)
      return Interval_Measure is
   begin
      return
      (  SI     => Unitless,
         From   => Left,
         To     => Left,
         Offset => 0.0
      );
   end To_Interval_Measure;

   function To_Interval_Measure (Left : Measure)
      return Interval_Measure is
   begin
      return
      (  SI     => Left.SI,
         From   => Left.Gain,
         To     => Left.Gain,
         Offset => Left.Offset
      );
   end To_Interval_Measure;

   function To_Interval_Measure (Left : Interval)
      return Interval_Measure is
   begin
      return
      (  SI     => Unitless,
         From   => Left.From,
         To     => Left.To,
         Offset => 0.0
      );
   end To_Interval_Measure;

   function "abs" (Left : Interval_Measure) return Interval_Measure is
      Result : constant Interval := abs ((Left.From, Left.To));
   begin
      return
      (  SI     => Left.SI,
         From   => Result.From,
         To     => Result.To,
         Offset => Left.Offset
      );
   end "abs";

   function "+" (Left : Interval_Measure) return Interval_Measure is
   begin
      return Left;
   end "+";

   function "-" (Left : Interval_Measure) return Interval_Measure is
      Result : constant Interval := -((Left.From, Left.To));
   begin
      return
      (  SI     => Left.SI,
         From   => Result.From,
         To     => Result.To,
         Offset => Left.Offset
      );
   end "-";

   function "&" (Left, Right : Interval_Measure) return Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return
         (  ((Left.From,  Left.To ) + Left.Offset )
         &  ((Right.From, Right.To) + Right.Offset)
         );
      end if;
   end "&";

   function "+" (Left, Right : Interval_Measure)
      return Interval_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
            Create
            (  Left.SI,
               (Left.From, Left.To) + (Right.From, Right.To),
               Left.Offset
            );
      end if;
   end "+";

   function "+" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
            Create
            (  Left.SI,
               (Left.From, Left.To) + Right.Gain,
               Left.Offset
            );
      end if;
   end "+";

   function "+" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
            Create
            (  Left.SI,
               Left.Gain + (Right.From, Right.To),
               Left.Offset
            );
      end if;
   end "+";

   function "-" (Left, Right : Interval_Measure)
      return Interval_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
            Create
            (  Left.SI,
               (Left.From, Left.To) - (Right.From, Right.To),
               Left.Offset
            );
      end if;
   end "-";

   function "-" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
            Create
            (  Left.SI,
               (Left.From, Left.To) - Right.Gain,
               Left.Offset
            );
      end if;
   end "-";

   function "-" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
            Create
            (  Left.SI,
               Left.Gain - (Right.From, Right.To),
               Left.Offset
            );
      end if;
   end "-";

   function "*" (Left : Interval_Measure; Right : Interval_Measure)
      return Interval_Measure is
   begin
      if Left.Offset = 0.0 then
         if Right.Offset = 0.0 or else Left.SI = Unitless then
            return
               Create
               (  Left.SI * Right.SI,
                  (Left.From, Left.To) * (Right.From, Right.To),
                  Right.Offset
               );
         end if;
      else
         if Right.Offset = 0.0 and then Right.SI = Unitless then
            return
               Create
               (  Left.SI,
                  (Left.From, Left.To) * (Right.From, Right.To),
                  Left.Offset
               );
         end if;
      end if;
      raise Unit_Error;
   end "*";

   function "*" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure is
   begin
      if Left.Offset = 0.0 then
         if Right.Offset = 0.0 or else Left.SI = Unitless then
            return
               Create
               (  Left.SI * Right.SI,
                  Left.Gain * (Right.From, Right.To),
                  Right.Offset
               );
         end if;
      else
         if Right.Offset = 0.0 and then Right.SI = Unitless then
            return
               Create
               (  Left.SI,
                  Left.Gain * (Right.From, Right.To),
                  Left.Offset
              );
         end if;
      end if;
      raise Unit_Error;
   end "*";

   function "*" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure is
   begin
      if Left.Offset = 0.0 then
         if Right.Offset = 0.0 or else Left.SI = Unitless then
            return
               Create
               (  Left.SI * Right.SI,
                  (Left.From, Left.To) * Right.Gain,
                  Right.Offset
               );
         end if;
      else
         if Right.Offset = 0.0 and then Right.SI = Unitless then
            return
               Create
               (  Left.SI,
                  (Left.From, Left.To) * Right.Gain,
                  Left.Offset
               );
         end if;
      end if;
      raise Unit_Error;
   end "*";

   function "*" (Left : Interval; Right : Interval_Measure)
      return Interval_Measure is
   begin
      return
         Create
         (  Right.SI,
            Left * (Right.From, Right.To),
            Right.Offset
         );
   end "*";

   function "*" (Left : Interval_Measure; Right : Interval)
      return Interval_Measure is
   begin
      return
         Create
         (  Left.SI,
            (Left.From, Left.To) * Right,
            Left.Offset
         );
   end "*";

   function "*" (Left : Number'Base; Right : Interval_Measure)
      return Interval_Measure is
   begin
      return
         Create
         (  Right.SI,
            Left * (Right.From, Right.To),
            Right.Offset
         );
   end "*";

   function "*" (Left : Interval_Measure; Right : Number'Base)
      return Interval_Measure is
   begin
      return
         Create
         (  Left.SI,
            (Left.From, Left.To) * Right,
            Left.Offset
         );
   end "*";

   function "*" (Left : Interval; Right : Measure)
      return Interval_Measure is
   begin
      return
         Create
         (  Right.SI,
            Left * Right.Gain,
            Right.Offset
         );
   end "*";

   function "*" (Left : Measure; Right : Interval)
      return Interval_Measure is
   begin
      return
         Create
         (  Left.SI,
            Left.Gain * Right,
            Left.Offset
         );
   end "*";

   function "/" (Left, Right : Interval_Measure)
      return Interval_Measure is
   begin
      if (  Right.Offset = 0.0
         and then
            (  Left.Offset = 0.0
            or else
               Right.SI = Unitless
         )  )
      then
         return
            Create
            (  Left.SI / Right.SI,
               (Left.From, Left.To) / (Right.From, Right.To),
               Left.Offset
            );
      end if;
      raise Unit_Error;
   end "/";

   function "/" (Left : Interval_Measure; Right : Measure)
      return Interval_Measure is
   begin
      if (  Right.Offset = 0.0
         and then
            (  Left.Offset = 0.0
            or else
               Right.SI = Unitless
         )  )
      then
         return
            Create
            (  Left.SI / Right.SI,
               (Left.From, Left.To) / Right.Gain,
               Left.Offset
            );
      end if;
      raise Unit_Error;
   end "/";

   function "/" (Left : Measure; Right : Interval_Measure)
      return Interval_Measure is
   begin
      if (  Right.Offset = 0.0
         and then
            (  Left.Offset = 0.0
            or else
               Right.SI = Unitless
         )  )
      then
         return
            Create
            (  Left.SI / Right.SI,
               Left.Gain / (Right.From, Right.To),
               Left.Offset
            );
      end if;
      raise Unit_Error;
   end "/";

   function "/" (Left : Interval_Measure; Right : Interval)
      return Interval_Measure is
   begin
      return
         Create
         (  Left.SI,
            (Left.From, Left.To) / Right,
            Left.Offset
         );
   end "/";

   function "/" (Left : Interval; Right : Interval_Measure)
      return Interval_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
            Create
            (  Units.Base.Unitless / Right.SI,
               Left / (Right.From, Right.To),
               0.0
            );
      end if;
   end "/";

   function "/" (Left : Interval_Measure; Right : Number'Base)
      return Interval_Measure is
   begin
      return
         Create
         (  Left.SI,
            (Left.From, Left.To) / Right,
            Left.Offset
         );
   end "/";

   function "/" (Left : Number'Base; Right : Interval_Measure)
      return Interval_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
            Create
            (  Units.Base.Unitless / Right.SI,
               Left / (Right.From, Right.To),
               0.0
            );
      end if;
   end "/";

   function "/" (Left : Measure; Right : Interval)
      return Interval_Measure is
   begin
      return
         Create
         (  Left.SI,
            Left.Gain / Right,
            Left.Offset
         );
   end "/";

   function "/" (Left : Interval; Right : Measure)
      return Interval_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
            Create
            (  Units.Base.Unitless / Right.SI,
               Left / Right.Gain,
               0.0
            );
      end if;
   end "/";

   function "**" (Left : Interval_Measure; Right : Natural)
      return Interval_Measure is
   begin
      if Left.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
            Create
            (  Left.SI ** Right,
               (Left.From, Left.To) ** Right,
               0.0
            );
      end if;
   end "**";

   function ">" (Left, Right : Interval_Measure)
      return Logical is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return
         (  (Left.From,  Left.To ) + Left.Offset
         >  (Right.From, Right.To) + Right.Offset
         );
      end if;
   end ">";

   function ">" (Left : Interval_Measure; Right : Measure)
      return Logical is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return
         (  (Left.From,  Left.To   ) + Left.Offset
         >  (Right.Gain, Right.Gain) + Right.Offset
         );
      end if;
   end ">";

   function ">"  (Left : Measure; Right : Interval_Measure)
      return Logical is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return
         (  (Left.Gain,  Left.Gain) + Left.Offset
         >  (Right.From, Right.To)  + Right.Offset
         );
      end if;
   end ">";

   function ">=" (Left, Right : Interval_Measure)
      return Logical is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return
         (  (Left.From,  Left.To ) + Left.Offset
         >= (Right.From, Right.To) + Right.Offset
         );
      end if;
   end ">=";

   function ">=" (Left : Interval_Measure; Right : Measure)
      return Logical is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return
         (  (Left.From,  Left.To  )  + Left.Offset
         >= (Right.Gain, Right.Gain) + Right.Offset
         );
      end if;
   end ">=";

   function ">=" (Left : Measure; Right : Interval_Measure)
      return Logical is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return
         (  (Left.Gain,  Left.Gain) + Left.Offset
         >= (Right.From, Right.To ) + Right.Offset
         );
      end if;
   end ">=";

   function "<=" (Left, Right : Interval_Measure)
      return Logical is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Interval_Measure; Right : Measure)
      return Logical is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Measure; Right : Interval_Measure)
      return Logical is
   begin
      return Right >= Left;
   end "<=";

   function "<" (Left, Right : Interval_Measure)
      return Logical is
   begin
      return Right > Left;
   end "<";

   function "<"  (Left : Interval_Measure; Right : Measure)
      return Logical is
   begin
      return Right > Left;
   end "<";

   function "<" (Left : Measure; Right : Interval_Measure)
      return Logical is
   begin
      return Right > Left;
   end "<";

end Intervals.Measures;
