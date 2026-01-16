--                                                                    --
--  package Measures                Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  10:51 06 Jul 2008  --
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

with Units.Base;  use Units.Base;

package body Measures is
   function "abs" (Right : Measure) return Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => abs Right.Gain,
         Offset => Right.Offset
      );
   end "abs";

   function "**" (Left : Measure; Right : Integer) return Measure is
   begin
      if Right = 1 then
         return Left;
      end if;
      if Left.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI ** Right,
            Gain   => Left.Gain ** Right,
            Offset => 0.0
         );
      end if;
   end "**";

   function "+" (Right : Measure) return Measure is
   begin
      return Right;
   end "+";

   function "-" (Right : Measure) return Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => -Right.Gain,
         Offset => Right.Offset
      );
   end "-";

   function "*" (Left : Number'Base; Right : Measure) return Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Right.Gain * Left,
         Offset => Right.Offset
      );
   end "*";

   function "*" (Left : Measure; Right : Number'Base) return Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "/" (Left : Number'Base; Right : Measure) return Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Units.Base.Unitless / Right.SI,
            Gain   => Left / Right.Gain,
            Offset => 0.0
         );
      end if;
   end "/";

   function "/" (Left : Measure; Right : Number'Base) return Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "and" (Left : Measure; Right : Number'Base) return Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain,
         Offset => Left.Offset + Right
      );
   end "and";

   function "*" (Left, Right : Measure) return Measure is
   begin
      if Left.Offset = 0.0 then
         if Right.Offset = 0.0 or else Left.SI = Unitless then
            return
            (  SI     => Left.SI   * Right.SI,
               Gain   => Left.Gain * Right.Gain,
               Offset => Right.Offset
            );
         end if;
      else
         if Right.Offset = 0.0 and then Right.SI = Unitless then
            return
            (  SI     => Left.SI,
               Gain   => Left.Gain * Right.Gain,
               Offset => Left.Offset
            );
         end if;
      end if;
      raise Unit_Error;
   end "*";

   function "/" (Left, Right : Measure) return Measure is
   begin
      if (  Right.Offset = 0.0
         and then
            (  Left.Offset = 0.0
            or else
               Right.SI = Unitless
         )  )
      then
         return
         (  SI     => Left.SI   / Right.SI,
            Gain   => Left.Gain / Right.Gain,
            Offset => Left.Offset
         );
      end if;
      raise Unit_Error;
   end "/";

   function "+" (Left, Right : Measure) return Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "-" (Left, Right : Measure) return Measure is
   begin
      if Left.SI /= Right.SI or Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain - Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "-";

   function ">" (Left, Right : Measure) return Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return Left.Gain + Left.Offset > Right.Gain + Right.Offset;
      end if;
   end ">";

   function "<" (Left, Right : Measure) return Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return Left.Gain + Left.Offset < Right.Gain + Right.Offset;
      end if;
   end "<";

   function "="  (Left, Right : Measure) return Boolean is
   begin
      return
      (  Left.SI = Right.SI
      and
         Left.Gain + Left.Offset = Right.Gain + Right.Offset
      );
   end "=";

   function ">=" (Left, Right : Measure) return Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return Left.Gain + Left.Offset >= Right.Gain + Right.Offset;
      end if;
   end ">=";

   function "<=" (Left, Right : Measure) return Boolean is
   begin
      if Left.SI /= Right.SI then
         raise Unit_Error;
      else
         return Left.Gain + Left.Offset <= Right.Gain + Right.Offset;
      end if;
   end "<=";

   function Get_Value (Value : Measure) return Number is
   begin
      return Value.Gain + Value.Offset;
   end Get_Value;

   function Get_Value_As (Value, Scale : Measure) return Number is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      else
         return (Value.Gain + Value.Offset - Scale.Offset) / Scale.Gain;
      end if;
   end Get_Value_As;

   function Get_Unit (Value : Measure) return Unit is
   begin
      return Value.SI;
   end Get_Unit;

   function Convert (Value, Scale : Measure) return Measure is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      else
         return
         (  SI     => Value.SI,
            Gain   => Value.Gain + Value.Offset - Scale.Offset,
            Offset => Scale.Offset
         );
      end if;
   end Convert;

   function Normalize (Value : Measure) return Measure is
   begin
      return (Value.SI, Value.Gain + Value.Offset, 0.0);
   end Normalize;

   function Shift (Value : Measure; Shift : Number'Base)
      return Measure is
   begin
      return
      (  SI     => Value.SI,
         Gain   => Value.Gain - Shift,
         Offset => Value.Offset + Shift
      );
   end Shift;

   function To_Measure (Value : Number) return Measure is
   begin
      return (SI => Unitless, Gain => Value, Offset => 0.0);
   end To_Measure;

end Measures;
