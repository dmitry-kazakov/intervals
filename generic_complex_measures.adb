--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Complex_Measures                    Luebeck            --
--  Implementation                                 Spring, 2023       --
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

with Units.Base;  use Units.Base;

package body Generic_Complex_Measures is

   function "**"
            (  Left  : Complex_Measure;
               Right : Integer
            )  return Complex_Measure is
   begin
      if Right = 1 then
         return Left;
      elsif Left.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI ** Right,
            Gain   => Left.Gain ** Right,
            Offset => 0.0
         );
      end if;
   end "**";

   function "abs" (Right : Complex_Measure) return Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => abs Right.Gain,
         Offset => Right.Offset
      );
   end "abs";

   function "+" (Right : Complex_Measure) return Complex_Measure is
   begin
      return Right;
   end "+";

   function "-" (Right : Complex_Measure) return Complex_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => -Right.Gain,
         Offset => Right.Offset
      );
   end "-";

   function "*"
            (  Left  : Number'Base;
               Right : Complex_Measure
            )  return Complex_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Right.Gain * Left,
         Offset => Right.Offset
      );
   end "*";

   function "*"
            (  Left  : Imaginary;
               Right : Measure
            )  return Complex_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Compose_From_Cartesian (Right.Gain * Left),
         Offset => Right.Offset
      );
   end "*";

   function "*"
            (  Left  : Imaginary;
               Right : Complex_Measure
            )  return Complex_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Right.Gain * Left,
         Offset => Right.Offset
      );
   end "*";

   function "*"
            (  Left  : Complex;
               Right : Complex_Measure
            )  return Complex_Measure is
   begin
      return
      (  SI     => Right.SI,
         Gain   => Right.Gain * Left,
         Offset => Right.Offset
      );
   end "*";

   function "*"
            (  Left  : Measure;
               Right : Complex_Measure
            )  return Complex_Measure is
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

   function "*"
            (  Left  : Complex_Measure;
               Right : Number'Base
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*"
            (  Left  : Measure;
               Right : Imaginary
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Compose_From_Cartesian  (Left.Gain * Right),
         Offset => Left.Offset
      );
   end "*";

   function "*"
            (  Left  : Complex_Measure;
               Right : Imaginary
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*"
            (  Left  : Complex_Measure;
               Right : Complex
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain * Right,
         Offset => Left.Offset
      );
   end "*";

   function "*" (Left, Right : Complex_Measure)
      return Complex_Measure is
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

   function "*"
            (  Left  : Complex_Measure;
               Right : Measure
            )  return Complex_Measure is
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

   function "/"
            (  Left  : Number'Base;
               Right : Complex_Measure
            )  return Complex_Measure is
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

   function "/"
            (  Left  : Imaginary;
               Right : Measure
            )  return Complex_Measure is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      else
         return
         (  SI     => Units.Base.Unitless / Right.SI,
            Gain   => Compose_From_Cartesian (Left / Right.Gain),
            Offset => 0.0
         );
      end if;
   end "/";

   function "/"
            (  Left  : Imaginary;
               Right : Complex_Measure
            )  return Complex_Measure is
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

   function "/"
            (  Left  : Complex;
               Right : Complex_Measure
            )  return Complex_Measure is
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

   function "/"
            (  Left  : Measure;
               Right : Complex_Measure
            )  return Complex_Measure is
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

   function "/"
            (  Left  : Complex_Measure;
               Right : Number'Base
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/"
            (  Left  : Measure;
               Right : Imaginary
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Compose_From_Cartesian (Left.Gain / Right),
         Offset => Left.Offset
      );
   end "/";

   function "/"
            (  Left  : Complex_Measure;
               Right : Imaginary
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/"
            (  Left  : Complex_Measure;
               Right : Complex
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain / Right,
         Offset => Left.Offset
      );
   end "/";

   function "/"
            (  Left  : Complex_Measure;
               Right : Measure
            )  return Complex_Measure is
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

   function "/" (Left, Right : Complex_Measure)
      return Complex_Measure is
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

   function "and"
            (  Left  : Complex_Measure;
               Right : Number'Base
            )  return Complex_Measure is
   begin
      return
      (  SI     => Left.SI,
         Gain   => Left.Gain,
         Offset => Left.Offset + Right
      );
   end "and";

   function "+" (Left, Right : Complex_Measure)
      return Complex_Measure is
   begin
      if Left.SI /= Right.SI or else Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "+"
            (  Left  : Complex_Measure;
               Right : Measure
            )  return Complex_Measure is
   begin
      if Left.SI /= Right.SI or else Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "+"
            (  Left  : Measure;
               Right : Complex_Measure
            )  return Complex_Measure is
   begin
      if Left.SI /= Right.SI or else Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain + Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "+";

   function "-" (Left, Right : Complex_Measure)
      return Complex_Measure is
   begin
      if Left.SI /= Right.SI or else Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain - Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "-";

   function "-"
            (  Left  : Complex_Measure;
               Right : Measure
            )  return Complex_Measure is
   begin
      if Left.SI /= Right.SI or else Left.Offset /= Right.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Left.SI,
            Gain   => Left.Gain - Right.Gain,
            Offset => Left.Offset
         );
      end if;
   end "-";

   function "-"
            (  Left  : Measure;
               Right : Complex_Measure
            )  return Complex_Measure is
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

   function "=" (Left, Right : Complex_Measure) return Boolean is
   begin
      return
      (  Left.SI = Right.SI
      and then
         Left.Gain.Re + Left.Offset = Right.Gain.Re + Right.Offset
      and then
         Left.Gain.Im + Left.Offset = Right.Gain.Im + Right.Offset
      );
   end "=";

   function Argument (X : Complex_Measure) return Number'Base is
   begin
      return Argument (X.Gain);
   end Argument;

   function Argument
            (  X     : Complex_Measure;
               Cycle : Number'Base
            )  return Number'Base is
   begin
      return Argument (X.Gain, Cycle);
   end Argument;

   function Conjugate (X : Complex_Measure) return Complex_Measure is
   begin
      return
      (  SI     => X.SI,
         Gain   => Conjugate (X.Gain),
         Offset => X.Offset
      );
   end Conjugate;

   function Compose_From_Cartesian (Re, Im : Measure)
      return Complex_Measure is
   begin
      if Re.SI /= Im.SI or else Re.Offset /= Im.Offset then
         raise Unit_Error;
      else
         return
         (  SI     => Re.SI,
            Gain   => (Re.Gain, Im.Gain),
            Offset => Re.Offset
         );
      end if;
   end Compose_From_Cartesian;

   function Compose_From_Cartesian (Re : Measure)
      return Complex_Measure is
   begin
      return
      (  SI     => Re.SI,
         Gain   => (Re.Gain, 0.0),
         Offset => Re.Offset
      );
   end Compose_From_Cartesian;

   function Compose_From_Polar
            (  Modulus  : Measure;
               Argument : Number'Base
            )  return Complex_Measure is
   begin
      return
      (  SI     => Modulus.SI,
         Gain   => Compose_From_Polar (Modulus.Gain, Argument),
         Offset => Modulus.Offset
      );
   end Compose_From_Polar;

   function Compose_From_Polar
            (  Modulus  : Measure;
               Argument : Number'Base;
               Cycle    : Number'Base
            )  return Complex_Measure is
   begin
      return
      (  SI     => Modulus.SI,
         Gain   => Compose_From_Polar (Modulus.Gain, Argument, Cycle),
         Offset => Modulus.Offset
      );
   end Compose_From_Polar;

   function Convert
            (  Value : Complex_Measure;
               Scale : Measure
            )  return Complex_Measure is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      end if;
      declare
         Result : Complex_Measure (Value.SI);
         Shift  : constant Number'Base := Value.Offset - Scale.Offset;
      begin
         Result.Gain.Re := Value.Gain.Re + Shift;
         Result.Gain.Im := Value.Gain.Im + Shift;
         Result.Offset  := Scale.Offset;
         return Result;
      end;
   end Convert;

   function Get_Value (Value : Complex_Measure) return Complex is
   begin
      return
      (  Re => Value.Gain.Re + Value.Offset,
         Im => Value.Gain.Im + Value.Offset
      );
   end Get_Value;

   function Get_Value_As
            (  Value : Complex_Measure;
               Scale : Measure
            )  return Complex is
   begin
      if Value.SI /= Scale.SI then
         raise Unit_Error;
      end if;
      declare
         Shift : constant Number'Base := Value.Offset - Scale.Offset;
      begin
         return
         (  Re => (Value.Gain.Re + Shift) / Scale.Gain,
            Im => (Value.Gain.Im + Shift) / Scale.Gain
         );
      end;
   end Get_Value_As;

   function Get_Unit (Value : Complex_Measure) return Unit is
   begin
      return Value.SI;
   end Get_Unit;

   function Im (X : Complex_Measure) return Measure is
   begin
      return
      (  SI     => X.SI,
         Gain   => X.Gain.Im,
         Offset => X.Offset
      );
   end Im;

   function Normalize (Value : Complex_Measure)
      return Complex_Measure is
      Result : Complex_Measure (Value.SI);
   begin
      Result.Gain.Re := Value.Gain.Re + Value.Offset;
      Result.Gain.Im := Value.Gain.Im + Value.Offset;
      Result.Offset  := 0.0;
      return Result;
   end Normalize;

   function Re (X : Complex_Measure) return Measure is
   begin
      return
      (  SI     => X.SI,
         Gain   => X.Gain.Re,
         Offset => X.Offset
      );
   end Re;

   procedure Set_Im (X : in out Complex_Measure; Im : Measure) is
   begin
      if X.SI /= Im.SI then
         raise Unit_Error;
      end if;
      X.Gain.Im := Im.Gain + Im.Offset - X.Offset;
   end Set_Im;

   procedure Set_Re (X : in out Complex_Measure; Re : Measure) is
   begin
      if X.SI /= Re.SI then
         raise Unit_Error;
      end if;
      X.Gain.Re := Re.Gain + Re.Offset - X.Offset;
   end Set_Re;

   function Shift (Value : Complex_Measure; Shift : Number'Base)
      return Complex_Measure is
      Result : Complex_Measure (Value.SI);
   begin
      Result.Gain.Re := Value.Gain.Re - Shift;
      Result.Gain.Im := Value.Gain.Im - Shift;
      Result.Offset  := Value.Offset  + Shift;
      return Result;
   end Shift;

   function To_Measure (Value : Complex) return Complex_Measure is
   begin
      return (SI => Unitless, Gain => Value, Offset => 0.0);
   end To_Measure;

end Generic_Complex_Measures;
