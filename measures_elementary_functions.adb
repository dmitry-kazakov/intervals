--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Elementary_Functions               Luebeck            --
--  Implementation                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  10:13 13 Oct 2007  --
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

with Units;             use Units;
with Units.Base;
with Ada.Numerics.Generic_Elementary_Functions;

package body Measures_Elementary_Functions is
   package Elementary_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (Number);
   use Elementary_Functions;

   function Sqrt (X : Measure) return Measure is
      SI : Unit;
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      begin
         SI := Sqrt (X.SI);
      exception
         when Constraint_Error => raise Unit_Error;
      end;
      return (SI, Sqrt (X.Gain), 0.0);
   end Sqrt;

   function Log (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Log (X.Gain), 0.0);
   end Log;

   function Log (X : Dimensionless; Base : Number'Base)
      return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Log (X.Gain, Base), 0.0);
   end Log;

   function Exp (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Exp (X.Gain), 0.0);
   end Exp;

   function "**" (Left : Dimensionless; Right : Dimensionless)
      return Dimensionless is
   begin
      if Left.Offset /= 0.0 or Right.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Left.Gain ** Right.Gain, 0.0);
   end "**";

   function "**" (Left : Dimensionless; Right : Number'Base)
      return Dimensionless is
   begin
      if Left.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Left.Gain ** Right, 0.0);
   end "**";

   function "**" (Left : Number'Base; Right : Dimensionless)
      return Dimensionless is
   begin
      if Right.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Left ** Right.Gain, 0.0);
   end "**";

   function Sin (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Sin (X.Gain), 0.0);
   end Sin;

   function Cos (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Cos (X.Gain), 0.0);
   end Cos;

   function Tan (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Tan (X.Gain), 0.0);
   end Tan;

   function Cot (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Cot (X.Gain), 0.0);
   end Cot;

   function Arcsin (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arcsin (X.Gain), 0.0);
   end Arcsin;

   function Arccos (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arccos (X.Gain), 0.0);
   end Arccos;

   function Arctan (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arctan (X.Gain), 0.0);
   end Arctan;

   function Arccot (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arccot (X.Gain), 0.0);
   end Arccot;

   function Arctan (Y, X : Measure) return Dimensionless is
   begin
      if X.SI /= Y.SI or X.Offset /= 0.0 or Y.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arctan (Y.Gain, X.Gain), 0.0);
   end Arctan;

   function Arccot (X, Y : Measure) return Dimensionless is
   begin
      if X.SI /= Y.SI or X.Offset /= 0.0 or Y.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arccot (X.Gain, Y.Gain), 0.0);
   end Arccot;

   function Sinh (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Sinh (X.Gain), 0.0);
   end Sinh;

   function Cosh (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Cosh (X.Gain), 0.0);
   end Cosh;

   function Tanh (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Tanh (X.Gain), 0.0);
   end Tanh;

   function Coth (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Coth (X.Gain), 0.0);
   end Coth;

   function Arcsinh (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arcsinh (X.Gain), 0.0);
   end Arcsinh;

   function Arccosh (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arccosh (X.Gain), 0.0);
   end Arccosh;

   function Arctanh (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arctanh (X.Gain), 0.0);
   end Arctanh;

   function Arccoth (X : Dimensionless) return Dimensionless is
   begin
      if X.Offset /= 0.0 then
         raise Unit_Error;
      end if;
      return (Units.Base.Unitless, Arccoth (X.Gain), 0.0);
   end Arccoth;
end Measures_Elementary_Functions;

