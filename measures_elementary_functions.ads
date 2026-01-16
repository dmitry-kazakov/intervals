--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Elementary_Functions               Luebeck            --
--  Interface                                      Spring, 2000       --
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
--
-- This  package  provides  elementary   functions   for   dimensionless
-- Measures. The package is generic. The formal generic parameter is  an
-- instance of the Measures package. To  instantiate  this  package  one
-- should write something like this: 
--
-- with Measures;
--    ...
--    --
--    -- Instantiate Measures with type Float as the parameter
--    --
--    package Real_Measures is new Measures (Float);
--    --
--    -- Instantiate Measures_Elementary_Functions
--    --
--    package Elementary_Functions is
--       new Measures_Elementary_Functions (Real_Measures);
--
-- Most of elementary  functions  can be applied to dimensionless Measures only.
-- There are two exceptions:
--
-- (o)  Sqrt  accepts any measures which units have base components with
--      even exponent part. For instance: 
--
--      Area : Measure := 25.0 * m**2;
--      Side : Measure;
--      ...
--      Side := Sqrt (Area);   -- OK, the result is 5 m
--      Side := Sqrt (Side);   -- Error, Unit_Error propagates
--
-- (o)  Arctan/Arccot  are  defined for any pair of compatible Measures.
--      For instance: 
--
--      subtype Height is Measure (Length);
--      subtype Width  is Measure (Length);
--      X : Width;
--      Y : Height;
--      Angle : Dimensionless;
--      ...
--      X := 25.0 * m;
--      Y := 30.1 * ft;       -- ft is declared in Measures_Irregular
--      Angle := Arctan (Y, X);
--
-- The parameter of trigonometric  functions  is  measured  in  radians.
-- There are no variants with the parameter Cycle, because arguments  in
-- degrees can be naturally exressed using units: 
--
--    Cos (180.0 * degree); -- degree is declared in Measures_Irregular
--    Cos (1.5 * rad);
--
-- The  package  Float_Measures_Elementary_Functions  is  a  non-generic
-- version of the package  Measures_Elementary_Functions  based  on  the
-- type Float. 
--
with Measures;

generic
   with package Measures is new Standard.Measures (<>);
package Measures_Elementary_Functions is
   use Measures;

   function Sqrt (X : Measure) return Measure;
   pragma Inline (Sqrt);

   function Exp (X : Dimensionless) return Dimensionless;
   function Log (X : Dimensionless) return Dimensionless;
   function Log (X : Dimensionless; Base : Number'Base)
      return Dimensionless;
   pragma Inline (Exp, Log);

   function "**" (Left : Dimensionless; Right : Dimensionless)
      return Dimensionless;
   function "**" (Left : Dimensionless; Right : Number'Base)
      return Dimensionless;
   function "**" (Left : Number'Base; Right : Dimensionless)
      return Dimensionless;
   pragma Inline ("**");

   function Sin (X : Dimensionless) return Dimensionless;
   function Cos (X : Dimensionless) return Dimensionless;
   function Tan (X : Dimensionless) return Dimensionless;
   function Cot (X : Dimensionless) return Dimensionless;
   pragma Inline (Sin, Cos, Tan, Cot);

   function Arcsin (X : Dimensionless) return Dimensionless;
   function Arccos (X : Dimensionless) return Dimensionless;
   function Arctan (X : Dimensionless) return Dimensionless;
   function Arccot (X : Dimensionless) return Dimensionless;
   function Arctan (Y, X : Measure) return Dimensionless;
   function Arccot (X, Y : Measure) return Dimensionless;
   pragma Inline (Arcsin, Arccos, Arctan, Arccot);

   function Sinh (X : Dimensionless) return Dimensionless;
   function Cosh (X : Dimensionless) return Dimensionless;
   function Tanh (X : Dimensionless) return Dimensionless;
   function Coth (X : Dimensionless) return Dimensionless;
   pragma Inline (Sinh, Cosh, Tanh, Coth);

   function Arcsinh (X : Dimensionless) return Dimensionless;
   function Arccosh (X : Dimensionless) return Dimensionless;
   function Arctanh (X : Dimensionless) return Dimensionless;
   function Arccoth (X : Dimensionless) return Dimensionless;
   pragma Inline (Arcsinh, Arccosh, Arctanh, Arccoth);
end Measures_Elementary_Functions;

