--                                                                    --
--  package Measures_Irregular      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  10:10 27 Apr 2013  --
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
-- This package provides some of the great variety  of  irregular  units
-- still  used  parallel to SI ones. It also contains some SI-compatible
-- units,  like  liter  and metric ton. They are defined as constants of
-- the  type  Measure.  The  package  is  generic.  The  formal  generic
-- parameter  is  an  instance  of  the  package  Measures_Derived.   To
-- instantiate this package one should write something like this:
--
-- with Measures;
--    ...
--    --
--    -- Instantiate Measures with type Float as the parameter
--    --
--    package Float_Measures is new Measures (Float);
--    --
--    -- Instantiate Measures_Derived
--    --
--    package Float_Measures_Derived is
--       new Measures_Derived (Float_Measures);
--    --
--    -- Instantiate Measures_Irregular
--    --
--    package Float_Measures_Irregular is
--       new Measures_Irregular (Float_Measures_Derived);
--
-- The package Float_Measures_Irregular is a non-generic version of  the
-- package based on the type Float.
--
with Measures_Derived;

generic
   with package Derived_Measures is new Measures_Derived (<>);
package Measures_Irregular is
   package Measures_Of renames Derived_Measures.Measures_Of;
   package Derived_Measures_Of renames Derived_Measures;
   use Measures_Of;
   use Derived_Measures;

   pi   : constant := 3.14159_26535_89793_23846_26433_83279_5;
   ln10 : constant := 2.30258_50929_94045_68401_79914_54684_4;
   --
   -- Some of irregular measurement units
   --
   ft   : aliased constant Measure := 0.3048 * m;        -- Foot
   inch : aliased constant Measure := ft / 12.0;         -- Inch
   lb   : aliased constant Measure := 0.453_592_37 * kg; -- Pound
   gal  : aliased constant Measure := 231.0 * inch**3;   -- Gallon

   acre : aliased constant Measure := 43_560.0 * ft**2; -- Acre
   ang  : aliased constant Measure := 1.0e-10 * m;      -- Angstroem
   are  : aliased constant Measure := 100.0 * m**2;     -- Are
   atm  : aliased constant Measure := 101_325.0 * Pa;   -- Atmosphere

   B          : aliased constant Measure := Np * ln10 * 0.5; -- Bel
   BTU        : aliased constant Measure := 1_055.056 * J;   -- British Termal Unit
   bar        : aliased constant Measure := 100_000.0 * Pa;  -- Bar
   barleycorn : aliased constant Measure := inch / 3.0;      -- Barleycorn
   barn       : aliased constant Measure := 1.0e-28 * m**2;  -- Barn
   barrel     : aliased constant Measure := 42.0 * gal;      -- Barrel

   Ci      : aliased constant Measure := 3.7e+10 * Bq;  -- Curie
   cal     : aliased constant Measure := 4.186_8 * J;   -- Calorie
   carat   : aliased constant Measure := 200.0e-6 * kg; -- Carat
   Celsius : aliased constant Measure := K and 273.15;  -- Celsius
   ch      : aliased constant Measure := 66.0 * Ft;     -- Chain
   cubit   : aliased constant Measure := 18.0 * inch;   -- Cubit

   d      : aliased constant Measure := 24.0 * 3600.0 * s;  -- Day
   dB     : aliased constant Measure := 10.0 * B;           -- Decibel
   degree : aliased constant Measure := pi * rad / 180.0;   -- Degree (angle)
   dram   : aliased constant Measure := lb / 256.0;         -- Dram
   dyn    : aliased constant Measure := 1.0e-5 * N;         -- Dyn

   eV  : aliased constant Measure := 1.602_18e-19 * J; -- Electronvolt
   ell : aliased constant Measure := 45.0 * inch;      -- Ell
   erg : aliased constant Measure := 1.0e-7 * J;       -- Erg

   Fahrenheit : aliased constant Measure := K / 1.8 and (459.67 / 1.8); -- Fahrenheit
   fathom     : aliased constant Measure := ft * 6.0;        -- Fathom
   finger     : aliased constant Measure := 4.5 * inch;      -- Finger
   fpm        : aliased constant Measure := ft / (60.0 * s); -- Feet per minute
   fps        : aliased constant Measure := ft / s;          -- Feet per second
   fur        : aliased constant Measure := ft * 660.0;      -- Furlong

   G      : aliased constant Measure := 1.0e-4 * kg / (A * s**2); -- Gauss
   gi     : aliased constant Measure := gal / 32.0;               -- Gill
   grain  : aliased constant Measure := 0.000_064_798_91 * kg;    -- Grain
   gram   : aliased constant Measure := kg / 1000.0;              -- Gram

   h       : aliased constant Measure := 3600.0 * s;     -- Hour
   hand    : aliased constant Measure := ft / 3.0;       -- Hand
   hectare : aliased constant Measure := 10000.0 * m**2; -- Hectare
   hp      : aliased constant Measure := 735.498_8 * W;  -- Horsepower

   INM : aliased constant Measure := 1852.0 * m; -- International Nautical Mile

   kcal : aliased constant Measure := 1_000.0 * cal;   -- Kilocalorie
   kgf  : aliased constant Measure := 9.806_65 * N;    -- Kilogram-force
   knot : aliased constant Measure := 1852.0 * m / h;  -- Knot

   L      : aliased constant Measure := 0.001 * m**3;       -- Liter
   league : aliased constant Measure := 3.0 * 5_280.0 * ft; -- League
   line   : aliased constant Measure := inch / 12.0;        -- Line
   link   : aliased constant Measure := 0.66 * ft;          -- Link
   ly     : aliased constant Measure := 9.460_73e+15 * m;    -- Light year

   mi         : aliased constant Measure := 5_280.0 * ft;  -- Land mile
   min        : aliased constant Measure := 60.0 * s;      -- Minute
   min_of_arc : aliased constant Measure := degree / 60.0; -- Minute of arc
   mpg        : aliased constant Measure := mi / gal;      -- Miles per gallon
   mph        : aliased constant Measure := mi / h;        -- Miles per hour
   mps        : aliased constant Measure := mi / s;        -- Miles per second

   nail : aliased constant Measure := Inch * 2.25;        -- Nail

   Oe : aliased constant Measure := (250.0 / Pi) * A / m;   -- Oersted
   oz : aliased constant Measure := lb / 16.0;              -- Ounce

   pace    : aliased constant Measure := 30.0 * inch;       -- Pace
   pc      : aliased constant Measure := 3.085_678e+16 * m; -- Parsec
   percent : aliased constant Measure := 0.01 * Np;         -- Percent
   point   : aliased constant Measure := 0.013_837 * Inch;  -- Point
   ppb     : aliased constant Measure := Np / 1.0e+09;      -- Parts per billion
   ppm     : aliased constant Measure := Np / 1.0e+06;      -- Parts per million
   ppt     : aliased constant Measure := Np / 1.0e+12;      -- Parts per trillion
   psi     : aliased constant Measure := Pa * 6894.7;       -- Pounds per square inch
   pt      : aliased constant Measure := gal / 8.0;         -- Pint

   qt : aliased constant Measure := gal / 4.0;         -- Quart

   R    : aliased constant Measure := 2.58e-4 * C / kg;     -- Roentgen
   rd   : aliased constant Measure := 5.5 * 3.0 * Ft;       -- Rod
   rood : aliased constant Measure := fur**2;               -- Rood
   rpm  : aliased constant Measure := 360.0 * degree / min; -- Revolutions per minute
   rps  : aliased constant Measure := 360.0 * degree / s;   -- Revolutions per second

   sec_of_arc : aliased constant Measure := degree / 3600.0;   -- Second of arc
   span       : aliased constant Measure := 9.0 * Inch;        -- Span

   t          : aliased constant Measure := 1000.0 * kg;            -- Metric ton
   tablespoon : aliased constant Measure := gi / 8.0;               -- Tablespoon
   teaspoon   : aliased constant Measure := tablespoon / 3.0;       -- Teaspoon
   torr       : aliased constant Measure := (101_325.0/760.0) * Pa; -- Torr (mm Hg)
   township   : aliased constant Measure := 36.0 * mi**2;           -- Township

   u  : aliased constant Measure := 1.660_54e-27 * kg;  -- Unified atomic mass
   ua : aliased constant Measure := 1.495_98e+11 * m;   -- Astronomical unit

   wineglass : aliased constant Measure := pt / 4.0;    -- Wineglass 4 fl oz

   yd   : aliased constant Measure := 0.914_4 * m;      -- International yard
   year : aliased constant Measure := 3.155_693e+7 * s; -- Year

end Measures_Irregular;
