--                                                                    --
--  package Measures_Derived        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  17:19 05 Jul 2008  --
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
-- This package provides the derived SI units as constants of  the  type
-- Measure.  The  package is generic. The formal generic parameter is an
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
--    -- Instantiate Measures_Derived
--    --
--    package Real_Measures_Derived is
--       new Measures_Derived (Float, Real_Measures);
--
-- The package Float_Measures_Derived is a non-generic  version  of  the
-- package based on the type Float.
--
-- Implementation  notes. The following SI units have full names instead
-- of abrreviated ones:
--
-- (o)  Siemens instead of S. Reason: S would conflict with s  (seconds)
--      defined in Measures.
--
-- (o)  Henry instead of H. Reason: to reserve place for h (hour), which
--      being  a non-SI unit would be still more important for many than
--      Henry.
--
-- (o)  Tesla instead of T. Reason: t is reserved for ton.
--
-- The constants are aliased. The reason  behind  is  that  the  package
-- Measures_Edit needs their addresses.
--
with Measures;

generic
   with package Measures is new Standard.Measures (<>);
package Measures_Derived is
   package Measures_Of renames Measures;
   use Measures_Of;
--
-- Derived SI measurement units
--
   C       : aliased constant Measure := A * s;                     -- Coulomb
   Bq      : aliased constant Measure := 1.0 / s;                   -- Becquerel
   F       : aliased constant Measure := s**4 * A**2 / (kg * m**2); -- Farad
   Gy      : aliased constant Measure := m**2 / s**2;               -- Gray
   Henry   : aliased constant Measure := m**2 * kg / (s**2 * A**2); -- Henry
   Hz      : aliased constant Measure := 1.0 / s;                   -- Hertz
   J       : aliased constant Measure := m**2 * kg / s**2;          -- Joule
   N       : aliased constant Measure := kg * m / s**2;             -- Newton
   Ohm     : aliased constant Measure := m**2 * kg / (s**3 * A**2); -- Ohm
   Pa      : aliased constant Measure := kg / (m * s**2);           -- Pascal
   Siemens : aliased constant Measure := A**2 * s**3 / (kg * m**2); -- Siemens
   Sv      : aliased constant Measure := Gy;                        -- Sievert
   Tesla   : aliased constant Measure := kg / (A* s**2);            -- Tesla
   V       : aliased constant Measure := m**2 * kg / (s**3 * A);    -- Volt
   W       : aliased constant Measure := J / s;                     -- Watt
   Wb      : aliased constant Measure := V * s;                     -- Weber
   lm      : aliased constant Measure := cd * sr;                   -- Lumen
   lx      : aliased constant Measure := lm / m**2;                 -- Lux
   kat     : aliased constant Measure := mol / s;                   -- Katal

end Measures_Derived;
