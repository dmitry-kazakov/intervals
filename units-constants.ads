--                                                                    --
--  package Units.Constants         Copyright (c)  Dmitry A. Kazakov  --
--  Interface (no body)                            Luebeck            --
--                                                 Spring, 2000       --
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

with Units.Base; use Units.Base;

package Units.Constants is
--
-- Geometry
--
   Area          : constant Unit := Length ** 2;
   Volume        : constant Unit := Length ** 3;
--
-- Mechanics
--
   Velocity      : constant Unit := Length / Time;
   Acceleration  : constant Unit := Length / Time ** 2;
   Force         : constant Unit := Mass * Acceleration;
   Pressure      : constant Unit := Force / Area;
   Energy        : constant Unit := Force * Length;
   Power         : constant Unit := Energy / Time;
--
-- Electricity
--
   Charge        : constant Unit := Current * Time;
   Potential     : constant Unit := Energy / Charge;
   Capacitance   : constant Unit := Charge / Potential;
   Resistance    : constant Unit := Potential / Current;
   Conductance   : constant Unit := Current / Potential;
   Inductance    : constant Unit := Potential * Time / Current;
--
-- Chemistry
--
   Concentration : constant Unit := Quantity / Volume;
   Density       : constant Unit := Mass / Volume;
--
-- Optic
--
   Luminance     : constant Unit := Luminescence / Area;
--
-- Other
--
   Frequency     : constant Unit := Unitless / Time;

end Units.Constants;
