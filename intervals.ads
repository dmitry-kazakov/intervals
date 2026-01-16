--                                                                    --
--  package Intervals               Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
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

package Intervals is
   pragma Pure (Intervals);
--
-- Logical -- Three-state logic
--
   type Logical is (False, True, Uncertain);
--
-- To_Logical -- Convert Bollean to Logical
--
   function To_Logical (Left : Boolean) return Logical;
   pragma Inline (To_Logical);
--
-- not -- Inversion
--
--      Left - The argument
--
-- Returns :
--
--      not Left
--
   function "not" (Left : Logical) return Logical;
   pragma Inline ("not");
--
-- and -- Logical and
--
--      Left  - The first argument
--      Right - The second argument
--
-- Either  both  arguments  are  Logical  or one of them is Boolean. The
-- result is False if one of arguments is  False,  else  the  result  is
-- Uncertain when at least one of arguments is Uncertain. Otherwise  the
-- result is True. 
--
-- Returns :
--
--      Left and Right
--
   function "and" (Left, Right : Logical) return Logical;
   function "and" (Left : Boolean; Right : Logical) return Logical;
   function "and" (Left : Logical; Right : Boolean) return Logical;
   pragma Inline ("and");
--
-- or -- Logical or
--
--      Left  - The first argument
--      Right - The second argument
--
-- Either  both  arguments  are  Logical  or one of them is Boolean. The
-- result is True if one of  arguments  is  True,  else  the  result  is
-- Uncertain when at least one of arguments is Uncertain. Otherwise  the
-- result is False. 
--
-- Returns :
--
--      Left or Right
--
   function "or" (Left, Right : Logical) return Logical;
   function "or" (Left : Boolean; Right : Logical) return Logical;
   function "or" (Left : Logical; Right : Boolean) return Logical;
   pragma Inline ("or");
--
-- xor -- Logical xor
--
--      Left  - The first argument
--      Right - The second argument
--
-- Either  both  arguments  are  Logical  or one of them is Boolean. The
-- result Uncertain  when  at  least  one  of  arguments  is  Uncertain.
-- Otherwise the result is Left xor Right like in Boolean logic. 
--
-- Returns :
--
--      Left xor Right
--
   function "xor" (Left, Right : Logical) return Logical;
   function "xor" (Left : Boolean; Right : Logical) return Logical;
   function "xor" (Left : Logical; Right : Boolean) return Logical;
   pragma Inline ("xor");

end Intervals;
