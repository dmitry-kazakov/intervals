--                                                                    --
--  package Intervals               Copyright (c)  Dmitry A. Kazakov  --
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

package body Intervals is

   function To_Logical (Left : Boolean) return Logical is
   begin
      case Left is
         when False => return False;
         when True  => return True;
      end case;
   end To_Logical;

   function "not" (Left : Logical) return Logical is
   begin
      case Left is
         when False     => return True;
         when True      => return False;
         when Uncertain => return Uncertain;
      end case;
   end "not";

   function "and" (Left, Right : Logical) return Logical is
   begin
      if Left = False or else Right = False then
         return False;
      elsif Left = Uncertain or else Right = Uncertain then
         return Uncertain;
      else
         return True;
      end if;
   end "and";

   function "and" (Left : Boolean; Right : Logical) return Logical is
   begin
      if Left = False or else Right = False then
         return False;
      elsif Right = Uncertain then
         return Uncertain;
      else
         return True;
      end if;
   end "and";

   function "and" (Left : Logical; Right : Boolean) return Logical is
   begin
      if Left = False or else Right = False then
         return False;
      elsif Left = Uncertain then
         return Uncertain;
      else
         return True;
      end if;
   end "and";

   function "or" (Left, Right : Logical) return Logical is
   begin
      if Left = True or else Right = True then
         return True;
      elsif Left = Uncertain or else Right = Uncertain then
         return Uncertain;
      else
         return False;
      end if;
   end "or";

   function "or" (Left : Boolean; Right : Logical) return Logical is
   begin
      if Left = True or else Right = True then
         return True;
      elsif Right = Uncertain then
         return Uncertain;
      else
         return False;
      end if;
   end "or";

   function "or" (Left : Logical; Right : Boolean) return Logical is
   begin
      if Left = True or else Right = True then
         return True;
      elsif Left = Uncertain then
         return Uncertain;
      else
         return False;
      end if;
   end "or";

   function "xor" (Left, Right : Logical) return Logical is
   begin
      if Left = Uncertain or else Right = Uncertain then
         return Uncertain;
      elsif Left = Right then
         return False;
      else
         return True;
      end if;
   end "xor";

   function "xor" (Left : Boolean; Right : Logical) return Logical is
   begin
      if Right = Uncertain then
         return Uncertain;
      elsif To_Logical (Left) = Right then
         return False;
      else
         return True;
      end if;
   end "xor";

   function "xor" (Left : Logical; Right : Boolean) return Logical is
   begin
      if Left = Uncertain then
         return Uncertain;
      elsif Left = To_Logical (Right) then
         return False;
      else
         return True;
      end if;
   end "xor";

end Intervals;
