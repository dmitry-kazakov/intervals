--                                                                    --
--  package Units.Edit              Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2005       --
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

package Units.Edit is
--
-- Image -- Unit to string conversion
--
--    Value  - To be converted
--    Latin1 - Latin-1 character set flag
--
-- This  function is used to convert a unit to string. The syntax of the
-- result is: 
--
--    <result>    ::= <list>/<list> | 1/<list> | <list> | 1
--    <list>      ::= <item> [<*><list>]
--    <item>      ::= <base-unit> [<power>] 
--    <base-unit> ::= A | cd | K | kg | m | mol | s
--
-- Here  <*>  is  either  * (if Latin1 is false), or Latin-1 point. When
-- Latin1 is false <power> is always ^<number>. With Latin-character set
-- enabled, powers  2  and  3  are  indicated  using  the  corresponding
-- superscript characters.
--
-- For instance: 
--
--    m/s^2    -- Velocity unit (Latin1 = false)
--      
-- Returns :
--
--    The string
--
   function Image
            (  Value  : Unit;
               Latin1 : Boolean := True
            )  return String;

end Units.Edit;
