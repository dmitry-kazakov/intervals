--                                                                    --
--  package Units.Edit              Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Strings_Edit.Integers;       use Strings_Edit.Integers;

package body Units.Edit is
   use type UnitPower;

   Mask : constant := 2**PowerBits - 1;

   function Image
            (  Value  : Unit;
               Latin1 : Boolean := True
            )  return String is
      --
      -- Operand -- Compose a unit operand
      --
      --    Power - Of the operand
      --    Name  - Base SI unit name
      --
      -- Returns :
      --
      --    <name>^<power>
      --
      function Operand
               (  Power : Positive;
                  Name  : String
               )  return String is
      begin
         if Power = 1 then
            return Name;
         end if;
         if Latin1 then
            case Power is
               when 1      => return Name;
               when 2      => return Name & '²';
               when 3      => return Name & '³';
               when others => null;
            end case;
         end if;
         return Name & '^' & Image (Power);
      end Operand;
      --
      -- Pos -- The base unit of positive power
      --
      --    Shift - To extract the power
      --    Name  - Of the corresponding base unit
      --
      -- Returns :
      --
      --    <name>^<power> if power is positive
      --
      function Pos (Shift : Natural; Name : String) return String is
         Power : Integer;
      begin
         Power :=
            Integer (UnitPower (Shift_Right (Value, Shift)) and Mask);
         if Power in 1..2**(PowerBits - 1) - 1 then
            return Operand (Power, Name);
         else
            return "";
         end if;
      end Pos;
      --
      -- Neg -- The base unit of negative power
      --
      --    Shift - To extract the power
      --    Name  - Of the corresponding base unit
      --
      -- Returns :
      --
      --    <name>^<-power> if power is negative
      --
      function Neg (Shift : Natural; Name : String) return String is
         Power : Integer;
      begin
         Power :=
            Integer (UnitPower (Shift_Right (Value, Shift)) and Mask);
         if Power in 2**(PowerBits - 1)..2**PowerBits - 1 then
            return Operand (2**PowerBits - Power, Name);
         else
            return "";
         end if;
      end Neg;
      --
      -- * -- Compose two operands
      --
      --    Left, Right - Operands
      --
      -- Returns :
      --
      --    <left> * <right>
      --
      function "*" (Left, Right : String) return String is
      begin
         if 0 = Left'Length then
            return Right;
         elsif 0 = Right'Length then
            return Left;
         else
            if Latin1 then
               return Left & '·' & Right;
            else
               return Left & '*' & Right;
            end if;
         end if;
      end "*";
      --
      -- / -- Compose two operands
      --
      --    Left, Right - Operands
      --
      -- Returns :
      --
      --    <left> / <right>
      --
      function "/" (Left, Right : String) return String is
      begin
         if 0 = Left'Length then
            if 0 = Right'Length then
               return "1";
            else
               return "1/" & Right;
            end if;
         else
            if 0 = Right'Length then
               return Left;
            else
               return Left & '/' & Right;
            end if;
         end if;
      end "/";
   begin
      return
      (  (  Pos (PowerBits*0, "A"  )
         *  Pos (PowerBits*1, "cd" )
         *  Pos (PowerBits*2, "K"  )
         *  Pos (PowerBits*3, "kg" )
         *  Pos (PowerBits*4, "m"  )
         *  Pos (PowerBits*5, "mol")
         *  Pos (PowerBits*6, "s"  )
         )
      /  (  Neg (PowerBits*0, "A"  )
         *  Neg (PowerBits*1, "cd" )
         *  Neg (PowerBits*2, "K"  )
         *  Neg (PowerBits*3, "kg" )
         *  Neg (PowerBits*4, "m"  )
         *  Neg (PowerBits*5, "mol")
         *  Neg (PowerBits*6, "s"  )
      )  );
   end Image;
   
end Units.Edit;
