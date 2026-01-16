--                                                                    --
--  package Units.UTF8_Edit         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2005       --
--                                                                    --
--                                Last revision :  09:59 09 Apr 2016  --
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

with Strings_Edit.Integers.Superscript;
use  Strings_Edit.Integers.Superscript;

package body Units.UTF8_Edit is
   use type UnitPower;

   Mask : constant := 2**PowerBits - 1;

   function "**" (Left : String; Right : Integer) return String is
   begin
      if Right = 1 then
         return Left;
      else
         return Left & Image (Right);
      end if;
   end "**";

   function "*" (Left, Right : String) return String is
   begin
      if Left = "" then
         return Right;
      elsif Right = "" then
         return Left;
      else
         return
         (  Left
         &  Character'Val (194)
         &  Character'Val (183)
         &  Right
         );
      end if;
   end "*";

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

   function Image (Value : Unit) return String is
      --
      -- Pos -- The base unit of positive power
      --
      --    Shift - To extract the power
      --    Name  - Of the corresponding base unit
      --
      -- Returns :
      --
      --    <name><power> if power is positive
      --
      function Pos (Shift : Natural; Name : String) return String is
         Power : constant Integer :=
            Integer (UnitPower (Shift_Right (Value, Shift)) and Mask);
      begin
         if Power in 1..2**(PowerBits - 1) - 1 then
            return Name ** Power;
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
      --    <name><-power> if power is negative
      --
      function Neg (Shift : Natural; Name : String) return String is
         Power : constant Integer :=
            Integer (UnitPower (Shift_Right (Value, Shift)) and Mask);
      begin
         if Power in 2**(PowerBits - 1)..2**PowerBits - 1 then
            return Name ** (2**PowerBits - Power);
         else
            return "";
         end if;
      end Neg;
      --
      -- Pow -- The base unit of negative power
      --
      --    Shift - To extract the power
      --    Name  - Of the corresponding base unit
      --
      -- Returns :
      --
      --    <name><power> if power is negative
      --
      function Pow (Shift : Natural; Name : String) return String is
         Power : constant Integer :=
            Integer (UnitPower (Shift_Right (Value, Shift)) and Mask);
      begin
         if Power in 2**(PowerBits - 1)..2**PowerBits - 1 then
            return Name ** (Power - 2**PowerBits);
         else
            return "";
         end if;
      end Pow;
      
      Numerator : constant String :=
                     (  Pos (PowerBits*0, "A"  )
                     *  Pos (PowerBits*1, "cd" )
                     *  Pos (PowerBits*2, "K"  )
                     *  Pos (PowerBits*3, "kg" )
                     *  Pos (PowerBits*4, "m"  )
                     *  Pos (PowerBits*5, "mol")
                     *  Pos (PowerBits*6, "s"  )
                     );
   begin
      if Numerator'Length = 0 then
         declare
            Denominator : constant String :=
                             (  Pow (PowerBits*0, "A"  )
                             *  Pow (PowerBits*1, "cd" )
                             *  Pow (PowerBits*2, "K"  )
                             *  Pow (PowerBits*3, "kg" )
                             *  Pow (PowerBits*4, "m"  )
                             *  Pow (PowerBits*5, "mol")
                             *  Pow (PowerBits*6, "s"  )
                             );
         begin
            if Denominator'Length = 0 then
               return "1";
            else
               return Denominator;
            end if;
         end;
      else
         return
         (  Numerator
         /  (  Neg (PowerBits*0, "A"  )
            *  Neg (PowerBits*1, "cd" )
            *  Neg (PowerBits*2, "K"  )
            *  Neg (PowerBits*3, "kg" )
            *  Neg (PowerBits*4, "m"  )
            *  Neg (PowerBits*5, "mol")
            *  Neg (PowerBits*6, "s"  )
         )  );
      end if;
   end Image;
   
end Units.UTF8_Edit;
