--                                                                    --
--  package Units                   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  11:50 30 May 2014  --
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

package body Units is
   use type UnitPower;

   EvenCarry : constant :=
                  (  2**(PowerBits*1)
                  +  2**(PowerBits*3)
                  +  2**(PowerBits*5)
                  +  2**(PowerBits*7)
                  );
   OddCarry  : constant :=
                  (  2**(PowerBits*2)
                  +  2**(PowerBits*4)
                  +  2**(PowerBits*6)
                  );
   Even      : constant :=
                  (  2**(PowerBits*1) - 2**(PowerBits*0)
                  +  2**(PowerBits*3) - 2**(PowerBits*2)
                  +  2**(PowerBits*5) - 2**(PowerBits*4)
                  +  2**(PowerBits*7) - 2**(PowerBits*6)
                  );
   Odd       : constant :=
                  (  2**(PowerBits*2) - 2**(PowerBits*1)
                  +  2**(PowerBits*4) - 2**(PowerBits*3)
                  +  2**(PowerBits*6) - 2**(PowerBits*5)
                  );
   Sign      : constant :=
                  (  2**(PowerBits*1 - 1)
                  +  2**(PowerBits*2 - 1)
                  +  2**(PowerBits*3 - 1)
                  +  2**(PowerBits*4 - 1)
                  +  2**(PowerBits*5 - 1)
                  +  2**(PowerBits*6 - 1)
                  +  2**(PowerBits*7 - 1)
                  );
   One       : constant :=
                  (  2**(PowerBits*0)
                  +  2**(PowerBits*1)
                  +  2**(PowerBits*2)
                  +  2**(PowerBits*3)
                  +  2**(PowerBits*4)
                  +  2**(PowerBits*5)
                  +  2**(PowerBits*6)
                  );

   function GetEven (Right : Unit) return UnitPower;
   function GetOdd  (Right : Unit) return UnitPower;
   pragma Inline (GetEven, GetOdd);

   function GetEven (Right : Unit) return UnitPower is
   begin
      return UnitPower (Right) and Even;
   end GetEven;

   function GetOdd (Right : Unit) return UnitPower is
   begin
      return UnitPower (Right) and Odd;
   end GetOdd;

   function "**" (Left : Unit; Right : Integer) return Unit is
      Result : Unit := Unitless;
   begin
      if Left /= Unitless and 0 /= Right then
         if Right > 0 then
            for Sum in 1..Right loop
               Result := Result * Left;
            end loop;
         else
            for Sum in Right..-1 loop
               Result := Result / Left;
            end loop;
         end if;
      end if;
      return Result;
   end "**";

   function "*" (Left, Right : Unit) return Unit is
      Result : UnitPower;
   begin
      Result :=
         (  ((GetEven (Left) + GetEven (Right)) and Even)
         or ((GetOdd  (Left) + GetOdd  (Right)) and Odd )
         );
      if (  0
         /= (  (Result xor UnitPower (Left))
            and
               (Result xor UnitPower (Right))
            and
               Sign
         )  )
      then
         raise Constraint_Error;
      end if;
      return Unit (Result);
   end "*";

   function "/" (Left, Right : Unit) return Unit is
      Result : UnitPower;
   begin
      Result :=
         (  (GetEven (Left) + (EvenCarry - GetEven (Right)) and Even)
         or (GetOdd  (Left) + (OddCarry  - GetOdd  (Right)) and Odd )
         );
      if (  0
         /= (  (Result xor UnitPower (Left))
            and
               (UnitPower (Left) xor UnitPower (Right))
            and
               Sign
         )  )
      then
         raise Constraint_Error;
      end if;
      return Unit (Result);
   end "/";

   procedure Split
             (  SI           : Unit;
                Current      : out Natural;
                Luminescence : out Natural;
                Temperature  : out Natural;
                Mass         : out Natural;
                Length       : out Natural;
                Quantity     : out Natural;
                Time         : out Natural
             )  is
      use Interfaces;
      X    : constant UnitPower := UnitPower (SI);
      Mask : constant UnitPower := 2**PowerBits - 1;
   begin
      Current      := Natural (             X               and Mask);
      Luminescence := Natural (Shift_Right (X, PowerBits  ) and Mask);
      Temperature  := Natural (Shift_Right (X, PowerBits*2) and Mask);
      Mass         := Natural (Shift_Right (X, PowerBits*3) and Mask);
      Length       := Natural (Shift_Right (X, PowerBits*4) and Mask);
      Quantity     := Natural (Shift_Right (X, PowerBits*6) and Mask);
      Time         := Natural (Shift_Right (X, PowerBits*6) and Mask);
   end Split;

   function Sqrt (X : Unit) return Unit is
   begin
      if 0 /= (UnitPower (X) and One) then
         raise Constraint_Error;
      end if;
      return
         Unit
         (  (UnitPower (X) and Sign)
         +  UnitPower (Shift_Right (X, 1))
         );
   end Sqrt;

end Units;
