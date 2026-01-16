--                                                                    --
--  package Test_Integer_Intervals  Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  23:35 29 May 2010  --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with Intervals;                use Intervals;
with Integer_Intervals;        use Integer_Intervals;

procedure Test_Integer_Intervals is
   function Image (Value : Logical) return String is
   begin
      case Value is
         when False     => return "False";
         when True      => return "True";
         when Uncertain => return "Uncertain";
      end case;
   end Image;

   procedure Check
             (  X, Y           : Interval;
                LT, LE, GE, GT : Logical
             )  is
      procedure Compare
                (  Result   : Logical;
                   Expected : Logical;
                   Name     : String
                )  is
      begin
         if Result /= Expected then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Comparison ["
               &  Integer'Image (X.From)
               &  ", "
               &  Integer'Image (X.To)
               &  "] "
               &  Name
               &  " ["
               &  Integer'Image (Y.From)
               &  ", "
               &  Integer'Image (Y.To)
               &  "] is "
               &  Image (Result)
               &  ", expected "
               &  Image (Expected)
            )  );
         end if;
      end Compare;
   begin
      Compare (X <  Y, LT, "<" );
      Compare (X >  Y, GT, ">" );
      Compare (X <= Y, LE, "<=");
      Compare (X >= Y, GE, ">=");
   end Check;

   procedure Check
             (  X              : Interval;
                Y              : Integer;
                LT, LE, GE, GT : Logical
             )  is
      procedure Compare
                (  Result   : Logical;
                   Expected : Logical;
                   Name     : String
                )  is
      begin
         if Result /= Expected then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Comparison ["
               &  Integer'Image (X.From)
               &  ", "
               &  Integer'Image (X.To)
               &  "] "
               &  Name
               &  Integer'Image (Y)
               &  " is "
               &  Image (Result)
               &  ", expected "
               &  Image (Expected)
            )  );
         end if;
      end Compare;
   begin
      Compare (X <  Y, LT, "<" );
      Compare (X >  Y, GT, ">" );
      Compare (X <= Y, LE, "<=");
      Compare (X >= Y, GE, ">=");
   end Check;

   procedure Check
             (  X              : Integer;
                Y              : Interval;
                LT, LE, GE, GT : Logical
             )  is
      procedure Compare
                (  Result   : Logical;
                   Expected : Logical;
                   Name     : String
                )  is
      begin
         if Result /= Expected then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Comparison "
               &  Integer'Image (X)
               &  " "
               &  Name
               &  " ["
               &  Integer'Image (Y.From)
               &  ", "
               &  Integer'Image (Y.To)
               &  "] is "
               &  Image (Result)
               &  ", expected "
               &  Image (Expected)
            )  );
         end if;
      end Compare;
   begin
      Compare (X <  Y, LT, "<" );
      Compare (X >  Y, GT, ">" );
      Compare (X <= Y, LE, "<=");
      Compare (X >= Y, GE, ">=");
   end Check;

begin
   Put_Line ("Testing integer interval arithmetic ...");
   if Distance ((1,2),(1,2)) /= 0 then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Distance ((1,2),(1,2)) /= 0"
      );
   end if;
   if Distance ((-2,-1),(1,2)) /= 3 then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Distance ((-2,-1),(1,2)) /= 3"
      );
   end if;
   if (      Is_In (0, (1,2))
      or not Is_In (1, (1,2))
      or not Is_In (2, (1,2))
      or     Is_In (3, (1,2))
      or     Is_In ((1,3), (1,2))
      or not Is_In ((1,2), (1,3))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Is_In"
      );
   end if;
   if (  Is_Negative (( 1,2))
      or Is_Negative (( 0,2))
      or Is_Negative ((-2,0))
      or not Is_Negative ((-2,-1))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Is_Negative"
      );
   end if;
   if (  Is_Positive ((-2,-2))
      or Is_Positive ((-3, 0))
      or Is_Positive (( 0, 2))
      or not Is_Positive ((1,2))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Is_Positive"
      );
   end if;
   if (  Length ((-2,-2)) /= 0
      or Length ((-3, 0)) /= 3
      or Length (( 0, 2)) /= 2
      or Length (( 1, 2)) /= 1
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Length"
      );
   end if;
   if (  abs (-2,-2) /= (2, 2)
      or abs (-3, 0) /= (0, 3)
      or abs ( 0, 2) /= (0, 2)
      or abs ( 1, 2) /= (1, 2)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in abs"
      );
   end if;
   if (      ((-2,-2) & ( 2, 2))
      or not ((-3, 0) & ( 0, 0))
      or not (( 0, 2) & ( 0, 2))
      or not (( 1, 2) & (-2, 1))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in &"
      );
   end if;
   if (  (-2,-2) + ( 2, 2) /= ( 0, 0)
      or (-2,-2) +       2 /= ( 0, 0)
      or    (-2) + ( 2, 2) /= ( 0, 0)
      or (-3, 0) + ( 1, 0) /= (-2, 0)
      or ( 0, 2) + ( 0, 2) /= ( 0, 4)
      or ( 1, 2) + (-2, 1) /= (-1, 3)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in +"
      );
   end if;
   if (  (-2,-2) - ( 2, 2) /= (-4,-4)
      or    (-2) - ( 2, 2) /= (-4,-4)
      or (-2,-2) -       2 /= (-4,-4)
      or (-2,-2) - (-2,-2) /= ( 0, 0)
      or    (-2) - (-2,-2) /= ( 0, 0)
      or (-2,-2) -    (-2) /= ( 0, 0)
      or (-3, 0) - ( 1, 0) /= (-3,-1)
      or ( 0, 2) - ( 0, 2) /= (-2, 2)
      or ( 1, 2) - (-2, 1) /= ( 0, 4)
      or ( 3, 5) - ( 7, 8) /= ( 3, 5) + (-( 7, 8))
      or (-2, 5) - ( 0, 8) /= (-2, 5) + (-( 0, 8))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in -"
      );
   end if;
   if (  ( 1, 2) * ( 3, 5) /= (  3, 10)
      or       2 * ( 3, 5) /= (  6, 10)
      or ( 1, 2) *       3 /= (  3,  6)
      or (-2,-1) * ( 3, 5) /= (-10, -3)
      or (-2,-1) * ( 3, 5) /= (-1) * (1,2) * (3,5)
      or    (-2) * ( 3, 5) /= (-10, -6)
      or (-2,-1) *       3 /= ( -6, -3)
      or ( 1, 2) * (-5,-3) /= (-10, -3)
      or       2 * (-5,-3) /= (-10, -6)
      or (-2,-1) *    (-3) /= (  3,  6)
      or (-2,-1) * (-5,-3) /= (  3, 10)
      or    (-2) * (-5,-3) /= (  6, 10)
      or (-2,-1) *    (-3) /= (  3,  6)
      or (-1, 2) * ( 3, 5) /= ( -5, 10)
      or (-1, 2) *       3 /= ( -3,  6)
      or (-1, 2) * (-5,-3) /= (-10,  5)
      or ( 1, 2) * (-5, 3) /= (-10,  6)
      or       2 * (-5, 3) /= (-10,  6)
      or (-2,-1) * (-5, 3) /= ( -6, 10)
      or (-2, 1) * (-5, 3) /= ( -6, 10)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in *"
      );
   end if;
   if (  ( 3, 3) / ( 2, 2) /= ( 1, 2)
      or ( 5, 7) / ( 2, 3) /= ( 1, 4)
      or ( 3, 8) / ( 2, 3) /= ( 1, 4)
      or       7 / ( 2, 3) /= ( 2, 4)
      or       6 / ( 2, 3) /= ( 2, 3)
      or ( 5, 7) /       2 /= ( 2, 4)
      or ( 4, 8) /       2 /= ( 2, 4)
      or (-7,-5) / ( 2, 3) /= (-4,-1)
      or (-8,-3) / ( 2, 3) /= (-4,-1)
      or    (-7) / ( 2, 3) /= (-4,-2)
      or    (-6) / ( 2, 3) /= (-3,-2)
      or (-7,-5) /       2 /= (-4,-2)
      or (-8,-4) /       2 /= (-4,-2)
      or ( 5, 7) / (-3,-2) /= (-4,-1)
      or ( 3, 8) / (-3,-2) /= (-4,-1)
      or       7 / (-3,-2) /= (-4,-2)
      or       6 / (-3,-2) /= (-3,-2)
      or ( 5, 7) /    (-2) /= (-4,-2)
      or ( 4, 8) /    (-2) /= (-4,-2)
      or (-7,-5) / (-3,-2) /= ( 1, 4)
      or (-8,-3) / (-3,-2) /= ( 1, 4)
      or    (-7) / (-3,-2) /= ( 2, 4)
      or    (-6) / (-3,-2) /= ( 2, 3)
      or (-7,-5) /    (-2) /= ( 2, 4)
      or (-8,-4) /    (-2) /= ( 2, 4)
      or (-5, 7) / ( 2, 3) /= (-3, 4)
      or (-6, 8) / ( 2, 3) /= (-3, 4)
      or (-5, 7) /       2 /= (-3, 4)
      or (-4, 8) /       2 /= (-2, 4)
      or (-5, 7) / (-3,-2) /= (-4, 3)
      or (-6, 8) / (-3,-2) /= (-4, 3)
      or (-5, 7) /    (-2) /= (-4, 3)
      or (-4, 8) /    (-2) /= (-4, 2)
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in /"
      );
   end if;
   declare
      Dummy : Interval;
   begin
      begin
         Dummy := ( 1, 2) / (-1, 1);
      exception
         when Constraint_Error =>
            begin
               Dummy := 1 / (-1, 1);
            exception
               when Constraint_Error =>
                  begin
                     Dummy := (-2,-1) / (-1, 1);
                  exception
                     when Constraint_Error =>
                        begin
                           Dummy := (-1) / (-1, 1);
                        exception
                           when Constraint_Error =>
                              Dummy := (-2, 1) / (-1, 1);
      end;  end;  end;  end;
      Put_Line ("Error in / (no exception raised)");
      return;
   exception
      when Constraint_Error =>
         null;
   end;
   --        X        Y       <      <=     >=     >
   Check ((-2,-1), (-4,-3), False, False, True,  True );
   Check ((-2,-1),      -4, False, False, True,  True );
   Check (     -2, (-4,-3), False, False, True,  True );
   Check ((-4,-3), (-2,-1), True,  True,  False, False);
   Check (     -4, (-2,-1), True,  True,  False, False);
   Check ((-4,-3),      -2, True,  True,  False, False);
   --       X      Y         <        <=          >=         >
   Check ((1,3), (3,4), Uncertain, True,      Uncertain, False);
   Check ((1,3),     1, False,     Uncertain, True,      Uncertain);
   Check ((1,3),     3, Uncertain, True,      Uncertain, False);
   Put_Line ("... Done");
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
      raise Program_Error;
end Test_Integer_Intervals;
