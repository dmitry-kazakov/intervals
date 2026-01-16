--                                                                    --
--  package Test_Float_Intervals    Copyright (c)  Dmitry A. Kazakov  --
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
with Float_Intervals;          use Float_Intervals;

procedure Test_Float_Intervals is
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
               &  Float'Image (X.From)
               &  ", "
               &  Float'Image (X.To)
               &  "] "
               &  Name
               &  " ["
               &  Float'Image (Y.From)
               &  ", "
               &  Float'Image (Y.To)
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
                Y              : Float;
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
               &  Float'Image (X.From)
               &  ", "
               &  Float'Image (X.To)
               &  "] "
               &  Name
               &  Float'Image (Y)
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
             (  X              : Float;
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
               &  Float'Image (X)
               &  " "
               &  Name
               &  " ["
               &  Float'Image (Y.From)
               &  ", "
               &  Float'Image (Y.To)
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
             (  Result, Expected : Interval;
                Operation        : String
             )  is
   begin
      if not
         Is_In
         (  Result,
            (  Float'Adjacent (Expected.From, Float'First),
               Float'Adjacent (Expected.To,   Float'Last)
         )  )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Operation
            &  " ["
            &  Float'Image (Result.From)
            &  ","
            &  Float'Image (Result.To)
            &  "] is not in ["
            &  Float'Image (Float'Adjacent (Expected.From, Float'First))
            &  ","
            &  Float'Image (Float'Adjacent (Expected.From, Float'Last))
            &  "]"
         )  );
      end if;
   end Check;
   
   procedure Check
             (  Result    : Float;
                Expected  : Float;
                Operation : String
             )  is
   begin
      if not
         Is_In
         (  Result,
            (  Float'Adjacent (Expected, Float'First),
               Float'Adjacent (Expected,  Float'Last)
         )  )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Operation
            &  Float'Image (Result)
            &  " is not in ["
            &  Float'Image (Float'Adjacent (Expected, Float'First))
            &  ","
            &  Float'Image (Float'Adjacent (Expected, Float'Last))
            &  "]"
         )  );
      end if;
   end Check;

begin
   Put_Line ("Testing floating-point interval arithmetic ...");
   Put_Line ("Machine_Rounds: " & Boolean'Image (Float'Machine_Rounds));
   Check
   (  Distance ((1.0,2.0),(1.0,2.0)),
      0.0,
      "Distance ((1.0,2.0),(1.0,2.0))"
   );
   Check
   (  Distance ((-2.0,-1.0),(1.0,2.0)),
      3.0,
      "Distance ((-2.0,-1.0),(1.0,2.0))"
   );
   if (      Is_In (0.0, (1.0,2.0))
      or not Is_In (1.0, (1.0,2.0))
      or not Is_In (2.0, (1.0,2.0))
      or     Is_In (3.0, (1.0,2.0))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Is_In"
      );
   end if;
   if (  Is_Negative (( 1.0,2.0))
      or Is_Negative (( 0.0,2.0))
      or Is_Negative ((-2.0,0.0))
      or not Is_Negative ((-2.0,-1.0))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Is_Negative"
      );
   end if;
   if (  Is_Positive ((-2.0,-2.0))
      or Is_Positive ((-3.0, 0.0))
      or Is_Positive (( 0.0, 2.0))
      or not Is_Positive ((1.0,2.0))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Is_Positive"
      );
   end if;
   Check (Length ((-2.0,-2.0)), 0.0, "Length ((-2.0,-2.0))");
   Check (Length ((-3.0, 0.0)), 3.0, "Length ((-3.0, 0.0))");
   Check (Length (( 0.0, 2.0)), 2.0, "Length (( 0.0, 2.0))");
   Check (Length (( 1.0, 2.0)), 1.0, "Length (( 1.0, 2.0))");
   Check (abs (-2.0,-2.0), (2.0, 2.0), "abs (-2.0,-2.0)");
   Check (abs (-3.0, 0.0), (0.0, 3.0), "abs (-3.0, 0.0)");
   Check (abs ( 0.0, 2.0), (0.0, 2.0), "abs ( 0.0, 2.0)");
   Check (abs ( 1.0, 2.0), (1.0, 2.0), "abs ( 1.0, 2.0)");
   if (      ((-2.0,-2.0) & ( 2.0, 2.0))
      or not ((-3.0, 0.0) & ( 0.0, 0.0))
      or not (( 0.0, 2.0) & ( 0.0, 2.0))
      or not (( 1.0, 2.0) & (-2.0, 1.0))
      )
   then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in &"
      );
   end if;
   Check ((-2.0,-2.0) + ( 2.0, 2.0), ( 0.0, 0.0), "+");
   Check ((-2.0,-2.0) +         2.0, ( 0.0, 0.0), "+");
   Check (     (-2.0) + ( 2.0, 2.0), ( 0.0, 0.0), "+");
   Check ((-3.0, 0.0) + ( 1.0, 0.0), (-2.0, 0.0), "+");
   Check (( 0.0, 2.0) + ( 0.0, 2.0), ( 0.0, 4.0), "+");
   Check (( 1.0, 2.0) + (-2.0, 1.0), (-1.0, 3.0), "+");

   Check ((-2.0,-2.0) - ( 2.0, 2.0), (-4.0,-4.0), "-");
   Check (     (-2.0) - ( 2.0, 2.0), (-4.0,-4.0), "-");
   Check ((-2.0,-2.0) -         2.0, (-4.0,-4.0), "-");
   Check ((-2.0,-2.0) - (-2.0,-2.0), ( 0.0, 0.0), "-");
   Check (     (-2.0) - (-2.0,-2.0), ( 0.0, 0.0), "-");
   Check ((-2.0,-2.0) -      (-2.0), ( 0.0, 0.0), "-");
   Check ((-3.0, 0.0) - ( 1.0, 0.0), (-3.0,-1.0), "-");
   Check (( 0.0, 2.0) - ( 0.0, 2.0), (-2.0, 2.0), "-");
   Check (( 1.0, 2.0) - (-2.0, 1.0), ( 0.0, 4.0), "-");
   Check (( 3.0, 5.0) - ( 7.0, 8.0), ( 3.0, 5.0) + (-( 7.0, 8.0)), "-");
   Check ((-2.0, 5.0) - ( 0.0, 8.0), (-2.0, 5.0) + (-( 0.0, 8.0)), "-");

   Check (( 1.0, 2.0) * ( 3.0, 5.0), (  3.0, 10.0), "*");
   Check (        2.0 * ( 3.0, 5.0), (  6.0, 10.0), "*");
   Check (( 1.0, 2.0) *         3.0, (  3.0,  6.0), "*");
   Check ((-2.0,-1.0) * ( 3.0, 5.0), (-10.0, -3.0), "*");
   Check (     (-2.0) * ( 3.0, 5.0), (-10.0, -6.0), "*");
   Check ((-2.0,-1.0) *         3.0, ( -6.0, -3.0), "*");
   Check (( 1.0, 2.0) * (-5.0,-3.0), (-10.0, -3.0), "*");
   Check (        2.0 * (-5.0,-3.0), (-10.0, -6.0), "*");
   Check ((-2.0,-1.0) *      (-3.0), (  3.0,  6.0), "*");
   Check ((-2.0,-1.0) * (-5.0,-3.0), (  3.0, 10.0), "*");
   Check (     (-2.0) * (-5.0,-3.0), (  6.0, 10.0), "*");
   Check ((-2.0,-1.0) *      (-3.0), (  3.0,  6.0), "*");
   Check ((-1.0, 2.0) * ( 3.0, 5.0), ( -5.0, 10.0), "*");
   Check ((-1.0, 2.0) *         3.0, ( -3.0,  6.0), "*");
   Check ((-1.0, 2.0) * (-5.0,-3.0), (-10.0,  5.0), "*");
   Check (( 1.0, 2.0) * (-5.0, 3.0), (-10.0,  6.0), "*");
   Check (        2.0 * (-5.0, 3.0), (-10.0,  6.0), "*");
   Check ((-2.0,-1.0) * (-5.0, 3.0), ( -6.0, 10.0), "*");
   Check ((-2.0, 1.0) * (-5.0, 3.0), ( -6.0, 10.0), "*");
   Check
   (  (-2.0,-1.0) * ( 3.0, 5.0),
      (-1.0) * (1.0,2.0) * (3.0,5.0),
      "*"
   );

   Check (( 3.0, 3.0) / ( 2.0, 2.0), ( 1.5,  1.5), "/");
   Check (( 5.0, 7.0) / ( 2.0, 4.0), ( 1.25, 3.5), "/");
   Check (( 3.0, 8.0) / ( 2.0, 3.0), ( 1.0,  4.0), "/");
   Check (        7.0 / ( 2.0, 4.0), ( 1.75, 3.5), "/");
   Check (        6.0 / ( 2.0, 3.0), ( 2.0,  3.0), "/");
   Check (( 5.0, 7.0) /         2.0, ( 2.5,  3.5), "/");
   Check (( 4.0, 8.0) /         2.0, ( 2.0,  4.0), "/");
   Check ((-7.0,-5.0) / ( 2.0, 4.0), (-3.5,-1.25), "/");
   Check ((-8.0,-3.0) / ( 2.0, 3.0), (-4.0, -1.0), "/");
   Check (     (-7.0) / ( 2.0, 4.0), (-3.5,-1.75), "/");
   Check (     (-6.0) / ( 2.0, 3.0), (-3.0, -2.0), "/");
   Check ((-7.0,-5.0) /         2.0, (-3.5, -2.5), "/");
   Check ((-8.0,-4.0) /         2.0, (-4.0, -2.0), "/");
   Check (( 5.0, 7.0) / (-3.0,-2.0), (-3.5,-1.25), "/");
   Check (( 3.0, 8.0) / (-3.0,-2.0), (-4.0, -1.0), "/");
   Check (        7.0 / (-4.0,-2.0), (-3.5,-1.75), "/");
   Check (        6.0 / (-3.0,-2.0), (-3.0, -2.0), "/");
   Check (( 5.0, 7.0) /      (-2.0), (-3.5, -2.5), "/");
   Check (( 4.0, 8.0) /      (-2.0), (-4.0, -2.0), "/");
   Check ((-7.0,-5.0) / (-4.0,-2.0), (1.25,  3.5), "/");
   Check ((-8.0,-3.0) / (-3.0,-2.0), ( 1.0,  4.0), "/");
   Check (     (-7.0) / (-4.0,-2.0), (1.25,  3.5), "/");
   Check (     (-6.0) / (-3.0,-2.0), ( 2.0,  3.0), "/");
   Check ((-7.0,-5.0) /      (-2.0), ( 2.5,  3.5), "/");
   Check ((-8.0,-4.0) /      (-2.0), ( 2.0,  4.0), "/");
   Check ((-5.0, 7.0) / ( 2.0, 4.0), (-2.5,  3.5), "/");
   Check ((-6.0, 8.0) / ( 2.0, 3.0), (-3.0,  4.0), "/");
   Check ((-5.0, 7.0) /         2.0, (-2.5,  3.5), "/");
   Check ((-4.0, 8.0) /         2.0, (-2.0,  4.0), "/");
   Check ((-5.0, 7.0) / (-4.0,-2.0), (-3.5,  2.5), "/");
   Check ((-6.0, 8.0) / (-3.0,-2.0), (-4.0,  3.0), "/");
   Check ((-5.0, 7.0) /      (-2.0), (-3.5,  2.5), "/");
   Check ((-4.0, 8.0) /      (-2.0), (-4.0,  2.0), "/");
   declare
      Dummy : Interval;
   begin
      begin
         Dummy := ( 1.0, 2.0) / (-1.0, 1.0);
      exception
         when Constraint_Error =>
            begin
               Dummy := 1.0 / (-1.0, 1.0);
            exception
               when Constraint_Error =>
                  begin
                     Dummy := (-2.0,-1.0) / (-1.0, 1.0);
                  exception
                     when Constraint_Error =>
                        begin
                           Dummy := (-1.0) / (-1.0, 1.0);
                        exception
                           when Constraint_Error =>
                              Dummy := (-2.0, 1.0) / (-1.0, 1.0);
      end;  end;  end;  end;
      Put_Line ("Error in / (no exception raised)");
      return;
   exception
      when Constraint_Error =>
         null;
   end;
   --        X        Y               <      <=     >=     >
   Check ((-2.0,-1.0), (-4.0,-3.0), False, False, True,  True );
   Check ((-2.0,-1.0),        -4.0, False, False, True,  True );
   Check (       -2.0, (-4.0,-3.0), False, False, True,  True );
   Check ((-4.0,-3.0), (-2.0,-1.0), True,  True,  False, False);
   Check (       -4.0, (-2.0,-1.0), True,  True,  False, False);
   Check ((-4.0,-3.0),        -2.0, True,  True,  False, False);
   --       X      Y               <        <=          >=         >
   Check ((1.0,3.0), (3.0,4.0), Uncertain, True,      Uncertain, False);
   Check ((1.0,3.0),       1.0, False,     Uncertain, True,      Uncertain);
   Check ((1.0,3.0),       3.0, Uncertain, True,      Uncertain, False);
   Check ((2.0,3.0), (2.0,3.0), Uncertain, Uncertain, Uncertain, Uncertain);
   Put_Line ("... Done");
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
      raise Program_Error;
end Test_Float_Intervals;
