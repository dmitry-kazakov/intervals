--                                                                    --
--  procedure Test_Measures         Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
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

with Ada.Calendar;                use type Ada.Calendar.Time;
with Ada.Exceptions;              use Ada.Exceptions;
with Units;                       use Units;
with Units.Base;                  use Units.Base;
with Units.Edit;                  use Units.Edit;
with Units.Constants;             use Units.Constants;
with Float_Measures;              use Float_Measures;
with Float_Measures_Derived;      use Float_Measures_Derived;
with Float_Measures_Irregular;    use Float_Measures_Irregular;
with Text_IO;                     use Text_IO;
with Strings_Edit;                use Strings_Edit;
with Strings_Edit.Floats;         use Strings_Edit.Floats;
with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

with Ada.Numerics.Complex_Types;
with Complex_Measures;
with Float_Measures_Edit;
with Float_Measures_UTF8_Edit;

procedure Test_Measures is

   function Equal (Left, Right : Float) return Boolean is
      Eps : constant Float := 1.0E-6;
   begin
      return abs (Left - Right) <= abs (Left + Right) * Eps;
   end Equal;

   function Equal (Left, Right : Measure) return Boolean is
   begin
      return
      (  Left.SI = Right.SI
      and then
         Equal (Left.Gain, Right.Gain)
      and then
         Equal (Left.Offset, Right.Offset)
      );
   end Equal;

   function Equal (Left, Right : Scaled) return Boolean is
   begin
      return
      (  Left.Format = Right.Format
      and then
         Equal (Left.Numeral, Right.Numeral)
      and then
         Equal (Left.Scale, Right.Scale)
      );
   end Equal;

   procedure TestUnit (X : Unit; Result : String) is
   begin
      if Image (X) /= Result then
         Put
         (  "Expected:'"
         &  Result
         &  "' Got:'"
         &  Image (X)
         &  "' ('"
         &  Image (X, Latin1 => False)
         &  "')"
         );
         New_Line;
         raise Data_Error;
      end if;
   end TestUnit;

   procedure TestIO
             (  X       : Measure;
                Result  : String;
                Small   : Integer := 0;
                Derived : Boolean := True;
                Latin1  : Boolean := True
             )  is
      use Float_Measures_Edit;
   begin
      if (  (  Result
            /= Image
               (  X,
                  Derived  => Derived,
                  Latin1   => Latin1,
                  AbsSmall => Small
            )  )
         or not Equal (X, Value (Result))
         )
      then
         Put
         (  "Expected:'"
         &  Result
         &  "', Got:'"
         &  Image
            (  X,
               Derived  => Derived,
               Latin1   => Latin1,
               AbsSmall => Small
            )
         &  "' U="
         &  Image (Value (Result).SI, False)
         &  ", G="
         &  Float'Image (Value (Result).Gain)
         &  ", O="
         &  Float'Image (Value (Result).Offset)
         &  "' Argument U="
         &  Image (X.SI, False)
         &  ", G="
         &  Float'Image (X.Gain)
         &  ", O="
         &  Float'Image (X.Offset)
         );
         New_Line;
         raise Data_Error;
      end if;
   end TestIO;

   procedure TestUTFIO
             (  X       : Measure;
                Result  : String;
                Small   : Integer := 0;
                Derived : Boolean := True
             )  is
      use Float_Measures_UTF8_Edit;
   begin
      if (  (  Result
            /= Image
               (  X,
                  Derived  => Derived,
                  AbsSmall => Small
            )  )
         or X /= Value (Result)
         )
      then
         Put
         (  "Expected:'"
         &  Result
         &  "', Got:'"
         &  Image
            (  X,
               Derived  => Derived,
               AbsSmall => Small
            )
         &  "' (U="
         &  Image (Value (Result).SI, False)
         &  ", G="
         &  Float'Image (Value (Result).Gain)
         &  ", O="
         &  Float'Image (Value (Result).Offset)
         );
         New_Line;
         raise Data_Error;
      end if;
   end TestUTFIO;

   procedure TestExpression
             (  Test     : String;
                Expected : Measure;
                Next     : Character := ASCII.Nul;
                Restrict : Boolean   := False
             )  is
      use Float_Measures_Edit;
      Result  : Measure;
      Pointer : Integer := Test'First;
   begin
      if Restrict then
         Get_Unit (Test, Pointer, Result);
      else
         Get (Test, Pointer, Result);
      end if;
      if Next = ASCII.Nul then
         if Pointer /= Test'Last + 1 then
            Put
            (  "Error in parsing of '"
            &  Test
            &  "', Unrecognized: '"
            &  Test (Pointer..Test'Last)
            &  "'"
            );
            New_Line;
            raise Data_Error;
         end if;
      else
         if Test (Pointer) /= Next then
            Put
            (  "Error in parsing of '"
            &  Test
            &  "', Rest: '"
            &  Test (Pointer..Test'Last)
            &  "'"
            );
            New_Line;
            raise Data_Error;
         end if;
      end if;
      if not Equal (Expected, Result) then
         Put
         (  "Expected:'"
         &  Image (Expected, Latin1 => False)
         &  "', Got:'"
         &  Image (Result, Latin1 => False)
         &  "'"
         );
         New_Line;
         raise Data_Error;
      end if;
   end TestExpression;

   procedure TestUTF
             (  Test     : String;
                Expected : Measure;
                Next     : Character := ASCII.Nul
             )  is
      use Float_Measures_UTF8_Edit;
      Result  : Measure;
      Pointer : Integer := Test'First;
   begin
      Get (Test, Pointer, Result);
      if Next = ASCII.Nul then
         if Pointer /= Test'Last + 1 then
            Put
            (  "Error in parsing of '"
            &  Test
            &  "', Unrecognized: '"
            &  Test (Pointer..Test'Last)
            &  "'"
            );
            New_Line;
            raise Data_Error;
         end if;
      else
         if Test (Pointer) /= Next then
            Put
            (  "Error in parsing of '"
            &  Test
            &  "', Rest: '"
            &  Test (Pointer..Test'Last)
            &  "'"
            );
            New_Line;
            raise Data_Error;
         end if;
      end if;
      if (  Expected.SI /= Result.SI
         or else
            not Equal (Expected.Gain, Result.Gain)
         or else
            not Equal (Expected.Offset, Result.Offset)
         )
      then
         Put
         (  "Expected:'"
         &  Float_Measures_Edit.Image (Expected, Latin1 => False)
         &  "', Got:'"
         &  Float_Measures_Edit.Image (Result, Latin1 => False)
         &  "'"
         );
         New_Line;
         raise Data_Error;
      end if;
   end TestUTF;

   procedure TestScaled
             (  Test     : String;
                Expected : Scaled;
                Next     : Character := ASCII.Nul
             )  is
      use Float_Measures_Edit;
      Result  : Scaled;
      Pointer : Integer := Test'First;
      function Image (Value : Scaled) return String is
      begin
         case Value.Format is
            when Scalar =>
               return Image (Value.Numeral) & " x 1 SI";
            when Numeric =>
               return
               (  Image (Value.Numeral)
               &  " x "
               &  Image (Value.Scale, Latin1 => False)
               &  " (numeric)"
               );
            when Canonic =>
               return
               (  Image (Value.Numeral)
               &  " x "
               &  Image (Value.Scale, Latin1 => False)
               );
            when Jumbled =>
               return Image (Value.Scale, Latin1 => False);
         end case;
      end Image;

   begin
      Get (Test, Pointer, Result);
      if Next = ASCII.Nul then
         if Pointer /= Test'Last + 1 then
            Put
            (  "Error in parsing of '"
            &  Test
            &  "', Unrecognized: '"
            &  Test (Pointer..Test'Last)
            &  "'"
            );
            New_Line;
            raise Data_Error;
         end if;
      else
         if Test (Pointer) /= Next then
            Put
            (  "Error in parsing of '"
            &  Test
            &  "', Rest: '"
            &  Test (Pointer..Test'Last)
            &  "'"
            );
            New_Line;
            raise Data_Error;
         end if;
      end if;
      if not Equal (Expected, Result) then
         Put
         (  "Expected:'"
         &  Image (Expected)
         &  "', Got:'"
         &  Image (Result)
         &  "'"
         );
         New_Line;
         raise Data_Error;
      end if;
   end TestScaled;

   subtype Speed is Measure (Velocity);
   V : Speed := 100.0 * m / h;
begin
   Put_Line ("Testing measures ...");

   begin
      TestUnit
      (  Current ** (-4) / Current ** 5,
         "Current ** (-4) / Current ** 5"
      );
   exception
      when Constraint_Error => null;
   end;
   begin
      TestUnit
      (  Current ** (-4) * Current ** (-5),
         "Current ** (-4) * Current ** (-5)"
      );
   exception
      when Constraint_Error => null;
   end;
   begin
      TestUnit
      (  Current ** (-8) * Current ** (-8),
         "Current ** (-8) * Current ** (-8)"
      );
   exception
      when Constraint_Error => null;
   end;
   begin
      TestUnit (Current ** 9, "Current ** 9");
   exception
      when Constraint_Error => null;
   end;
   begin
      TestUnit (Mass ** 4 * Mass ** 4, "Mass ** 4 * Mass ** 4");
   exception
      when Constraint_Error => null;
   end;
   begin
      TestUnit (Mass ** 7 * Mass ** 7, "Mass ** 7 * Mass ** 7");
   exception
      when Constraint_Error => null;
   end;
   begin
      TestUnit (Mass ** 8, "Mass ** 8");
   exception
      when Constraint_Error => null;
   end;
   begin
      TestUnit (Sqrt (Mass), "Sqrt (Mass)");
   exception
      when Constraint_Error => null;
   end;

   TestUnit (Unitless,      "1"           );
   TestUnit (Current,       "A"           );
   TestUnit (Luminescence,  "cd"          );
   TestUnit (Temperature,   "K"           );
   TestUnit (Mass,          "kg"          );
   TestUnit (Length,        "m"           );
   TestUnit (Quantity,      "mol"         );
   TestUnit (Time,          "s"           );
   TestUnit (Area,          "m²"          );
   TestUnit (Volume,        "m³"          );
   TestUnit (Velocity,      "m/s"         );
   TestUnit (Acceleration,  "m/s²"        );
   TestUnit (Force,         "kg·m/s²"     );
   TestUnit (Pressure,      "kg/m·s²"     );
   TestUnit (Energy,        "kg·m²/s²"    );
   TestUnit (Power,         "kg·m²/s³"    );
   TestUnit (Charge,        "A·s"         );
   TestUnit (Potential,     "kg·m²/A·s³"  );
   TestUnit (Capacitance,   "A²·s^4/kg·m²");
   TestUnit (Resistance,    "kg·m²/A²·s³" );
   TestUnit (Conductance,   "A²·s³/kg·m²" );
   TestUnit (Inductance,    "kg·m²/A²·s²" );
   TestUnit (Concentration, "mol/m³"      );
   TestUnit (Density,       "kg/m³"       );
   TestUnit (Luminance,     "cd/m²"       );
   TestUnit (Frequency,     "1/s"         );

   TestUnit (Mass / Mass, "1");
   TestUnit (Mass * Mass, "kg²");

   TestUnit (Mass ** 0,   "1"     );
      TestUnit (Sqrt (Mass ** 0), "1");
   TestUnit (Mass ** 1,   "kg"    );
   TestUnit (Mass ** 2,   "kg²"   );
      TestUnit (Sqrt (Mass ** 2), "kg");
   TestUnit (Mass ** 3,   "kg³"   );
   TestUnit (Mass ** 4,   "kg^4"  );
      TestUnit (Sqrt (Mass ** 4), "kg²");
   TestUnit (Mass ** 5,   "kg^5"  );
   TestUnit (Mass ** 6,   "kg^6"  );
      TestUnit (Sqrt (Mass ** 6), "kg³");
   TestUnit (Mass ** 7,   "kg^7"  );

   TestUnit (Mass **(-1), "1/kg"  );
   TestUnit (Mass **(-2), "1/kg²" );
      TestUnit (Sqrt (Mass **(-2)), "1/kg" );
   TestUnit (Mass **(-3), "1/kg³" );
   TestUnit (Mass **(-4), "1/kg^4");
      TestUnit (Sqrt (Mass **(-4)), "1/kg²" );
   TestUnit (Mass **(-5), "1/kg^5");
   TestUnit (Mass **(-6), "1/kg^6");
      TestUnit (Sqrt (Mass **(-6)), "1/kg³");
   TestUnit (Mass **(-7), "1/kg^7");
   TestUnit (Mass **(-8), "1/kg^8");
      TestUnit (Sqrt (Mass **(-8)), "1/kg^4");
   TestExpression ("1 Pa", kg/(m*s**2));
   TestExpression ("Pa", kg/(m*s**2), Restrict => True);
   TestExpression ("Pa/m", kg/(m*s**2), Next => '/', Restrict => True);
   TestExpression ("Pa / m", kg/(m*s**2), Next => ' ', Restrict => True);
   TestExpression (" m/m*s**2]", 1.0 / s**2, ']');
   TestExpression (" m/(m*s**2) and 3 x", 1.0 / s**2 and 3.0, 'x');
   TestExpression (" m - 2*m + m ]", 0.0 * m, ']');
   TestExpression (" m + m - 2*m ", 0.0 * m);
   TestExpression (" 2*m + m ", 3.0 * m);
   TestExpression (" m - (2*m + m) ", -2.0 * m);
   TestExpression (" m - -2*m + -m ", 2.0 * m);
   TestExpression (" 500 * % ", 5.0 * Np);
   TestExpression (" 500*% ", 5.0 * Np);
   TestExpression (" 500 % ", 5.0 * Np);
   TestExpression (" 500% ", 5.0 * Np);
   TestExpression (" 500%]", 5.0 * Np, ']');
   TestExpression (" 500%2", 10.0 * Np);
   TestExpression (" 500% x", 5.0 * Np, 'x');
   TestExpression (" 500%x", 5.0 * Np, 'x');
   TestExpression (" 10**2% x", 1.0 * Np, 'x');
   TestExpression (" 10**(1+1)% x", 1.0 * Np, 'x');
   TestExpression (" 10^(1+1)% x", 1.0 * Np, 'x');
   TestExpression (" 10  **  (  1  +  1  )  %   x", 1.0 * Np, 'x');
   TestExpression (" 10km/s", 10000.0 * m/s);
   TestExpression ("°C", K and 273.15);
   TestExpression ("°C", K and 273.15, Restrict => True);
   TestExpression ("m°C", K/1000.0 and 273.15);
   TestExpression ("m°C", K/1000.0 and 273.15, Restrict => True);
   TestExpression ("%", Np / 100.0);
   TestExpression ("mg", kg/1_000_000.0);
   TestExpression ("kg", kg);
   TestExpression ("g", kg/1_000.0);
   TestExpression ("µg", kg/1_000_000_000.0);

   TestIO (0.000_001 * kg, "mg", Small => -MaxSmall);
   TestIO (0.000_000_001 * kg, "µg", Small => -MaxSmall);
   TestIO (0.001 * kg, "g", Small => -MaxSmall);
   TestIO (Np, "1");
   TestIO (25.0 * Np, "25");
   TestIO (Hz, "Hz");
   TestIO (Ohm, "ohm");
   TestIO (Ohm, "kg*m^2/A^2*s^3", Derived => False, Latin1 => False);
   TestIO (Ohm, "kg·m²/A²·s³", Derived => False);
   TestIO (Hz, "1/s", Derived => False);
   TestIO (25.0 * Hz, "25/s", Derived => False);
   TestIO (Celsius, "°C");
   TestIO (1.0 * kg, "kg");
   TestIO (100.0 * m/s, "100·m/s");
   TestIO (1000.0 * Hz, "kHz", Small => -MaxSmall);
   TestIO (m, "m");
   TestIO (h, "3600·s");
   TestIO (h, "3.6·ks", Small => 2);
   TestIO (2_000.0 * Celsius, Image (2.0) & "·k°C", Small => -MaxSmall);
   TestIO (2_000.0 * Celsius, "2·k°C", Small => 3);
   TestIO (1_000.0 * m/s, "km/s", Small => 3);
   TestIO (0.000_031 * Pa, "31·µPa", Small => -6);
   TestIO (0.000_031 * Pa, Image (31.0) & "·µPa", Small => -MaxSmall);
   TestIO (10_000.0 * Hz, "10·kHz", Small => 3);
   TestIO (12_000.0 * m/s, "12·km/s", Small => 3);
   TestIO (0.0052 * s, "5.2·ms", Small => -4);
   TestIO (25.0 * N, "25·N");
   TestIO (25.0 * N, "25·kg·m/s²",  Derived => False);
   TestIO (25.0 * N, "25*kg*m/s^2", Derived => False, Latin1 => False);
   TestIO (0.0 * W, "0·W");
   TestIO (0.0 * W, "0·W", Small => -MaxSmall);

   TestUTFIO (m**2, To_UTF8 (String'("m²")));
   TestUTFIO (m*s, To_UTF8 (String'("m·s")));
   TestUTFIO (25.0*N, To_UTF8 (String'("25·N")));
   TestUTFIO (25.0*N, To_UTF8 (String'("25·kg·m/s²")), Derived => False);

   TestUTF ("1 Pa", kg/(m*s**2));
   TestUTF ("1 foot^2", 0.09290305 * m**2);
   TestUTF (" m/m*s**2]", 1.0 / s**2, ']');
   TestUTF (" m/(m*s**2) and 3 x", 1.0 / s**2 and 3.0, 'x');
   TestUTF (" m - 2*m + m ]", 0.0 * m, ']');
   TestUTF (" m + m - 2*m ", 0.0 * m);
   TestUTF (" 2*m + m ", 3.0 * m);
   TestUTF (" m - (2*m + m) ", -2.0 * m);
   TestUTF (" m - -2*m + -m ", 2.0 * m);
   TestUTF (" 500 * % ", 5.0 * Np);
   TestUTF (" 500*% ", 5.0 * Np);
   TestUTF (" 500 % ", 5.0 * Np);
   TestUTF (" 500% ", 5.0 * Np);
   TestUTF (" 500%]", 5.0 * Np, ']');
   TestUTF (" 500%2", 10.0 * Np);
   TestUTF (" 500% x", 5.0 * Np, 'x');
   TestUTF (" 500%x", 5.0 * Np, 'x');
   TestUTF (" 10**2% x", 1.0 * Np, 'x');
   TestUTF (" 10**(1+1)% x", 1.0 * Np, 'x');
   TestUTF (" 10^(1+1)% x", 1.0 * Np, 'x');
   TestUTF (" 10  **  (  1  +  1  )  %   x", 1.0 * Np, 'x');
   TestUTF (" 10km/s", 10000.0 * m/s);
   TestUTF (To_UTF8 (String'("°C")),     K and 273.15);
   TestUTF (To_UTF8 (String'("m°C")),    K/1000.0 and 273.15);
   TestUTF ("m" & UTF8.Image (16#2103#), K/1000.0 and 273.15);
   TestUTF (UTF8.Image (16#212A#), K);
   TestUTF ("m" & UTF8.Image (16#212A#), K/1000.0);
   TestUTF ("mm", m/1000.0);
   TestUTF ("m" & UTF8.Image (16#212A#), K/1000.0);
   TestUTF ("m" & UTF8.Image (16#3A9#),  Ohm/1000.0);
   TestUTF ("m" & UTF8.Image (16#2126#), Ohm/1000.0);
   TestUTF (UTF8.Image (16#2109#), Fahrenheit);
   TestUTF ("m" & UTF8.Image (16#2070#), Np);
   TestUTF ("m" & UTF8.Image (16#B9#), m);
   TestUTF ("m" & UTF8.Image (16#B2#), m**2);
   TestUTF ("m" & UTF8.Image (16#B3#), m**3);
   TestUTF ("m " & UTF8.Image (16#2075#), m**5);
   TestUTF ("m" & UTF8.Image (16#207A#) & UTF8.Image (16#2075#), m**5);
   TestUTF ("m" & UTF8.Image (16#207A#) & UTF8.Image (16#2076#), m**6);
   TestUTF ("m" & UTF8.Image (16#207A#) & UTF8.Image (16#2077#), m**7);
   TestUTF
   (  "m" & UTF8.Image (16#207B#) & UTF8.Image (16#2075#),
      m**(-5)
   );
   TestUTF
   (  "m" & UTF8.Image (16#207B#) & UTF8.Image (16#2078#),
      m**(-8)
   );

   TestScaled ("4 * (1 and 3)", (Canonic, 4.0, Np and 3.0));
   TestScaled ("1+3",           (Scalar,  4.0, Np));
   TestScaled ("(1+5)",         (Scalar,  6.0, Np));
   TestScaled ("(1*3)",         (Scalar,  3.0, Np));
   TestScaled ("5*6",           (Numeric, 5.0, 6.0 * Np));
   TestScaled ("(1*3) m",       (Canonic, 3.0, m));
   TestScaled ("km/h",          (Jumbled, 1.0, 1000.0 * m/h));
   TestScaled ("(1 m)*s",       (Jumbled, 1.0, m*s));
   TestScaled ("(1 m)",         (Jumbled, 1.0, m));
   TestScaled ("(1 m/m)",       (Jumbled, 1.0, Np));
   TestScaled ("(1 m/m)m",      (Jumbled, 1.0, m));
   TestScaled ("1 and 3",       (Jumbled, 1.0, 1.0 * Np and 3.0));
   TestScaled ("2/3/4 A",       (Canonic, 2.0, ((1.0/3.0)/4.0)/A));
   TestScaled ("(2) m",         (Canonic, 2.0, m));
   TestScaled ("(2**2 + 1) m",  (Canonic, 5.0, m));
   TestScaled ("m",             (Jumbled, 1.0, m));
   TestScaled ("1.24 km/h",     (Canonic, 1.24, 1000.0 * m/h));
   TestScaled ("3/s",           (Canonic, 3.0, Hz));
   TestScaled ("10 * 4 feet",   (Canonic, 10.0, 4.0 * ft));
   TestScaled ("123",           (Scalar,  123.0, Np));
   TestScaled ("12*3",          (Numeric, 12.0, 3.0 * Np));
   TestScaled ("%",             (Jumbled, 1.0,  Np/100.0));

   if 5.0 * Celsius /= (Temperature, 5.0, 273.15) then
      raise Data_Error;
   end if;
   if Celsius + Celsius /= (Temperature, 2.0, 273.15) then
      raise Data_Error;
   end if;
   begin
      TestIO (Celsius * Celsius, "Celsius * Celsius", 0);
   exception
      when Unit_Error => null;
   end;
   begin
      TestIO (Celsius * K, "Celsius * K", 0);
   exception
      when Unit_Error => null;
   end;
   begin
      TestIO (Celsius / K, "Celsius / K", 0);
   exception
      when Unit_Error => null;
   end;
   begin
      TestIO (Celsius + K, "Celsius + K", 0);
   exception
      when Unit_Error => null;
   end;
   begin
      TestIO (Celsius - K, "Celsius - K", 0);
   exception
      when Unit_Error => null;
   end;
   if m * ft /= (Length * Length, 0.3048, 0.0) then
      raise Data_Error;
   end if;
   if m + ft /= (Length, 1.3048, 0.0) then
      raise Data_Error;
   end if;
   declare
      use Ada.Numerics.Complex_Types;
      use Complex_Measures;
      X : Complex_Measure;
   begin
      X := 1.0 * m + i * 2.0 * m;
      if Re (X) /= 1.0 * m or else Im (X) /= 2.0 * m then
         raise Data_Error;
      end if;
   end;
   --
   -- Performance test
   --
   Put_Line ("Now a small performance test ...");
   declare
      type Revolution  is mod 8;
      type Generator   is array (Revolution) of Float;
      subtype Speed    is Measure (Velocity);
      subtype Distance is Measure (Length);
      subtype TimeSpan is Measure (Time);
      Dice   : Generator  := (0.5, 0.1, 0.2, 0.7, 0.4, 0.6, 0.3, 0.8);
      Count  : Revolution := 0;
      M_Time : Duration;
      F_Time : Duration;
      Sum    : Float;
      Start  : Ada.Calendar.Time;
   begin
      declare
         X : Distance := 0.0 * m;
         V : Speed    := 6.0 * m/s;
         T : TimeSpan := 0.5 * s;
      begin
         Count := 0;
         Start := Ada.Calendar.Clock;
         for Index in 1..3_000_000 loop
            X := X + V * T;
            V := V * Dice (Count); Count := Count + 1;
            T := T * Dice (Count); Count := Count + 1;
         end loop;
         M_Time := Ada.Calendar.Clock - Start;
         Sum := Get_Value (X);
      end;
      declare
         X : Float := 0.0;
         V : Float := 6.0;
         T : Float := 0.5;
      begin
         Count := 0;
         Start := Ada.Calendar.Clock;
         for Index in 1..3_000_000 loop
            X := X + V * T;
            V := V * Dice (Count); Count := Count + 1;
            T := T * Dice (Count); Count := Count + 1;
         end loop;
         F_Time := Ada.Calendar.Clock - Start;
         if Sum /= X then
            raise Unit_Error;
         end if;
      end;
      Put_Line
      (  "Operations with Float : "
      &  Strings_Edit.Floats.Image (Float (F_Time), AbsSmall => -3)
      &  " s"
      );
      Put_Line
      (  "Same with Measure     : "
      &  Strings_Edit.Floats.Image (Float (M_Time), AbsSmall => -3)
      &  " s"
      );
      Put_Line
      (  "Performance hit       : "
      &  Strings_Edit.Floats.Image (Float (M_Time / F_Time), AbsSmall => -2)
      &  " times"
      );
      Put_Line ("... Done");
   end;
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Test_Measures;
