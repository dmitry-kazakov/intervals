--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_UTF8                                   Luebeck            --
--  Test                                           Autumn, 2018       --
--                                                                    --
--                                Last revision :  11:03 04 Dec 2025  --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;

with Strings_Edit.Integer_Edit;
with Strings_Edit.Integers;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

with Strings_Edit.UTF8.Maps.Constants;
use  Strings_Edit.UTF8.Maps.Constants;

with Strings_Edit.UTF8.Normalization;
use  Strings_Edit.UTF8.Normalization;

procedure Test_UTF8 is
   function I (Code : Strings_Edit.UTF8.UTF8_Code_Point) return String
      renames Strings_Edit.UTF8.Image;

   function Dump (Code : Strings_Edit.UTF8.UTF8_Code_Point)
      return String is
      type Integer_64 is range -2**63..2**63 - 1;
      package Edit is new Strings_Edit.Integer_Edit (Integer_64);
   begin
      return Edit.Image (Integer_64 (Code), Base => 16);
   end Dump;

   function Dump (S : String) return String is
      use Strings_Edit;
      use Strings_Edit.Integers;
      Result  : String (1..120);
      Pointer : Integer := 1;
   begin
      for I in S'Range loop
         if Pointer > 1 then
            Put (Result, Pointer, ",");
         end if;
         Put (Result, Pointer, Character'Pos (S (I)), Base => 16);
      end loop;
      return Result (1..Pointer - 1);
   end Dump;

   function Dump (S : Strings_Edit.UTF8.UTF8_Code_Point_Array)
      return String is
      use Strings_Edit;
      Result  : String (1..120);
      Pointer : Integer := 1;
   begin
      for I in S'Range loop
         if Pointer > 1 then
            Put (Result, Pointer, ",");
         end if;
         Put (Result, Pointer, Dump (S (I)));
      end loop;
      return Result (1..Pointer - 1);
   end Dump;

begin
   declare
      procedure Check
                (  S : Strings_Edit.UTF8.UTF8_Code_Point_Array;
                   C : Strings_Edit.UTF8.UTF8_Code_Point;
                   E : Strings_Edit.UTF8.UTF8_Code_Point_Array;
                   F : Normalization_Form
                )  is
         use Strings_Edit.UTF8;
         R : UTF8_Code_Point_Array (1..128);
         P : Integer := S'Length + 1;
      begin
         R (1..S'Length) := S;
         Decompose (R, P, C, F);
         if E /= R (1..P - 1) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Decompose one "
               &  Normalization_Form'Image (F)
               &  " of "
               &  Dump (S)
               &  " error: "
               &  Dump (R (1..P - 1))
               &  " /= "
               &  Dump (E)
               &  " (expected)"
           )  );
         end if;
      end Check;
   begin
      Check
      (  (16#0073#,16#0323#),
         16#0307#,
         (16#0073#,16#0323#,16#0307#),
         NFD
      );
      Check
      (  (16#0073#,16#0307#),
         16#0323#,
         (16#0073#,16#0323#,16#0307#),
         NFD
      );
   end;
   declare
      procedure Check
                (  S : Strings_Edit.UTF8.UTF8_Code_Point_Array;
                   E : Strings_Edit.UTF8.UTF8_Code_Point_Array
                )  is
         use Strings_Edit.UTF8;
         R : UTF8_Code_Point_Array := S;
         P : Integer;
      begin
         Compose (R, P);
         if E /= R (R'First..P - 1) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Compose of "
               &  Dump (S)
               &  " error: "
               &  Dump (R (R'First..P - 1))
               &  " /= "
               &  Dump (E)
               &  " (expected)"
           )  );
         end if;
      end Check;
   begin
      Check ((16#0066#,16#0069#),    (16#0066#,16#0069#));
      Check ((16#0073#,16#0323#,16#0307#), (1=>16#1E69#));
      Check ((16#017F#,16#0307#),          (1=>16#1E9B#));
      Check ((1=>16#0065#),                (1=>16#0065#));
      Check ((1=>16#0065#),                (1=>16#0065#));
      Check ((16#0065#,16#0067#),     (16#0065#,16#0067#));
      Check ((16#0065#,16#0067#),     (16#0065#,16#0067#));
   end;
   declare
      procedure Check
                (  D : String;
                   S : Strings_Edit.UTF8.Code_Point;
                   E : String;
                   F : Normalization_Form
                )  is
         R : String (1..120);
         I : Integer := D'Length + 1;
      begin
         R (1..D'Length) := D;
         Decompose (R, I, S, F);
         if E /= R (1..I - 1) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Decompose "
               &  Normalization_Form'Image (F)
               &  " of "
               &  Dump (D)
               &  " error: "
               &  Dump (R (1..I - 1))
               &  " /= "
               &  Dump (E)
               &  " (expected)"
           )  );
         end if;
      end Check;
   begin
      Check
      (  I(16#0064#)&I(16#0307#),
         16#0323#,
         I(16#0064#)&I(16#0323#)&I(16#0307#),
         NFKD
      );
   end;
   declare
      procedure Check
                (  S : Strings_Edit.UTF8.Code_Point;
                   E : String;
                   F : Normalization_Form
                )  is
         R : constant String := Normalize (S, F);
      begin
         if E /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Normalization "
               &  Normalization_Form'Image (F)
               &  " of "
               &  I (S)
               &  " "
               &  Dump (S)
               &  " ("
               &  Dump (I (S))
               &  ") error: "
               &  Dump (R)
               &  " /= "
               &  Dump (E)
               &  " (expected)"
           )  );
         end if;
      end Check;
   begin
      Check (16#FB01#, I(16#0066#)&I(16#0069#),             NFKC);
      Check (16#1EBF#, I(16#1EBF#),                         NFKC);
      Check (16#1EBF#, I(16#0065#)&I(16#0302#)&I(16#0301#), NFKD);
      Check (16#1EBF#, I(16#0065#)&I(16#0302#)&I(16#0301#), NFD);
      Check (16#00EA#, I(16#0065#)&I(16#0302#),             NFKD);
      Check (16#1EBF#, I(16#1EBF#),                         NFC);
      Check (16#2126#, I(16#03A9#),                         NFD);
      Check (16#1EBF#, I(16#0065#)&I(16#0302#)&I(16#0301#), NFKD);
      Check (16#00EA#, I(16#0065#)&I(16#0302#),             NFD);
      Check (16#1E69#, I(16#0073#)&I(16#0323#)&I(16#0307#), NFD);
      Check (16#FB01#, I(16#FB01#),                         NFD);
      Check (16#FB01#, I(16#0066#)&I(16#0069#),             NFKD);
      Check (16#00E4#, I(16#0061#)&I(16#0308#),             NFD);
      Check (16#2126#, I(16#03A9#),                         NFC);
      Check (16#2126#, I(16#03A9#),                         NFD);
      Check (16#2126#, I(16#03A9#),                         NFKD);
      Check (16#00BE#, I(16#0033#)&I(16#2044#)&I(16#0034#), NFKD);
   end;
   declare
      procedure Check
                (  S : Strings_Edit.UTF8.Code_Point;
                   E : Boolean;
                   F : Normalization_Form
                )  is
         R : constant Boolean := Is_Normalized (S, F);
      begin
         if E /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Is_Normalized point "
               &  Normalization_Form'Image (F)
               &  " of "
               &  I (S)
               &  " error: "
               &  Boolean'Image (R)
               &  " /= "
               &  Boolean'Image (E)
               &  " (expected)"
           )  );
         end if;
      end Check;
   begin
      Check (16#0040#, True,  NFD);
      Check (16#0040#, True,  NFKD);
      Check (16#FB01#, True,  NFD);
      Check (16#FB01#, False, NFKD);
   end;
   declare
      procedure Check
                (  S : String;
                   E : Boolean;
                   F : Normalization_Form
                )  is
         R : constant Boolean := Is_Normalized (S, F);
      begin
         if E /= R then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Is_Normalized string "
               &  Normalization_Form'Image (F)
               &  " of "
               &  Dump (S)
               &  " error: "
               &  Boolean'Image (R)
               &  " /= "
               &  Boolean'Image (E)
               &  " (expected)"
           )  );
         end if;
      end Check;
   begin
      Check (I(16#0040#),                         True,  NFD);
      Check (I(16#0073#)&I(16#0323#)&I(16#0307#), True,  NFD);
      Check (I(16#0073#)&I(16#0307#)&I(16#0323#), False, NFD);
   end;
   declare
      procedure Check
                (  S : String;
                   E : String;
                   F : Normalization_Form
                )  is
         R : String (1..160);
         P : Integer := S'First;
      begin
         Compose (R, P, S);
         if E /= R (R'First..P - 1) then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Compose "
               &  Normalization_Form'Image (F)
               &  " of "
               &  Dump (S)
               &  " error: "
               &  Dump (R (R'First..P - 1))
               &  " /= "
               &  Dump (E)
               &  " (expected)"
           )  );
         end if;
      end Check;
   begin
      Check (I(16#0073#)&I(16#0323#)&I(16#0307#), I(16#1E69#), NFD);
      Check (I(16#0066#)&I(16#0069#), I(16#0066#)&I(16#0069#), NFKD);
      Check (I(16#017F#)&I(16#0307#),             I(16#1E9B#), NFD);
      Check (I(16#0065#),                         I(16#0065#), NFD);
      Check (I(16#0065#),                         I(16#0065#), NFKD);
      Check (I(16#0065#)&I(16#0067#), I(16#0065#)&I(16#0067#), NFKD);
      Check (I(16#0065#)&I(16#0067#), I(16#0065#)&I(16#0067#), NFD);
   end;
   declare
      procedure Check
                (  L : String;
                   R : String;
                   C : Boolean;
                   E : Precedence;
                   F : Normalization_Form
                )  is
      begin
         if C then
            declare
               Result : constant Precedence :=
                        Compare_Decomposed (L, R, Lower_Case_Map, F);
            begin
               if E /= Result then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Compare case-insensitive "
                     &  Normalization_Form'Image (F)
                     &  " "
                     &  Dump (L)
                     &  " to "
                     &  Dump (R)
                     &  " "
                     &  Precedence'Image (Result)
                     &  " /= "
                     &  Precedence'Image (E)
                     &  " (expected)"
                  )  );
               end if;
            end;
         else
            declare
               Result : constant Precedence :=
                        Compare_Decomposed (L, R, F);
            begin
               if E /= Result then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Compare "
                     &  Normalization_Form'Image (F)
                     &  " "
                     &  Dump (L)
                     &  " to "
                     &  Dump (R)
                     &  " "
                     &  Precedence'Image (Result)
                     &  " /= "
                     &  Precedence'Image (E)
                     &  " (expected)"
                  )  );
               end if;
            end;
         end if;
      end Check;
   begin
      Check
      (  I(16#0073#)&I(16#0323#)&I(16#0307#),
         I(16#1E69#),
         False,
         Equal,
         NFD
      );
      Check
      (  I(16#0073#)&I(16#0307#)&I(16#0323#),
         I(16#1E69#),
         False,
         Equal,
         NFD
      );
      Check (I(16#0065#), I(16#0069#), True,  Less,    NFD);
      Check (I(16#00E4#), I(16#00C4#), False, Greater, NFD);
      Check (I(16#00E4#), I(16#00C4#), True,  Equal,   NFD);
      Check (I(16#03C9#), I(16#2126#), True,  Equal,   NFKD);
      Check (I(16#03C9#), I(16#2126#), True,  Equal,   NFD);
   end;
   declare
      procedure Check
                (  L : String;
                   R : String;
                   C : Boolean;
                   E : Boolean;
                   F : Normalization_Form;
                   N : Normalized_Type := None
                )  is
      begin
         if C then
            declare
               Result : constant Boolean :=
                        Is_Normalized_Prefix (L, R, Lower_Case_Map, F);
            begin
               if E /= Result then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Is_Prefix case-insensitive "
                     &  Normalization_Form'Image (F)
                     &  " "
                     &  Dump (L)
                     &  " to "
                     &  Dump (R)
                     &  " "
                     &  Boolean'Image (Result)
                     &  " /= "
                     &  Boolean'Image (E)
                     &  " (expected)"
                  )  );
               end if;
            end;
         else
            declare
               Result : constant Boolean :=
                        Is_Normalized_Prefix (L, R, F, N);
            begin
               if E /= Result then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Is_Prefix "
                     &  Normalization_Form'Image (F)
                     &  " "
                     &  Dump (L)
                     &  " to "
                     &  Dump (R)
                     &  " "
                     &  Boolean'Image (Result)
                     &  " /= "
                     &  Boolean'Image (E)
                     &  " (expected)"
                  )  );
               end if;
            end;
         end if;
      end Check;
   begin
      Check
      (  I(16#1E69#),
         I(16#0073#)&I(16#0323#)&I(16#0307#)&"abc",
         False,
         True,
         NFC,
         First
      );
      Check
      (  I(16#1E69#),
         I(16#0073#)&I(16#0323#)&I(16#0307#)&I(16#0065#),
         False,
         True,
         NFC
      );
      Check
      (  I(16#1E69#),
         I(16#0073#)&I(16#0307#)&I(16#0323#)&I(16#0065#),
         False,
         True,
         NFC
      );
      Check
      (  I(16#0073#)&I(16#0307#)&I(16#0323#)&I(16#0065#),
         I(16#1E69#),
         False,
         False,
         NFC
      );
   end;
exception
   when Error : others =>
      Put ("Error: ");
      Put_Line (Exception_Information (Error));
end Test_UTF8;
