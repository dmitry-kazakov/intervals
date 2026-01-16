--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Normalization_Generator   Luebeck            --
--  Mapping generation                             Autumn, 2025       --
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
--
--  This procedure adjusts Strings_Edit.UTF8.Normalization.adb according
--  to the file unicodedata.txt. It is used as follows:
--
--      Strings_Edit-UTF8-Normalization <source-file> <data-file>
--
--   Here:
--
--      <source-file> is strings_edit.utf8.mapping.adb
--      <data-file>   is unicodedata.txt
--
--  The file UnicodeData.txt can be obtained from:
--
--     ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt
--
--  It is distributed by Unicode, Inc.
--
--     http://www.unicode.org
--
with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Strings_Edit.Integer_Edit;

procedure Strings_Edit.UTF8.Normalization_Generator is
   Compatibility : constant := 16#10000000#;

   File    : File_Type;
   Line    : String (1..1048);
   Line_No : Natural := 0;
   Length  : Integer;
   Pointer : Integer;

   type Text_Buffer_State is
        (  Prologue,
           Has_Mapping_Index,
           Has_Mapping_Begin,
           Has_Mapping_End,
           Has_Classes_Index,
           Has_Classes_Begin,
           Has_Classes_End,
           NFD_Index,
           NFD_Begin,
           NFD_End
        );

   type Text_Buffer_Index is range 1..40_000;
   type Text_Buffer is array (Text_Buffer_Index) of Unbounded_String;

   State            : Text_Buffer_State := Prologue;
   Buffer           : Text_Buffer;
   Classes_Index_At : Text_Buffer_Index;
   Classes_At       : Text_Buffer_Index;
   Mapping_Index_At : Text_Buffer_Index;
   Mapping_Data_At  : Text_Buffer_Index;
   NFD_Index_At     : Text_Buffer_Index;
   NFD_At           : Text_Buffer_Index;
   Free             : Text_Buffer_Index := Text_Buffer_Index'First;

   type Code_Position is range Code_Point'First..Code_Point'Last;
   package Code_Point_Edit is
      new Strings_Edit.Integer_Edit (Code_Position);
   use Code_Point_Edit;

   Size : Code_Position := 0;
   type Item;
   type Item_Ptr is access Item;
   type Sequence is array (Positive range <>) of Code_Position;

   type Sequence_Ptr is access Sequence;
   type Item is record
      List : Sequence_Ptr;
      Next : Item_Ptr;
   end record;

   function "<" (Left, Right : Sequence) return Boolean is
      J : Positive := Right'First;
      function Less (Left, Right : Code_Position) return Boolean is
         L : Code_Position := Left;
         R : Code_Position := Right;
      begin
         if L >= Compatibility then
            L := L mod Compatibility;
         end if;
         if R >= Compatibility then
            R := R mod Compatibility;
         end if;
         return L < R;
      end Less;
   begin
      for I in Left'Range loop
         if J > Right'Last or else Less (Right (J), Left (I)) then
            return False;
         elsif Less (Left (I), Right (J)) then
            return True;
         end if;
         J := J + 1;
      end loop;
      return J <= Right'Last;
   end "<";

   function "<" (Left, Right : Item_Ptr) return Boolean is
   begin
      return Left.List  (2..Left.List'Last)
           < Right.List (2..Right.List'Last);
   end "<";

   Head : Item_Ptr;
   Tail : Item_Ptr;

   type Class_Pair;
   type Class_Pair_Ptr is access Class_Pair;
   type Class_Pair is record
      Code  : Code_Position;
      Class : Code_Position;
      Next  : Class_Pair_Ptr;
   end record;
   Classes_No  : Code_Position := 0;
   First_Class : Class_Pair_Ptr;
   Last_Class  : Class_Pair_Ptr;

   type Reversed is array (Code_Position range <>) of Item_Ptr;
   NFD         : Reversed (1..10_000);
   NFKD        : Reversed (1..10_000);
   NFD_Length  : Code_Position := 0;
   NFKD_Length : Code_Position := 0;

   procedure Add_Line is
   begin
      if Free = Text_Buffer_Index'Last then
         Raise_Exception
         (  Data_Error'Identity,
            "File is too long when " & Text_Buffer_State'Image (State)
         );
      end if;
      Buffer (Free) := To_Unbounded_String (Line (1..Length));
      Free := Free + 1;
   end Add_Line;

   function Dump (Code : Code_Position)
      return String is
      type Integer_64 is range -2**63..2**63 - 1;
      package Edit is new Strings_Edit.Integer_Edit (Integer_64);
   begin
      return Edit.Image (Integer_64 (Code), Base => 16);
   end Dump;

   function Dump (S : Sequence) return String is
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

   function Dump (S : String) return String is
      Result : String (1..120);
      Code   : Code_Position;
      From   : Integer := S'First;
      To     : Integer := Result'First;
   begin
      while From <= S'Last loop
         Get (S, From, Code);
         Put (Result, To, Dump (Code) & " ");
      end loop;
      return Result (1..To - 1);
   end Dump;

   procedure Read_Line is
   begin
      Get_Line (File, Line, Length);
      Line_No := Line_No + 1;
      if Length = Line'Last then
         Raise_Exception
         (  Data_Error'Identity,
            "Too long line" & Integer'Image (Line_No)
         );
      end if;
      while (  Length > Line'First
            and then
               (  Line (Length) = LF
               or else
                  Line (Length) = CR
            )  )
      loop
         Length := Length - 1;
      end loop;
      Line (Length + 1) := Character'Val (0);
      Pointer := Line'First;
   end Read_Line;

   procedure Get (Point : out Code_Position) is
   begin
      Get (Line, Pointer, SpaceAndTab);
      Get (Line, Pointer, Point, 16);
   exception
      when End_Error | Data_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Hexadecimal code point number is expected at"
            &  Integer'Image (Pointer)
         )  );
      when Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Hexadecimal code point number at"
            &  Integer'Image (Pointer)
            &  " is out of range"
         )  );
   end Get;

   procedure Get_Optional
             (  Point  : out Code_Position;
                Compat : out Code_Position
             )  is
      Start : Integer;
   begin
      Compat := 0;
      Get (Line, Pointer, SpaceAndTab);
      if Pointer <= Length and then Line (Pointer) = ';' then
         Point := 0;
         return;
      end if;
      if Pointer <= Length and then Line (Pointer) = '<' then
         Pointer := Pointer + 1;
         Start   := Pointer;
         while Pointer <= Length and then Line (Pointer) /= '>' loop
            Pointer := Pointer + 1;
         end loop;
         if Pointer <= Length then
            if Line (Start..Pointer - 1) = "initial" then
               Compat := Compatibility * 2;
            elsif Line (Start..Pointer - 1) = "medial" then
               Compat := Compatibility * 3;
            elsif Line (Start..Pointer - 1) = "final" then
               Compat := Compatibility * 4;
            else
               Compat := Compatibility;
            end if;
            Pointer := Pointer + 1;
            Get (Line, Pointer, SpaceAndTab);
         end if;
      end if;
      Get (Line, Pointer, Point, 16);
   exception
      when End_Error | Data_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Hexadecimal code point number is expected at"
            &  Integer'Image (Pointer)
         )  );
      when Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Hexadecimal code point number at"
            &  Integer'Image (Pointer)
            &  " is out of range"
         )  );
   end Get_Optional;

   procedure Get_Semicolon is
   begin
      Get (Line, Pointer, SpaceAndTab);
      if Pointer <= Length then
         if Line (Pointer) = ';' then
            Pointer := Pointer + 1;
         else
            Raise_Exception
            (  Data_Error'Identity,
               (  "Semicolon is expected at"
               &  Integer'Image (Pointer)
            )  );
         end if;
      end if;
   end Get_Semicolon;

begin
   if Argument_Count /= 2 then
      Put_Line
      (  Standard_Error,
         (  "Use: strings_edit-utf8-normalization_generator "
         &  "../strings_edit-utf8-normalization.adb unicodedata.txt"
      )  );
      Set_Exit_Status (Failure);
      return;
   end if;
   declare
      Output : constant String := Argument (1);
      Input  : constant String := Argument (2);
   begin
      begin
         Open (File, In_File, Output);
      exception
         when Name_Error =>
            Put_Line ("Cannot open " & Quote (Output));
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Cannot open "
            &  Quote (Output)
            &  ": "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      begin
         --
         -- Reading the Strings_Edit.UTF8.Mappings file
         --
         Line_No := 0;
         loop
            Read_Line;
            case State is
               when Prologue =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix
                     (  "type Normalization_Map_Size is",
                        Line,
                        Pointer
                     )
                  then
                     State := Has_Mapping_Index;
                     Mapping_Index_At := Free;
                  else
                     Add_Line;
                  end if;
               when Has_Mapping_Index =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix ("S", Line, Pointer) then
                     State := Has_Mapping_Begin;
                     Mapping_Data_At := Free;
                  else
                     Add_Line;
                  end if;
               when Has_Mapping_Begin =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix ("S", Line, Pointer) then
                     null;
                  elsif Is_Prefix (");", Line, Pointer) then
                     State := Has_Mapping_End;
                  end if;
               when Has_Mapping_End =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix ("type Class_Index", Line, Pointer) then
                     State := Has_Classes_Index;
                     Classes_Index_At := Free;
                  else
                     Add_Line;
                  end if;
               when Has_Classes_Index =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix
                     (  "Classes : constant Class_Array",
                        Line,
                        Pointer
                     )  then
                     State := Has_Classes_Begin;
                     Classes_At := Free;
                  else
                     Add_Line;
                  end if;
               when Has_Classes_Begin =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix (");", Line, Pointer) then
                     State := Has_Classes_End;
                  end if;
               when Has_Classes_End =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix
                     (  "type NFD_Index is",
                        Line,
                        Pointer
                     )
                  then
                     State := NFD_Index;
                     NFD_Index_At := Free;
                  else
                     Add_Line;
                  end if;
               when NFD_Index =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix
                     (  "NFD_List : constant NFD_Array",
                        Line,
                        Pointer
                     )  then
                     State := NFD_Begin;
                     NFD_At := Free;
                  else
                     Add_Line;
                  end if;
               when NFD_Begin =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix (");", Line, Pointer) then
                     State := NFD_End;
                  end if;
               when NFD_End =>
                  Add_Line;
            end case;
         end loop;
      exception
         when End_Error =>
            Close (File);
            if State /= NFD_End then
               Put_Line
               (  "File "
               &  Quote (Output)
               &  " is not recognized as "
               &  "Strings_Edit.UTF8.Normalization source"
               );
               return;
            end if;
         when Error : Data_Error =>
            Put_Line
            (  "Malformed "
            &  Quote (Output)
            &  " (Line"
            &  Integer'Image (Line_No)
            &  ") "
            &  Exception_Message (Error)
            );
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Internal error while reading "
            &  Quote (Output)
            &  " (Line"
            &  Integer'Image (Line_No)
            &  ") "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      --
      -- Dealing with UnicodeData.txt
      --
      begin
         Open (File, In_File, Input);
      exception
         when Name_Error =>
            Put_Line ("Cannot open " & Quote (Input));
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Cannot open "
            &  Quote (Input)
            &  ": "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      declare
         This   : Sequence (1..80);
         Class  : Code_Position;
         Compat : Code_Position;
         Count  : Natural;
      begin
         --
         -- Reading the UnicodeData file
         --
         Line_No := 0;
         loop
            Read_Line;
            Get (Line, Pointer, SpaceAndTab);
            if Pointer <= Length then
               Count := 1;
               Get (Code_Position (This (1)));
               Count := Count + 1;
               for Semicolon in 1..3 loop
                  Get (Line, Pointer, SpaceAndTab);
                  while Pointer < Length and then Line (Pointer) /= ';'
                  loop
                     Pointer := Pointer + 1;
                  end loop;
                  Get_Semicolon;
               end loop;
               begin
                  Get (Line, Pointer, Class);
               exception
                  when End_Error | Data_Error =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Combining class number is expected at"
                        &  Integer'Image (Pointer)
                     )  );
                  when Constraint_Error =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Combining class number at"
                        &  Integer'Image (Pointer)
                        &  " is out of range"
                     )  );
               end;
               Get (Line, Pointer, SpaceAndTab);
               Get_Semicolon;
               if Class > 0 then
                  if First_Class = null then
                     First_Class :=
                        new Class_Pair'(This (1), Class, null);
                     Last_Class := First_Class;
                  else
                     Last_Class.Next :=
                        new Class_Pair'(This (1), Class, null);
                     Last_Class := Last_Class.Next;
                  end if;
                  Classes_No := Classes_No + 1;
               end if;
               Get (Line, Pointer, SpaceAndTab);
               while Pointer < Length and then Line (Pointer) /= ';'
               loop
                  Pointer := Pointer + 1;
               end loop;
               Get_Semicolon;
               Get (Line, Pointer, SpaceAndTab);
               if Pointer + 1 > Length then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Normalization specification is expected at"
                     &  Integer'Image (Pointer)
                  )  );
               end if;
               loop
                  Get_Optional (This (Count), Compat);
                  This (Count) := This (Count) + Compat;
                  exit when This (Count) = 0;
                  Count := Count + 1;
                  Get (Line, Pointer, SpaceAndTab);
               end loop;
               Count := Count - 1;
               Get_Semicolon;
               if Count > 1 then
                  if Tail = null then
                     Head := new Item;
                     Tail := Head;
                     Tail.List := new Sequence'(This (1..Count));
                  else
                     Tail.Next := new Item;
                     Tail := Tail.Next;
                     Tail.List := new Sequence'(This (1..Count));
                  end if;
                  Size := Size + 1;
               end if;
            end if;
         end loop;
      exception
         when End_Error =>
            Close (File);
         when Error : Data_Error =>
            Put_Line
            (  "Malformed "
            &  Quote (Input)
            &  " (Line"
            &  Integer'Image (Line_No)
            &  ") "
            &  Exception_Message (Error)
            );
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Internal error while reading "
            &  Quote (Input)
            &  " (Line"
            &  Integer'Image (Line_No)
            &  ") "
            &  Integer'Image (Line_No)
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      --
      -- Writing the output
      --
      declare
         procedure Add
                   (  List   : in out Reversed;
                      Length : in out Code_Position;
                      Item   : Item_Ptr
                   )  is
         begin
            if Length = 0 then
               List (1) := Item;
            else
               declare
                  From    : Code_Position := List'First;
                  To      : Code_Position := List'First + Length - 1;
                  This    : Code_Position;
                  Current : Item_Ptr;
               begin
                  loop
                     This := (From + To) / 2;
                     Current := List (This);
                     if Item < Current then
                        if This = From then
                           List (This + 1..List'First + Length) :=
                              List (This..List'First + Length - 1);
                           List (This) := Item;
                           exit;
                        end if;
                        To := This - 1;
                     elsif Current < Item then
                        if This = To then
                           List (This + 2..List'First + Length) :=
                              List (This + 1..List'First + Length - 1);
                           List (This + 1) := Item;
                           exit;
                        end if;
                        From := This + 1;
                     else
                        return;
                        --  Put_Line
                        --  (  "Program error "
                        --  &  Image (Current.List (1), Base => 16)
                        --  &  ", "
                        --  &  Image (Current.List (2), Base => 16)
                        --  &  " = "
                        --  &  Image (Item.List (1), Base => 16)
                        --  &  ", "
                        --  &  Image (Item.List (2), Base => 16)
                        --  &  " < = "
                        --  &  Boolean'Image (Item < Current)
                        --  &  " > = "
                        --  &  Boolean'Image (Current < Item)
                        --  &  " lengths ="
                        --  &  Integer'Image (Current.List'Length)
                        --  &  ","
                        --  &  Integer'Image (Item.List'Length)
                        --  );
                        --  Set_Exit_Status (Failure);
                        --  raise Program_Error;
                     end if;
                  end loop;
               end;
            end if;
            Length := Length + 1;
         end Add;

         Next : Item_Ptr := Head;
      begin
         while Next /= null loop
            if Next.List'Length > 2 then
               if Next.List (2) < Compatibility then
                  Add (NFD, NFD_Length, Next);
               elsif Next.List (2) < Compatibility * 2 - 1 then
                  Add (NFKD, NFKD_Length, Next);
               end if;
            end if;
            Next := Next.Next;
         end loop;
         Open (File, Out_File, Output);
      exception
         when Name_Error =>
            Put_Line ("Cannot open " & Quote (Output) & " for write");
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Cannot open "
            &  Quote (Output)
            &  ": "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      declare
         Count : Natural := 0;
      begin
         for Line in 1..Mapping_Index_At - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Put_Line
         (  File,
            (  "   type Normalization_Map_Size is range "
            &  "1.."
            &  Image (Size)
            &  ";"
         )  );
         for Line in Mapping_Index_At..Mapping_Data_At - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Tail := Head;
         while Tail /= null loop
            Put (File, "   S");
            Put (File, Image (Tail.List (1), 16));
            Put (File, " : aliased constant Code_Point_Array := (");
            if Tail.List'Length = 1 then
               Put (File, "1=>");
            end if;
            for Index in Tail.List'First..Tail.List'Last loop
               Put (File, "16#");
               Put (File, Image (Tail.List (Index), 16));
               if Index < Tail.List'Last then
                  Put (File, "#,");
               else
                  Put (File, "#");
               end if;
            end loop;
            Put (File, ");");
            Tail := Tail.Next;
            New_Line (File);
         end loop;
         Tail := Head;
         Put_Line
         (  File,
            "   Mapping : constant Normalization_Map :="
         );
         Put (File, "   (  ");
         while Tail /= null loop
            Put (File, "S");
            Put (File, Image (Tail.List (1), 16));
            Put (File, "'Access");
            if Size > 1 then
               Put (File, ",");
               Size := Size - 1;
            end if;
            if Count = 4 then
               New_Line (File);
               Put (File, "      ");
               Count := 0;
            else
               Count := Count + 1;
            end if;
            Tail := Tail.Next;
         end loop;
         New_Line (File);
         Put_Line (File, "   );");

         for Line in Mapping_Data_At..Classes_Index_At - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Put_Line
         (  File,
            (  "   type Class_Index is range "
            &  "1.."
            &  Image (Classes_No)
            &  ";"
         )  );
         for Line in Classes_Index_At..Classes_At - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Last_Class := First_Class;
         Count := 0;
         Put_Line
         (  File,
            "   Classes : constant Class_Array :="
         );
         Put (File, "   (  ");
         while Last_Class /= null loop
            Put (File, "(16#");
            Put (File, Image (Last_Class.Code, 16));
            Put (File, "#,");
            Put (File, Image (Last_Class.Class));
            Put (File, ")");
            if Classes_No > 1 then
               Put (File, ",");
            end if;
            Classes_No := Classes_No - 1;
            if Count = 4 then
               New_Line (File);
               Put (File, "      ");
               Count := 0;
            else
               Count := Count + 1;
            end if;
            Last_Class := Last_Class.Next;
         end loop;
         New_Line (File);
         Put_Line (File, "   );");

         for Line in Classes_At..NFD_Index_At - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Put_Line
         (  File,
            (  "   type NFD_Index is range "
            &  "1.."
            &  Image (NFD_Length)
            &  ";"
         )  );
         for Line in NFD_Index_At..NFD_At - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Count := 0;
         Put_Line
         (  File,
            "   NFD_List : constant NFD_Array :="
         );
         Put (File, "   (  ");
         for I in 1..NFD_Length loop
            Put (File, "S");
            Put (File, Image (NFD (I).List (1), 16));
            Put (File, "'Access");
            if I < NFD_Length then
               Put (File, ",");
            end if;
            if Count = 4 then
               New_Line (File);
               Put (File, "      ");
               Count := 0;
            else
               Count := Count + 1;
            end if;
         end loop;
         New_Line (File);
         Put_Line (File, "   );");

         for Line in NFD_At..Free - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Close (File);
      exception
         when Error : others =>
            Put_Line
            (  "Cannot write "
            &  Quote (Output)
            &  ": "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
   end;
   Set_Exit_Status (Success);
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Strings_Edit.UTF8.Normalization_Generator;
