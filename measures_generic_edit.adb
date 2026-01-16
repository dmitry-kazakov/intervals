--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Measures_Generic_Edit                      Luebeck            --
--  Implementation                                 Spring, 2005       --
--                                                                    --
--                                Last revision :  09:15 26 Nov 2022  --
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
-- Implementation  notes.  The  package  Tables  is  instantiated at the
-- library level (no other way) by the packages:
--
--    Measures_Table_Of_Measure
--    Measures_Table_Of_Integer
--
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Measures_Table_Of_Measure;  use Measures_Table_Of_Measure;
with Measures_Table_Of_Integer;  use Measures_Table_Of_Integer;
with System;                     use type System.Address;
with IO_Exceptions;              use IO_Exceptions;

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with Units.Base;

package body Measures_Generic_Edit is
   package Measure_Address is new
      System.Address_To_Access_Conversions (Measure);
   use Float_Edit;
   use Irregular_Measures.Derived_Measures_Of;
   use Irregular_Measures;

   type Operation is
        (  Power,
           Multiply,
           Unary_Plus,
           Unary_Minus,
           Default_Multiply,
           Divide,
           Minus,
           Plus,
           Shift,
           Right_Bracket,
           Left_Bracket           -- Lowest priority operation
        );
   Latin1_Flag : constant := 64;
   UTF8_Flag   : constant := 128;
   --
   -- Some of UTF-8 characters
   --
   Omega : constant String := Character'Val (206) & Character'Val (169);
   Mu    : constant String := Character'Val (206) & Character'Val (188);
   Mu_L  : constant String := Character'Val (194) & Character'Val (181);
   Dot   : constant String := Character'Val (194) & Character'Val (183);
   Ring  : constant String := Character'Val (194) & Character'Val (176);
   UC_AR : constant String := Character'Val (195) & Character'Val (133);
   LC_AR : constant String := Character'Val (195) & Character'Val (165);
   LC_OE : constant String := Character'Val (195) & Character'Val (182);

   Degree_Celsius    : constant String :=
      Character'Val (226) & Character'Val (132) & Character'Val (131);
   Degree_Fahrenheit : constant String :=
      Character'Val (226) & Character'Val (132) & Character'Val (137);
   Ohm_Sign          : constant String :=
      Character'Val (226) & Character'Val (132) & Character'Val (166);
   Kelvin_Sign       : constant String :=
      Character'Val (226) & Character'Val (132) & Character'Val (170);
   Angstrom_Sign     : constant String :=
      Character'Val (226) & Character'Val (132) & Character'Val (171);
   Ounce_Sign        : constant String :=
      Character'Val (226) & Character'Val (132) & Character'Val (165);
   --
   -- Latin1 units
   --
   L_ang        : aliased constant Measure := ang;        -- Angstroem
   L_degree     : aliased constant Measure := degree;     -- °
   L_Fahrenheit : aliased constant Measure := Fahrenheit; -- °F
   L_K          : aliased constant Measure := K;          -- °K
   L_Celsius    : aliased constant Measure := Celsius;    -- °C
   --
   -- UTF8 units
   --
   U_ang        : aliased constant Measure := ang;        -- Angstroem
   U_degree     : aliased constant Measure := degree;     -- °
   U_Fahrenheit : aliased constant Measure := Fahrenheit; -- °F
   U_K          : aliased constant Measure := K;          -- °K
   U_Celsius    : aliased constant Measure := Celsius;    -- °C
   U_Ohm        : aliased constant Measure := Ohm;        -- Ohm
   U_oz         : aliased constant Measure := oz;         -- Ounce

   type Affinity is (Prefix, Dyadic);
   --
   -- >= -- Compare priorities
   --
   --    Left   - Operation
   --    Right  - Operation
   --
   -- Retunrs :
   --
   --    True  if  Left's  priority  is  higher.  I.e.  the   expression
   --    A<Left>B<Right>C is associated as (A<Left>B)<Right>C.
   --
   function ">=" (Left, Right : Operation) return Boolean;
   pragma Inline (">=");
   --
   -- IsBreak -- Test if the keyword end is reached
   --
   --    Source  - The string
   --    Pointer - The current position
   --    Mode    - The current character set
   --
   -- Returns :
   --
   --    True if the position can be considered as a break
   --
   function Is_Break
            (  Source  : String;
               Pointer : Integer;
               Mode    : Code_Set
            )  return Boolean;
   pragma Inline (Is_Break);
   --
   -- Generic_Stack -- A simple generic stack package
   --
   -- The stack is built of segments.  To  improve  performance  in  the
   -- cases when the stack is small, the first segment is  preallocated.
   -- This  causes  however  a  problem  of  taking  the  address of the
   -- preallocated  segment  and  storing  it into a field of the record
   -- representing  the  stack  when the segment itself is a part of the
   -- record.  Apart  from  using  'Unchecked_Access, it simply does not
   -- work when the record is passed by copy. Therefore the  type  Stack
   -- in full view is made limited, which ensures  by-reference  passing
   -- (ARM 6.2(7)).
   --
   Stack_Segment : constant := 100;      -- The segment size (elements)

   generic
      type Element is private;
   package Generic_Stack is
      type Stack is limited private;
      type Stack_Ptr is access Stack;
      --
      -- Init -- Initialize the stack
      --
      --    LIFO - The stack
      --
      -- This procedure should be called once for each variable of Stack
      -- type prior use of any other stack subroutines.
      --
      procedure Init (LIFO : in out Stack);
      --
      -- Depth -- Get the stack depth
      --
      --    LIFO - The stack
      --
      -- Returns :
      --
      --    The number of elements on the stack
      --
      function Depth (LIFO : Stack) return Natural;
      pragma Inline (Depth);
      --
      -- Destroy -- Finalize the stack
      --
      --    LIFO - The stack
      --
      -- This procedure shall be called for each variable of Stack type.
      -- It reclaims the dynamic memory allocated for the stack:
      --
      --    LIFO : Stack;
      -- begin
      --    Init (LIFO);
      --    ...
      --    Destroy (LIFO);     -- Reclaim memory
      -- exception
      --    when others =>
      --       Destroy (LIFO);  -- Even if an exception happens
      --       raise;
      -- end;
      --
      procedure Destroy (LIFO : in out Stack);
      --
      -- Is_Empty -- Check if the stack is empty
      --
      --    LIFO - The stack
      --
      -- Returns :
      --
      --    True if the stack is empty
      --
      function Is_Empty (LIFO : Stack) return Boolean;
      pragma Inline (Is_Empty);
      --
      -- Pop -- Pop the topmost element
      --
      --    LIFO - The stack
      --
      -- Exceptions :
      --
      --    Data_Error  - The stack is empty
      --
      procedure Pop (LIFO : in out Stack);
      --
      -- Push -- Push a new element
      --
      --    LIFO - The stack
      --    Item - To be pushed
      --
      procedure Push (LIFO : in out Stack; Item : Element);
      --
      -- Top -- Get the topmost element
      --
      --    LIFO - The stack
      --
      -- Returns :
      --
      --    The topmost stack element
      --
      -- Exceptions :
      --
      --    Data_Error  - The stack is empty
      --
      function  Top (LIFO : Stack) return Element;
      pragma Inline (Top);

   private
      type Segment;
      type Segment_Ptr is access all Segment;
      type Segment_Data is array (Integer range 1..Stack_Segment)
         of Element;
      type Segment is record
         Next     : Segment_Ptr;
         Previous : Segment_Ptr;
         Data     : Segment_Data;
      end record;
      type Stack is limited record
         Used    : Integer;
         Current : Segment_Ptr;
         First   : aliased Segment;
      end record;
   end Generic_Stack;

   package body Generic_Stack is
      procedure Free is new
         Ada.Unchecked_Deallocation (Segment, Segment_Ptr);

      procedure Init (LIFO : in out Stack) is
      begin
         LIFO.Used := 0;
         LIFO.First.Previous := null;
         LIFO.First.Next := null;
         LIFO.Current := LIFO.First'Unchecked_Access;
      end Init;

      procedure Destroy (LIFO : in out Stack) is
         Ptr : Segment_Ptr := LIFO.Current;
      begin
         while Ptr.Next /= null loop
            Ptr := Ptr.Next;
         end loop;
         while Ptr.Previous /= null loop
            Ptr := Ptr.Previous;
            Free (Ptr.Next);
         end loop;
      end Destroy;

      procedure Push (LIFO : in out Stack; Item : Element) is
      begin
         if LIFO.Used >= Stack_Segment then
            if LIFO.Current.Next = null then
               LIFO.Current.Next := new Segment;
               LIFO.Current.Next.Previous := LIFO.Current;
               LIFO.Current.Next.Next := null;
            end if;
            LIFO.Current := LIFO.Current.Next;
            LIFO.Used := 1;
         else
            LIFO.Used := LIFO.Used + 1;
         end if;
         LIFO.Current.Data (LIFO.Used) := Item;
      end Push;

      procedure Pop (LIFO : in out Stack) is
      begin
         case LIFO.Used is
            when 0 =>
               raise Data_Error;
            when 1 =>
               if LIFO.Current.Previous = null then
                  LIFO.Used := 0;
               else
                  LIFO.Current := LIFO.Current.Previous;
                  LIFO.Used := Stack_Segment;
               end if;
            when others =>
               LIFO.Used := LIFO.Used - 1;
         end case;
      end Pop;

      function Top (LIFO : Stack) return Element is
      begin
         if LIFO.Used = 0 then
            raise Data_Error;
         else
            return LIFO.Current.Data (LIFO.Used);
         end if;
      end Top;

      function Is_Empty (LIFO : Stack) return Boolean is
      begin
         return LIFO.Used = 0;
      end Is_Empty;

      function Depth (LIFO : Stack) return Natural is
      begin
         return LIFO.Used;
      end Depth;

   end Generic_Stack;

   Short_Prefix      : Measures_Table_Of_Integer.Table;
   Long_Prefix       : Measures_Table_Of_Integer.Table;
   Short_Base_Units  : Measures_Table_Of_Measure.Table;
   Long_Base_Units   : Measures_Table_Of_Measure.Table;
   Other_Units       : Measures_Table_Of_Measure.Table;

   Prefix_Operations : Measures_Table_Of_Integer.Table;
   Dyadic_Operations : Measures_Table_Of_Integer.Table;

   function ">=" (Left, Right : Operation) return Boolean is
   begin
      return Operation'Pos (Left) <= Operation'Pos (Right);
   end ">=";

   function Is_Break
            (  Source  : String;
               Pointer : Integer;
               Mode    : Code_Set
            )  return Boolean is
   begin
      if Pointer not in Source'First + 1..Source'Last then
         return True;
      end if;
      declare
         Last : Character := Source (Pointer - 1);
         Next : constant Character := Source (Pointer);
      begin
         case Mode is
            when ASCII_Set =>
               return not (Is_Letter (Last) and then Is_Letter (Next));
            when Latin1_Set =>
               return
                  not
                  (  Is_Letter (Last)
                  and then
                     (Is_Letter (Next) or else '°' = Next)
                  );
            when UTF8_Set =>
               if not (Is_ISO_646 (Last) and then Is_Letter (Last)) then
                  return True;
               end if;
               if Is_ISO_646 (Next) and then Is_Letter (Next) then
                  return False;
               end if;
               if Pointer >= Source'Last then
                  return True;
               end if;
               Last := Source (Pointer + 1);
               if (  (Next = Ring (1)  and then Last = Ring (2))
                  or else
                     (Next = Omega (1) and then Last = Omega (2))
                  )
               then
                  return False;
               end if;
               if Pointer + 1 >= Source'Last then
                  return True;
               end if;
               if (  Source (Pointer..Pointer + 2) = Ohm_Sign
                  or else
                     Source (Pointer..Pointer + 2) = Kelvin_Sign
                  or else
                     Source (Pointer..Pointer + 2) = Degree_Celsius
                  )
               then
                  return False;
               end if;
               return True;
        end case;
     end;
   end Is_Break;
   --
   -- Get_SI_Unit -- Get a SI unit
   --
   --    Source   - The string
   --    Pointer  - The current position
   --    Prefixes - The SI prefix table
   --    Units    - The unit table
   --    Mode     - The current character set
   --    Result   - The measure corresponding to the input unit
   --    Got_It   - Result flag (true if success)
   --
   procedure Get_SI_Unit
             (  Source   : String;
                Pointer  : in out Integer;
                Prefixes : Measures_Table_Of_Integer.Table;
                Units    : Measures_Table_Of_Measure.Table;
                Mode     : Code_Set;
                Result   : out Measure;
                Got_It   : in out Boolean
             )  is
      Index     : Integer := Pointer;
      Reference : System.Address;
   begin
      --
      -- Try  to  recognize a base unit without prefix first. The reason
      -- is that prefixes may conflict with base units. As it is in  the
      -- case P (petta) vs. Pa. If prefix is recognized first,  then  Pa
      -- will never be matched.
      --
      begin
         Get (Source, Index, Units, Reference);
         if (  (  Mode = Latin1_Set
               or else
                  Reference /= L_Celsius'Address
               )
            and then
               (  Mode = UTF8_Set
               or else
                  (  Reference /= U_Celsius'Address
                  and then
                     Reference /= U_K'Address
                  and then
                     Reference /= U_Ohm'Address
               )  )
            and then
               Is_Break (Source, Index, Mode)
            )
         then
            Result  := Measure_Address.To_Pointer (Reference).all;
            Pointer := Index;
            Got_It  := True;
            return;
         end if;
      exception
         when End_Error => null;
      end;
      --
      -- Because no base unit is here. We can  try  to  match  a  prefix
      -- followed by a base unit.
      --
      declare
         Power : Integer := 0;
      begin
         Index := Pointer;
         Get (Source, Index, Prefixes, Power);
         if Power < -Latin1_Flag then
            if Power < -UTF8_Flag then
               if Mode = UTF8_Set then
                  Power := Power + UTF8_Flag;
               else
                  Got_It := False;
                  return;
               end if;
            else
               if Mode = Latin1_Set then
                  Power := Power + Latin1_Flag;
               else
                  Got_It := False;
                  return;
               end if;
            end if;
         end if;
         Get (Source, Index, Units, Reference);
         if (  (  Mode = Latin1_Set
               or else
                  Reference /= L_Celsius'Address
               )
            and then
               (  Mode = UTF8_Set
               or else
                  (  Reference /= U_Celsius'Address
                  and then
                     Reference /= U_K'Address
                  and then
                     Reference /= U_Ohm'Address
               )  )
            and then
               Is_Break (Source, Index, Mode)
            )
         then
            Result :=
               (  Measure_Address.To_Pointer (Reference).all
               *  10.0 ** Power
               );
            Pointer := Index;
            Got_It  := True;
            return;
         end if;
      exception
         when End_Error => null;
      end;
      Got_It := False;
   end Get_SI_Unit;
   --
   -- Get_Unit -- Get any unit or number
   --
   --    Source   - The string
   --    Pointer  - The current position
   --    Mode     - The current character set
   --    Result   - The measure corresponding to the input unit
   --    Got_It   - Result flag (true if success)
   --
   procedure Get_Unit
             (  Source  : String;
                Pointer : in out Integer;
                Mode    : Code_Set;
                Result  : out Measure;
                Got_It  : in out Boolean
             )  is
   begin
      --
      -- Try to get a SI unit (short forms)
      --
      Get_SI_Unit
      (  Source,
         Pointer,
         Short_Prefix,
         Short_Base_Units,
         Mode,
         Result,
         Got_It
      );
      if Got_It then
         return;
      end if;
      --
      -- Try to get a SI unit (full forms)
      --
      Get_SI_Unit
      (  Source,
         Pointer,
         Long_Prefix,
         Long_Base_Units,
         Mode,
         Result,
         Got_It
      );
      if Got_It then
         return;
      end if;
      --
      -- Try to get an irregular unit
      --
      declare
         Reference : System.Address;
         Index     : Integer := Pointer;
      begin
         Get (Source, Index, Other_Units, Reference);
         case Mode is
            when ASCII_Set =>
               if (  Reference = L_ang'Address
                  or else
                     Reference = L_degree'Address
                  or else
                     Reference = L_Fahrenheit'Address
                  or else
                     Reference = L_K'Address
                  or else
                     Reference = L_Celsius'Address
                  or else
                     Reference = U_ang'Address
                  or else
                     Reference = U_degree'Address
                  or else
                     Reference = U_Fahrenheit'Address
                  or else
                     Reference = U_K'Address
                  or else
                     Reference = U_Celsius'Address
                  or else
                     Reference = U_Ohm'Address
                  or else
                     Reference = U_Oz'Address
                  )
               then
                  Got_It := False;
                  return;
               end if;
            when Latin1_Set =>
               if (  Reference = U_ang'Address
                  or else
                     Reference = U_degree'Address
                  or else
                     Reference = U_Fahrenheit'Address
                  or else
                     Reference = U_K'Address
                  or else
                     Reference = U_Celsius'Address
                  or else
                     Reference = U_Ohm'Address
                  or else
                     Reference = U_Oz'Address
                  )
               then
                  Got_It := False;
                  return;
               end if;
            when UTF8_Set =>
               if (  Reference = L_ang'Address
                  or else
                     Reference = L_degree'Address
                  or else
                     Reference = L_Fahrenheit'Address
                  or else
                     Reference = L_K'Address
                  or else
                     Reference = L_Celsius'Address
                  )
               then
                  Got_It := False;
                  return;
               end if;
         end case;
         if Is_Break (Source, Index, Mode) then
            Result  := Measure_Address.To_Pointer (Reference).all;
            Pointer := Index;
            Got_It  := True;
            return;
         end if;
      exception
         when End_Error => null;
      end;
      Got_It := False;
   end Get_Unit;
--
-- Expression_State -- Expression parsing state
--
--    Start   - Before the first operand
--    Numeric - The first numeric operand before any diadic operation
--    Canonic - Numeral * Expression representation
--    Mixed   - Nothing from above
--
   type Expression_State is (Start, Numeric, Canonic, Mixed);

   package Argument_Stack  is new Generic_Stack (Measure);
   package Operation_Stack is new Generic_Stack (Operation);

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Scaled;
                Mode    : Code_Set
             )  is
      use Argument_Stack;
      use Operation_Stack;

      State         : Expression_State := Start;
      Dimensionless : Boolean          := True;
      Numeral       : Number'Base;
      AS            : aliased Argument_Stack.Stack;
      OS            : aliased Operation_Stack.Stack;

      Index   : Integer := Pointer;

      procedure Do_Power (Left : in out Measure; Power : Integer) is
      begin
         if Is_Empty (AS) then
            case State is
               when Numeric =>
                  Numeral := Numeral ** Power;
               when Canonic =>
                  Left  := (Left * Numeral) ** Power;
                  State := Mixed;
               when others =>
                  Left := Left ** Power;
            end case;
         else
            Left := Left ** Power;
         end if;
      end Do_Power;
      --
      -- Execute -- Execute an operation
      --
      --    Operator - To be executed
      --
      procedure Execute (Operator : Operation) is
         Left  : Measure;
         Right : Measure;
      begin
         case Operator is
            when Power =>
               Right := Top (AS); Pop (AS);
               Left  := Top (AS); Pop (AS);
               if (  Right.SI = Units.Base.Unitless
                  and
                     Right.Offset = 0.0
                  and
                     Right.Gain = Number'Floor (Right.Gain)
                  )
               then
                  Do_Power (Left, Integer (Number'Floor (Right.Gain)));
               else
                  raise Unit_Error;
               end if;
            when Divide =>
               Right := Top (AS); Pop (AS);
               Left  := Top (AS); Pop (AS);
               Left := Left / Right;
               if Is_Empty (AS) and then State = Numeric then
                  State := Canonic;
               end if;
            when Multiply | Default_Multiply =>
               Right := Top (AS); Pop (AS);
               Left  := Top (AS); Pop (AS);
               Left  := Left * Right;
               if Is_Empty (AS) and then State = Numeric then
                  State := Canonic;
               end if;
            when Plus =>
               Right := Top (AS); Pop (AS);
               Left  := Top (AS); Pop (AS);
               if Is_Empty (AS) then
                  case State is
                     when Numeric =>
                        Left      := Left * Numeral + Right;
                        Numeral   := Left.Gain;
                        Left.Gain := 1.0;
                     when Canonic =>
                        Left  := Left * Numeral + Right;
                        State := Mixed;
                     when others =>
                        Left := Left + Right;
                  end case;
               else
                  Left := Left + Right;
               end if;
            when Minus =>
               Right := Top (AS); Pop (AS);
               Left  := Top (AS); Pop (AS);
               if Is_Empty (AS) then
                  case State is
                     when Numeric =>
                        Left      := Left * Numeral - Right;
                        Numeral   := Left.Gain;
                        Left.Gain := 1.0;
                     when Canonic =>
                        Left  := Left * Numeral - Right;
                        State := Mixed;
                     when others =>
                        Left := Left - Right;
                  end case;
               else
                  Left := Left - Right;
               end if;
            when Unary_Minus =>
               Left := Top (AS); Pop (AS);
               if Is_Empty (AS) and then State in Numeric..Canonic then
                  Numeral := -Numeral;
               else
                  Left := -Left;
               end if;
            when Shift =>
               Right := Top (AS); Pop (AS);
               Left  := Top (AS); Pop (AS);
               if (  Right.SI /= Units.Base.Unitless
                  or
                     Right.Offset /= 0.0
                  )
               then
                  raise Unit_Error;
               end if;
               if Is_Empty (AS) and then State in Numeric..Canonic then
                  State := Mixed;
                  Left  := Left * Numeral and Right.Gain;
               else
                  Left := Left and Right.Gain;
               end if;
               Dimensionless := False;
            when others =>
               return;
         end case;
         Push (AS, Left);
      end Execute;
      --
      -- Unload -- Unload the operation stack
      --
      --    Right - The operation that unloads stack
      --
      procedure Unload (Right : Operation) is
      begin
         while not Is_Empty (OS) and then Top (OS) >= Right loop
            Execute (Top (OS)); Pop (OS);
         end loop;
      end Unload;
      --
      -- Get_Operation -- Gets an operation from the string
      --
      --    Folder - The operation table
      --    Mode   - Prefix, Dyadic, Postfix
      --
      -- On  successful  completion  the  operation  is  pushed onto the
      -- operation stack.
      --
      -- Returns :
      --
      --    True  - Success
      --    False - Failure
      --
      function Get_Operation
               (  Folder : Measures_Table_Of_Integer.Table;
                  Mode   : Affinity
               )  return Boolean is
         Priority : Integer;
         Position : Integer := Index;
      begin
         --
         -- Get the name
         --
         Get (Source, Position, Folder, Priority);
         if Priority >= Latin1_Flag then
            if Priority >= UTF8_Flag then
               if Get.Mode = UTF8_Set then
                  Priority := Priority - UTF8_Flag;
               else
                  return False;
               end if;
            else
               if Get.Mode = Latin1_Set then
                  Priority := Priority - Latin1_Flag;
               else
                  return False;
               end if;
            end if;
         end if;
         if not Is_Break (Source, Position, Get.Mode) then
            return False;
         end if;
         if Operation'Val (Priority) = Right_Bracket then
            --
            -- This is a right bracket
            --
            Unload (Right_Bracket);
            if Is_Empty (OS) or else Top (OS) /= Left_Bracket then
               return False;
            end if;
            Pop (OS);
         else
            --
            -- This is some other operation
            --
            if Mode = Dyadic then
               Unload (Operation'Val (Priority));
            end if;
            Push (OS, Operation'Val (Priority));
         end if;
         Index := Position;
         return True;
      exception
         when End_Error => return False;
      end Get_Operation;
      --
      -- Get_Postfix -- Gets subscript power
      --
      -- On  successful  completion  the  operation  is  pushed onto the
      -- operation stack.
      --
      -- Returns :
      --
      --    True  - Success
      --    False - Failure
      --
      function Get_Postfix return Boolean is
         Power : Integer;
      begin
         if Index > Source'Last then
            return False;
         end if;
         if Source (Index) = ')' then
            Unload (Right_Bracket);
            if Is_Empty (OS) or else Top (OS) /= Left_Bracket then
               return False;
            end if;
            Pop (OS);
            if Depth (AS) = 1 and then State = Canonic then
               declare
                  Left : constant Measure := Top (AS) * Numeral;
               begin
                  Pop (AS);
                  if Dimensionless then
                     State   := Numeric;
                     Numeral := Left.Gain;
                     Push (AS, Np);
                  else
                     State := Mixed;
                     Push (AS, Left);
                  end if;
               end;
            end if;
            Index := Index + 1;
         else
            case Mode is
               when ASCII_Set =>
                  return False;
               when Latin1_Set =>
                  case Source (Index) is
                     when '¹' => Power := 1;
                     when '²' => Power := 2;
                     when '³' => Power := 3;
                     when others =>
                        return False;
                  end case;
                  Index := Index + 1;
               when UTF8_Set =>
                  begin
                     Get_Superscript (Source, Index, Power, Mode);
                  exception
                     when End_Error | Data_Error =>
                        return False;
                  end;
            end case;
            declare
               Left : Measure := Top (AS);
            begin
               Pop (AS);
               Do_Power (Left, Power);
               Push (AS, Left);
            end;
         end if;
         return True;
      end Get_Postfix;
      --
      -- Get_Operand -- Get an operand
      --
      -- On  successful  completion  the  operand  is  pushed  onto  the
      -- argument stack.
      --
      -- Returns :
      --
      --    True  - Success
      --    False - Failure
      --
      function Get_Operand return Boolean is
      begin
         --
         -- Try to get a floating-point number
         --
         declare
            Value : Number'Base;
         begin
            Get (Source, Index, Value);
            if State = Start then
               --
               -- The first ever operand is a plain number.  Let's  take
               -- it  as the numeral and start evaluating the scale from
               -- the value 1.0.
               --
               Numeral := Value;
               State   := Numeric;
               Push (AS, Np);
            else
               Push (AS, Value * Np);
            end if;
            return True;
         exception
            when End_Error => null;
         end;
         --
         -- Try to get a unit
         --
         declare
            Value  : Measure;
            Got_It : Boolean := False;
         begin
            Get_Unit (Source, Index, Mode, Value, Got_It);
            if Got_It then
               Dimensionless := False;
               Push (AS, Value);
               if State = Start then
                  --
                  -- The first operand is not a number. This cannot be a
                  -- canonic representation.
                  --
                  State := Mixed;
               end if;
               return True;
            else
               return False;
            end if;
         end;
      end Get_Operand;
   begin
      Init (AS);
      Init (OS);
      if (  Index < Source'First
         or else
            (  Index > Source'Last
            and then
               Index - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      loop
         --
         -- Get prefix operations
         --
         Get (Source, Index, SpaceAndTab);
         while Get_Operation (Prefix_Operations, Prefix) loop
            Get (Source, Index, SpaceAndTab);
         end loop;
         --
         -- Get an operand
         --
         Get (Source, Index, SpaceAndTab);
         exit when not Get_Operand;
         --
         -- Get postfix operations
         --
         Get (Source, Index, SpaceAndTab);
         while Get_Postfix loop
            Get (Source, Index, SpaceAndTab);
         end loop;
         --
         -- Get a dyadic operation
         --
         Get (Source, Index, SpaceAndTab);
         if not Get_Operation (Dyadic_Operations, Dyadic) then
            --
            -- Assume a multiplication here
            --
            Unload (Default_Multiply);
            Push (OS, Default_Multiply);
         end if;
      end loop;
      if Is_Empty (AS) then
         raise End_Error;
      end if;
      if Is_Empty (OS) or else Top (OS) /= Default_Multiply then
         raise Data_Error;
      end if;
      Pop (OS);
      Unload (Right_Bracket);
      declare
         Result : constant Measure := Top (AS);
      begin
         Pop (AS);
         if not Is_Empty (OS) or not Is_Empty (AS) then
            raise Data_Error;
         end if;
         if not (  Result.Gain'Valid
                and then
                   (  State = Mixed
                   or else
                      Numeral'Valid
                )  )
         then
            raise Constraint_Error;
         end if;
         Destroy (OS);
         Destroy (AS);
         Get (Source, Index, SpaceAndTab);
         Pointer   := Index;
         case State is
            when Numeric =>
               Value := (Scalar, Numeral, Np);
            when Canonic =>
               if Dimensionless then
                  Value := (Numeric, Numeral, Result);
               else
                  Value := (Canonic, Numeral, Result);
               end if;
            when others =>
               if Dimensionless then
                  Value := (Scalar, Result.Gain, Np);
               else
                  Value := (Jumbled, 1.0, Result);
               end if;
         end case;
      end;
   exception
      when others =>
         Destroy (OS);
         Destroy (AS);
         raise;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure;
                Mode    : Code_Set
             )  is
      Result : Scaled;
   begin
      Get (Source, Pointer, Result, Mode);
      Value := Result.Numeral * Result.Scale;
      if not Value.Gain'Valid then
         raise Constraint_Error;
      end if;
   end Get;

   procedure Get_Unit
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure;
                Mode    : Code_Set
             )  is
      Got_It : Boolean := False;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      Get_Unit (Source, Pointer, Mode, Value, Got_It);
      if not Got_It then
         raise End_Error;
      end if;
   end Get_Unit;

   function Value (Source : String; Mode : Code_Set) return Measure is
      Result  : Measure;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, Result, Mode);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   type Triplet is range -8..8;
   Celsius : constant String :=
                Character'Val (16#C2#) & Character'Val (16#B0#) & 'C';

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Measure;
                Mode        : Code_Set;
                Derived     : Boolean   := True;
                RelSmall    : Positive  := MaxSmall;
                AbsSmall    : Integer   :=-MaxSmall;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Gain   : Number  := abs Value.Gain;
      Offset : Number  := Value.Offset;
      Small  : Integer := AbsSmall;
      Power  : Triplet := 0;
   --
   -- Put_Unit -- Put unit, gain and offset
   --
   --    Name       - The SI unit name (like km/s)
   --    Use_Prefix - Use SI prefix to reduce gain
   --
   -- This procedure is called to complete execution of Put. It  outputs
   -- [<gain> *] <name> [and <offset>].
   --
      procedure Put_Unit
                (  Name       : String;
                   Use_Prefix : Boolean := True
                )  is
         function Do_Name (Name : String) return String is
         begin
            case Power is
               when  8 => return "Y" & Name;
               when  7 => return "Z" & Name;
               when  6 => return "E" & Name;
               when  5 => return "P" & Name;
               when  4 => return "T" & Name;
               when  3 => return "G" & Name;
               when  2 => return "M" & Name;
               when  1 => return "k" & Name;
               when  0 => return Name;
               when -1 => return "m" & Name;
               when -2 =>
                  case Mode is
                     when ASCII_Set =>
                        Gain := Gain * 1000.0;
                        return 'n' & Name;
                     when Latin1_Set =>
                        return Character'Val (16#B5#) & Name;
                     when UTF8_Set =>
                        return
                        (  Character'Val (16#C2#)
                        &  Character'Val (16#B5#)
                        &  Name
                        );
                  end case;
               when -3 => return "n" & Name;
               when -4 => return "p" & Name;
               when -5 => return "f" & Name;
               when -6 => return "a" & Name;
               when -7 => return "z" & Name;
               when -8 => return "y" & Name;
            end case;
         end Do_Name;

         function Do_Base (Name : String) return String is
         begin
            if 1.0 = Gain then
               return Do_Name (Name);
            elsif Name = "1" then
               return
                  Image
                  (  Value.Gain,
                     AbsSmall => AbsSmall,
                     RelSmall => RelSmall
                  );
            elsif Name (Name'First) = '1' then
               return
               (  Image
                  (  Value.Gain,
                     AbsSmall => AbsSmall,
                     RelSmall => RelSmall
                  )
               &  Do_Name (Name (Name'First + 1..Name'Last))
               );
            else
               case Mode is
                  when ASCII_Set =>
                     return
                     (  Image
                        (  Gain,
                           AbsSmall => Small - Integer (Power * 3),
                           RelSmall => RelSmall
                        )
                     &  '*'
                     &  Do_Name (Name)
                     );
                  when Latin1_Set =>
                     return
                     (  Image
                        (  Gain,
                           AbsSmall => Small - Integer (Power * 3),
                           RelSmall => RelSmall
                        )
                     &  '·'
                     &  Do_Name (Name)
                     );
                  when UTF8_Set =>
                     return
                     (  Image
                        (  Gain,
                           AbsSmall => Small - Integer (Power * 3),
                           RelSmall => RelSmall
                        )
                     &  Dot
                     &  Do_Name (Name)
                     );
               end case;
            end if;
         end Do_Base;
      begin
         if Use_Prefix then
            if Gain >= 1.0 then
               while (  Power < Triplet'Last
                     and then
                        Small /= Integer (Power) * 3
                     and then
                        Gain >= 1000.0
                     )
               loop
                  Gain  := Gain / 1000.0;
                  Power := Power + 1;
               end loop;
            elsif Gain /= 0.0 then
               while (  Power > Triplet'First
                     and then
                        Small < Integer (Power) * 3
                     and then
                        Gain < 1.0
                     )
               loop
                  Gain  := Gain * 1000.0;
                  Power := Power - 1;
               end loop;
            end if;
         end if;
         if Value.Gain < 0.0 then
            Gain := -Gain;
         end if;
         if 0.0 = Offset then
            Put
            (  Destination,
               Pointer,
               Do_Base (Name),
               Field,
               Justify,
               Fill
            );
         else
            Put
            (  Destination,
               Pointer,
               (  Do_Base (Name)
               &  " and "
               &  Image
                  (  Offset,
                     AbsSmall => Small,
                     RelSmall => RelSmall
               )  ),
               Field,
               Justify,
               Fill
            );
         end if;
      end Put_Unit;
   begin
      if Derived then
         if    Value.SI = C.SI     then Put_Unit ("C"  ); return;
         elsif Value.SI = F.SI     then Put_Unit ("F"  ); return;
         elsif Value.SI = Gy.SI    then Put_Unit ("Gy" ); return;
         elsif Value.SI = Henry.SI then Put_Unit ("H"  ); return;
         elsif Value.SI = Hz.SI    then Put_Unit ("Hz" ); return;
         elsif Value.SI = J.SI     then Put_Unit ("J"  ); return;
         elsif Value.SI = N.SI     then Put_Unit ("N"  ); return;
         elsif Value.SI = Pa.SI    then Put_Unit ("Pa" ); return;
         elsif Value.SI = Tesla.SI then Put_Unit ("T"  ); return;
         elsif Value.SI = V.SI     then Put_Unit ("V"  ); return;
         elsif Value.SI = W.SI     then Put_Unit ("W"  ); return;
         elsif Value.SI = Wb.SI    then Put_Unit ("Wb" ); return;
         elsif Value.SI = lm.SI    then Put_Unit ("lm" ); return;
         elsif Value.SI = lx.SI    then Put_Unit ("lx" ); return;
         elsif Value.SI = Ohm.SI then
            if Mode = UTF8_Set then
               Put_Unit (Omega);
            else
               Put_Unit ("ohm");
            end if;
            return;
         elsif (  Mode /= ASCII_Set
               and then
                  Value.SI = Units.Base.Temperature
               and then
                  Offset = 273.15
               )  then
            Offset := 0.0;
            if Mode = UTF8_Set then
               Put_Unit (Celsius);
            else
               Put_Unit ("°C");
            end if;
            return;
         end if;
      end if;
      declare
         Unit : constant String := Image (Value.SI, Mode);
      begin
         if Is_Prefix ("kg", Unit) then
            Power := 1;
            Small := Small + 3;
            Put_Unit (Unit (Unit'First + 1..Unit'Last));
         else
            declare
               Powers : array (1..7) of Natural;
            begin
               Split
               (  Value.SI,
                  Powers (1),
                  Powers (2),
                  Powers (3),
                  Powers (4),
                  Powers (5),
                  Powers (6),
                  Powers (7)
               );
               for Index in Powers'Range loop
                  case Powers (Index) is
                     when 0 =>
                        null;
                     when 1 =>
                        Put_Unit (Unit);
                        return;
                     when others =>
                        Power := 0; -- Disable prefix
                        Put_Unit (Unit, False);
                        return;
                  end case;
               end loop;
               Put_Unit (Unit);
            end;
         end if;
      end;
   end Put;

   function Image
            (  Value    : Measure;
               Mode     : Code_Set;
               Derived  : Boolean  := True;
               RelSmall : Positive := MaxSmall;
               AbsSmall : Integer  :=-MaxSmall
            )  return String is
      Text    : String (1..256);
      Pointer : Integer := Text'First;
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Value,
         Mode        => Mode,
         Derived     => Derived,
         RelSmall    => RelSmall,
         AbsSmall    => AbsSmall
      );
      return Text (Text'First..Pointer - 1);
   end Image;

begin
   Add (Short_Prefix, "Q",  30);
   Add (Short_Prefix, "R",  27);
   Add (Short_Prefix, "Y",  24);
   Add (Short_Prefix, "Z",  21);
   Add (Short_Prefix, "E",  18);
   Add (Short_Prefix, "P",  15);
   Add (Short_Prefix, "T",  12);
   Add (Short_Prefix, "G",   9);
   Add (Short_Prefix, "M",   6);
   Add (Short_Prefix, "k",   3);
   Add (Short_Prefix, "h",   2);
   Add (Short_Prefix, "da",  1);
   Add (Short_Prefix, "d",  -1);
   Add (Short_Prefix, "c",  -2);
   Add (Short_Prefix, "m",  -3);
   Add (Short_Prefix, "n",  -9);
   Add (Short_Prefix, "p", -12);
   Add (Short_Prefix, "f", -15);
   Add (Short_Prefix, "a", -18);
   Add (Short_Prefix, "z", -21);
   Add (Short_Prefix, "y", -24);
   Add (Short_Prefix, "r", -27);
   Add (Short_Prefix, "q", -30);
--
-- Mu (power -6) is handled in a special way
--
   Add (Short_Prefix, "µ",  -6 - Latin1_Flag);
   Add (Short_Prefix, Mu,   -6 - UTF8_Flag);
   Add (Short_Prefix, Mu_L, -6 - UTF8_Flag);

   Add (Long_Prefix, "quetta", 30);
   Add (Long_Prefix, "ronna",  27);
   Add (Long_Prefix, "yotta",  24);
   Add (Long_Prefix, "zetta",  21);
   Add (Long_Prefix, "exa",    18);
   Add (Long_Prefix, "petta",  15);
   Add (Long_Prefix, "tera",   12);
   Add (Long_Prefix, "giga",    9);
   Add (Long_Prefix, "mega",    6);
   Add (Long_Prefix, "kilo",    3);
   Add (Long_Prefix, "hecto",   2);
   Add (Long_Prefix, "deka",    1);
   Add (Long_Prefix, "deci",   -1);
   Add (Long_Prefix, "centi",  -2);
   Add (Long_Prefix, "milli",  -3);
   Add (Long_Prefix, "micro",  -6);
   Add (Long_Prefix, "nano",   -9);
   Add (Long_Prefix, "pico",  -12);
   Add (Long_Prefix, "femto", -15);
   Add (Long_Prefix, "atto",  -18);
   Add (Long_Prefix, "zepto", -21);
   Add (Long_Prefix, "yocto", -24);
   Add (Long_Prefix, "ronto", -27);
   Add (Long_Prefix, "quecto", -30);

   Add (Short_Base_Units, "A",   A'Address      );
   Add (Short_Base_Units, "B",   B'Address      );
   Add (Short_Base_Units, "Bq",  Bq'Address     );
   Add (Short_Base_Units, "C",   C'Address      );
   Add (Short_Base_Units, "Ci",  Ci'Address     );
   Add (Short_Base_Units, "F",   F'Address      );
   Add (Short_Base_Units, "G",   G'Address      );
   Add (Short_Base_Units, "Gy",  Gy'Address     );
   Add (Short_Base_Units, "H",   Henry'Address  );
   Add (Short_Base_Units, "Hz",  Hz'Address     );
   Add (Short_Base_Units, "J",   J'Address      );
   Add (Short_Base_Units, "K",   K'Address      );
   Add (Short_Base_Units, "L",   L'Address      );
   Add (Short_Base_Units, "N",   N'Address      );
   Add (Short_Base_Units, "Pa",  Pa'Address     );
   Add (Short_Base_Units, "R",   R'Address      );
   Add (Short_Base_Units, "S",   Siemens'Address);
   Add (Short_Base_Units, "Sv",  Sv'Address     );
   Add (Short_Base_Units, "T",   Tesla'Address  );
   Add (Short_Base_Units, "V",   V'Address      );
   Add (Short_Base_Units, "W",   W'Address      );
   Add (Short_Base_Units, "Wb",  Wb'Address     );
   Add (Short_Base_Units, "b",   barn'Address   );
   Add (Short_Base_Units, "cd",  cd'Address     );
   Add (Short_Base_Units, "erg", erg'Address    );
   Add (Short_Base_Units, "g",   gram'Address   );
   Add (Short_Base_Units, "kat", kat'Address    );
   Add (Short_Base_Units, "l",   L'Address      );
   Add (Short_Base_Units, "lm",  lm'Address     );
   Add (Short_Base_Units, "lx",  lx'Address     );
   Add (Short_Base_Units, "m",   m'Address      );
   Add (Short_Base_Units, "mol", mol'Address    );
   Add (Short_Base_Units, "rad", rad'Address    );
   Add (Short_Base_Units, "s",   s'Address      );
   Add (Short_Base_Units, "sr",  sr'Address     );
   Add (Short_Base_Units, "t",   t'Address      );
   --
   -- °C is allowed to be used with SI prefixes
   --
   Add (Short_Base_Units, "°C",           L_Celsius'Address);
   Add (Short_Base_Units, Ring & "C",     U_Celsius'Address);
   Add (Short_Base_Units, Degree_Celsius, U_Celsius'Address);
   Add (Short_Base_Units, Kelvin_Sign,    U_K'Address      );
   Add (Short_Base_Units, Ohm_Sign,       U_Ohm'Address    );
   Add (Short_Base_Units, Omega,          U_Ohm'Address    );

   Add (Long_Base_Units, "ampere",    A'Address      );
   Add (Long_Base_Units, "bar",       bar'Address    );
   Add (Long_Base_Units, "barn",      barn'Address   );
   Add (Long_Base_Units, "becquerel", Bq'Address     );
   Add (Long_Base_Units, "bel",       B'Address      );
   Add (Long_Base_Units, "candela",   cd'Address     );
   Add (Long_Base_Units, "coulomb",   C'Address      );
   Add (Long_Base_Units, "curie",     Ci'Address     );
   Add (Long_Base_Units, "erg",       erg'Address    );
   Add (Long_Base_Units, "farad",     F'Address      );
   Add (Long_Base_Units, "gauss",     G'Address      );
   Add (Long_Base_Units, "gram",      gram'Address   );
   Add (Long_Base_Units, "gramme",    gram'Address   );
   Add (Long_Base_Units, "grammes",   gram'Address   );
   Add (Long_Base_Units, "grams",     gram'Address   );
   Add (Long_Base_Units, "gray",      Gy'Address     );
   Add (Long_Base_Units, "henry",     Henry'Address  );
   Add (Long_Base_Units, "hertz",     Hz'Address     );
   Add (Long_Base_Units, "joule",     J'Address      );
   Add (Long_Base_Units, "katal",     kat'Address    );
   Add (Long_Base_Units, "kelvin",    K'Address      );
   Add (Long_Base_Units, "liter",     L'Address      );
   Add (Long_Base_Units, "liters",    L'Address      );
   Add (Long_Base_Units, "litre",     L'Address      );
   Add (Long_Base_Units, "litres",    L'Address      );
   Add (Long_Base_Units, "lumen",     lm'Address     );
   Add (Long_Base_Units, "lux",       lx'Address     );
   Add (Long_Base_Units, "meter",     m'Address      );
   Add (Long_Base_Units, "meters",    m'Address      );
   Add (Long_Base_Units, "metre",     m'Address      );
   Add (Long_Base_Units, "metres",    m'Address      );
   Add (Long_Base_Units, "mole",      mol'Address    );
   Add (Long_Base_Units, "newton",    N'Address      );
   Add (Long_Base_Units, "ohm",       Ohm'Address    );
   Add (Long_Base_Units, "Ohm",       Ohm'Address    );
   Add (Long_Base_Units, "pascal",    Pa'Address     );
   Add (Long_Base_Units, "radian",    rad'Address    );
   Add (Long_Base_Units, "roentgen",  R'Address      );
   Add (Long_Base_Units, "second",    s'Address      );
   Add (Long_Base_Units, "seconds",   s'Address      );
   Add (Long_Base_Units, "siemens",   Siemens'Address);
   Add (Long_Base_Units, "sievert",   Sv'Address     );
   Add (Long_Base_Units, "steradian", sr'Address     );
   Add (Long_Base_Units, "tesla",     Tesla'Address  );
   Add (Long_Base_Units, "ton",       t'Address      );
   Add (Long_Base_Units, "tonne",     t'Address      );
   Add (Long_Base_Units, "tonnes",    t'Address      );
   Add (Long_Base_Units, "tons",      t'Address      );
   Add (Long_Base_Units, "volt",      V'Address      );
   Add (Long_Base_Units, "watt",      W'Address      );
   Add (Long_Base_Units, "weber",     Wb'Address     );

   Add (Other_Units, "%",              percent'Address   );
   Add (Other_Units, "'",              min_of_arc'Address);
   Add (Other_Units, """",             sec_of_arc'Address);
   Add (Other_Units, "BTU",            BTU'Address       );
   Add (Other_Units, "Btu",            BTU'Address       );
   Add (Other_Units, "Celsius",        Celsius'Address   );
   Add (Other_Units, "Fahrenheit",     Fahrenheit'Address);
   Add (Other_Units, "INM",            INM'Address       );
   Add (Other_Units, "Kcal",           kcal'Address      );
   Add (Other_Units, "kcal",           kcal'Address      );
   Add (Other_Units, "Kelvin",         K'Address         );
   Add (Other_Units, "Oe",             Oe'Address        );
   Add (Other_Units, "PSI",            psi'Address       );
   Add (Other_Units, "a.",             acre'Address      );
   Add (Other_Units, "acre",           acre'Address      );
   Add (Other_Units, "acres",          acre'Address      );
   Add (Other_Units, "are",            are'Address       );
   Add (Other_Units, "ares",           are'Address       );
   Add (Other_Units, "atm",            atm'Address       );
   Add (Other_Units, "atmosphere",     atm'Address       );
   Add (Other_Units, "atmospheres",    atm'Address       );
   Add (Other_Units, "barleycorn",     barleycorn'Address);
   Add (Other_Units, "barleycorns",    barleycorn'Address);
   Add (Other_Units, "barrel",         barrel'Address    );
   Add (Other_Units, "barrels",        barrel'Address    );
   Add (Other_Units, "bbl",            barrel'Address    );
   Add (Other_Units, "btu",            BTU'Address       );
   Add (Other_Units, "cal",            cal'Address       );
   Add (Other_Units, "calorie",        cal'Address       );
   Add (Other_Units, "calories",       cal'Address       );
   Add (Other_Units, "carat",          carat'Address     );
   Add (Other_Units, "carats",         carat'Address     );
   Add (Other_Units, "ch",             ch'Address        );
   Add (Other_Units, "chain",          ch'Address        );
   Add (Other_Units, "chains",         ch'Address        );
   Add (Other_Units, "cubit",          cubit'Address     );
   Add (Other_Units, "cubits",         cubit'Address     );
   Add (Other_Units, "d",              d'Address         );
   Add (Other_Units, "day",            d'Address         );
   Add (Other_Units, "days",           d'Address         );
   Add (Other_Units, "degree",         degree'Address    );
   Add (Other_Units, "degrees",        degree'Address    );
   Add (Other_Units, "dr",             dram'Address      );
   Add (Other_Units, "dram",           dram'Address      );
   Add (Other_Units, "drams",          dram'Address      );
   Add (Other_Units, "dyn",            dyn'Address       );
   Add (Other_Units, "dyne",           dyn'Address       );
   Add (Other_Units, "eV",             eV'Address        );
   Add (Other_Units, "ell",            ell'Address       );
   Add (Other_Units, "ells",           ell'Address       );
   Add (Other_Units, "f",              fathom'Address    );
   Add (Other_Units, "fathom",         fathom'Address    );
   Add (Other_Units, "fathoms",        fathom'Address    );
   Add (Other_Units, "feet",           ft'Address        );
   Add (Other_Units, "finger",         finger'Address    );
   Add (Other_Units, "fingers",        finger'Address    );
   Add (Other_Units, "foot",           ft'Address        );
   Add (Other_Units, "fpm",            fpm'Address       );
   Add (Other_Units, "fps",            fps'Address       );
   Add (Other_Units, "fur",            fur'Address       );
   Add (Other_Units, "furlong",        fur'Address       );
   Add (Other_Units, "furlongs",       fur'Address       );
   Add (Other_Units, "gal",            gal'Address       );
   Add (Other_Units, "gallon",         gal'Address       );
   Add (Other_Units, "gallons",        gal'Address       );
   Add (Other_Units, "gi",             gi'Address        );
   Add (Other_Units, "gill",           gi'Address        );
   Add (Other_Units, "gills",          gi'Address        );
   Add (Other_Units, "grain",          grain'Address     );
   Add (Other_Units, "grains",         grain'Address     );
   Add (Other_Units, "h",              h'Address         );
   Add (Other_Units, "hand",           hand'Address      );
   Add (Other_Units, "hands",          hand'Address      );
   Add (Other_Units, "hectare",        hectare'Address   );
   Add (Other_Units, "hectares",       hectare'Address   );
   Add (Other_Units, "horsepower",     hp'Address        );
   Add (Other_Units, "hour",           h'Address         );
   Add (Other_Units, "hours",          h'Address         );
   Add (Other_Units, "hp",             hp'Address        );
   Add (Other_Units, "in.",            inch'Address      );
   Add (Other_Units, "inch",           inch'Address      );
   Add (Other_Units, "inches",         inch'Address      );
   Add (Other_Units, "kgf",            kgf'Address       );
   Add (Other_Units, "kilogram-force", kgf'Address       );
   Add (Other_Units, "knot",           knot'Address      );
   Add (Other_Units, "knots",          knot'Address      );
   Add (Other_Units, "lb",             lb'Address        );
   Add (Other_Units, "league",         league'Address    );
   Add (Other_Units, "leagues",        league'Address    );
   Add (Other_Units, "lightyear",      ly'Address        );
   Add (Other_Units, "lightyears",     ly'Address        );
   Add (Other_Units, "line",           line'Address      );
   Add (Other_Units, "lines",          line'Address      );
   Add (Other_Units, "link",           link'Address      );
   Add (Other_Units, "links",          link'Address      );
   Add (Other_Units, "liqpt",          pt'Address        );
   Add (Other_Units, "liquidpint",     pt'Address        );
   Add (Other_Units, "ly",             ly'Address        );
   Add (Other_Units, "mi",             mi'Address        );
   Add (Other_Units, "mile",           mi'Address        );
   Add (Other_Units, "miles",          mi'Address        );
   Add (Other_Units, "min",            min'Address       );
   Add (Other_Units, "minute",         min'Address       );
   Add (Other_Units, "minutes",        min'Address       );
   Add (Other_Units, "mmHg",           torr'Address      );
   Add (Other_Units, "mpg",            mpg'Address       );
   Add (Other_Units, "mph",            mph'Address       );
   Add (Other_Units, "mps",            mps'Address       );
   Add (Other_Units, "nail",           nail'Address      );
   Add (Other_Units, "nails",          nail'Address      );
   Add (Other_Units, "oersted",        Oe'Address        );
   Add (Other_Units, "ounce",          oz'Address        );
   Add (Other_Units, "ounces",         oz'Address        );
   Add (Other_Units, "oz",             oz'Address        );
   Add (Other_Units, "pace",           pace'Address      );
   Add (Other_Units, "paces",          pace'Address      );
   Add (Other_Units, "parsec",         pc'Address        );
   Add (Other_Units, "parsecs",        pc'Address        );
   Add (Other_Units, "pc",             pc'Address        );
   Add (Other_Units, "pint",           pt'Address        );
   Add (Other_Units, "pints",          pt'Address        );
   Add (Other_Units, "point",          point'Address     );
   Add (Other_Units, "points",         point'Address     );
   Add (Other_Units, "pound",          lb'Address        );
   Add (Other_Units, "pounds",         lb'Address        );
   Add (Other_Units, "ppb",            ppb'Address       );
   Add (Other_Units, "ppm",            ppm'Address       );
   Add (Other_Units, "ppt",            ppt'Address       );
   Add (Other_Units, "psi",            psi'Address       );
   Add (Other_Units, "pt",             pt'Address        );
   Add (Other_Units, "qt",             qt'Address        );
   Add (Other_Units, "quart",          qt'Address        );
   Add (Other_Units, "quarts",         qt'Address        );
   Add (Other_Units, "rd",             rd'Address        );
   Add (Other_Units, "rod",            rd'Address        );
   Add (Other_Units, "rods",           rd'Address        );
   Add (Other_Units, "rood",           rood'Address      );
   Add (Other_Units, "roods",          rood'Address      );
   Add (Other_Units, "rpm",            rpm'Address       );
   Add (Other_Units, "rps",            rps'Address       );
   Add (Other_Units, "sec",            s'Address         );
   Add (Other_Units, "span",           span'Address      );
   Add (Other_Units, "spans",          span'Address      );
   Add (Other_Units, "tablespoon",     tablespoon'Address);
   Add (Other_Units, "tablespoons",    tablespoon'Address);
   Add (Other_Units, "teaspoon",       teaspoon'Address  );
   Add (Other_Units, "teaspoons",      teaspoon'Address  );
   Add (Other_Units, "torr",           torr'Address      );
   Add (Other_Units, "township",       township'Address  );
   Add (Other_Units, "townships",      township'Address  );
   Add (Other_Units, "u",              u'Address         );
   Add (Other_Units, "ua",             ua'Address        );
   Add (Other_Units, "wineglass",      wineglass'Address );
   Add (Other_Units, "wineglasses",    wineglass'Address );
   Add (Other_Units, "yard",           yd'Address        );
   Add (Other_Units, "yards",          yd'Address        );
   Add (Other_Units, "yd",             yd'Address        );
   Add (Other_Units, "year",           year'Address      );
   Add (Other_Units, "years",          year'Address      );
   --
   -- Latin1 part
   --
   Add (Other_Units, "ångström",       L_ang'Address       );
   Add (Other_Units, "Å",              L_ang'Address       );
   Add (Other_Units, "Ångström",       L_ang'Address       );
   Add (Other_Units, "°",              L_degree'Address    );
   Add (Other_Units, "°F",             L_Fahrenheit'Address);
   Add (Other_Units, "°K",             L_K'Address         );
   --
   -- UTF8 part
   --
   Add (Other_Units, Angstrom_Sign,                 U_ang'Address);
   Add (Other_Units, LC_AR & "ngstr" & LC_OE & "m", U_ang'Address);
   Add (Other_Units, UC_AR & "ngstr" & LC_OE & "m", U_ang'Address);
   Add (Other_Units, UC_AR,                         U_ang'Address);
   Add (Other_Units, Ring,                       U_degree'Address);
   Add (Other_Units, Ring & 'F',             U_Fahrenheit'Address);
   Add (Other_Units, Degree_Fahrenheit,      U_Fahrenheit'Address);
   Add (Other_Units, Ring & 'K',                      U_K'Address);
   Add (Other_Units, Ounce_Sign,                     U_oz'Address);

   Add (Prefix_Operations, "(",  Operation'Pos (Left_Bracket));
   Add (Prefix_Operations, "+",  Operation'Pos (Unary_Plus));
   Add (Prefix_Operations, "-",  Operation'Pos (Unary_Minus));

   Add (Dyadic_Operations, "^", Operation'Pos (Power));
   Add (Dyadic_Operations, "*", Operation'Pos (Multiply));
   Add (Dyadic_Operations, "·", Operation'Pos (Multiply) + Latin1_Flag);
   Add (Dyadic_Operations, Dot, Operation'Pos (Multiply) + UTF8_Flag);
   Add (Dyadic_Operations, "/", Operation'Pos (Divide));
   Add (Dyadic_Operations, "+", Operation'Pos (Plus));
   Add (Dyadic_Operations, "-", Operation'Pos (Minus));
   Add (Dyadic_Operations, "**",  Operation'Pos (Power));
   Add (Dyadic_Operations, "and", Operation'Pos (Shift));

end Measures_Generic_Edit;
