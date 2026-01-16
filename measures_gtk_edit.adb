--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Gtk_Edit                           Luebeck            --
--  Implementation                                 Winter, 2007       --
--                                                                    --
--                                Last revision :  11:26 29 May 2020  --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Gdk.Rectangle;             use Gdk.Rectangle;
with GLib;                      use GLib;
with GLib.Object;               use GLib.Object;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with GLib.Values;               use GLib.Values;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Strings_Edit;              use Strings_Edit;
with Strings_Edit.UTF8;         use Strings_Edit.UTF8;

with Ada.Unchecked_Deallocation;
with Ada.IO_Exceptions;
with Interfaces.C.Strings;
with Units.Base;

package body Measures_Gtk_Edit is
   use Gtk;
   use Units;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   SI_Unit        : constant GInt := 0;
   Regular_Unit   : constant GInt := 1;
   Irregular_Unit : constant GInt := 2;

   Signals : constant Interfaces.C.Strings.Chars_Ptr_Array :=
             (  1 => Interfaces.C.Strings.New_String ("commit"),
                2 => Interfaces.C.Strings.New_String ("reject")
             );

   function Button_Press
            (  Widget    : access Gtk_Widget_Record'Class;
               Event     : Gdk_Event;
               Selection : Gtk_Unit_Selection
            )  return Boolean is
      use Unit_Selection_Handlers;
   begin
      case Get_Event_Type (Event) is
         when Gdk_2button_Press =>
            declare
               Iter  : Gtk_Tree_Iter;
               Model : Gtk_Tree_Model;
            begin
               Get_Selected
               (  Get_Selection (Selection.Selection),
                  Model,
                  Iter
               );
               if (  Iter /= Null_Iter
                  and then
                     not Has_Child (Model, Iter)
                  )
               then
                  Emit_By_Name (Selection, "commit");
                  return True;
               else
                  return False;
               end if;
            end;
         when Key_Press =>
            case Get_Key_Val (Event) is
               when GDK_Return | GDK_KP_Enter | GDK_ISO_Enter =>
                  Emit_By_Name (Selection, "commit");
                  return True;
               when GDK_Escape =>
                  Emit_By_Name (Selection, "reject");
                  return True;
               when others =>
                  return False;
            end case;
         when others =>
            return False;
      end case;
   end Button_Press;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Gtk_Unit_Selection_Record'Class,
             Gtk_Unit_Selection
          );

   function Filter
            (  Widget : Gtk_Unit_Selection_Record;
               Value  : Measure
            )  return Boolean is
   begin
      return
      (  not Widget.Constrained
      or else
         Widget.Constraint = Value.SI
      );
   end Filter;

   function Get
            (  Widget : not null access Gtk_Unit_Selection_Record
            )  return Measure is
      Text : String renames Get_Text (Widget.Edit);
      Unit : Measure;
   begin
      Unit := UTF8_Edit.Value (Text);
      if Filter (Widget.all, Unit) then
         return Unit;
      else
         raise Constraint_Error;
      end if;
   end Get;

   function Get
            (  Widget : not null access Gtk_Unit_Selection_Record
            )  return UTF8_String is
      Text     : String renames Get_Text (Widget.Edit);
      Pointer  : Integer := Text'First;
      From, To : Integer;
      Unit     : Measure;
   begin
      Get (Text, Pointer, SpaceAndTab);
      From := Pointer;
      begin
         UTF8_Edit.Get (Text, Pointer, Unit);
      exception
         when Ada.IO_Exceptions.End_Error =>
            if Pointer <= Text'Length then
               raise Ada.IO_Exceptions.Data_Error;
            else
               raise;
            end if;
      end;
      To := Pointer - 1;
      Get (Text, Pointer, SpaceAndTab);
      if Pointer <= Text'Length then
         raise Ada.IO_Exceptions.Data_Error;
      end if;
      if Filter (Widget.all, Unit) then
         return Text (From..To);
      else
         raise Constraint_Error;
      end if;
   end Get;

   function Get_Entry
            (  Widget : not null access Gtk_Unit_Selection_Record
            )  return Gtk_Entry is
   begin
      return Widget.Edit;
   end Get_Entry;

   function Get_Tree_View
            (  Widget : not null access Gtk_Unit_Selection_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.Selection;
   end Get_Tree_View;

   function Get_Type return Gtk_Type is
      procedure Add (Name : String) is
      begin
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => To_Lower (Name),
               Nick    => Name,
               Blurb   => Name & ", units of",
               Default => Name
         )  );
      end Add;
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Box.Get_Vbox_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name,
            Signals      => Signals
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "unit-column-title",
               Nick    => "Unit",
               Blurb   => "The unit column, name of",
               Default => "Unit"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "equivalent-column-title",
               Nick    => "Equivalent",
               Blurb   => "The equivalent SI unit",
               Default => "SI equivalent"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "power-label",
               Nick    => "Exponent",
               Blurb   => "The exponent of the SI-prefix",
               Default => "Power"
         )  );
         --
         -- Sections names
         --
         Add ("Geometry");
            Add ("Angle");
            Add ("Area");
            Add ("Length");
            Add ("Volume");
         Add ("Mechanics");
            Add ("Acceleration");
            Add ("Energy");
            Add ("Force");
            Add ("Mass");
            Add ("Power");
            Add ("Pressure");
            Add ("Time");
            Add ("Velocity");
         Add ("Electricity");
            Add ("Capacitance");
            Add ("Charge");
            Add ("Conductance");
            Add ("Current");
            Add ("Frequency");
            Add ("Inductance");
            Add ("Potential");
            Add ("Resistance");
         Add ("Chemistry");
            Add ("Amount");
            Add ("Concentration");
            Add ("Temperature");
         Add ("Optic");
            Add ("Intensity");
            Add ("Illuminance");
            Add ("Flux");
            Add ("Luminance");
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget     : out Gtk_Unit_Selection;
                Constraint : Unit;
                Initial    : GLib.UTF8_String := ""
             )  is
   begin
      Widget := new Gtk_Unit_Selection_Record;
      Measures_Gtk_Edit.Initialize (Widget, Constraint, Initial);
   exception
      when others =>
         Free (Widget);
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Unit_Selection;
                Initial : GLib.UTF8_String := ""
             )  is
   begin
      Widget := new Gtk_Unit_Selection_Record;
      Measures_Gtk_Edit.Initialize (Widget, Initial);
   exception
      when others =>
         Free (Widget);
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Unit_Selection_Record'Class;
                Initial : GLib.UTF8_String := ""
             )  is
      Edit : Gtk_HBox;
   begin
      G_New (Widget, Get_Type);
      Initialize_VBox (Widget);
      --
      -- Selection tree view control
      --
      Gtk_New (Widget.Selection);
      Widget.Selection.Set_Rules_Hint (True);
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 0);
         Column_No := Widget.Selection.Append_Column (Column);
         Column.Set_Resizable (True);
         Column.Set_Sort_Column_Id (0);

         Gtk_New (Column);
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 1);
         Column_No := Widget.Selection.Append_Column (Column);
         Column.Set_Resizable (True);
         Column.Set_Sort_Column_Id (1);
      end;
      Get_Selection (Widget.Selection).Set_Mode (Selection_Single);
      --
      -- Manual edit control
      --
      Gtk_New_HBox (Edit);
      Gtk_New (Widget.Radix);
      Gtk_New (Widget.Edit);
      Gtk_New (Widget.Scale, -24.0, 24.0, 1.0);
      Set_Value (Widget.Scale, 0.0);
      Edit.Pack_Start (Widget.Edit);
      Edit.Pack_Start (Widget.Radix, False);
      Edit.Pack_Start (Widget.Scale, False);
      if Initial'Length > 0 then
         declare
            Value    : Measure;
            Pointer  : Integer := Initial'First;
            From, To : Integer;
         begin
            Get (Initial, Pointer, SpaceAndTab);
            From := Pointer;
            UTF8_Edit.Get (Initial, Pointer, Value);
            To := Pointer - 1;
            Get (Initial, Pointer, SpaceAndTab);
            if Pointer > Initial'Length then
               Widget.Edit.Set_Text (Initial (From..To));
            end if;
         exception
            when others =>
               null;
         end;
      end if;
      --
      -- Putting them together
      --
      Gtk_New (Widget.Scroll);
      Widget.Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Widget.Scroll.Add (Widget.Selection);
      Widget.Pack_Start (Edit, False);
      Widget.Pack_Start (Widget.Scroll);
      Edit.Show_All;
      Widget.Scroll.Show_All;
      --
      -- Setting up events handlers
      --
      Unit_Selection_Handlers.Connect
      (  Widget,
         "style-updated",
         Unit_Selection_Handlers.To_Marshaller (Style_Updated'Access)
      );
      Event_Handlers.Connect
      (  Widget.Selection,
         "key_press_event",
         Event_Handlers.To_Marshaller (Button_Press'Access),
         Widget.all'Access
      );
      Event_Handlers.Connect
      (  Widget.Selection,
         "button_press_event",
         Event_Handlers.To_Marshaller (Button_Press'Access),
         Widget.all'Access
      );
      Event_Handlers.Connect
      (  Widget.Edit,
         "key_press_event",
         Event_Handlers.To_Marshaller (Button_Press'Access),
         Widget.all'Access
      );
      Event_Handlers.Connect
      (  Widget.Radix,
         "key_press_event",
         Event_Handlers.To_Marshaller (Button_Press'Access),
         Widget.all'Access
      );
      Event_Handlers.Connect
      (  Widget.Scale,
         "key_press_event",
         Event_Handlers.To_Marshaller (Button_Press'Access),
         Widget.all'Access
      );
      Style_Updated (Widget);
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                             Gtk_Unit_Selection_Record'Class;
                Constraint : Unit;
                Initial    : GLib.UTF8_String := ""
             )  is
   begin
      Widget.Constraint  := Constraint;
      Widget.Constrained := True;
      Initialize (Widget, Initial);
   end Initialize;

   procedure On_Focus
             (  Edit   : access Gtk_Widget_Record'Class;
                Parent : Gtk_Unit_Selection
             )  is
   begin
      if Parent.Initialized then
         Hide (Parent.Radix);
         Hide (Parent.Scale);
      end if;
   end On_Focus;

   procedure On_Scale
             (  Spin   : access Gtk_Widget_Record'Class;
                Parent : Gtk_Unit_Selection
             )  is
      function Get_Prefix return String is
      begin
         case Integer (Get_Value (Parent.Scale)) is
            when -24    => return "y";
            when -21    => return "z";
            when -18    => return "a";
            when -15    => return "f";
            when -12    => return "p";
            when -9     => return "n";
            when -6     => return Image (16#00B5#);
            when -3     => return "m";
            when -2     => return "c";
            when -1     => return "d";
            when  1     => return "da";
            when  2     => return "h";
            when  3     => return "k";
            when  6     => return "M";
            when  9     => return "G";
            when 12     => return "T";
            when 15     => return "P";
            when 18     => return "E";
            when 21     => return "Z";
            when 24     => return "Y";
            when others => return "";
         end case;
      end Get_Prefix;

      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Value : GValue;
   begin
      declare
         Scale : constant Integer := Integer (Get_Value (Parent.Scale));
      begin
         if abs Scale > 3 then
            case Scale mod 3 is
               when 1 =>
                  Set_Value (Parent.Scale, GDouble (Scale + 2));
               when 2 =>
                  Set_Value (Parent.Scale, GDouble (Scale - 2));
               when others =>
                  null;
            end case;
         end if;
      end;
      Get_Selected (Get_Selection (Parent.Selection), Model, Iter);
      if (  Iter /= Null_Iter
         and then
            not Has_Child (Model, Iter)
         )
      then
         Get_Value (Model, Iter, 2, Value);
         begin
            if Get_Int (Value) in SI_Unit..Regular_Unit then
               Unset (Value);
               Get_Value (Model, Iter, 0, Value);
               if "kg" = Get_String (Value) then
                  Set_Text (Parent, Get_Prefix & "g");
               else
                  Set_Text
                  (  Parent,
                     Get_Prefix & Get_String (Value)
                  );
               end if;
            end if;
         exception
            when others =>
               Unset (Value);
               raise;
         end;
         Unset (Value);
      end if;
   end On_Scale;

   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Parent    : Gtk_Unit_Selection
             )  is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Value : GValue;
   begin
      Get_Selected (Selection, Model, Iter);
      if Iter /= Null_Iter and then not Has_Child (Model, Iter) then
         Get_Value (Model, Iter, 2, Value);
         begin
            if Get_Int (Value) in SI_Unit..Regular_Unit then
               Unset (Value);
               Get_Value (Model, Iter, 0, Value);
               if "kg" = Get_String (Value) then
                  Set_Value (Parent.Scale, 3.0);
               else
                  Set_Value (Parent.Scale, 0.0);
               end if;
               Show (Parent.Radix);
               Show (Parent.Scale);
               Set_Text (Parent, Get_String (Value));
            else
               Unset (Value);
               Get_Value (Model, Iter, 0, Value);
               Hide (Parent.Radix);
               Hide (Parent.Scale);
               Set_Text (Parent, Get_String (Value));
            end if;
         exception
            when others =>
               Unset (Value);
               raise;
         end;
         Unset (Value);
         Parent.Initialized := True;
      end if;
   end On_Selection;

   function Lg (X : Number) return Integer is
      Value  : Number  := X;
      Result : Integer := 0;
      Diff   : Number;
   begin
      if Value = 1.0 then
         return 0;
      elsif Value > 1.0 then
         loop
            Value  := Value / 10.0;
            Result := Result + 1;
            Diff   := Value - 1.0;
            if abs Diff <= Number'Model_Epsilon then
               return Result;
            elsif Diff < 0.0 or else Result >= 24 then
               return 25;
            end if;
         end loop;
      else
         loop
            Value  := Value * 10.0;
            Result := Result - 1;
            Diff   := 1.0 - Value;
            if abs Diff <= Number'Model_Epsilon then
               return Result;
            elsif Diff < 0.0 or else Result <= -24 then
               return -25;
            end if;
         end loop;
      end if;
   end Lg;

   procedure Set_Text
             (  Widget : access Gtk_Unit_Selection_Record;
                Text   : String
             )  is
      use UTF8_Edit;
      use UTF8_Edit.Float_Edit;
      Old_Text  : constant String := Get_Text (Widget.Edit);
      New_Value : Measure;
      Old_Value : Scaled;
      Pointer   : Integer := Old_Text'First;
   begin
      New_Value := Value (Text);
      Get (Old_Text, Pointer, SpaceAndTab);
      Get (Old_Text, Pointer, Old_Value);
      Get (Old_Text, Pointer, SpaceAndTab);
      if Pointer >= Old_Text'Last then
         case Old_Value.Format is
            when Scalar | Numeric =>
               Set_Text
               (  Widget.Edit,
                  (  Image (Old_Value.Numeral * Old_Value.Scale.Gain)
                  &  ' '
                  &  Text
               )  );
            when Canonic =>
               if Old_Value.Scale.SI = New_Value.SI then
                  Set_Text
                  (  Widget.Edit,
                     (  Image
                        (  Get_Value_As
                           (  Old_Value.Scale * Old_Value.Numeral,
                              New_Value
                        )  )
                     &  ' '
                     &  Text
                  )  );
               else
                  Set_Text
                  (  Widget.Edit,
                     (  Image (Old_Value.Numeral)
                     &  ' '
                     &  Text
                  )  );
               end if;
            when Jumbled =>
               Set_Text (Widget.Edit, Text);
         end case;
      else
         Set_Text (Widget.Edit, Text);
      end if;
   exception
      when others =>
         Set_Text (Widget.Edit, Text);
   end Set_Text;

   procedure Style_Updated
             (  Widget : access Gtk_Unit_Selection_Record'Class
             )  is
      Selection  : Gtk_Tree_Store;
      Section    : Gtk_Tree_Iter := Null_Iter;
      Subsection : Gtk_Tree_Iter := Null_Iter;
      Select_Row : Gtk_Tree_Iter := Null_Iter;
      Initialize : Boolean       := not Widget.Initialized;
      Initial    : Measure;
      Text       : GValue;
      Mode       : GValue;

      procedure Purge_Subsection is
      begin
         if (  Subsection /= Null_Iter
            and then
               not Has_Child (Selection, Subsection)
            )
         then
            Remove (Selection, Subsection);
         end if;
      end Purge_Subsection;

      procedure Purge_Section is
      begin
         Purge_Subsection;
         if (  Section /= Null_Iter
            and then
               not Has_Child (Selection, Section)
            )
         then
            Remove (Selection, Section);
         end if;
         Subsection := Null_Iter;
      end Purge_Section;

      procedure Add_Section (Name : String) is
      begin
         Purge_Section;
         Append (Selection, Section, Null_Iter);
         Set_String (Text, Style_Get (Widget, Name));
         Set_Value (Selection, Section, 0, Text);
      end Add_Section;

      procedure Add_Subsection (Name : String) is
      begin
         Purge_Subsection;
         Append (Selection, Subsection, Section);
         Set_String (Text, Style_Get (Widget, Name));
         Set_Value (Selection, Subsection, 0, Text);
      end Add_Subsection;

      procedure Add_Unit (Name : String; Kind_Of : GInt) is
         Row   : Gtk_Tree_Iter := Null_Iter;
         Value : constant Measure := UTF8_Edit.Value (Name);
         Scale : Integer;
      begin
         if Filter (Widget.all, Value) then
            Append (Selection, Row, Subsection);
            Set_String (Text, Name);
            Set_Value (Selection, Row, 0, Text);
            if Kind_Of /= SI_Unit then
               Set_String (Text, UTF8_Edit.Image (Value));
               Set_Value (Selection, Row, 1, Text);
            end if;
            Set_Int (Mode, Kind_Of);
            Set_Value (Selection, Row, 2, Mode);
            if (  Initialize
               and then
                  Value.SI = Initial.SI
               and then
                  Value.Offset = Initial.Offset
               )
            then
               if Kind_Of in SI_Unit..Regular_Unit then
                  -- See if a SI-prefix would apply
                  Scale := Lg (Initial.Gain / Value.Gain);
                  case abs Scale is
                     when 0..3 | 6 | 9 | 12 | 15 | 18 | 21 | 24 =>
                        Initialize := False;
                        if Value.SI = Units.Base.Mass then
                           -- kg is a special case
                           Scale := Scale + 3;
                        end if;
                        Set_Value (Widget.Scale, GDouble (Scale));
                        Select_Row := Row;
                     when others =>
                        null;
                  end case;
               else
                  -- See if it is exactly same
                  if Value.Gain = Initial.Gain then
                     Initialize := False;
                     Select_Row := Row;
                     Hide (Widget.Scale);
                     Hide (Widget.Radix);
                  end if;
               end if;
            end if;
         end if;
      end Add_Unit;

   begin
      Init (Text, GType_String);
      Init (Mode, GType_Int);
      if Initialize then
         begin
            Initial := UTF8_Edit.Value (Get_Text (Widget.Edit));
         exception
            when others => -- Ignoring wrong intitial value
               Initialize := False;
               Hide (Widget.Scale);
               Hide (Widget.Radix);
         end;
      end if;
      Set_Title
      (  Get_Column (Widget.Selection, 0),
         Style_Get (Widget, "unit-column-title")
      );
      Set_Title
      (  Get_Column (Widget.Selection, 1),
         Style_Get (Widget, "equivalent-column-title")
      );
      Set_Text (Widget.Radix, Style_Get (Widget, "power-label"));
      Widget.Selection.Set_Model (Null_Gtk_Tree_Model);
      Gtk_New (Selection, (GType_String, GType_String, GType_Int));
      Add_Section ("geometry");
         Add_Subsection ("angle");
            Add_Unit ("rad",                     Regular_Unit);
            Add_Unit (Image (16#00B0#),          Irregular_Unit);
            Add_Unit ("'",                       Irregular_Unit);
            Add_Unit ("""",                      Irregular_Unit);
         Add_Subsection ("area");
            Add_Unit ("m" & Image (16#00B2#), SI_Unit);
            Add_Unit ("are",                     Irregular_Unit);
            Add_Unit ("acre",                    Irregular_Unit);
            Add_Unit ("hectare",                 Irregular_Unit);
         Add_Subsection ("length");
            Add_Unit ("m",                       SI_Unit);
            Add_Unit (Image (16#00C5#),          Irregular_Unit);
            Add_Unit ("fathom",                  Irregular_Unit);
            Add_Unit ("foot",                    Irregular_Unit);
            Add_Unit ("inch",                    Irregular_Unit);
            Add_Unit ("league",                  Irregular_Unit);
            Add_Unit ("lightyear",               Irregular_Unit);
            Add_Unit ("mile",                    Irregular_Unit);
            Add_Unit ("parsec",                  Irregular_Unit);
            Add_Unit ("yd",                      Irregular_Unit);
            Add_Unit ("ua",                      Irregular_Unit);
         Add_Subsection ("volume");
            Add_Unit ("m" & Image (16#00B3#), SI_Unit);
            Add_Unit ("l",                       Regular_Unit);
            Add_Unit ("barrel",                  Irregular_Unit);
            Add_Unit ("gallon",                  Irregular_Unit);
            Add_Unit ("gill",                    Irregular_Unit);
            Add_Unit ("pint",                    Irregular_Unit);
            Add_Unit ("quart",                   Irregular_Unit);
            Add_Unit ("tablespoon",              Irregular_Unit);
            Add_Unit ("teaspoon",                Irregular_Unit);
      Add_Section ("mechanics");
         Add_Subsection ("mass");
            Add_Unit ("kg",                      SI_Unit);
            Add_Unit ("carat",                   Irregular_Unit);
            Add_Unit ("dram",                    Irregular_Unit);
            Add_Unit ("oz",                      Irregular_Unit);
            Add_Unit ("pound",                   Irregular_Unit);
            Add_Unit ("t",                       Regular_Unit);
         Add_Subsection ("velocity");
            Add_Unit ("m/s",                     SI_Unit);
            Add_Unit ("km/h",                    Irregular_Unit);
            Add_Unit ("mph",                     Irregular_Unit);
            Add_Unit ("knot",                    Irregular_Unit);
         Add_Subsection ("acceleration");
            Add_Unit ("m/s" & Image (16#00B2#),  SI_Unit);
         Add_Subsection ("energy");
            Add_Unit ("J",                       SI_Unit);
            Add_Unit ("cal",                     Regular_Unit);
            Add_Unit ("eV",                      Irregular_Unit);
            Add_Unit ("erg",                     Regular_Unit);
         Add_Subsection ("force");
            Add_Unit ("N",                       SI_Unit);
            Add_Unit ("dyne",                    Irregular_Unit);
         Add_Subsection ("frequency");
            Add_Unit ("Hz",                      SI_Unit);
            Add_Unit ("Bq",                      SI_Unit);
            Add_Unit ("rpm",                     Irregular_Unit);
            Add_Unit ("rps",                     Irregular_Unit);
         Add_Subsection ("power");
            Add_Unit ("W",                       SI_Unit);
            Add_Unit ("hp",                      Irregular_Unit);
         Add_Subsection ("pressure");
            Add_Unit ("Pa",                      SI_Unit);
            Add_Unit ("atm",                     Irregular_Unit);
            Add_Unit ("mmHg",                    Irregular_Unit);
            Add_Unit ("psi",                     Irregular_Unit);
         Add_Subsection ("time");
            Add_Unit ("s",                       SI_Unit);
            Add_Unit ("min",                     Irregular_Unit);
            Add_Unit ("hour",                    Irregular_Unit);
            Add_Unit ("day",                     Irregular_Unit);
            Add_Unit ("year",                    Irregular_Unit);
      Add_Section ("electricity");
         Add_Subsection ("capacitance");
            Add_Unit ("F",                       SI_Unit);
         Add_Subsection ("charge");
            Add_Unit ("C",                       SI_Unit);
         Add_Subsection ("conductance");
            Add_Unit ("S",                       SI_Unit);
         Add_Subsection ("current");
            Add_Unit ("A",                       SI_Unit);
         Add_Subsection ("flux");
            Add_Unit ("T",                       SI_Unit);
         Add_Subsection ("frequency");
            Add_Unit ("Hz",                      SI_Unit);
         Add_Subsection ("inductance");
            Add_Unit ("H",                       SI_Unit);
         Add_Subsection ("potential");
            Add_Unit ("V",                       SI_Unit);
         Add_Subsection ("resistance");
            Add_Unit (Image (16#2126#),          SI_Unit);
      Add_Section ("chemistry");
         Add_Subsection ("amount");
            Add_Unit ("mol",                     SI_Unit);
         Add_Subsection ("concentration");
            Add_Unit ("%",                       Irregular_Unit);
            Add_Unit ("ppb",                     Irregular_Unit);
            Add_Unit ("ppm",                     Irregular_Unit);
            Add_Unit ("ppt",                     Irregular_Unit);
         Add_Subsection ("temperature");
            Add_Unit ("K",                       SI_Unit);
            Add_Unit (Image (16#00B0#) & "C",    Regular_Unit);
            Add_Unit (Image (16#00B0#) & "F",    Irregular_Unit);
      Add_Section ("optic");
         Add_Subsection ("flux");
            Add_Unit ("lm",                      SI_Unit);
         Add_Subsection ("intensity");
            Add_Unit ("cd",                      SI_Unit);
         Add_Subsection ("illuminance");
            Add_Unit ("lx",                      SI_Unit);
         Add_Subsection ("luminance");
            Add_Unit ("cd/m" & Image (16#00B2#), SI_Unit);
      Purge_Subsection;
      Purge_Section;
      Unset (Text);
      Unset (Mode);
      Widget.Selection.Set_Model (To_Interface (Selection));
      Unref (Selection);
      Expand_All (Widget.Selection);
      if not Widget.Initialized then
         if Initialize then
            Widget.Initialized := True;
            Hide (Widget.Scale);
            Hide (Widget.Radix);
         elsif Select_Row /= Null_Iter then
            Select_Iter
            (  Get_Selection (Widget.Selection),
               Select_Row
            );
            declare
               Path : constant Gtk_Tree_Path :=
                      Get_Path (Selection, Select_Row);
            begin
               Scroll_To_Cell
               (  Widget.Selection,
                  Path,
                  Get_Column (Widget.Selection, 0),
                  True,
                  0.5,
                  0.0
               );
               Path_Free (Path);
            end;
         end if;
         Selection_Handlers.Connect
         (  Widget.Scale,
            "value_changed",
            Selection_Handlers.To_Marshaller (On_Scale'Access),
            Widget.all'Access
         );
         Tree_Selection_Handlers.Connect
         (  Get_Selection (Widget.Selection),
            "changed",
            Tree_Selection_Handlers.To_Marshaller (On_Selection'Access),
            Widget.all'Access
         );
         Selection_Handlers.Connect
         (  Widget.Edit,
            "grab_focus",
            Selection_Handlers.To_Marshaller (On_Focus'Access),
            Widget.all'Access
         );
      end if;
      Widget.Selection.Columns_Autosize;
      declare
         Dummy     : GInt;
         Height    : GInt;
         Width     : GInt;
         Full_Area : Gdk_Rectangle;
         Tree_Area : Gdk_Rectangle;
      begin
         Widget.Get_Allocation (Full_Area);
         Widget.Selection.Get_Allocation (Tree_Area);
         Widget.Selection.Get_Preferred_Width  (Dummy, Width);
         Width := Width + Full_Area.Width - Tree_Area.Width;
         Widget.Selection.Get_Preferred_Height (Dummy, Height);
         Height := Height + Full_Area.Height - Tree_Area.Height;
         Widget.Set_Size_Request
         (  GInt'Min (Width,  600),
            GInt'Min (Height, 500)
         );
      end;
   end Style_Updated;

end Measures_Gtk_Edit;
