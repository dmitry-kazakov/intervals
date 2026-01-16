--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Gtk_Edit.GEntry                    Luebeck            --
--        Cell_Renderer                            Summer, 2007       --
--  Implementation                                                    --
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

with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with GLib.Properties;       use GLib.Properties;
with Glib.Values.Handling;  use Glib.Values.Handling;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Style_Context;     use Gtk.Style_Context;
with Pango.Enums;           use Pango.Enums;
with Strings_Edit;          use Strings_Edit;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Units.Base;

package body Measures_Gtk_Edit.GEntry.Cell_Renderer is
   use Gtk.Widget;

   Renderer_Type : GType := GType_Invalid;
   Value_ID      : constant Property_ID := 1;
   After_ID      : constant Property_ID := 2;
   Scale_ID      : constant Property_ID := 3;
   Text_ID       : constant Property_ID := 4;

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Gtk_Cell_Renderer_Measure_Record'Class,
             Gtk_Cell_Renderer_Measure
          );

   type Class_Init_Ptr is access procedure (Class : GObject_Class);
   pragma Convention (C, Class_Init_Ptr);
   procedure Class_Init (Class : GObject_Class);
   pragma Convention (C, Class_Init);

   procedure Class_Init (Class : GObject_Class) is
   begin
      Base_Class_Init (Class);
      Class_Install_Property
      (  Class,
         Value_ID,
         Gnew_Double
         (  Name    => "value",
            Nick    => "value",
            Blurb   => "Scaled number",
            Minimum => GDouble'First,
            Maximum => GDouble'Last,
            Default => 0.0
      )  );
      Class_Install_Property
      (  Class,
         After_ID,
         Gnew_UInt
         (  Name    => "after",
            Nick    => "aft",
            Blurb   => "Digits after decimal point",
            Minimum => 0,
            Maximum => GDouble'Digits,
            Default => 0
      )  );
      Class_Install_Property
      (  Class,
         Scale_ID,
         Gnew_String
         (  Name    => "scale",
            Nick    => "scale",
            Default => "",
            Blurb   =>
               (  "Dimensioned scale of the values. "
               &  "It also constraints the input values to "
               &  "its dimension. Empty scale correponds to "
               &  "1 SI"
      )  )     );
      Class_Install_Property
      (  Class,
         Text_ID,
         Gnew_String
         (  Name    => "text",
            Nick    => "text",
            Default => "",
            Blurb   =>
               (  "Value of the renderer as a text. "
               &  "When the value does not contain unit "
               &  "specification, then the scale is assumend. "
               &  "Otherwise, the unit is checked and scaled "
               &  "according to the scale"
      )  )     );
   end Class_Init;

   procedure Editing_Done
             (  Editor : access Gtk_Unit_Entry_Record'Class;
                Cell   : Gtk_Cell_Renderer_Measure
             )  is
   begin
      if Cell.Focus_Out.Id /= Null_Handler_Id then
         Disconnect (Editor, Cell.Focus_Out);
         Cell.Focus_Out.Id := Null_Handler_Id;
      end if;
      if Editing_Canceled (Editor) then
         Cell.Stop_Editing (True);
      else
         Cell.Value :=
            Get_Value_As
            (  Get (Editor, Cell.Scale),
               Cell.Scale
            );
         Stop_Editing (Cell, False);
         Cell.Commit;
      end if;
   exception
      when others =>
         Stop_Editing (Cell, True);
   end Editing_Done;

   procedure Finalize
             (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
             )  is
      use Gtk.Cell_Renderer.Abstract_Renderer;
   begin
      if Cell.Text /= null then
         Unref (Cell.Text);
      end if;
      Free (Cell.Scale_Text);
      Finalize (Gtk_Abstract_Renderer_Record (Cell.all)'Access);
   end Finalize;

   function Focus_Out
            (  Editor : access Gtk_Unit_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Cell   : Gtk_Cell_Renderer_Measure
            )  return Boolean is
   begin
      Editing_Done (Editor, Cell);
      return False;
   end Focus_Out;

   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
            )  return Number is
   begin
      return Cell.Value;
   end Get;

   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
            )  return Measure is
   begin
      return Cell.Value * Cell.Scale;
   end Get;

   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
            )  return UTF8_String is
      Text : String renames
                UTF8_Edit.Float_Edit.Image
                (  Cell.Value,
                   AbsSmall => -Cell.After
                );
   begin
      if Cell.Scale_Text'Length > 0 then
         return
         (  Text
         &  Character'Val (16#C2#)	-- Multiplication
         &  Character'Val (16#B7#)
         &  Cell.Scale_Text.all
         );
      else
         return Text;
      end if;
   end Get;

   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
            )  return GValue is
      Result : GValue;
   begin
      Init (Result, GType_Double);
      begin
         Set_Double (Result, GDouble (Cell.Value));
      exception
         when Constraint_Error | Numeric_Error =>
            if Cell.Value > 0.0 then
               Set_Double (Result, GDouble'Last);
            else
               Set_Double (Result, GDouble'First);
            end if;
      end;
      return Result;
   end Get;

   function Get_Aligned_Area
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Measure_Record;
               Widget : not null access Gtk_Widget_Record'Class;
               Flags  : Gtk_Cell_Renderer_State;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle is
      Area   : constant Gdk_Rectangle :=
               Cell.Get_Size (Widget, Cell_Area);
      Result : Gdk_Rectangle;
   begin
      Result.X :=
         (  Cell_Area.X
         +  GInt (Get_X_Pad (Cell))
         +  Area.X
         +  (Cell.Max_Offset - Cell.Left_Width)
         );
      Result.Y :=
         (  Cell_Area.Y
         +  GInt (Get_Y_Pad (Cell))
         +  Area.Y
         );
      Result.Width :=
         GInt'Min
         (  Result.X - Cell_Area.X + Cell_Area.Width,
            Area.Width
         );
      Result.Height :=
         GInt'Min (Result.Y - Cell_Area.Y + Cell_Area.Height, Area.Height);
      return Result;
   end Get_Aligned_Area;

   procedure Get_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Measure_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Value_ID =>
            Value := Get (Cell);
         when After_ID =>
            Init (Value, GType_UInt);
            Set_UInt (Value, GUInt (Cell.After));
         when Scale_ID =>
            Init (Value, GType_String);
            Set_String (Value, UTF8_Edit.Image (Cell.Scale));
         when Text_ID =>
            Init (Value, GType_String);
            Set_String (Value, Get (Cell));
         when others =>
            Init (Value, GType_String);
            Set_String (Value, "unknown");
      end case;
   end Get_Property;

   function Get_Size
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Measure_Record;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Gdk_Rectangle is
   begin
      Update (Cell.all, Widget);
      return
      (  X      => 0,
         Y      => 0,
         Width  => Cell.Width,
         Height => Cell.Height
      );
   end Get_Size;

   function Get_Size
            (  Cell      : not null access
                           Gtk_Cell_Renderer_Measure_Record;
               Widget    : not null access
                           Gtk_Widget_Record'Class;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle is
   begin
      Update (Cell.all, Widget);
      return
      (  X      => GInt
                   (  Get_X_Align (Cell)
                   *  GFloat (Cell_Area.Width  - Cell.Width)
                   ),
         Y      => GInt
                   (  Get_Y_Align (Cell)
                   *  GFloat (Cell_Area.Height - Cell.Height)
                   ),
         Width  => Cell.Width,
         Height => Cell.Height
      );
   end Get_Size;

   function Get_Type return Gtk.Gtk_Type is
      function "+" is
         new Ada.Unchecked_Conversion (Class_Init_Ptr, C_Class_Init);
   begin
      if Renderer_Type = GType_Invalid then
         Renderer_Type :=
            Register
            (  Class_Name & "CellRenderer",
               +Class_Init'Access
            );
      end if;
      return Renderer_Type;
   end Get_Type;

   procedure Gtk_New
             (  Cell  : out Gtk_Cell_Renderer_Measure;
                Scale : UTF8_String  := "";
                After : Natural      := 0
             )  is
   begin
      Cell := new Gtk_Cell_Renderer_Measure_Record;
      Initialize (Cell, Scale, After);
   exception
      when others =>
         Free (Cell);
         raise;
   end Gtk_New;

   procedure Initialize
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record'Class;
                Scale : UTF8_String;
                After : Natural
             )  is
   begin
      Set_Scale (Cell, Scale);
      Cell.After := After;
      Cell.Scale_Text := new String'(Scale);
      Gtk.Cell_Renderer.Abstract_Renderer.Initialize (Cell, Get_Type);
   end Initialize;

   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Value : Number
             )  is
   begin
      Cell.Value := Value;
   end Put;

   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Value : Measure
             )  is
   begin
      Cell.Value := Get_Value_As (Value, Cell.Scale);
   end Put;

   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Value : UTF8_String
             )  is
      Pointer : Integer := Value'First;
      Result  : Number;
      Scale   : Measure;
   begin
      Get (Value, Pointer, SpaceAndTab);
      UTF8_Edit.Float_Edit.Get (Value, Pointer, Result);
      Get (Value, Pointer, SpaceAndTab);
      if Pointer > Value'Last then
         Cell.Value := Result;
      else
         UTF8_Edit.Get_Unit (Value, Pointer, Scale);
         Get (Value, Pointer, SpaceAndTab);
         if Pointer <= Value'Last then
            raise Data_Error;
         end if;
         Cell.Value := Get_Value_As (Result * Scale, Cell.Scale);
      end if;
   end Put;

   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Value : GValue
             )  is
   begin
      case Get_Type (Value) is
         when GType_Double =>
            declare
               Result : constant GDouble := Get_Double (Value);
            begin
               Cell.Value := Number (Result);
            exception
               when Constraint_Error | Numeric_Error =>
                  if Result > 0.0 then
                     Cell.Value := Number'Last;
                  else
                     Cell.Value := Number'First;
                  end if;
            end;
         when GType_String =>
            Put (Cell, Get_String (Value));
         when others =>
            raise Constraint_Error;
      end case;
   end Put;

   procedure Render
             (  Cell            : not null access
                                  Gtk_Cell_Renderer_Measure_Record;
                Context         : Cairo_Context;
                Widget          : not null access
                                  Gtk_Widget_Record'Class;
                Background_Area : Gdk_Rectangle;
                Cell_Area       : Gdk_Rectangle;
                Flags           : Gtk_Cell_Renderer_State
             )  is
      Area  : constant Gdk_Rectangle :=
              Cell.Get_Size (Widget, Cell_Area);
      Style : constant Gtk_Style_Context := Get_Style_Context (Widget);
   begin
      Save (Context);
      Rectangle
      (  Context,
         GDouble (Cell_Area.X),
         GDouble (Cell_Area.Y),
         GDouble (Cell_Area.Width),
         GDouble (Cell_Area.Height)
      );
      Clip (Context);
      Render_Layout
      (  Style,
         Context,
         GDouble
         (  Cell_Area.X
         +  GInt (Get_X_Pad (Cell))
         +  Area.X
         +  (Cell.Max_Offset - Cell.Left_Width)
         ),
         GDouble
         (  Cell_Area.Y
         +  GInt (Get_Y_Pad (Cell))
         +  Area.Y
         ),
         Cell.Text
      );
      Restore (Context);
   end Render;

   procedure Set_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Measure_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Value_ID =>
            begin
               Put (Cell, Value);
            exception
               when others =>
                  null;
            end;
         when After_ID =>
            Cell.After := Integer (Get_UInt (Value));
         when Scale_ID =>
            begin
               Set_Scale (Cell, Get_String (Value));
            exception
               when Constraint_Error | Data_Error | Unit_Error |
                    End_Error =>
                  null;
            end;
         when Text_ID =>
            begin
               Put (Cell, Get_String (Value));
            exception
               when Constraint_Error | Data_Error | Unit_Error |
                    End_Error =>
                  null;
            end;
         when others =>
            null;
      end case;
   end Set_Property;

   procedure Set_Scale
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Scale : UTF8_String
             )  is
   begin
      if (  Cell.Scale_Text /= null
         and then
            Cell.Scale_Text.all = Scale
         )
      then
         return;
      end if;
      declare
         Pointer : Integer := Scale'First;
         Result  : Measure := (Units.Base.Unitless, 1.0, 0.0);
      begin
         if Scale'Length /= 0 then
            begin
               Get (Scale, Pointer, SpaceAndTab);
               UTF8_Edit.Get_Unit (Scale, Pointer, Result);
               Get (Scale, Pointer, SpaceAndTab);
               if Pointer <= Scale'Last then
                  raise Data_Error;
               end if;
               if Result.Gain <= 0.0 then
                  raise Constraint_Error;
               end if;
               Cell.Scale := Result;
            exception
               when End_Error =>
                  null;
            end;
         end if;
         Free (Cell.Scale_Text);
         Cell.Scale_Text := new String'(Scale);
         Cell.Scale      := Result;
      end;
   end Set_Scale;

   function Start_Editing
            (  Cell            : not null access
                                 Gtk_Cell_Renderer_Measure_Record;
               Event           : Gdk_Event;
               Widget          : not null access
                                 Gtk_Widget_Record'Class;
               Path            : String;
               Background_Area : Gdk_Rectangle;
               Cell_Area       : Gdk_Rectangle;
               Flags           : Gtk_Cell_Renderer_State
            )  return Gtk_Widget is
      Editor : Gtk_Unit_Entry;
   begin
      Gtk_New (Editor, Cell.Scale.SI, Get (Cell));
      Set_Property (Editor, Build ("xalign"), Get_X_Align (Cell));
      Set_Property (Editor, Build ("has-frame"), False);
      Entry_Callbacks.Connect
      (  Editor,
         "editing_done",
         Entry_Callbacks.To_Marshaller (Editing_Done'Access),
         Cell.all'Access
      );
      Cell.Focus_Out :=
         Entry_Return_Callbacks.Connect
         (  Editor,
            "focus_out_event",
            Entry_Return_Callbacks.To_Marshaller (Focus_Out'Access),
            Cell.all'Access
         );
      Editor.Show;
      --
      -- Determining the widget's position for a  renderer  is  somewhat
      -- broken. So let's give a hint to the Editor where it should  be.
      -- Here it is not too late.
      --
      if Widget.all in Gtk_Tree_View_Record'Class then
         Get_Origin
         (  Gtk_Tree_View_Record'Class (Widget.all).Get_Bin_Window,
            Editor.X,
            Editor.Y
         );
      else
         Get_Screen_Position (Widget, Editor.X, Editor.Y);
      end if;
      Editor.X := Editor.X + Background_Area.X;
      Editor.Y := Editor.Y + Background_Area.Y;
      Editor.Is_Renderer := True;
      return Editor.all'Access;
   end Start_Editing;

   procedure Update
             (  Cell   : in out Gtk_Cell_Renderer_Measure_Record;
                Widget : access Gtk.Widget.Gtk_Widget_Record'Class
             )  is
      --
      -- Note that the function Line_Index_To_X declared in Pango.Layout
      -- is wrong. This is a correct version of.
      --
      procedure Index_To_X
                (  Line     : Pango_Layout_Line;
                   Index    : GInt;
                   Trailing : GInt;
                   X_Pos    : out GInt
                );
      pragma Import (C, Index_To_X, "pango_layout_line_index_to_x");
      Text      : String renames
                     UTF8_Edit.Float_Edit.Image
                     (  Cell.Value,
                        AbsSmall => -Cell.After
                     );
      Point_Pos : Integer := Text'Last + 1;
      Right     : constant GInt := Cell.Width - Cell.Max_Offset;
   begin
      if Cell.Text = null then
         Cell.Text := Create_Pango_Layout (Widget);
      end if;
      for Index in Text'Range loop
         -- Find the position of the decimal point in the output
         if '.' = Text (Index) then
            Point_Pos := Index;
            exit;
         end if;
      end loop;
      Cell.Text.Set_Text (Text (Text'First..Text'Last));
      Get_Pixel_Size (Cell.Text, Cell.Width, Cell.Height);
      if Point_Pos <= Text'Last then
         Index_To_X
         (  Get_Line (Cell.Text, 0),
            GInt (Point_Pos - Text'First),
            0,
            Cell.Left_Width
         );
         Cell.Left_Width := To_Pixels (Cell.Left_Width);
      else
         Cell.Left_Width := Cell.Width;
      end if;
      Cell.Max_Offset := GInt'Max (Cell.Left_Width, Cell.Max_Offset);
      Cell.Width :=
         (  Cell.Max_Offset
         +  GInt'Max (Right, Cell.Width - Cell.Left_Width)
         );
   end Update;

end Measures_Gtk_Edit.GEntry.Cell_Renderer;
