--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Gtk_Edit.GEntry                    Luebeck            --
--  Implementation                                 Summer, 2007       --
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

with Gdk.Main;                  use Gdk.Main;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Window;                use Gdk.Window;
with GLib;                      use GLib;
with GLib.Object;               use GLib.Object;
with GLib.Types;                use GLib.Types;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Gtkada.Types;              use Gtkada.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Strings_Edit;              use Strings_Edit;

with Ada.Unchecked_Deallocation;
with Ada.IO_Exceptions;
with Gtk.Cell_Editable;
with Gtk.Main;

package body Measures_Gtk_Edit.GEntry is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;
--
-- Editing_Done -- Emits editing-done
--
   procedure Editing_Done (Editable : System.Address);
   pragma Import (C, Editing_Done, "gtk_cell_editable_editing_done");
--
-- Remove_Widget -- Emits remove-widget
--
   procedure Remove_Widget (Editable : System.Address);
   pragma Import (C, Remove_Widget, "gtk_cell_editable_remove_widget");
--
-- Gtk_Cell_Editable_Iface -- Interface of GtkCellEditable
--
   type Gtk_Cell_Editable_Iface is record
      Editing_Done  : System.Address;
      Remove_Widget : System.Address;
      Start_Editing : Start_Editing_Entry;
   end record;
   pragma Convention (C, Gtk_Cell_Editable_Iface);

   type Gtk_Cell_Editable_Iface_Ptr is
      access all Gtk_Cell_Editable_Iface;
   pragma Convention (C, Gtk_Cell_Editable_Iface_Ptr);
--
-- Interface_Peek -- Get the interface's "virtual" table
--
   function Interface_Peek
            (  Class : GObject_Class;
               IFace : GType := Gtk.Cell_Editable.Get_Type
            )  return Gtk_Cell_Editable_Iface_Ptr;
   pragma Import (C, Interface_Peek, "g_type_interface_peek");

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Gtk_Unit_Entry_Record'Class,
             Gtk_Unit_Entry
          );
--
-- Get -- Interpret input
--
   function Get
            (  Widget : not null access Gtk_Unit_Entry_Record'Class;
               Text   : UTF8_String
            )  return Measure;
   function Get
            (  Widget : not null access Gtk_Unit_Entry_Record'Class;
               Text   : UTF8_String;
               Scale  : Measure
            )  return Measure;

   procedure Beep (Widget : not null access Gtk_Widget_Record'Class) is
   begin
      Widget.Get_Display.Beep;
   end Beep;

   function Get_Window (Widget : access Gtk_Widget_Record'Class)
      return Gtk_Window is
      Parent : constant Gtk_Widget := Widget.Get_Toplevel;
   begin
      if Parent /= null and then Parent.all in Gtk_Window_Record'Class
      then
         return Gtk_Window_Record'Class (Parent.all)'Access;
      else
         return null;
      end if;
   end Get_Window;

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Unit_Entry
             )  is
   begin
      if Edit.Popup /= null then
         Edit.Done_Popup;
      end if;
   end Destroy;

   function Button_Press
            (  Widget : access Gtk_Unit_Entry_Record'Class;
               Event  : Gdk_Event
            )  return Boolean is
      Window  : constant Gtk_Window := Get_Window (Widget);
      Frame   : Gtk_Frame;
      Dummy   : Gdk_Grab_Status;
   begin
      if Widget.Popup /= null then
         return False;
      end if;
      Widget.Canceled := True;
      -- Creating the popup window
      Gtk_New (Widget.Popup, Window_Popup);
      Widget.Popup.Set_Events (Key_Press_Mask);
      --
      -- Focus out for the popup window will be detected by catching the
      -- mouse  button  press  events. The pointer will be grabbed later
      -- on. When a button get clicked Button_Press_Popup will determine
      -- if it was outside the window.
      --
      Focus_Handlers.Connect
      (  Widget.Popup,
         "button_press_event",
         Focus_Handlers.To_Marshaller (Button_Press_Popup'Access),
         Widget.all'Access
      );
      if Widget.Is_Renderer then
         Widget.Popup.Move (Widget.X, Widget.Y);
      else
         declare
            X, Y : GInt;
         begin
            Get_Screen_Position (Widget, X, Y);
            Widget.Popup.Move (X, Y);
         end;
      end if;
      Widget.Popup.Set_Screen (Widget.Get_Screen);
      Widget.Popup.Set_Type_Hint (Window_Type_Hint_Combo);
      if Window /= null then
         Widget.Popup.Get_Group.Add_Window (Window);
         Widget.Popup.Set_Transient_For (Window);
      end if;
      -- Frame around the popup window
      Gtk_New (Frame);
      Widget.Popup.Add (Frame);
      Frame.Set_Shadow_Type (Shadow_Out);
      Frame.Show;
      -- Creating the selection widget
      if Widget.Constrained then
         Gtk_New
         (  Widget.Selection,
            Widget.Constraint,
            Get_Text (Widget)
         );
      else
         Gtk_New (Widget.Selection, Get_Text (Widget));
      end if;
      Get_Tree_View (Widget.Selection).Set_Headers_Visible
      (  Style_Get (Widget, "has-header")
      );
      Frame.Add (Widget.Selection);
      Edit_Handlers.Connect     -- Then we'll be ready to grab focus
      (  Widget,
         "destroy",
         Edit_Handlers.To_Marshaller (Destroy'Access),
         Widget.all'Access
      );
      Edit_Handlers.Connect	-- Editing done
      (  Widget.Selection,
         "commit",
         Edit_Handlers.To_Marshaller (Commit'Access),
         Widget.all'Access
      );
      Edit_Handlers.Connect	-- Editing cancelled
      (  Widget.Selection,
         "reject",
         Edit_Handlers.To_Marshaller (Reject'Access),
         Widget.all'Access
      );
      Widget.Selection.Show;
      Widget.Popup.Show;
      Widget.Popup.Grab_Focus;
      Widget.Popup.Set_Modal;
      Dummy :=
         Pointer_Grab
         (  Widget.Popup.Get_Window,
            True,
            Button_Press_Mask
         );
      Dummy := Keyboard_Grab (Get_Window (Widget.Popup));
      Widget.Popup.Present;
      Widget.Ref;
      return True;
   end Button_Press;

   function Button_Press_Popup
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Edit   : Gtk_Unit_Entry
            )  return Boolean is
      This  : constant Gtk_Widget := Widget.all'Access;
      Child : Gtk_Widget := Gtk.Main.Get_Event_Widget (Event);
   begin
      if Child /= This then
         while Child /= null loop
            Child := Get_Parent (Child);
            if Child = This then
               return False;
            end if;
         end loop;
      end if;
      --
      -- Mouse  button  press  happened  ouside  any  of  the   widget's
      -- children, that means outside the popup  window.  So  we  cancel
      -- editing
      --
      Edit.Done_Popup;
      return True;
   end Button_Press_Popup;

   procedure Commit
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Unit_Entry
             )  is
      Result : Measure;
      Text   : constant String := Edit.Selection.Get_Entry.Get_Text;
   begin
      if Edit.Numeric then
         Result := Edit.Get (Text, (Edit.Constraint, 1.0, 0.0));
      else
         Result := Edit.Get (Text);
      end if;
      Edit.Set_Text (Text);
      Edit.Canceled := False;
      Edit.Done_Popup;
   exception
      when Constraint_Error | Ada.IO_Exceptions.Data_Error |
           Unit_Error | Ada.IO_Exceptions.End_Error =>
         Beep (Edit.Selection);
      when others =>
         Edit.Done_Popup;
   end Commit;

   procedure Done_Popup
             (  Widget : not null access Gtk_Unit_Entry_Record
             )  is
   begin
      Pointer_Ungrab;
      Keyboard_Ungrab;
      Widget.Popup.Destroy;
      Editing_Done  (Get_Object (Widget));
      Remove_Widget (Get_Object (Widget));
      Widget.Popup := null;
      Widget.Unref;
   end Done_Popup;

   function Editing_Canceled
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Boolean is
   begin
      return Widget.Canceled;
   end Editing_Canceled;

   function Get
            (  Widget : not null access Gtk_Unit_Entry_Record'Class;
               Text   : UTF8_String;
               Scale  : Measure
            )  return Measure is
      Result  : Scaled;
      Pointer : Integer := Text'First;
   begin
      if Widget.Constrained and then Scale.SI /= Widget.Constraint then
         raise Constraint_Error;
      end if;
      Get (Text, Pointer, SpaceAndTab);
      UTF8_Edit.Get (Text, Pointer, Result);
      Get (Text, Pointer, SpaceAndTab);
      if Pointer <= Text'Last then
         raise Data_Error;
      end if;
      case Result.Format is
         when Scalar | Numeric =>
            return Result.Numeral * Result.Scale * Scale;
         when Canonic | Jumbled =>
            if (  Widget.Constrained
                and then
                  Result.Scale.SI /= Widget.Constraint
               )
            then
               raise Constraint_Error;
            else
               return Result.Numeral * Result.Scale;
            end if;
      end case;
   end Get;

   function Get
            (  Widget : not null access Gtk_Unit_Entry_Record'Class;
               Text   : UTF8_String
            )  return Measure is
      Result : constant Measure := UTF8_Edit.Value (Text);
   begin
      if Widget.Constrained and then Result.SI /= Widget.Constraint then
         raise Constraint_Error;
      end if;
      return Result;
   end Get;

   function Get
            (  Widget : not null access Gtk_Unit_Entry_Record;
               Scale  : Measure
            )  return Measure is
   begin
      return Widget.Get (Widget.Get_Text, Scale);
   end Get;

   function Get (Widget : not null access Gtk_Unit_Entry_Record)
      return Measure is
   begin
      return Widget.Get (Widget.Get_Text);
   end Get;

   function Get_Constraint
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Unit is
   begin
      if Widget.Constrained then
         return Widget.Constraint;
      else
         raise Constraint_Error;
      end if;
   end Get_Constraint;

   function Get_Type return Gtk_Type is
      Editable : Gtk_Cell_Editable_Iface_Ptr;
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.GEntry.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name & "Entry"
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boolean
            (  Name    => "has-header",
               Nick    => "Has header",
               Blurb   => "Allow columns header in selection window",
               Default => False
         )  );
      end if;
      Editable :=
         Interface_Peek (Class_Peek (Class_Record.The_Type));
      Editable.Start_Editing := Start_Editing_Ptr;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget     : out Gtk_Unit_Entry;
                Constraint : Unit;
                Initial    : GLib.UTF8_String := ""
             )  is
   begin
      Widget := new Gtk_Unit_Entry_Record;
      Initialize (Widget, Constraint, Initial);
   exception
      when others =>
         Free (Widget);
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Unit_Entry;
                Initial : GLib.UTF8_String := ""
             )  is
   begin
      Widget := new Gtk_Unit_Entry_Record;
      Initialize (Widget, Initial);
   exception
      when others =>
         Free (Widget);
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access Gtk_Unit_Entry_Record'Class;
                Constraint : Unit;
                Initial    : GLib.UTF8_String := ""
             )  is
   begin
      Initialize (Widget, Initial);
      Widget.Constrained := True;
      Widget.Constraint  := Constraint;
   end Initialize;

   procedure Initialize
             (  Widget  : not null access Gtk_Unit_Entry_Record'Class;
                Initial : GLib.UTF8_String := ""
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.GEntry.Initialize (Widget);
      Widget.Set_Text (Initial);
      Widget.Add_Events (Enter_Notify_Mask);
      Widget.Set_Editable (False);
      Press_Handlers.Connect
      (  Widget,
         "button_press_event",
         Press_Handlers.To_Marshaller (Button_Press'Access)
      );
   end Initialize;

   function Is_Constrained
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Boolean is
   begin
      return Widget.Constrained;
   end Is_Constrained;

   function Is_Numeric
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Boolean is
   begin
      return Widget.Numeric;
   end Is_Numeric;

   procedure Reject
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Unit_Entry
             )  is
   begin
      Done_Popup (Edit);
   end Reject;

   procedure Reset_Constraint
             (  Widget : not null access Gtk_Unit_Entry_Record
             )  is
   begin
      Widget.Constrained := False;
   end Reset_Constraint;

   procedure Set_Constraint
             (  Widget     : not null access Gtk_Unit_Entry_Record;
                Constraint : Unit
             )  is
   begin
      Widget.Constrained := True;
      Widget.Constraint  := Constraint;
   end Set_Constraint;

   procedure Set_Numeric
             (  Widget  : not null access Gtk_Unit_Entry_Record;
                Allowed : Boolean
             )  is
   begin
      Widget.Numeric := Allowed;
   end Set_Numeric;

   procedure Start_Editing
             (  Cell_Editable : System.Address;
                Event         : Gdk_Event
             )  is
      Ptr : constant GObject := Convert (Cell_Editable);
   begin
      if (  Ptr /= null
         and then
            Ptr.all in Gtk_Unit_Entry_Record'Class
         and then
            Button_Press
            (  Gtk_Unit_Entry_Record'Class (Ptr.all)'Access,
               Event
         )  )
      then
         null;
      end if;
   end Start_Editing;

end Measures_Gtk_Edit.GEntry;
