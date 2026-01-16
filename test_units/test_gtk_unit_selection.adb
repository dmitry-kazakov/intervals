--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Unit_Selection                     Luebeck            --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with GLib;                         use GLib;
with GLib.Values;                  use GLib.Values;
with Gtk.Float_Measures_Dialogs;   use Gtk.Float_Measures_Dialogs;
with Gtk.Float_Measures_Entry;     use Gtk.Float_Measures_Entry;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Button;                   use Gtk.Button;
with Gtk.Cell_Renderer;            use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Label;                    use Gtk.Label;
with Gtk.List_Store;               use Gtk.List_Store;
with Gtk.Paned;                    use Gtk.Paned;
with Gtk.Table;                    use Gtk.Table;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Tree_Selection;           use Gtk.Tree_Selection;
with Gtk.Tree_Store;               use Gtk.Tree_Store;
with Gtk.Tree_View;                use Gtk.Tree_View;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Window;                   use Gtk.Window;
with Gtk.Widget;                   use Gtk.Widget;
with Gtk.Widget.Styles.CSS_Store;  use Gtk.Widget.Styles.CSS_Store;
with Strings_Edit.UTF8.Handling;   use Strings_Edit.UTF8.Handling;

with Ada.Unchecked_Conversion;
with Units.Base;

with Gtk.Main.Router;
with Gtk.Missed;
with System;

with Gtk.Cell_Renderer.Abstract_Renderer;
use  Gtk.Cell_Renderer.Abstract_Renderer;

with Gtk.Float_Measures_Cell_Renderer;
use  Gtk.Float_Measures_Cell_Renderer;

procedure Test_Gtk_Unit_Selection is

   File_Name : constant String := "test_gtk_widgets.css-file";

   type Local_Callback is access function return Gtk_Widget;
   function Call is
      new Ada.Unchecked_Conversion (System.Address, Local_Callback);

   type Local_Handler is access
      procedure (Widget : access Gtk_Button_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion (Local_Handler, Cb_Gtk_Button_Void);

   type Local_Selection_Handler is access
      procedure (Selection : access Gtk_Tree_Selection_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Selection_Handler,
             Cb_Gtk_Tree_Selection_Void
          );

   type Local_Commit_Handler is access
      procedure (Cell : access Gtk_Abstract_Renderer_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Commit_Handler,
             Commit_Callback
          );

   Window        : Gtk_Window;
   Pane          : Gtk_HPaned;
   Test_Widget   : Gtk_Widget;
   Label         : Gtk_Label;
   List          : Gtk_List_Store;
   Box           : Gtk_Box;
   Get_RC_Button : Gtk_Button;

   procedure On_Get_RC (Button : access Gtk_Button_Record'Class) is
      File : File_Type;
   begin
      if Test_Widget /= null then
         Create (File, Out_File, File_Name);
         Put_CSS_Styles (File, Test_Widget);
         Close (File);
         Set_Label
         (  Get_RC_Button,
            File_Name & " has been written. Press to rewrite"
         );
      end if;
   end On_Get_RC;

   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class
             )  is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Value : GValue;
   begin
      Get_Selected (Selection, Model, Iter);
      if Iter /= Null_Iter and then not Has_Child (Model, Iter) then
         Get_Value (Model, Iter, 1, Value);
         if Test_Widget /= null then
            Remove (Pane, Test_Widget);
            Test_Widget := null;
         end if;
         Test_Widget := Call (Get_Address (Value)).all;
         Add2 (Pane, Test_Widget);
         Show_All (Test_Widget);
         Unset (Value);
      end if;
   end On_Selection;
--
-- Individual tests
--
   procedure Test_1_Button_1
             (  Button : access Gtk_Button_Record'Class
             )  is
   begin
      Label.Set_Text ("SI input constrained to length");
      Label.Set_Text
      (  Get
         (  Constraint => Units.Base.Length,
            Initial    => "km",
            Parent     => Window
      )  );
   exception
      when End_Error =>
         Label.Set_Text ("Cancelled");
   end Test_1_Button_1;

   procedure Test_1_Button_2
             (  Button : access Gtk_Button_Record'Class
             )  is
   begin
      Label.Set_Text ("Mass, a special case");
      Label.Set_Text
      (  Get
         (  Constraint => Units.Base.Mass,
            Initial    => "mg",
            Parent     => Window
      )  );
   exception
      when End_Error =>
         Label.Set_Text ("Cancelled");
   end Test_1_Button_2;

   procedure Test_1_Button_3
             (  Button : access Gtk_Button_Record'Class
             )  is
   begin
      Label.Set_Text ("Irregular unit, unconstrained");
      Label.Set_Text (Get (Initial => "foot", Parent => Window));
   exception
      when End_Error =>
         Label.Set_Text ("Cancelled");
   end Test_1_Button_3;

   procedure Test_1_Button_4
             (  Button : access Gtk_Button_Record'Class
             )  is
   begin
      Label.Set_Text ("Unconstrained input");
      Label.Set_Text (Get (Parent => Window));
   exception
      when End_Error =>
         Label.Set_Text ("Cancelled");
   end Test_1_Button_4;

   procedure Test_1_Button_5
             (  Button : access Gtk_Button_Record'Class
             )  is
   begin
      Label.Set_Text ("Constrained unitless input");
      Label.Set_Text
      (  Get
         (  Constraint => Units.Base.Unitless,
            Initial    => "%",
            Parent     => Window
      )  );
   exception
      when End_Error =>
         Label.Set_Text ("Cancelled");
   end Test_1_Button_5;

   function Test_1 return Gtk_Widget is
      Table  : Gtk_Table;
      Button : Gtk_Button;
   begin
      Gtk_New (Table, 2, 5, False);
      if Label = null then
         Gtk_New (Label, "");
         Label.Ref;
      end if;
      Attach (Table, Label, 1, 5, 1, 2);

      Gtk_New (Button, "km");
      Button.On_Clicked (+Test_1_Button_1'Access);
      Table.Attach (Button, 0, 1, 0, 1, YOptions => 0);

      Gtk_New (Button, "mg");
      Button.On_Clicked (+Test_1_Button_2'Access);
      Table.Attach (Button, 1, 2, 0, 1, YOptions => 0);

      Gtk_New (Button, "foot");
      Button.On_Clicked (+Test_1_Button_3'Access);
      Table.Attach (Button, 2, 3, 0, 1, YOptions => 0);

      Gtk_New (Button, "any");
      Button.On_Clicked (+Test_1_Button_4'Access);
      Table.Attach (Button, 3, 4, 0, 1, YOptions => 0);

      Gtk_New (Button, "%");
      Button.On_Clicked (+Test_1_Button_5'Access);
      Table.Attach (Button, 4, 5, 0, 1, YOptions => 0);
      return Table.all'Access;
   end Test_1;

   function Test_2 return Gtk_Widget is
      Table : Gtk_Table;
      Edit  : Gtk_Unit_Entry;
   begin
      Gtk_New (Table, 2, 5, False);
      if Label = null then
         Gtk_New (Label, "");
         Label.Ref;
      end if;
      Attach (Table, Label, 1, 5, 1, 2);

      Gtk_New (Edit, Units.Base.Length, "km");
      Attach (Table, Edit, 0, 1, 0, 1, YOptions => 0);

      Gtk_New (Edit, Units.Base.Mass, "mg");
      Attach (Table, Edit, 1, 2, 0, 1, YOptions => 0);

      Gtk_New (Edit, Units.Base.Unitless, "%");
      Attach (Table, Edit, 2, 3, 0, 1, YOptions => 0);

      Gtk_New (Edit, "foot");
      Attach (Table, Edit, 3, 4, 0, 1, YOptions => 0);

      Gtk_New (Edit);
      Attach (Table, Edit, 4, 5, 0, 1, YOptions => 0);
      return Table.all'Access;
   end Test_2;

   procedure Test_3_Edit_0
             (  Cell : access Gtk_Abstract_Renderer_Record'Class
             )  is
      Row   : constant Gtk_Tree_Iter :=
                       List.Get_Iter_From_String (Cell.Get_Path);
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value := Gtk_Cell_Renderer_Measure_Record'Class (Cell.all).Get;
         List.Set_Value (Row, 0, Value);
         Unset (Value);
      end if;
   end Test_3_Edit_0;

   procedure Test_3_Edit_1
             (  Cell : access Gtk_Abstract_Renderer_Record'Class
             )  is
      Row   : constant Gtk_Tree_Iter :=
                       List.Get_Iter_From_String (Cell.Get_Path);
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Value := Gtk_Cell_Renderer_Measure_Record'Class (Cell.all).Get;
         List.Set_Value (Row, 1, Value);
         Unset (Value);
      end if;
   end Test_3_Edit_1;

   function Test_3 return Gtk_Widget is
      Table   : Gtk_Tree_View;
      Row     : Gtk_Tree_Iter := Null_Iter;
      Value   : GValue;
      Element : GDouble := -1.0;
   begin
      Gtk_New (Table);
      Table.Set_Rules_Hint (True);
      if List = null then
         Gtk_New (List, (GType_Double, GType_Double));
         Init (Value, GType_Double);
         for I in 1..5 loop
            Append (List, Row);
            Set_Double (Value, Element);
            Set_Value (List, Row, 0, Value);
            Element := Element + 11.1;
            Set_Double (Value, Element);
            Set_Value (List, Row, 1, Value);
            Element := Element + 12.1;
         end loop;
         Unset (Value);
      end if;
      declare
         Column    : Gtk_Tree_View_Column;
         Renderer  : Gtk_Cell_Renderer_Measure;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Renderer, To_UTF8 (String'("°C")), 2);
         Set_Mode (Renderer, Cell_Renderer_Mode_Editable);
         Pack_Start (Column, Renderer, True);
         Add_Attribute (Column, Renderer, "value", 0);
         Column_No := Append_Column (Table, Column);
         Set_Title (Column, "Temperature");
         Set_Resizable (Column, True);
         Renderer.On_Commit (+Test_3_Edit_0'Access);

         Gtk_New (Column);
         Gtk_New (Renderer, "", 1);
         Set_Mode (Renderer, Cell_Renderer_Mode_Editable);
         Pack_Start (Column, Renderer, True);
         Add_Attribute (Column, Renderer, "value", 1);
         Column_No := Append_Column (Table, Column);
         Set_Title (Column, "Dimensionless");
         Set_Resizable (Column, True);
         Renderer.On_Commit (+Test_3_Edit_1'Access);
      end;
      Table.Set_Model (To_Interface (List));
      return Table.all'Access;
   end Test_3;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Gtk.Window.Set_Default_Size (Window, 600, 400);
   Window.Set_Title ("Test units GTK+ widgets");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Window.Set_Border_Width (10);
   Gtk_New_HPaned (Pane);
   -- The list of tests
   declare
      Scroll : Gtk_Scrolled_Window;
      View   : Gtk_Tree_View;
      List   : Gtk_Tree_Store;
      Row    : Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter := Null_Iter;
      Ptr    : GValue;

      procedure Add_Test (Name : String; Test : System.Address) is
      begin
         Append (List, Row, Parent);
         Set (List, Row, 0, Name);
         Set_Address (Ptr, Test);
         Set_Value (List, Row, 1, Ptr);
      end Add_Test;
   begin
      Init (Ptr, GType_Pointer);
      Gtk_New (View);
      Gtk_New (List, (GType_String, GType_Pointer));
      -- Creating the list of tests
         -- Unit dialogs
         Append (List, Parent, Null_Iter);
         Set (List, Parent, 0, "Unit selection dialogs");
            Add_Test ("Measures_Gtk_Edit.Dialogs", Test_1'Address);
         -- Unit widgets
         Append (List, Parent, Null_Iter);
         Set (List, Parent, 0, "Unit selection widgets");
            Add_Test ("Measures_Gtk_Edit.GEntry", Test_2'Address);
         -- Unit renderers
         Append (List, Parent, Null_Iter);
         Set (List, Parent, 0, "Unit cell renderers");
            Add_Test
            (  "Measures_Gtk_Edit.GEntry.Cell_Renderer",
               Test_3'Address
            );
         -- Done with the list
      Unset (Ptr);
      Set_Rules_Hint (View, True);
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Text);
         Pack_Start (Column, Text, True);
         Add_Attribute (Column, Text, "text", 0);
         Column_No := Append_Column (View, Column);
         Set_Title (Column, "Test");
         Set_Resizable (Column, True);
         Set_Sort_Column_Id (Column, 0);
      end;
      Set_Mode (Get_Selection (View), Selection_Single);
      View.Set_Model (To_Interface (List));
      View.Expand_All;
      Get_Selection (View).On_Changed (+On_Selection'Access);
      Gtk_New (Scroll);
      Scroll.Add (View);
      Pane.Add1 (Scroll);
   end;
   Gtk_New_VBox (Box);
   Gtk_New (Get_RC_Button);
   Get_RC_Button.Set_Label ("Take CSS file from");
   Box.Pack_Start (Pane);
   Box.Pack_Start (Get_RC_Button, False, False);
   Get_RC_Button.On_Clicked (+On_Get_RC'Access);
   Window.Add (Box);
   Box.Show_All;
   Window.Show;
   Gtk.Main.Main;
exception
   when Error : others =>
      Gtk.Main.Router.Say (Exception_Information (Error));
end Test_Gtk_Unit_Selection;
