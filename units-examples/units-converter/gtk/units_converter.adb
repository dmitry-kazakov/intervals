--                                                                    --
--  procedure Unit_Converter        Copyright (c)  Dmitry A. Kazakov  --
--  Sample program for GTK+                        Luebeck            --
--  Implementation                                 Autumn, 2004       --
--                                                                    --
--                                Last revision :  09:43 08 Oct 2016  --
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

with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
--
-- Things related to Gtk-API
--
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Frame;   use Gtk.Frame;
with Gtk.Button;  use Gtk.Button;
with Gtk.GEntry;  use Gtk.GEntry;
with Gtk.Label;   use Gtk.Label;
with Gtk.Table;   use Gtk.Table;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Window;  use Gtk.Window;

with Ada.Unchecked_Conversion;
with Gtk.Main;
with Gtk.Missed;
--
-- This is what we need for unit conversions
--
with Float_Measures;            use Float_Measures;
with Float_Measures_UTF8_Edit;  use Float_Measures_UTF8_Edit;
with Units;                     use Units;

procedure Units_Converter is
--
-- Read_Only_Text -- Sunken labels
--
   type Read_Only_Text is record
      Text  : Gtk_Label;
      Frame : Gtk_Frame;
   end record;
--
-- Gtk_New -- Initializes the label
--
   procedure Gtk_New
             (  Text  : in out Read_Only_Text;
                Label : String := ""
             )  is
   begin
      Gtk_New (Text.Frame);
      Text.Frame.Set_Shadow_Type (Shadow_In);
      Gtk_New (Text.Text, Label);
      Text.Text.Set_Halign (Align_Start);
      Text.Text.Set_Valign (Align_Center);
      Text.Frame.Add (Text.Text);
   end Gtk_New;
--
-- Show -- The label
--
   procedure Show (Text : in out Read_Only_Text) is
   begin
      Text.Text.Show;
      Text.Frame.Show;
   end Show;

   Window              : Gtk_Window;
   Grid                : Gtk_Table;
   Label1              : Gtk_Label;
   Label2              : Gtk_Label;
   Label3              : Gtk_Label;
   Value_To_Convert    : Gtk_Entry;
   Value_In_SI         : Gtk_Entry;
   Value_In_Base_Units : Gtk_Entry;
   Button              : Gtk_Button;
   Message             : Read_Only_Text;
--
-- Conversions of callbacks to circumvent accessibility checks
--
   type Local_Handler is access procedure
        (  Widget : not null access Gtk_Widget_Record'Class
        );
   function "+" is
      new Ada.Unchecked_Conversion (Local_Handler, Cb_Gtk_Button_Void);
   function "+" is
      new Ada.Unchecked_Conversion (Local_Handler, Cb_Gtk_Entry_Void);
--
-- Go -- When the Go button gets pressed
--
--    Widget - The button
--
   procedure Go (Widget : not null access Gtk_Widget_Record'Class) is
      Number : Measure;
   begin
      Value_In_SI.Set_Text ("");
      Value_In_Base_Units.Set_Text ("");
      Message.Text.Set_Text ("");
      Number := Value (Value_To_Convert.Get_Text);
      Value_In_SI.Set_Text (Image (Number));
      Value_In_Base_Units.Set_Text (Image (Number, Derived => False));
   exception
      when Constraint_Error =>
         Message.Text.Set_Text ("Numeric error");
      when Data_Error =>
         Message.Text.Set_Text ("Syntax error");
      when End_Error =>
         Message.Text.Set_Text ("Nothing recognized");
      when Unit_Error =>
         Message.Text.Set_Text ("Unit error");
   end Go;

begin
   --
   -- Initialization
   --
   Gtk.Main.Init;
   --
   -- Creating the main window and handle its events
   --
   Gtk.Window.Gtk_New (Window);
   Window.Set_Title ("Unit conversion (Ada95 GTK+)");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Window.Set_Border_Width (10);
   --
   -- Creating the grid, a table to align all other widgets
   --
   Gtk_New (Grid, 3, 4, False);
   Grid.Set_Row_Spacings (3);
   Grid.Set_Col_Spacings (3);
   Window.Add (Grid);
   --
   -- The left column are labels
   --
   Gtk_New (Label1, "Value to convert" & LF & "For example 23.5 bar");
   Grid.Attach_Defaults (Label1, 0, 1, 0, 1);
   Label1.Set_Justify (Justify_Right);
   Label1.Set_Halign (Align_End);
   Label1.Set_Valign (Align_Center);
   Gtk_New (Label2, "SI equivalent");
   Grid.Attach_Defaults (Label2, 0, 1, 1, 2);
   Label2.Set_Halign (Align_End);
   Label2.Set_Valign (Align_Center);
   Gtk_New (Label3, "Base units only");
   Grid.Attach_Defaults (Label3, 0, 1, 3, 4);
   Label3.Set_Halign (Align_End);
   Label3.Set_Valign (Align_Center);
   --
   -- The central column is the edit fields
   --
   Gtk_New (Value_To_Convert);
   Value_To_Convert.On_Activate (+Go'Access);
   Grid.Attach_Defaults (Value_To_Convert, 1, 2, 0, 1);
   Gtk_New (Value_In_SI);
   Grid.Attach_Defaults (Value_In_SI, 1, 2, 1, 2);
   Gtk_New (Value_In_Base_Units);
   Grid.Attach_Defaults (Value_In_Base_Units, 1, 2, 3, 4);
   --
   -- The right column is the button Go
   --
   Gtk_New (Button, " Go ");
   Button.On_Clicked (+Go'Access);
   Attach_Defaults (Grid, Button, 3, 4, 0, 4);
   --
   -- Error messages is beneath
   --
   Gtk_New (Message, "Source: www.dmitry-kazakov.de/ada/units.htm");
   Grid.Attach_Defaults (Message.Frame, 0, 4, 4, 5);
   --
   -- Show everything
   --
   Window.Show_All;
   --
   -- Enter the events processing loop
   --
   Gtk.Main.Main;
end Units_Converter;
