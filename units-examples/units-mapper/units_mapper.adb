--                                                                    --
--  procedure Units_Mapper          Copyright (c)  Dmitry A. Kazakov  --
--  Sample program for GTK+                        Luebeck            --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
--
-- Things related to Gtk-API
--
with Gtk.Editable;      use Gtk.Editable;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Table;         use Gtk.Table;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;

with Ada.Unchecked_Conversion;
with Gtk.Main;
with Gtk.Missed;
--
-- This is what we need for unit conversions
--
with Gtk.Float_Measures_Entry;  use Gtk.Float_Measures_Entry;
with Float_Measures;	        use Float_Measures;
with Strings_Edit.Floats;       use Strings_Edit.Floats;
with Units;                     use Units;

procedure Units_Mapper is

   Window     : Gtk_Window;
   Grid       : Gtk_Table;
   From_Unit  : Gtk_Unit_Entry;
   From_Value : Gtk_Entry;
   To_Unit    : Gtk_Unit_Entry;
   To_Value   : Gtk_Entry;
   Ignore     : Boolean := False;
--
-- Conversions of callbacks to circumvent accessibility checks
--
   type Local_Callback is access procedure (Cell : Gtk_Editable);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Callback,
             Cb_Gtk_Editable_Void
          );

   procedure Changed_From (Widget : Gtk_Editable) is
   begin
      if Ignore then
         return;
      end if;
      Ignore := True;
      begin
         To_Value.Set_Text
         (  Image
            (  Get_Value_As
               (  Value (From_Value.Get_Text) * From_Unit.Get,
                  To_Unit.Get
         )  )  );
      exception
         when Data_Error =>
            To_Value.Set_Text ("Not a number");
         when End_Error | Constraint_Error =>
            To_Value.Set_Text ("");
         when Unit_Error =>
            To_Value.Set_Text ("Unit error");
      end;
      Ignore := False;
   end Changed_From;

   procedure Changed_To (Widget : Gtk_Editable) is
   begin
      if Ignore then
         return;
      end if;
      Ignore := True;
      begin
         From_Value.Set_Text
         (  Image
            (  Get_Value_As
               (  Value (To_Value.Get_Text) * To_Unit.Get,
                  From_Unit.Get
         )  )  );
      exception
         when Data_Error =>
            From_Value.Set_Text ("Not a number");
         when End_Error | Constraint_Error =>
            From_Value.Set_Text ("");
         when Unit_Error =>
            From_Value.Set_Text ("Unit error");
      end;
      Ignore := False;
   end Changed_To;

   procedure Changed_Unit (Widget : Gtk_Editable) is
   begin
      To_Unit.Set_Text (From_Unit.Get_Text);
      To_Unit.Set_Constraint (From_Unit.Get.SI);
      Changed_From (Widget);
   exception
      when End_Error =>
         null;
   end Changed_Unit;

begin
   --
   -- Initialization
   --
   Gtk.Main.Init;
   --
   -- Creating the main window and handle its events
   --
   Gtk_New (Window);
   Window.Set_Title ("Unit mapper (Ada GTK+)");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Window.Set_Border_Width (10);
   --
   -- Creating the grid, a table to align all other widgets
   --
   Gtk_New (Grid, 2, 2, False);
   Grid.Set_Row_Spacings (3);
   Grid.Set_Col_Spacings (3);
   Window.Add (Grid);
   --
   -- The left column are labels
   --
   Gtk_New (From_Value);
   From_Value.Set_Tooltip_Text ("First value to convert");
   Grid.Attach_Defaults (From_Value, 0, 1, 0, 1);
   On_Changed (+From_Value, +Changed_From'Access);

   Gtk_New (From_Unit);
   From_Unit.Set_Tooltip_Text ("First value unit");
   Grid.Attach_Defaults (From_Unit, 1, 2, 0, 1);
   On_Changed (+From_Unit, +Changed_Unit'Access);

   Gtk_New (To_Value);
   To_Value.Set_Tooltip_Text ("Second value to convert");
   Grid.Attach_Defaults (To_Value, 0, 1, 1, 2);
   On_Changed (+To_Value, +Changed_To'Access);

   Gtk_New (To_Unit);
   To_Unit.Set_Tooltip_Text ("Second value unit");
   Grid.Attach_Defaults (To_Unit, 1, 2, 1, 2);
   On_Changed (+To_Unit, +Changed_From'Access);
   --
   -- Show everything
   --
   Window.Show_All;
   --
   -- Enter the events processing loop
   --
   Gtk.Main.Main;
end Units_Mapper;
