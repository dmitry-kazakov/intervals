--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Gtk_Edit.Dialogs                   Luebeck            --
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

with Gtk.Missed;  use Gtk.Missed;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;

package body Measures_Gtk_Edit.Dialogs is

   procedure Initialize
             (  View    : Gtk_Unit_Selection;
                Dialog  : out Gtk_Dialog;
                Title   : GLib.UTF8_String;
                Confirm : GLib.UTF8_String;
                Cancel  : GLib.UTF8_String;
                Parent  : Gtk_Window;
                Flags   : Gtk_Dialog_Flags
             )  is
   begin
      Gtk_New (Dialog, Title, Parent, Flags);
      Dialog.Get_Content_Area.Pack_Start (View);
      View.Show_All;
      Add_Button_From_Stock
      (  Dialog   => Dialog,
         Response => Gtk_Response_OK,
         Icon     => Stock_OK,
         Label    => Confirm
      );
      if Cancel'Length /= 0 then
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_Cancel,
            Icon     => Stock_Cancel,
            Label    => Cancel
         );
      end if;
      Commit_Handlers.Connect
      (  View,
         "commit",
         Commit_Handlers.To_Marshaller (On_Commit'Access),
         Dialog
      );
      Commit_Handlers.Connect
      (  View,
         "reject",
         Commit_Handlers.To_Marshaller (On_Reject'Access),
         Dialog
      );
   end Initialize;

   function Run
            (  View         : Gtk_Unit_Selection;
               Dialog       : Gtk_Dialog;
               Title        : GLib.UTF8_String;
               Missing      : GLib.UTF8_String;
               Erroneous    : GLib.UTF8_String;
               Incompatible : GLib.UTF8_String
            )  return GLib.UTF8_String is
      procedure Say (Message : GLib.UTF8_String) is
      begin
         Message_Dialog
         (  Message => Message,
            Title   => Title,
            Parent  => Dialog
         );
      end Say;
   begin
      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               begin
                  declare
                     Result : constant GLib.UTF8_String := Get (View);
                  begin
                     GLib.Object.Checked_Destroy (Dialog);
                     return Result;
                  end;
               exception
                  when Constraint_Error =>
                     Say (Incompatible);
                  when Ada.IO_Exceptions.Data_Error | Unit_Error =>
                     Say (Erroneous);
                  when Ada.IO_Exceptions.End_Error =>
                     Say (Missing);
                  when others =>
                     GLib.Object.Checked_Destroy (Dialog);
                     raise;
               end;
            when others =>
               GLib.Object.Checked_Destroy (Dialog);
               raise Ada.IO_Exceptions.End_Error;
         end case;
      end loop;
   end Run;

   function Run
            (  View         : Gtk_Unit_Selection;
               Dialog       : Gtk_Dialog;
               Title        : GLib.UTF8_String;
               Missing      : GLib.UTF8_String;
               Erroneous    : GLib.UTF8_String;
               Incompatible : GLib.UTF8_String
            )  return Measure is
      procedure Say (Message : GLib.UTF8_String) is
      begin
         Message_Dialog
         (  Message => Message,
            Title   => Title,
            Parent  => Dialog
         );
      end Say;
   begin
      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               begin
                  declare
                     Result : constant Measure := Get (View);
                  begin
                     GLib.Object.Checked_Destroy (Dialog);
                     return Result;
                  end;
               exception
                  when Constraint_Error =>
                     Say (Incompatible);
                  when Ada.IO_Exceptions.Data_Error | Unit_Error =>
                     Say (Erroneous);
                  when Ada.IO_Exceptions.End_Error =>
                     Say (Missing);
                  when others =>
                     GLib.Object.Checked_Destroy (Dialog);
                     raise;
               end;
            when others =>
               GLib.Object.Checked_Destroy (Dialog);
               raise Ada.IO_Exceptions.End_Error;
         end case;
      end loop;
   end Run;

   function Get
            (  Initial      : GLib.UTF8_String := "";
               Title        : GLib.UTF8_String := "Unit selection";
               Confirm      : GLib.UTF8_String := Stock_OK;
               Cancel       : GLib.UTF8_String := Stock_Cancel;
               Parent       : Gtk_Window       := null;
               Flags        : Gtk_Dialog_Flags := Modal;
               Missing      : GLib.UTF8_String := "No unit specified";
               Erroneous    : GLib.UTF8_String := "Illegal unit";
               Incompatible : GLib.UTF8_String := "Incompatible unit"
            )  return GLib.UTF8_String is
      View   : Gtk_Unit_Selection;
      Dialog : Gtk_Dialog;
   begin
      Gtk_New (View, Initial);
      Initialize (View, Dialog, Title, Confirm, Cancel, Parent, Flags);
      return
         Run (View, Dialog, Title, Missing, Erroneous, Incompatible);
   end Get;

   function Get
            (  Constraint   : Unit;
               Initial      : GLib.UTF8_String := "";
               Title        : GLib.UTF8_String := "Unit selection";
               Confirm      : GLib.UTF8_String := Stock_OK;
               Cancel       : GLib.UTF8_String := Stock_Cancel;
               Parent       : Gtk_Window       := null;
               Flags        : Gtk_Dialog_Flags := Modal;
               Missing      : GLib.UTF8_String := "No unit specified";
               Erroneous    : GLib.UTF8_String := "Illegal unit";
               Incompatible : GLib.UTF8_String := "Incompatible unit"
            )  return GLib.UTF8_String is
      View   : Gtk_Unit_Selection;
      Dialog : Gtk_Dialog;
   begin
      Gtk_New (View, Constraint, Initial);
      Initialize (View, Dialog, Title, Confirm, Cancel, Parent, Flags);
      return
         Run (View, Dialog, Title, Missing, Erroneous, Incompatible);
   end Get;

   function Get
            (  Initial      : GLib.UTF8_String := "";
               Title        : GLib.UTF8_String := "Unit selection";
               Confirm      : GLib.UTF8_String := Stock_OK;
               Cancel       : GLib.UTF8_String := Stock_Cancel;
               Parent       : Gtk_Window       := null;
               Flags        : Gtk_Dialog_Flags := Modal;
               Missing      : GLib.UTF8_String := "No unit specified";
               Erroneous    : GLib.UTF8_String := "Illegal unit";
               Incompatible : GLib.UTF8_String := "Incompatible unit"
            )  return Measure is
      View   : Gtk_Unit_Selection;
      Dialog : Gtk_Dialog;
   begin
      Gtk_New (View, Initial);
      Initialize (View, Dialog, Title, Confirm, Cancel, Parent, Flags);
      return
         Run (View, Dialog, Title, Missing, Erroneous, Incompatible);
   end Get;

   function Get
            (  Constraint   : Unit;
               Initial      : GLib.UTF8_String := "";
               Title        : GLib.UTF8_String := "Unit selection";
               Confirm      : GLib.UTF8_String := Stock_OK;
               Cancel       : GLib.UTF8_String := Stock_Cancel;
               Parent       : Gtk_Window       := null;
               Flags        : Gtk_Dialog_Flags := Modal;
               Missing      : GLib.UTF8_String := "No unit specified";
               Erroneous    : GLib.UTF8_String := "Illegal unit";
               Incompatible : GLib.UTF8_String := "Incompatible unit"
            )  return Measure is
      View   : Gtk_Unit_Selection;
      Dialog : Gtk_Dialog;
   begin
      Gtk_New (View, Constraint, Initial);
      Initialize (View, Dialog, Title, Confirm, Cancel, Parent, Flags);
      return
         Run (View, Dialog, Title, Missing, Erroneous, Incompatible);
   end Get;

   procedure On_Commit
             (  Widget : access Gtk_Unit_Selection_Record'Class;
                Dialog : Gtk_Dialog
             )  is
   begin
      Dialog.Response (Gtk_Response_OK);
   end On_Commit;

   procedure On_Reject
             (  Widget : access Gtk_Unit_Selection_Record'Class;
                Dialog : Gtk_Dialog
             )  is
   begin
      Dialog.Response (Gtk_Response_Cancel);
   end On_Reject;

end Measures_Gtk_Edit.Dialogs;
