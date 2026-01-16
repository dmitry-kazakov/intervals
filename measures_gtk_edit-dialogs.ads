--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Gtk_Edit.Dialogs                   Luebeck            --
--  Interface                                      Winter, 2007       --
--                                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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
--  This package provides for  interactive  measurement  unit  selection
--  dialogs.
--
with Gtk.Dialog;  use Gtk.Dialog;
with Gtk.Stock;   use Gtk.Stock;
with Gtk.Window;  use Gtk.Window;

generic
   type Custom_Unit_Selection_Record is
      new Gtk_Unit_Selection_Record with private;
package Measures_Gtk_Edit.Dialogs is
--
-- Get -- A measurement unit using a dialog box
--
--  [ Constraint ] - On the measures
--    Initial      - The initial selection
--    Title        - Of the dialog
--    Confirm      - The label of the OK button
--    Cancel       - The label of the Cancel button
--    Parent       - The parent of the dialog
--    Flags        - Of the dialog box
--    Missing      - The message text on no unit error
--    Erroneous    - The message text on unit error
--    Incompatible - The message text on unit constraint error
--
-- These function  cause a  dialog to  appear.  The dialog  has the unit
-- selection widget in it and up to two buttons.  The function does  not
-- return  until the dialog closes. When the user presses the OK button,
-- which label is specified  by  the  parameter  Confirm,  the  selected
-- measurement unit is checked for validity and it is the result of  the
-- function. The result is either a  string  or  a  measure.  On  failed
-- checks the dialog does not end and an  appropriate  message  text  is
-- shown as the parameters Missing, Erroneous and Incompatible  specify.
-- When the parameter Cancel is not an empty string, the  Cancel  button
-- is  shown.  When the dialog box is closed using the window manager or
-- the Cancel button, the functions propagate End_Error exception.
--
-- Returns :
--
--    The measurement unit specified
--
-- Exceptions :
--
--    End_Error - The dialog was forcibly closed
--
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
            )  return GLib.UTF8_String;
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
            )  return Measure;
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
            )  return GLib.UTF8_String;
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
            )  return Measure;

private
   package Commit_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Unit_Selection_Record,
             Gtk_Dialog
          );
   procedure On_Commit
             (  Widget : access Gtk_Unit_Selection_Record'Class;
                Dialog : Gtk_Dialog
             );
   procedure On_Reject
             (  Widget : access Gtk_Unit_Selection_Record'Class;
                Dialog : Gtk_Dialog
             );
end Measures_Gtk_Edit.Dialogs;
