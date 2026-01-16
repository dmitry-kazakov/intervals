--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Gtk_Edit.GEntry                    Luebeck            --
--  Interface                                      Summer, 2007       --
--                                                                    --
--                                Last revision :  11:50 30 May 2014  --
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
--  This  package  provides  a GTK+ widget for a comfortable measurement
--  unit  input  in  a form similar to combo box. The widget is an entry
--  which being activated drops down a unit selection window. The window
--  is  closed  when  user  hits  enter, escape or leaves it using other
--  means.
--
with Gdk.Event;   use Gdk.Event;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Window;  use Gtk.Window;

with System;

generic
   type Custom_Unit_Selection_Record is
      new Gtk_Unit_Selection_Record with private;
package Measures_Gtk_Edit.GEntry is
--
-- Gtk_Unit_Entry_Record -- The widget type
--
-- Style properties :
--
--    has-header - True if the unit  selection  tree  view  should  have
--                 column's header. Boolean, the default is False;
--
   type Gtk_Unit_Entry_Record is
      new Gtk_Entry_Record with private;
   type Gtk_Unit_Entry is
      access all Gtk_Unit_Entry_Record'Class;
--
-- Editing_Canceled -- Editing status
--
--    Widget - The unit entry widget
--
-- This  function  is used when the widget is used as an editable widget
-- for a cell renderer.  When  the  renderer  handles  the  editing-done
-- signal it may check if the user has cancelled editing.
--
-- Retunrs :
--
--    True if the last editing was cancelled
--
   function Editing_Canceled
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Boolean;
--
-- Get -- The currently specified measure
--
--    Widget - The unit entry widget
--    Scale  - Of the unitless values
--
-- This function retrieves the current value  set  in  the  widget.  The
-- value  is  checked  against the widget constraint. When the parameter
-- Scale  is  specified  it  must have units compatible with the current
-- widget constraint, otherwise Constraint_Error is propagated. When the
-- widget's  content  is  numeric  the  result  is  its  numeric   value
-- multiplied  by  Scale.  When  the  content  is  dimensioned, Scale is
-- ignored.  When  Scale is omited numeric value causes Constraint_Error
-- if the Widget unit is constrained to be non-unitless.
--
-- Returns :
--
--    The measure specified
--
-- Exceptions :
--
--    Constraint_Error - Wrong measure selected (filtered out)
--    Data_Error       - Syntax error
--    End_Error        - No measure selected
--    Unit_Error       - Unit error (an illegal unit expression)
--
   function Get
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Measure;
   function Get
            (  Widget : not null access Gtk_Unit_Entry_Record;
               Scale  : Measure
            )  return Measure;
--
-- Get_Constraint -- Change the widget constraint
--
--    Widget - The unit entry widget
--
-- Returns :
--
--    The current constraint of
--
-- Exceptions :
--
--    Constraint_Error - The widget is unconstrained
--
   function Get_Constraint
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Unit;
--
-- Get_Type -- Get the GTK+ type of the widgets
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Widget     - The result
--    Constraint - The unit constraint to enforce
--    Initial    - The default unit to select initially
--
-- The parameter Initial specifies the initial selection. It can be  any
-- string.  Note  that  Initial is not checked to be a valid measurement
-- unit,  neither  it  is  passed through Filter. Instead it is accepted
-- as-is. The parameter Constraint is optional. It specifies the unit to
-- which the widget must be constrained.
--
   procedure Gtk_New
             (  Widget     : out Gtk_Unit_Entry;
                Constraint : Unit;
                Initial    : GLib.UTF8_String := ""
             );
   procedure Gtk_New
             (  Widget     : out Gtk_Unit_Entry;
                Initial    : GLib.UTF8_String := ""
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget     - The widget to initialize
--    Constraint - The unit constraint to enforce
--    Initial    - To initialize with
--
-- Any derived type is  responsible  to  call  to  Initialize  from  its
-- Initialize.
--
-- Exceptions :
--
--    Unit_Error - Initial does not contain a proper measurement unit
--
   procedure Initialize
             (  Widget : not null access Gtk_Unit_Entry_Record'Class;
                Constraint : Unit;
                Initial    : GLib.UTF8_String := ""
             );
   procedure Initialize
             (  Widget  : not null access Gtk_Unit_Entry_Record'Class;
                Initial : GLib.UTF8_String := ""
             );
--
-- Is_Constrained -- Check if the widget is constrained
--
--    Widget - The unit entry widget
--
-- Retunrs :
--
--    True if the widget is constrained
--
   function Is_Constrained
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Boolean;
--
-- Is_Numeric -- Check if numeric values allowed
--
--    Widget - The unit entry widget
--
-- Retunrs :
--
--    True if the widget allows numeric values
--
   function Is_Numeric
            (  Widget : not null access Gtk_Unit_Entry_Record
            )  return Boolean;
--
-- Reset_Constraint -- Remove the widget constraint
--
--    Widget - The unit entry widget
--
-- This procedure removes the unit constraint of the widget.
--
   procedure Reset_Constraint
             (  Widget : not null access Gtk_Unit_Entry_Record
             );
--
-- Set_Constraint -- Change the widget constraint
--
--    Widget     - The unit entry widget
--    Constraint - The constraint to set
--
-- This procedure changes the unit constraint of the widget.
--
   procedure Set_Constraint
             (  Widget     : not null access Gtk_Unit_Entry_Record;
                Constraint : Unit
             );
--
-- Set_Numeric -- Change the widget's treating of numeric values
--
--    Widget  - The unit entry widget
--    Allowed - The flag value to set
--
-- This procedure changes allowance of numeric values. When  the  widget
-- is constrained to a specific unit and this flag is set, then  numeric
-- values  having  no  dimension  specified  will allowed in the widget.
-- These numbers can be then interpreted in a special way by the  parent
-- widget.
--
   procedure Set_Numeric
             (  Widget  : not null access Gtk_Unit_Entry_Record;
                Allowed : Boolean
             );
private
   type Gtk_Unit_Entry_Record is new Gtk_Entry_Record with record
      Constraint   : Unit;
      Popup        : Gtk_Window;
      Selection    : Gtk_Unit_Selection;
      Constrained  : Boolean   := False;
      Canceled     : Boolean   := True;
      Numeric      : Boolean   := True;
      Is_Renderer  : Boolean   := False;
      X, Y         : GLib.GInt := 0;
   end record;

   function Button_Press
            (  Widget : access Gtk_Unit_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event
            )  return Boolean;

   function Button_Press_Popup
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Edit   : Gtk_Unit_Entry
            )  return Boolean;

   procedure Commit
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Unit_Entry
             );

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Unit_Entry
             );

   procedure Done_Popup
             (  Widget : not null access Gtk_Unit_Entry_Record
             );

   procedure Reject
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Unit_Entry
             );

   package Edit_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Unit_Entry
          );

   package Focus_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Widget_Record,
             Boolean,
             Gtk_Unit_Entry
          );

   package Press_Handlers is
      new Gtk.Handlers.Return_Callback
          (  Gtk_Unit_Entry_Record,
             Boolean
          );
--
-- Start_Editing_Entry -- Callback on start editing
--
   type Start_Editing_Entry is access procedure
        (  Cell_Editable : System.Address;
	   Event         : Gdk_Event
        );
   pragma Convention (C, Start_Editing_Entry);

   procedure Start_Editing
             (  Cell_Editable : System.Address;
     	        Event         : Gdk_Event
             );
   pragma Convention (C, Start_Editing);

   Start_Editing_Ptr : constant Start_Editing_Entry :=
                          Start_Editing'Access;

end Measures_Gtk_Edit.GEntry;
