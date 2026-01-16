--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Gtk_Edit                           Luebeck            --
--  Interface                                      Winter, 2007       --
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
--  unit input. The widget supports direct input of measurement unit  in
--  a free form such as yd/s etc. The input is then parsed to be a valid
--  unit expression. Alternatively a commonly used measurement unit  can
--  selected from a tree view control. For regular units SI-prefixes can
--  be also selected rather than typed from a scaler spin box.
--
with Gdk.Event;            use Gdk.Event;
with Gtk;                  use Gtk;
with Gtk.Box;              use Gtk.Box;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Label;            use Gtk.Label;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Spin_Button;      use Gtk.Spin_Button;
with Gtk.Tree_Selection;   use Gtk.Tree_Selection;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Widget;           use Gtk.Widget;
with Units;                use Units;

with GLib;
with Gtk.Handlers;
with Measures_UTF8_Edit;

generic
   Class_Name : String;
   with package UTF8_Edit is new Measures_UTF8_Edit (<>);
package Measures_Gtk_Edit is
   package Measures_Of renames UTF8_Edit.Measures_Of;
   use Measures_Of;
--
-- Gtk_Unit_Selection_Record -- The widget type
--
-- The widget has the following appearance:
--
--    .__________________._________.
--    |                  |         |
--    |  Unit entry box  |  Scaler |
--    |__________________|_________|
--    |                            |
--    |  Unit selection tree       |
--    |                            |
--    |                            |
--    |____________________________|
--
-- The  user  can  either directy type a unit in the entry box or browse
-- the  selection tree for a unit there. A unit selected in the tree can
-- be scaled with the scaler spin box, when  it  is  regular  unit  that
-- accepts SI prefixes.
--
-- Style properties :
--
--    unit-column-title       - The title of the  first  column  in  the
--                              unit selection tree. String, the default
--                              is "Unit";
--    equivalent-column-title - The  title  of  the second column in the
--                              unit selection tree. String, the default
--                              is "SI Equivalent";
--    power-label             - The  label  of  the  scaler. String, the
--                              default is "Power";
--
--    The following styles are all strings and denote  various  sections
--    of the unit selection tree. The default value is same as the style
--    name, with the first letter capitalized:
--
--    acceleration   amount        angle         area
--    capacitance    charge        chemistry     concentration
--    conductance    current       electricity   energy
--    flux           force         frequency     geometry
--    illuminance    inductance    intensity     length
--    luminance      mass          mechanics     optic
--    potential      power         pressure      resistance
--    temperature    time          velocity      volume
--
-- Signals :
--
--    commit - Button Enter was pressed
--    reject - Button ESC was pressed
--
   type Gtk_Unit_Selection_Record is
      new Gtk_Box_Record with private;
   type Gtk_Unit_Selection is
      access all Gtk_Unit_Selection_Record'Class;
--
-- Filter -- Unit filter
--
--    Widget - The unit selection widget
--    Value  - A measure to be check
--
-- This function is used to check the measurements units which appear in
-- the unit selection tree and in the entry box. In the later case check
-- happens upon request, when the function Get is  called.  The  default
-- filter implementation returns False if Value is incompatible with the
-- unit constraint if the latter is specified. When no constraint  given
-- it returns True.
--
   function Filter
            (  Widget : Gtk_Unit_Selection_Record;
               Value  : Measure
            )  return Boolean;
--
-- Get -- The currently selected measurement unit
--
--    Widget - The unit selection widget
--
-- These functions return the  currently  selected  or  otherwise  input
-- measure. It is the content of unit entry box. The  value  is  checked
-- using  the function Filter and Constraint_Error is propagated when it
-- does not match. Also  Constraint_Error  is  propagated  upon  numeric
-- errors. Data_Error is propagated on unit syntax errors. Unit_Error is
-- propagated  on  illegal  unit  expressions,  like  m/°C. End_Error is
-- propagated when no unit was selected or input.
--
-- Returns :
--
--    The selected measure either as a measure or else as text
--
-- Exceptions :
--
--    Constraint_Error - Wrong measure selected (filtered out)
--    Data_Error       - Syntax error
--    End_Error        - No measure selected
--    Unit_Error       - Unit error (an illegal unit expression)
--
   function Get
            (  Widget : not null access Gtk_Unit_Selection_Record
            )  return Measure;
   function Get
            (  Widget : not null access Gtk_Unit_Selection_Record
            )  return GLib.UTF8_String;
--
-- Get_Entry -- Get the entry box of the widget
--
--    Widget - The unit selection widget
--
-- Returns :
--
--    The entry field of
--
   function Get_Entry
            (  Widget : not null access Gtk_Unit_Selection_Record
            )  return Gtk_Entry;
--
-- Get_Tree_View -- Get the tree view of the widget
--
--    Widget - The unit selection widget
--
-- Returns :
--
--    The tree view field of
--
   function Get_Tree_View
            (  Widget : not null access Gtk_Unit_Selection_Record
            )  return Gtk_Tree_View;
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
-- which the widget must be constrained.  When  specified,  the  default
-- implementation  of  Filter  will  accept only the measures compatible
-- with Constraint.
--
   procedure Gtk_New
             (  Widget     : out Gtk_Unit_Selection;
                Constraint : Unit;
                Initial    : GLib.UTF8_String := ""
             );
   procedure Gtk_New
             (  Widget     : out Gtk_Unit_Selection;
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
             (  Widget     : not null access
                             Gtk_Unit_Selection_Record'Class;
                Constraint : Unit;
                Initial    : GLib.UTF8_String := ""
             );
   procedure Initialize
             (  Widget     : not null access
                             Gtk_Unit_Selection_Record'Class;
                Initial    : GLib.UTF8_String := ""
             );
private
   type Gtk_Unit_Selection_Record is new Gtk_Box_Record with record
      Selection   : Gtk_Tree_View;
      Edit        : Gtk_Entry;
      Radix       : Gtk_Label;
      Scale       : Gtk_Spin_Button;
      Scroll      : Gtk_Scrolled_Window;
      Constraint  : Unit;
      Constrained : Boolean := False;
      Initialized : Boolean := False;
   end record;
--
-- Set_Text -- Change widget's edit text
--
--    Widget - The widget
--    Text   - To set
--
-- When the widget has a text set, it is interpreted as value multiplied
-- by  scale. When that is possible, the value is used with the new text
-- as  the scale of. Additionally, if new text has compatible units, the
-- value  is  converted  to  retain  the value. When the value cannot be
-- represented in the canonic form then Text supersedes it completely.
--
   procedure Set_Text
             (  Widget : access Gtk_Unit_Selection_Record;
                Text   : String
             );

   procedure On_Focus
             (  Edit   : access Gtk_Widget_Record'Class;
                Parent : Gtk_Unit_Selection
             );

   procedure On_Scale
             (  Spin   : access Gtk_Widget_Record'Class;
                Parent : Gtk_Unit_Selection
             );

   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Parent    : Gtk_Unit_Selection
             );

   procedure Style_Updated
             (  Widget : access Gtk_Unit_Selection_Record'Class
             );

   package Unit_Selection_Handlers is
      new Gtk.Handlers.Callback (Gtk_Unit_Selection_Record);

   package Tree_Selection_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Tree_Selection_Record,
             Gtk_Unit_Selection
          );

   package Event_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Widget_Record,
             Boolean,
             Gtk_Unit_Selection
          );

   function Button_Press
            (  Widget    : access Gtk_Widget_Record'Class;
               Event     : Gdk_Event;
               Selection : Gtk_Unit_Selection
            )  return Boolean;

   package Selection_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Unit_Selection
          );

end Measures_Gtk_Edit;
