--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Measures_Gtk_Edit.GEntry                    Luebeck            --
--        Cell_Renderer                            Summer, 2007       --
--  Interface                                                         --
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
with Cairo;                     use Cairo;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Window;                use Gdk.Window;
with GLib;                      use GLib;
with GLib.Object;               use GLib.Object;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Values;               use GLib.Values;
with Gtk.Cell_Renderer;         use Gtk.Cell_Renderer;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Widget;                use Gtk.Widget;
with Pango.Layout;              use Pango.Layout;

with Gtk.Cell_Renderer.Abstract_Renderer;
use  Gtk.Cell_Renderer.Abstract_Renderer;

generic
package Measures_Gtk_Edit.GEntry.Cell_Renderer is
--
-- Gtk_Unit_Entry_Record -- The widget type
--
-- Properties :
--
--    after - The number of decimal digits after  decimal  point.  UInt,
--            the default is 0;
--    scale - The   scale   used  for  the  indicated  values.  It  also
--            constraints  the  input  values  to  its  dimension.  When
--            specified empty, scale is  1  SI.  When  non-empty  it  is
--            parsed using Measures_UTF8_Edit.Value. String, the default
--            "";
--    text  - The  value  it textual form. When used to set the renderer
--            value it may contain no unit specification, and  then  the
--            scale is assumend. Otherwise,  the  unit  is  checked  and
--            scaled  according to the scale. When read it contains both
--            the value and the scale. String;
--    value - The renderer's value. GDouble.
--
   type Gtk_Cell_Renderer_Measure_Record is
      new Gtk_Abstract_Renderer_Record with private;
   type Gtk_Cell_Renderer_Measure is
      access all Gtk_Cell_Renderer_Measure_Record'Class;
--
-- Finalize -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Finalize
             (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
             );
--
-- Get -- The current value of the renderer
--
--    Cell - The renderer
--
-- When the result is a string it is composed out of the  current  value
-- formatted according to the renderer settings. The value  is  followed
-- by the scale specification text of the renderer. When the result is a
-- GLib value it has the type GType_Double with the value scaled to  the
-- current renderer's scale.
--
-- Returns :
--
--    The value
--
   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
            )  return Number;
   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
            )  return Measure;
   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
            )  return GValue;
   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Measure_Record
            )  return UTF8_String;
--
-- Get_Aligned_Area -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Get_Aligned_Area
            (  Cell      : not null access
                           Gtk_Cell_Renderer_Measure_Record;
               Widget    : not null access Gtk_Widget_Record'Class;
               Flags     : Gtk_Cell_Renderer_State;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle;
--
-- Get_Property -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Get_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Measure_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             );
--
-- Get_Size -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Get_Size
            (  Cell      : not null access
                           Gtk_Cell_Renderer_Measure_Record;
               Widget    : not null access
                           Gtk.Widget.Gtk_Widget_Record'Class;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle;
   overriding
   function Get_Size
            (  Cell      : not null access
                           Gtk_Cell_Renderer_Measure_Record;
               Widget    : not null access
                           Gtk.Widget.Gtk_Widget_Record'Class
            )  return Gdk_Rectangle;
--
-- Gtk_New -- Factory
--
--    Cell  - The result
--    Scale - The scale of the values rendered
--    After - The number of digits after decimal point
--
-- Scale's  gain  must  be  positive,  Constraint_Error  is   propagated
-- otherwise.
--
-- Exceptions :
--
--    Constraint_Error - Numeric error in unit expression
--    Data_Error       - Syntax error
--    Unit_Error       - Illegal unit expression (like m/°C)
--
   procedure Gtk_New
             (  Cell  : out Gtk_Cell_Renderer_Measure;
                Scale : UTF8_String  := "";
                After : Natural      := 0
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Cell  - The renderer to initialize
--    Scale - The scale of the values rendered
--    After - The number of digits after decimal point
--
-- This procedure is never called directly, only from Gtk_New or else
-- from Initialize of a derived type. In the latter case a call to
-- Initialize is obligatory.
--
-- Exceptions :
--
--    Constraint_Error - Numeric error in unit expression
--    Data_Error       - Syntax error
--    Unit_Error       - Illegal unit expression (like m/°C)
--
   procedure Initialize
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record'Class;
                Scale : UTF8_String;
                After : Natural
             );
--
-- Put -- Change the current value of the renderer
--
--    Cell  - The renderer
--    Value - The value to set
--
-- When Value is a measure, the value set is scaled to the  the  current
-- scale of the renderer. Unit_Error is propagated when Value has  units
-- incompatible with the widget's scale. When Value is a string,  it  is
-- parsed  using Measures_UTF8_Edit.Value. The result is used as if were
-- Measure. When  Value  is  a  GLib  value  it  has  to  be  either  of
-- GType_Dobule   or   GType_String,   otherwise   Constraint_Error   is
-- propagated. When the value is GType_String, then behavior is same  as
-- for a string argument.
--
-- Returns :
--
--    The value
--
-- Exceptions :
--
--    Constraint_Error - Numeric error in unit expression
--    Data_Error       - Syntax error
--    End_Error        - There is no measure in the string
--    Unit_Error       - Illegal  unit  expression  (like  m/°C) or else
--                       incompatible units
--
   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Value : Number
             );
   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Value : Measure
             );
   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Value : UTF8_String
             );
   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Value : GValue
             );
--
-- Render -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Render
             (  Cell            : not null access
                                  Gtk_Cell_Renderer_Measure_Record;
                Context         : Cairo_Context;
                Widget          : not null access
                                  Gtk_Widget_Record'Class;
                Background_Area : Gdk_Rectangle;
                Cell_Area       : Gdk_Rectangle;
                Flags           : Gtk_Cell_Renderer_State
             );
--
-- Set_Property -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Set_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Measure_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             );
--
-- Set_Scale -- Change the renderer's scale
--
--    Cell  - The renderer to initialize
--    Scale - The scale of the values rendered
--
-- Exceptions :
--
--    Constraint_Error - Numeric error in unit expression
--    Data_Error       - Syntax error
--    End_Error        - There is no measure in the string
--    Unit_Error       - Illegal unit expression (like m/°C)
--
   procedure Set_Scale
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Measure_Record;
                Scale : UTF8_String
             );
--
-- Start_Editing -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
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
            )  return Gtk_Widget;
private
   type String_Ptr is access String;
--
-- Gtk_Cell_Renderer_Fixed_Record -- Implementation
--
-- The renderer maintains its state global to the column it renders.
-- That is the text widget it uses to render the number, the number of
-- places after the decimal point and the maximal width of the number
-- places before the point including the sign. This field is evaluated
-- dynamically and adjusted each time the renderer is queried for its
-- size or asked to render a cell. This heuristics might not work if new
-- rows are added to the tree model after it was rendered once.
--
   type Gtk_Cell_Renderer_Measure_Record is
      new Gtk.Cell_Renderer.
          Abstract_Renderer.Gtk_Abstract_Renderer_Record with
   record
      Text        : Pango_Layout;     -- The text to display
      Value       : Number  := 0.0;   -- Current value
      After       : Natural := 0;     -- Places after the point
      Max_Offset  : GInt    := 0;     -- Pixel offset to the point
      Height      : GInt    := 0;     -- Current pixel height
      Width       : GInt    := 0;     -- Current pixel width
      Left_Width  : GInt;             -- Current space before the point
      Focus_Out   : Handler_Id;       -- Current focus_out_event handler
      Scale       : Measure;          -- Current scale
      Scale_Text  : String_Ptr;       -- Scale text
   end record;
--
-- Update -- The widget associated with the renderer
--
--    Cell   - The renderer
--    Widget - The widget it is used at
--
-- This  procedure  is  used  upon  each  call to either to render or to
-- evaluate  the geometry of a cell.
--
   procedure Update
             (  Cell   : in out Gtk_Cell_Renderer_Measure_Record;
                Widget : access Gtk.Widget.Gtk_Widget_Record'Class
             );
--
-- Editing_Done -- Handler of editing_done
--
   procedure Editing_Done
             (  Editor : access Gtk_Unit_Entry_Record'Class;
                Cell   : Gtk_Cell_Renderer_Measure
             );
--
-- Focus_Out -- Handler of focus_out_event
--
   function Focus_Out
            (  Editor : access Gtk_Unit_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Cell   : Gtk_Cell_Renderer_Measure
            )  return Boolean;
--
-- Entry_Callbacks -- To handle editing_done
--
   package Entry_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Widget_Type => Gtk_Unit_Entry_Record,
             User_Type   => Gtk_Cell_Renderer_Measure
          );
--
-- Entry_Return_Callbacks -- To handle focus_out_event
--
   package Entry_Return_Callbacks is
      new Gtk.Handlers.User_Return_Callback
          (  Widget_Type => Gtk_Unit_Entry_Record,
             Return_Type => Boolean,
             User_Type   => Gtk_Cell_Renderer_Measure
          );

end Measures_Gtk_Edit.GEntry.Cell_Renderer;
