--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Measures_Universal_Edit                    Luebeck            --
--  Implementation                                 Spring, 2005       --
--                                                                    --
--                                Last revision :  09:24 07 Aug 2009  --
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

with Units.Edit;
with Units.UTF8_Edit;
with Measures_Generic_Edit;
with Strings_Edit.Integers.Superscript;

package body Measures_Universal_Edit is

   procedure Get_Power
                  (  Source  : String;
                     Pointer : in out Integer;
                     Number  : out Integer;
                     Mode    : Code_Set
                  )  is
   begin
      Strings_Edit.Integers.Superscript.Get (Source, Pointer, Number);
   end Get_Power;

   function Get_Image (Value : Unit; Mode : Code_Set) return String is
   begin
      case Mode is
         when ASCII_Set  =>
            return Units.Edit.Image (Value, False);
         when Latin1_Set =>
            return Units.Edit.Image (Value, True);
         when UTF8_Set =>
            return Units.UTF8_Edit.Image (Value);
      end case;
   end Get_Image;

   package Implementation is
      new Measures_Generic_Edit
          (  Irregular_Measures => Irregular_Measures,
             Float_Edit         => Float_Edit,
             Get_Superscript    => Get_Power,
             Image              => Get_Image
          );

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure;
                Mode    : Code_Set
             )  renames Implementation.Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Scaled;
                Mode    : Code_Set
             )  renames Implementation.Get;

   procedure Get_Unit
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure;
                Mode    : Code_Set
             )  renames Implementation.Get_Unit;

   function Image
            (  Value    : Measure;
               Mode     : Code_Set;
               Derived  : Boolean  := True;
               RelSmall : Positive := Strings_Edit.MaxSmall;
               AbsSmall : Integer  :=-Strings_Edit.MaxSmall
            )  return String renames Implementation.Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Measure;
                Mode        : Code_Set;
                Derived     : Boolean  := True;
                RelSmall    : Positive := Strings_Edit.MaxSmall;
                AbsSmall    : Integer  :=-Strings_Edit.MaxSmall;
                Field       : Natural  := 0;
                Justify     : Strings_Edit.Alignment :=
                                 Strings_Edit.Left;
                Fill        : Character := ' '
             )  renames Implementation.Put;

   function Value (Source : String; Mode : Code_Set) return Measure
      renames Implementation.Value;

end Measures_Universal_Edit;
