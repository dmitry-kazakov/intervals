--                                                                    --
--  package Measures.UTF8_Edit      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2005       --
--                                                                    --
--                                Last revision :  13:42 19 Mar 2011  --
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

with Units;  use Units;

package body Measures_UTF8_Edit is

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure
             )  is
   begin
      Universal_Edit.Get (Source, Pointer, Value, UTF8_Set);
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Scaled
             )  is
   begin
      Universal_Edit.Get (Source, Pointer, Value, UTF8_Set);
   end Get;

   procedure Get_Unit
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure
             )  is
   begin
      Universal_Edit.Get_Unit (Source, Pointer, Value, UTF8_Set);
   end Get_Unit;

   function Value (Source : String) return Measure is
   begin
      return Universal_Edit.Value (Source, UTF8_Set);
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Measure;
                Derived     : Boolean  := True;
                RelSmall    : Positive := Strings_Edit.MaxSmall;
                AbsSmall    : Integer  := -Strings_Edit.MaxSmall;
                Field       : Natural  := 0;
                Justify     : Strings_Edit.Alignment :=
                                 Strings_Edit.Left;
                Fill        : Character := ' '
             )  is
   begin
      Universal_Edit.Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Value       => Value,
         Mode        => UTF8_Set,
         Derived     => Derived,
         RelSmall    => RelSmall,
         AbsSmall    => AbsSmall,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Image
            (  Value    : Measure;
               Derived  : Boolean  := True;
               RelSmall : Positive := Strings_Edit.MaxSmall;
               AbsSmall : Integer  := -Strings_Edit.MaxSmall
            )  return String is
   begin
      return
         Universal_Edit.Image
         (  Value    => Value,
            Mode     => UTF8_Set,
            Derived  => Derived,
            RelSmall => RelSmall,
            AbsSmall => AbsSmall
         );
   end Image;

end Measures_UTF8_Edit;
