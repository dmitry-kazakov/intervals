--                                                                    --
--  package Measures.Edit           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  10:13 13 Oct 2007  --
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
with Units;              use Units;

with Measures_Generic_Edit;
with Units.Edit;

package body Measures_Edit is

   procedure Get_Power
             (  Source  : String;
                Pointer : in out Integer;
                Number  : out Integer;
                Mode    : Code_Set
             )  is
   begin
      raise End_Error;
   end Get_Power;

   function Get_Image (Value : Unit; Mode : Code_Set) return String is
   begin
      case Mode is
         when ASCII_Set  =>
            return Units.Edit.Image (Value, False);
         when others =>
            return Units.Edit.Image (Value, True);
      end case;
   end Get_Image;
--
-- Implementation -- Of string I/O
--
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
                Latin1  : Boolean := True
             )  is
   begin
      if Latin1 then
         Implementation.Get (Source, Pointer, Value, Latin1_Set);
      else
         Implementation.Get (Source, Pointer, Value, ASCII_Set);
      end if;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Scaled;
                Latin1  : Boolean := True
             )  is
   begin
      if Latin1 then
         Implementation.Get (Source, Pointer, Value, Latin1_Set);
      else
         Implementation.Get (Source, Pointer, Value, ASCII_Set);
      end if;
   end Get;

   procedure Get_Unit
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Measure;
                Latin1  : Boolean := True
             )  is
   begin
      if Latin1 then
         Implementation.Get_Unit (Source, Pointer, Value, Latin1_Set);
      else
         Implementation.Get_Unit (Source, Pointer, Value, ASCII_Set);
      end if;
   end Get_Unit;

   function Value
            (  Source  : String;
               Latin1  : Boolean := True
            )  return Measure is
   begin
      if Latin1 then
         return Implementation.Value (Source, Latin1_Set);
      else
         return Implementation.Value (Source, ASCII_Set);
      end if;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Measure;
                Latin1      : Boolean   := True;
                Derived     : Boolean   := True;
                RelSmall    : Positive  := MaxSmall;
                AbsSmall    : Integer   :=-MaxSmall;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      if Latin1 then
         Implementation.Put
         (  Destination => Destination,
            Pointer     => Pointer,
            Value       => Value,
            Mode        => Latin1_Set,
            Derived     => Derived,
            RelSmall    => RelSmall,
            AbsSmall    => AbsSmall,
            Field       => Field,
            Justify     => Justify,
            Fill        => Fill
         );
      else
         Implementation.Put
         (  Destination => Destination,
            Pointer     => Pointer,
            Value       => Value,
            Mode        => ASCII_Set,
            Derived     => Derived,
            RelSmall    => RelSmall,
            AbsSmall    => AbsSmall,
            Field       => Field,
            Justify     => Justify,
            Fill        => Fill
         );
      end if;
   end Put;

   function Image
            (  Value    : Measure;
               Latin1   : Boolean  := True;
               Derived  : Boolean  := True;
               RelSmall : Positive := MaxSmall;
               AbsSmall : Integer  :=-MaxSmall
            )  return String is
   begin
      if Latin1 then
         return Implementation.Image
                (  Value    => Value,
                   Mode     => Latin1_Set,
                   Derived  => Derived,
                   RelSmall => RelSmall,
                   AbsSmall => AbsSmall
                );
      else
         return Implementation.Image
                (  Value    => Value,
                   Mode     => ASCII_Set,
                   Derived  => Derived,
                   RelSmall => RelSmall,
                   AbsSmall => AbsSmall
                );
      end if;
   end Image;

end Measures_Edit;
