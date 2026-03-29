--                                                                    --
--  package Strings_Edit.RFC_3897   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2026       --
--                                                                    --
--                                Last revision :  12:14 29 Mar 2026  --
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

with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with Strings_Edit.Fields;     use Strings_Edit.Fields;
with Strings_Edit.Integers;   use Strings_Edit.Integers;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

package body Strings_Edit.RFC_3897 is
   use Strings_Edit.UTF8;

   Digit : constant String := "0123456789abcdef";

   Unreserved_Char : constant Unicode_Set :=
                  To_Set (Character'Pos ('0'), Character'Pos ('9')) or
                  To_Set (Character'Pos ('A'), Character'Pos ('Z')) or
                  To_Set (Character'Pos ('a'), Character'Pos ('z')) or
                  To_Set ( "-._~")                                  or
                  To_Set (   16#A0#, 16#D7FF# )                     or
                  To_Set ( 16#F900#, 16#FDCF# )                     or
                  To_Set ( 16#FDF0#, 16#FFEF# )                     or
                  To_Set (16#10000#, 16#1FFFD#)                     or
                  To_Set (16#20000#, 16#2FFFD#)                     or
                  To_Set (16#30000#, 16#3FFFD#)                     or
                  To_Set (16#40000#, 16#4FFFD#)                     or
                  To_Set (16#50000#, 16#5FFFD#)                     or
                  To_Set (16#60000#, 16#6FFFD#)                     or
                  To_Set (16#70000#, 16#7FFFD#)                     or
                  To_Set (16#80000#, 16#8FFFD#)                     or
                  To_Set (16#90000#, 16#9FFFD#)                     or
                  To_Set (16#A0000#, 16#AFFFD#)                     or
                  To_Set (16#B0000#, 16#BFFFD#)                     or
                  To_Set (16#C0000#, 16#CFFFD#)                     or
                  To_Set (16#D0000#, 16#DFFFD#)                     or
                  To_Set (16#E1000#, 16#EFFFD#);

   Sub_Delims_Char : constant Unicode_Set := To_Set ("!$&'()*+,;=");

   Regular_Name_Char : constant Unicode_Set :=
      Unreserved_Char or Sub_Delims_Char;

   IP_Char : constant Unicode_Set :=
      Unreserved_Char or Sub_Delims_Char or To_Set (":@");

   Path_Char : constant Unicode_Set := IP_Char or To_Set ("/");

   User_Info_Char : constant Unicode_Set :=
      Unreserved_Char or Sub_Delims_Char or To_Set (":");

   Fragment_Char : constant Unicode_Set := IP_Char or To_Set ("/?");

   Query_Char : constant Unicode_Set :=
                  Fragment_Char                   or
                  To_Set (  16#E000#, 16#F8FF#  ) or
                  To_Set ( 16#F0000#, 16#FFFFD# ) or
                  To_Set (16#100000#, 16#10FFFD#);

   function "&" (ID : IRI; Query : String) return IRI is
   begin
      if ID.Query_Length < 0 then
         declare
            Result : IRI
                     (  Scheme_Length        => ID.Scheme_Length,
                        Fragment_Length      => ID.Fragment_Length,
                        Hierarchical_Kind_Of => ID.Hierarchical_Kind_Of,
                        Authority_Kind_Of    => ID.Authority_Kind_Of,
                        Regular_Name_Length  => ID.Regular_Name_Length,
                        User_Info_Length     => ID.User_Info_Length,
                        Default_Port         => ID.Default_Port,
                        Path_Length          => ID.Path_Length,
                        Query_Length         => Query'Length
                     );
         begin
            Result.Scheme       := ID.Scheme;
            Result.Hierarchical := ID.Hierarchical;
            Result.Query        := Create (Query);
            Result.Fragment     := ID.Fragment;
            return Result;
         end;
      else
         declare
            Result : IRI
               (  Scheme_Length        => ID.Scheme_Length,
                  Fragment_Length      => ID.Fragment_Length,
                  Hierarchical_Kind_Of => ID.Hierarchical_Kind_Of,
                  Authority_Kind_Of    => ID.Authority_Kind_Of,
                  Regular_Name_Length  => ID.Regular_Name_Length,
                  User_Info_Length     => ID.User_Info_Length,
                  Default_Port         => ID.Default_Port,
                  Path_Length          => ID.Path_Length,
                  Query_Length         => ID.Query_Length
                                        + Query'Length
              );
         begin
            Result.Scheme       := ID.Scheme;
            Result.Hierarchical := ID.Hierarchical;
            Result.Query        := Create (ID.Query.Value & Query);
            Result.Fragment     := ID.Fragment;
            return Result;
         end;
      end if;
   end "&";

   function "&" (ID : IRI; Query : IP_Query) return IRI is
   begin
      if Query.Length >= 0 then
         return ID & Query.Value;
      else
         return ID;
      end if;
   end "&";

   function "+" (Scheme : String; Authority : IP_Authority)
      return IRI is
      Result : IRI
         (  Scheme_Length        => Scheme'Length,
            Query_Length         => -1,
            Fragment_Length      => -1,
            Hierarchical_Kind_Of => Authority_Part,
            Authority_Kind_Of    => Authority.Kind_Of,
            Regular_Name_Length  => Authority.Regular_Name_Length,
            User_Info_Length     => Authority.User_Info_Length,
            Default_Port         => Authority.Default_Port,
            Path_Length          => -1
       );
   begin
      Result.Scheme                 := Scheme;
      Result.Hierarchical.Authority := Authority;
      return Result;
   end "+";

   function "+" (Scheme : String; Hierarchical : IP_Hierarchical_Part)
      return IRI is
      Result : IRI
         (  Scheme_Length        => Scheme'Length,
            Query_Length         => -1,
            Fragment_Length      => -1,
            Hierarchical_Kind_Of => Hierarchical.Kind_Of,
            Authority_Kind_Of    => Hierarchical.Authority_Kind_Of,
            Regular_Name_Length  => Hierarchical.Regular_Name_Length,
            User_Info_Length     => Hierarchical.User_Info_Length,
            Default_Port         => Hierarchical.Default_Port,
            Path_Length          => Hierarchical.Path_Length
       );
   begin
      Result.Scheme       := Scheme;
      Result.Hierarchical := Hierarchical;
      return Result;
   end "+";

   function "+" (ID : IRI; Fragment : String) return IRI is
   begin
      if ID.Fragment_Length < 0 then
         declare
            Result : IRI
                     (  Scheme_Length        => ID.Scheme_Length,
                        Query_Length         => ID.Query_Length,
                        Hierarchical_Kind_Of => ID.Hierarchical_Kind_Of,
                        Authority_Kind_Of    => ID.Authority_Kind_Of,
                        Regular_Name_Length  => ID.Regular_Name_Length,
                        User_Info_Length     => ID.User_Info_Length,
                        Default_Port         => ID.Default_Port,
                        Path_Length          => ID.Path_Length,
                        Fragment_Length      => Fragment'Length
                     );
         begin
            Result.Scheme       := ID.Scheme;
            Result.Hierarchical := ID.Hierarchical;
            Result.Query        := ID.Query;
            Result.Fragment     := Create (Fragment);
            return Result;
         end;
      else
         declare
            Result : IRI
               (  Scheme_Length        => ID.Scheme_Length,
                  Query_Length         => ID.Query_Length,
                  Hierarchical_Kind_Of => ID.Hierarchical_Kind_Of,
                  Authority_Kind_Of    => ID.Authority_Kind_Of,
                  Regular_Name_Length  => ID.Regular_Name_Length,
                  User_Info_Length     => ID.User_Info_Length,
                  Default_Port         => ID.Default_Port,
                  Path_Length          => ID.Path_Length,
                  Fragment_Length      => ID.Fragment_Length
                                        + Fragment'Length
              );
         begin
            Result.Scheme       := ID.Scheme;
            Result.Hierarchical := ID.Hierarchical;
            Result.Query        := ID.Query;
            Result.Fragment :=
                Create (ID.Fragment.Value & Fragment);
            return Result;
         end;
      end if;
   end "+";

   function "+" (ID : IRI; Fragment : IP_Fragment) return IRI is
   begin
      if Fragment.Length >= 0 then
         return ID + Fragment.Value;
      else
         return ID;
      end if;
   end "+";

   function "+" (Scheme : String; Host : String) return IRI is
      Result : IRI
               (  Scheme_Length        => Scheme'Length,
                  Hierarchical_Kind_Of => Authority_Part,
                  Authority_Kind_Of    => Regular_Name_Host,
                  Regular_Name_Length  => Host'Length,
                  User_Info_Length     => 0,
                  Default_Port         => True,
                  Path_Length          => -1,
                  Query_Length         => -1,
                  Fragment_Length      => -1
               );
   begin
      Result.Scheme       := Scheme;
      Result.Hierarchical := Create (Create (Host));
      Result.Fragment     := No_Fragment;
      Result.Query        := No_Query;
      return Result;
   end "+";

   function "+" (Scheme : String; Path : IP_Path) return IRI is
   begin
      if Path.Length < 0 then
         raise Constraint_Error;
      end if;
      declare
         Host : constant String  := Get_Segment (Path, 1);
         Tail : constant IP_Path := Get_Suffix  (Path);
         Result : IRI
                  (  Scheme_Length        => Scheme'Length,
                     Hierarchical_Kind_Of => Authority_Part,
                     Authority_Kind_Of    => Regular_Name_Host,
                     Regular_Name_Length  => Host'Length,
                     User_Info_Length     => 0,
                     Default_Port         => True,
                     Path_Length          => Tail.Length,
                     Query_Length         => -1,
                     Fragment_Length      => -1
                  );
      begin
         Result.Scheme                 := Scheme;
         Result.Hierarchical.Authority := Create (Host);
         Result.Hierarchical.Path      := Tail;
         Result.Fragment               := No_Fragment;
         Result.Query                  := No_Query;
         return Result;
      end;
   end "+";

   function "-" (Scheme : String; Path : String) return IRI is
      Tail : IP_Path (Path'Length);
   begin
      Tail.Value := Path;
      return Scheme - Tail;
   end "-";

   function "-" (Scheme : String; Path : IP_Path) return IRI is
   begin
      if Path.Length < 0 then
         declare -- Empty path
            Result : IRI
                     (  Scheme_Length        => Scheme'Length,
                        Hierarchical_Kind_Of => Rootless_Part,
                        Authority_Kind_Of    => Regular_Name_Host,
                        Regular_Name_Length  => 0,
                        User_Info_Length     => 0,
                        Default_Port         => True,
                        Path_Length          => -1,
                        Query_Length         => -1,
                        Fragment_Length      => -1
                     );
         begin
            Result.Scheme   := Scheme;
            Result.Fragment := No_Fragment;
            Result.Query    := No_Query;
            return Result;
         end;
      elsif Path.Length = 0 then
         raise Constraint_Error;
      elsif Path.Value (1) = '/' then -- Absolute path
         if Path.Length = 1 or else Path.Value (2) = '/' then
            raise Constraint_Error;
         end if;
         declare
            Result : IRI
                     (  Scheme_Length        => Scheme'Length,
                        Hierarchical_Kind_Of => Absolute_Part,
                        Authority_Kind_Of    => Regular_Name_Host,
                        Regular_Name_Length  => 0,
                        User_Info_Length     => 0,
                        Default_Port         => True,
                        Path_Length          => Path.Length - 1,
                        Query_Length         => -1,
                        Fragment_Length      => -1
                     );
         begin
            Result.Scheme            := Scheme;
            Result.Hierarchical.Path := Get_Suffix (Path);
            Result.Fragment          := No_Fragment;
            Result.Query             := No_Query;
            return Result;
         end;
      else -- Roothless path
         declare
            Result : IRI
                     (  Scheme_Length        => Scheme'Length,
                        Hierarchical_Kind_Of => Rootless_Part,
                        Authority_Kind_Of    => Regular_Name_Host,
                        Regular_Name_Length  => 0,
                        User_Info_Length     => 0,
                        Default_Port         => True,
                        Path_Length          => Path.Length,
                        Query_Length         => -1,
                        Fragment_Length      => -1
                     );
         begin
            Result.Scheme            := Scheme;
            Result.Hierarchical.Path := Path;
            Result.Fragment          := No_Fragment;
            Result.Query             := No_Query;
            return Result;
         end;
      end if;
   end "-";

   function "*" (User_Info : String; Authority : IP_Authority)
      return IP_Authority is
   begin
      if Authority.User_Info_Length > 0 then
         raise Constraint_Error;
      end if;
      declare
         Result : IP_Authority
            (  Kind_Of             => Authority.Kind_Of,
               Regular_Name_Length => Authority.Regular_Name_Length,
               Default_Port        => Authority.Default_Port,
               User_Info_Length    => Authority.User_Info_Length
                                    + User_Info'Length
            );
      begin
         Result.User_Info := User_Info & Authority.User_Info;
         Result.Host := Authority.Host;
         if not Authority.Default_Port then
            Result.Port := Authority.Port;
         end if;
         return Result;
      end;
   end "*";

   function "*" (User_Info : String; Host : IP_Host)
      return IP_Authority is
      Result : IP_Authority
               (  Kind_Of             => Host.Kind_Of,
                  Regular_Name_Length => Host.Length,
                  User_Info_Length    => User_Info'Length,
                  Default_Port        => True
               );
   begin
      Result.User_Info := User_Info;
      Result.Host      := Host;
      return Result;
   end "*";

   function "*" (User_Info : String; Name : String)
      return IP_Authority is
      Result : IP_Authority
               (  Kind_Of              => Regular_Name_Host,
                  Regular_Name_Length  => Name'Length,
                  User_Info_Length     => User_Info'Length,
                  Default_Port         => True
              );
   begin
      Result.User_Info := User_Info;
      Result.Host.Name := Name;
      return Result;
   end "*";

   function "/" (Left : String; Right : String) return IP_Path is
       Result : IP_Path (Left'Length + Right'Length + 1);
   begin
       Result.Value := Left & '/' & Right;
      return Result;
   end "/";

   function "/" (Left : IP_Path; Right : String) return IP_Path is
   begin
      if Left.Length < 0 then
         declare
            Result : IP_Path (Right'Length);
         begin
            Result.Value := Right;
            return Result;
         end;
      else
         declare
            Result : IP_Path (Left.Length + Right'Length + 1);
         begin
            Result.Value := Left.Value & '/' & Right;
            return Result;
         end;
      end if;
   end "/";

   function "/" (Name : String; Port : IP_Port) return IP_Authority is
   begin
      return
      (  Kind_Of             => Regular_Name_Host,
         Regular_Name_Length => Name'Length,
         User_Info_Length    => 0,
         User_Info           => "",
         Host                => Create (Name),
         Default_Port        => False,
         Port                => Port
      );
   end "/";

   function "/" (Address : IPv4_Address; Port : IP_Port)
      return IP_Authority is
   begin
      return
      (  Kind_Of             => IPv4_Host,
         Regular_Name_Length => 0,
         User_Info_Length    => 0,
         User_Info           => "",
         Host                => Create (Address),
         Default_Port        => False,
         Port                => Port
      );
   end "/";

   function "/" (Address : IPv6_Address; Port : IP_Port)
      return IP_Authority is
   begin
      return
      (  Kind_Of             => IPv6_Host,
         Regular_Name_Length => 0,
         User_Info_Length    => 0,
         User_Info           => "",
         Host                => Create (Address),
         Default_Port        => False,
         Port                => Port
      );
   end "/";

   function "/" (Authority : IP_Authority; Port : IP_Port)
      return IP_Authority is
   begin
      if not Authority.Default_Port then
         raise Constraint_Error;
      end if;
      declare
         Result : IP_Authority
                  (  Kind_Of             => Authority.Kind_Of,
                     User_Info_Length    => Authority.User_Info_Length,
                     Default_Port        => False,
                     Regular_Name_Length =>
                        Authority.Regular_Name_Length
                  );
      begin
         Result.User_Info := Authority.User_Info;
         Result.Host      := Authority.Host;
         Result.Port      := Port;
         return Result;
      end;
   end "/";

   function "/" (Address : IPv4_Address; Path : String)
      return IP_Hierarchical_Part is
      Authority : constant IP_Authority :=
                  (  Kind_Of             => IPv4_Host,
                     Regular_Name_Length => 0,
                     User_Info_Length    => 0,
                     User_Info           => "",
                     Host                => Create (Address),
                     Default_Port        => True
                  );
   begin
      return Create (Authority) / Path;
   end "/";

   function "/" (Address : IPv6_Address; Path : String)
      return IP_Hierarchical_Part is
      Authority : constant IP_Authority :=
                  (  Kind_Of             => IPv6_Host,
                     Regular_Name_Length => 0,
                     User_Info_Length    => 0,
                     User_Info           => "",
                     Host                => Create (Address),
                     Default_Port        => True
                  );
   begin
      return Create (Authority) / Path;
   end "/";

   function "/" (Authority : IP_Authority; Path : String)
      return IP_Hierarchical_Part is
      Result : IP_Hierarchical_Part
               (  Kind_Of             => Authority_Part,
                  Authority_Kind_Of   => Authority.Kind_Of,
                  Regular_Name_Length => Authority.Regular_Name_Length,
                  User_Info_Length    => Authority.User_Info_Length,
                  Default_Port        => Authority.Default_Port,
                  Path_Length         => Path'Length
               );
   begin
      Result.Path      := Create (Path);
      Result.Authority := Authority;
      return Result;
   end "/";

   function "/" (Hierarchical : IP_Hierarchical_Part; Path : String)
      return IP_Hierarchical_Part is
   begin
      case Hierarchical.Kind_Of is
         when Authority_Part =>
            if Hierarchical.Path_Length = 0 then
               declare
                  Result : IP_Hierarchical_Part
                     (  Kind_Of => Authority_Part,
                        Authority_Kind_Of =>
                           Hierarchical.Authority_Kind_Of,
                        Regular_Name_Length =>
                           Hierarchical.Regular_Name_Length,
                        User_Info_Length =>
                           Hierarchical.User_Info_Length,
                        Default_Port => Hierarchical.Default_Port,
                        Path_Length => Path'Length
                     );
               begin
                  Result.Path      := Create (Path);
                  Result.Authority := Hierarchical.Authority;
                  return Result;
               end;
            else
               declare
                  Result : IP_Hierarchical_Part
                     (  Kind_Of => Authority_Part,
                        Authority_Kind_Of =>
                           Hierarchical.Authority_Kind_Of,
                        Regular_Name_Length =>
                           Hierarchical.Regular_Name_Length,
                        User_Info_Length =>
                           Hierarchical.User_Info_Length,
                        Default_Port => Hierarchical.Default_Port,
                        Path_Length =>
                           Hierarchical.Path_Length + Path'Length + 1
                     );
               begin
                  Result.Path      := Hierarchical.Path / Path;
                  Result.Authority := Hierarchical.Authority;
                  return Result;
               end;
            end if;
         when Absolute_Part =>
            declare
               Result : IP_Hierarchical_Part
                  (  Kind_Of             => Absolute_Part,
                     Authority_Kind_Of   => Regular_Name_Host,
                     Regular_Name_Length => 0,
                     User_Info_Length    => 0,
                     Default_Port        => True,
                     Path_Length =>
                        Hierarchical.Path_Length + Path'Length + 1
                  );
            begin
               Result.Path := Hierarchical.Path / Path;
               return Result;
            end;
         when Rootless_Part =>
            if Hierarchical.Path_Length = 0 then
               if Path'Length = 0 then
                  raise Constraint_Error;
               elsif Path (Path'First) = '/' then
                  if Path'Length <= 1 or else
                     Path (Path'First + 1) = '/' then
                        raise Constraint_Error;
                  else
                     return
                     (  Kind_Of             => Absolute_Part,
                        Authority_Kind_Of   => Regular_Name_Host,
                        Regular_Name_Length => 0,
                        User_Info_Length    => 0,
                        Default_Port        => True,
                        Path_Length         => Path'Length - 1,
                        Path                =>
                           Create (Path (Path'First + 1..Path'Last))
                     );
                  end if;
               else
                  return
                  (  Kind_Of             => Rootless_Part,
                     Authority_Kind_Of   => Regular_Name_Host,
                     Regular_Name_Length => 0,
                     User_Info_Length    => 0,
                     Default_Port        => True,
                     Path_Length         => Path'Length,
                     Path                => Create (Path)
                  );
               end if;
            else
               declare
                  Result : IP_Hierarchical_Part
                     (  Kind_Of             => Rootless_Part,
                        Authority_Kind_Of   => Regular_Name_Host,
                        Regular_Name_Length => 0,
                        User_Info_Length    => 0,
                        Default_Port        => True,
                        Path_Length =>
                           Hierarchical.Path_Length + Path'Length + 1
                     );
               begin
                  Result.Path := Hierarchical.Path / Path;
                  return Result;
               end;
           end if;
      end case;
   end "/";

   function Create (Value : String) return IP_Path is
      Result : IP_Path (Value'Length);
   begin
      Result.Value := Value;
      return Result;
   end Create;

   function Create (Name : String) return IP_Authority is
   begin
      return
      (  Kind_Of             => Regular_Name_Host,
         Regular_Name_Length => Name'Length,
         User_Info_Length    => 0,
         User_Info           => "",
         Default_Port        => True,
         Host                => Create (Name)
      );
   end Create;

   function Create (Address : IPv4_Address) return IP_Host is
   begin
      return (IPv4_Host, 0, Address);
   end Create;

   function Create (Address : IPv6_Address) return IP_Host is
   begin
      return (IPv6_Host, 0, Address);
   end Create;

   function Create (Name : String) return IP_Host is
   begin
      return (Regular_Name_Host, Name'Length, Name);
   end Create;

   function Create (Query : String) return IP_Query is
      Result : IP_Query (Query'Length);
   begin
      Result.Value := Query;
      return Result;
   end Create;

   function Create (Fragment : String) return IP_Fragment is
      Result : IP_Fragment (Fragment'Length);
   begin
      Result.Value := Fragment;
      return Result;
   end Create;

   function Create (Authority : IP_Authority)
      return IP_Hierarchical_Part is
      Result : IP_Hierarchical_Part
               (  Kind_Of              => Authority_Part,
                  Authority_Kind_Of    => Authority.Kind_Of,
                  Regular_Name_Length  => Authority.Regular_Name_Length,
                  User_Info_Length     => 0,
                  Default_Port         => Authority.Default_Port,
                  Path_Length          => -1
                 );
   begin
      Result.Authority := Authority;
      return Result;
   end Create;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out IPv4_Address
             )  is
      Result : IPv4_Address := (others => 0);
      Index  : Integer      := Pointer;

      function Get return IPv4_Octet is
         Result : Integer := 0;
         Start  : constant Integer := Index;
      begin
         if Index > Source'Last         or else
            Source (Index) not in '0'..'9' then
            if Index = Pointer then
               raise End_Error;
            else
               raise Data_Error;
            end if;
         end if;
         if Source (Index) = '0' then
            Index := Index + 1;
            if Index <= Source'Last   and then
               Source (Index) in '0'..'9' then
               raise Data_Error;
            else
               return 0;
            end if;
         else
            loop
               Result := Result * 10
                       + Character'Pos (Source (Index))
                       - Character'Pos ('0');
               Index := Index + 1;
               exit when Index > Source'Last or else
                         Source (Index) not in '0'..'9';
               if Index - Start > 3 then
                  raise Data_Error;
               end if;
            end loop;
            if Result > 255 then                 
               raise Data_Error;
            end if;
            return IPv4_Octet (Result);
         end if;
      end Get;

   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      Result (1) := Get;
      for I in 2..4 loop
         if Index > Source'Last or else Source (Index) /= '.' then
            raise Data_Error;
         end if;
         Index := Index + 1;
         Result (I) := Get;
      end loop;
      Value   := Result;
      Pointer := Index;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out IPv6_Address
             )  is
      Result : IPv6_Address := (others => 0);
      Index  : Integer      := Pointer;
      Zeros  : Integer      := 0;
      Group  : Integer      := 0;
      Stop   : Integer;

      function Get return IPv6_Group is
         Result : Integer := 0;
      begin
         Get (Source, Index, Result, 16, 0, 2**16 - 1);
         return IPv6_Group (Result);
      exception
         when Constraint_Error | Data_Error | End_Error =>
            raise Data_Error;
      end Get;

   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      loop
         case Source (Index) is
            when 'A'..'F' | 'a'..'f' =>
               if Group = 8 then
                  raise Data_Error;
               end if;
               Group := Group + 1;
               Result (Group) := Get;
            when '0'..'9' =>
               if Group = 6 or else (Group < 6 and then Zeros > 0) then
                  Stop := Index + 1;
                  while Stop <= Source'Last and then
                        Source (Stop) in '0'..'9' loop
                     Stop := Stop + 1;
                  end loop;
                  if Stop <= Source'Last and then
                     Source (Stop) = '.'     then
                     declare
                        Tail : IPv4_Address;
                     begin
                        Get (Source, Index, Tail);
                        Group := Group + 1;
                        Result (Group) := IPv6_Group (Tail (1)) * 2**8
                                        + IPv6_Group (Tail (2));
                        Group := Group + 1;
                        Result (Group) := IPv6_Group (Tail (3)) * 2**8
                                        + IPv6_Group (Tail (4));
                     end;
                     exit;
                  end if;
               end if;
               if Group = 8 then
                  raise Data_Error;
               end if;
               Group := Group + 1;
               Result (Group) := Get;
            when ':' =>
               if Index < Source'Last  and then
                  Source (Index + 1) = ':' then
                  if Zeros > 0 then
                     raise Data_Error;
                  end if;
                  Zeros := Group + 1;
                  Index := Index + 2;
                  exit when Group = 7;
               else
                  Index := Index + 1;
               end if;
            when others =>
               exit;             
         end case;
         exit when Index > Source'Last;
      end loop;
      if Zeros = 0 then
         if Group /= 8 then
            raise Data_Error;
         end if;
      elsif Group = 8 or else Zeros > 8 then
         raise Data_Error;
      else
         Stop := Result'Last - (Group - Zeros);
         for To in reverse Stop..Result'Last loop
            Result (To) := Result (Group);
            Group := Group - 1;
         end loop;
         for To in Zeros..Stop - 1 loop
            Result (To) := 0;
         end loop;
      end if;
      Pointer := Index;
      Value   := Result;
   end Get;

   function Get
            (  Source  : String;
               Pointer : access Integer
            )  return IRI is
      Index  : Integer := Pointer.all;
      Scheme : Integer;

      function Get_Hexadecimal return Integer is
      begin
         if Index > Source'Last then
            raise Data_Error;
         end if;
         declare
            Code   : constant Character := Source (Index);
            Result : Integer := Character'Pos (Code);
         begin
            case Code is
               when '0'..'9' =>
                  Result := Result - Character'Pos ('0');
               when 'A'..'F' =>
                  Result := Result - Character'Pos ('A') + 10;
               when 'a'..'f' =>
                  Result := Result - Character'Pos ('a') + 10;
               when others =>
                  raise Data_Error;
            end case;
            Index := Index + 1;
            return Result;
         end;
      end Get_Hexadecimal;

      procedure Get_PCT
                (  Destination : in out String;
                   Pointer     : in out Integer
                )  is
         Code : Integer;
      begin
         if Index >= Source'Last then
            raise Data_Error;
         end if;
         Code := Get_Hexadecimal * 16;
         Code := Code + Get_Hexadecimal;
         if Code > Integer (UTF8_Code_Point'Last) then
            raise Data_Error;
         end if;
         Put (Destination, Pointer, UTF8_Code_Point (Code));
      end Get_PCT;

      function Get_PCT return Positive is
         Code : Integer;
      begin
         if Index >= Source'Last then
            raise Data_Error;
         end if;
         Code := Get_Hexadecimal * 16;
         Code := Code + Get_Hexadecimal;
         if Code > Integer (UTF8_Code_Point'Last) then
            raise Data_Error;
         end if;
         return Size (UTF8_Code_Point (Code));
      end Get_PCT;

      function Get_PCT
               (  Set        : Unicode_Set;
                  Terminator : String := ""
               )  return String is
         Code   : UTF8_Code_Point;
         Length : Natural := 0;
         Start  : constant Integer := Index;
         Next   : Integer;
      begin
         while Index <= Source'Last loop
            case Source (Index) is
               when '%' =>
                  Index  := Index  + 1;
                  Length := Length + Get_PCT;
               when others =>
                  Next := Index;
                  Get (Source, Next, Code);
                  exit when not Is_In (Code, Set);
                  Index  := Next;
                  Length := Length + Size (Code);
            end case;
         end loop;
         if Is_Prefix (Terminator, Source, Index) then 
            declare
               Result  : String (1..Length);
               Pointer : Integer := 1;
            begin
               Index := Start;
               while Index <= Source'Last loop
                  case Source (Index) is
                     when '%' =>
                        Index := Index + 1;
                        Get_PCT (Result, Pointer);
                     when others =>
                        Next := Index;
                        Get (Source, Next, Code);
                        exit when not Is_In (Code, Set);
                        Index := Next;
                        Put (Result, Pointer, Code);
                  end case;
               end loop;
               Index := Index + Terminator'Length;
               return Result;
            end;
         else
            Index := Start;
            return "";
         end if;
      end Get_PCT;

      function Get_Host return IP_Host is
      begin
          if Index > Source'Last then
             raise Data_Error;
          end if;
          case Source (Index) is
             when '[' =>
                Index := Index + 1;
                declare
                   Address : IPv6_Address;
                begin
                   Get (Source, Index, Address);
                   if Index > Source'Last or else
                      Source (Index) /= ']'  then
                      raise Data_Error;
                   end if;
                   Index := Index + 1;             
                   return Create (Address);
                end;
             when '0'..'9' =>
                declare
                   Address : IPv4_Address;
                begin
                   Get (Source, Index, Address);
                   return Create (Address);
                end;
             when others =>
                return Create (Get_PCT (Regular_Name_Char));
          end case;
      end Get_Host;

      function Get_Path (No_Empty_First : Boolean) return String is
         Result : constant String := Get_PCT (Path_Char);
      begin
         if No_Empty_First then
            if Result'Length >= 1      and then
               Result (Result'First) = '/' then
               raise Data_Error;
            end if;
         end if;
         return Result;
      end Get_Path;

      function Get_User_Info return String is
      begin
         return Get_PCT (User_Info_Char, "@");
      end Get_User_Info;

      function Get_Scheme return Integer is
      begin
         if Index >= Source'Last then
            raise End_Error;
         end if;
         case Source (Index) is
            when 'a'..'z' | 'A'..'Z' =>
               Index := Index + 1;
            when others =>
               raise End_Error;
         end case;
         while Index <= Source'Last loop
            case Source (Index) is
               when 'a'..'z' | 'A'..'Z' | '0'..'9' | '+' | '-' | '.' =>
                  Index := Index + 1;
               when ':' =>
                  Index := Index + 1;
                  return Index - 2;
               when others =>
                  raise Data_Error;
            end case;
         end loop;
         raise End_Error;
      end Get_Scheme;

      function Get_Hierarchical return IP_Hierarchical_Part is
      begin
         if Index <= Source'Last and then Source (Index) = '/' then
            Index := Index + 1;
            if Index <= Source'Last and then Source (Index) = '/' then
               Index := Index + 1;
               declare
                  User_Info : constant String := Get_User_Info;
               begin
                  declare
                     Host : constant IP_Host := Get_Host;
                  begin
                     if Index > Source'Last then
                        return Create (User_Info * Host);
                     elsif Source (Index) = ':' then
                        Index := Index + 1;
                        declare
                           Port : IP_Port;
                        begin
                           begin
                              Get (Source, Index, Integer (Port));
                           exception
                              when others =>
                                 raise Data_Error;
                           end;
                           if Index <= Source'Last and then
                              Source (Index) = '/'    then
                              Index := Index + 1;
                              return User_Info * Host / Port /
                                     Get_Path (False);
                           else
                              return Create (User_Info * Host / Port);
                           end if;
                        end;
                     elsif Source (Index) = '/' then
                           Index := Index + 1;
                        return User_Info * Host / Get_Path (False);
                     else
                        return Create (User_Info * Host);
                    end if;
                 end;
               end;
            else
               return No_Authority / ("/" & Get_Path (True));
            end if;
         else
            declare
               Path : constant String := Get_Path (True);
            begin
               if Path'Length = 0 then
                  return No_Authority;
               else
                  return No_Authority / Path;
               end if;
            end;
         end if;      
      end Get_Hierarchical;

      function Get_Query return IP_Query is
      begin
         if Index > Source'Last or else Source (Index) /= '?' then
            return No_Query;
         else
            Index := Index + 1;
            return Create (Get_PCT (Query_Char));
         end if;
      end Get_Query;

      function Get_Fragment return IP_Fragment is
      begin
         if Index > Source'Last or else Source (Index) /= '#' then
            return No_Fragment;
         else
            Index := Index + 1;
            return Create (Get_PCT (Fragment_Char));
         end if;
      end Get_Fragment;

   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      Scheme := Get_Scheme;
      declare
         Hierarchical : constant IP_Hierarchical_Part :=
                                 Get_Hierarchical;
      begin
         declare
            Query : constant IP_Query := Get_Query;
         begin
            declare
               Fragment : constant IP_Fragment := Get_Fragment;
               Result   : constant IRI :=
                             Source (Pointer.all..Scheme) +
                             Hierarchical                 &
                             Query                        +
                             Fragment;
            begin
               Pointer.all := Index;
               return Result;
            end;
         end;
      end;
   end Get;

   function Get_Length (Value : IP_Path) return Natural is
      Length : Natural := 1;
   begin
      if Value.Length < 0 then
         return 0;
      end if;
      declare
         Path : String renames Value.Value;
      begin
         for Index in Path'Range loop
            if Path (Index) = '/' then
               Length := Length + 1;
            end if;
         end loop;
         return Length;
      end;
   end Get_Length;

   function Get_Length
            (  Value : IP_Hierarchical_Part
            )  return Natural is
   begin
      return Get_Length (Value.Path);
   end Get_Length;

   function Get_Length (Value : IRI) return Natural is
   begin
      return Get_Length (Value.Hierarchical.Path);
   end Get_Length;

   function Get_Segment (Value : IP_Path; No : Positive)
      return String is
   begin
      if Value.Length < 0 then
         raise Constraint_Error;
      end if;
      declare
         Path   : String renames Value.Value;
         Length : Natural  := 1;
         Start  : Positive := Path'First;
      begin
         for Index in Path'Range loop
            if Path (Index) = '/' then
               Length := Length + 1;
               if Length > No then
                  return Path (Start..Index - 1);
               end if;
               Start := Index + 1;
            end if;
         end loop;
         if Length = No then
            return Path (Start..Path'Last);
         else
            raise Constraint_Error;
         end if;
      end;
   end Get_Segment;

   function Get_Segment
            (  Value : IP_Hierarchical_Part;
               No    : Positive
            )  return String is
   begin
      return Get_Segment (Value.Path, No);
   end Get_Segment;

   function Get_Segment (Value : IRI; No : Positive) return String is
   begin
      return Get_Segment (Value.Hierarchical.Path, No);
   end Get_Segment;

   function Get_Suffix (Value : IP_Path) return IP_Path is
   begin
      if Value.Length < 0 then
         return Null_Path;
      end if;
      declare
         Path : String renames Value.Value;
      begin
         for Index in Path'Range loop
            if Path (Index) = '/' then
               declare
                  Result : IP_Path (Path'Last - Index);
               begin
                  Result.Value := Path (Index + 1..Path'Last);
                  return Result;
               end;
            end if;
         end loop;
         return Null_Path;
      end;
   end Get_Suffix;

   function Get_Suffix (Value : IP_Hierarchical_Part) return IP_Path is
   begin
      return Get_Suffix (Value.Path);
   end Get_Suffix;

   function Get_Suffix (Value : IRI) return IP_Path is
   begin
      return Get_Suffix (Value.Hierarchical.Path);
   end Get_Suffix;

   function Get_Scheme_Length (Source : String) return Natural is
      Pointer : Integer := Source'First;
   begin
      if Pointer > Source'Last then
         return 0;
      end if;
      case Source (Pointer) is
         when 'a'..'z' | 'A'..'Z' =>
            Pointer := Pointer + 1;
         when others =>
            return 0;
      end case;
      while Pointer <= Source'Last loop
         case Source (Pointer) is
            when 'a'..'z' | 'A'..'Z' | '0'..'9' | '+' | '-' | '.' =>
               Pointer := Pointer + 1;
            when ':' =>
               return Pointer - Source'First + 1;
            when others =>
               return 0;
         end case;
      end loop;
      return 0;
   end Get_Scheme_Length;

   function Get_Scheme (Source : String) return String is
      Pointer : aliased Integer := Source'First;
   begin
      return Get_Scheme (Source, Pointer'Access);
   end Get_Scheme;

   function Get_Scheme (Source : String; Pointer : access Integer)
      return String is
      Index : constant Integer := Pointer.all;
   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index - 1 > Source'Last then
            raise Layout_Error;
         else
            return "";
         end if;
      end if;
      Pointer.all :=
         Index + Get_Scheme_Length (Source (Index..Source'Last));
      return Source (Index..Pointer.all - 1);
   end Get_Scheme;

   function Is_Scheme (Source : String; Pointer : Integer)
      return Boolean is
   begin
      if Pointer < Source'First then
         raise Layout_Error;
      end if;
      if Pointer > Source'Last then
         if Pointer - 1 > Source'Last then
            raise Layout_Error;
         else
            return False;
         end if;
      end if;
      return Is_Scheme (Source (Pointer..Source'Last));
   end Is_Scheme;

   function Is_Scheme (Source : String) return Boolean is
   begin
      return Get_Scheme_Length (Source) /= 0;
   end Is_Scheme;

   function Image (Value : IPv4_Address) return String is
      Result  : String (1..3 * 4 + 3);
      Pointer : Integer := 1;
   begin
      Put (Result, Pointer, Integer (Value (1)));
      Put (Result, Pointer, ".");
      Put (Result, Pointer, Integer (Value (2)));
      Put (Result, Pointer, ".");
      Put (Result, Pointer, Integer (Value (3)));
      Put (Result, Pointer, ".");
      Put (Result, Pointer, Integer (Value (4)));
      return Result (1..Pointer - 1);
   end Image;

   function Image (Value : IPv6_Address) return String is
      Result  : String (1..3 * 4 + 3 + 7 * 4 + 2);
      Pointer : Integer := 1;
      From    : Integer := Result'Last + 1; -- Zero sequence
      To      : Integer := From;
      Colon   : Boolean := False;
      Length  : Integer := 0;

      procedure Put (Group : IPv6_Group) is
         Text   : String (1..4);
         Index  : Integer    := Text'Last + 1;
         Value  : IPv6_Group := Group;
         Length : Integer;
      begin
         loop
            Index := Index - 1;
            Text (Index) := Digit (Integer (Value mod 16) + 1);
            Value := Value / 16;
            exit when Value = 0;
         end loop;
         Length := Text'Last - Index + 1;
         Result (Pointer..Pointer + Length - 1) :=
            Text (Index..Text'Last);
         Pointer := Pointer + Length;
      end;
   begin
      for Index in Value'Range loop
         if Value (Index) = 0 then
            Length := Length + 1;
         elsif Length > 0 then
            if Length > 1 and then Length - 1 > To - From then
               From := Index - Length;
               To   := Index - 1;
            end if;
            Length := 0;
         end if;
      end loop;
      if Length > 1 and then Length - 1 > To - From then
         From := 9 - Length;
         To   := 8;
      end if;
      for Index in 1..Value'Last loop
         if Index = From then
            Result (Pointer) := ':';
            Pointer := Pointer + 1;
            Result (Pointer) := ':';
            Pointer := Pointer + 1;
            Colon := False;
         elsif Index < From or else Index > To then
            if Colon then
               Result (Pointer) := ':';
               Pointer := Pointer + 1;
            end if;
            Put (Value (Index));
            Colon := True;
         end if;
      end loop;
      return Result (1..Pointer - 1);
   end Image;

   function Image
            (  Value   : IRI;
               Unicode : Boolean := True
            )  return String is
      Size : Integer := 512;
   begin
      loop
         declare
            Result  : String (1..Size);
            Pointer : Integer := 1;
         begin
            Put (Result, Pointer, Value, Unicode);
            return Result (1..Pointer - 1);
         exception
            when Layout_Error =>
               Size := (Size * 3) / 2;
         end;
      end loop;
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : IPv4_Address;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put (Destination, Pointer, Image (Value), Field, Justify, Fill);
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : IPv6_Address;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put (Destination, Pointer, Image (Value), Field, Justify, Fill);
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : IRI;
                Unicode     : Boolean   := True;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text  : Output renames
                     Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;

      procedure Put_PCT (Source : String; Set : Unicode_Set) is
         Code    : UTF8_Code_Point;
         Pointer : Integer := Source'First;
      begin
         while Pointer <= Source'Last loop
            Get (Source, Pointer, Code);
            if Is_In (Code, Set) and then
               (Unicode or else Code in 33..126)
            then
               Put (Text, Index, Code);
            else
               Put (Text, Index, '%');
               Put
                (  Destination => Text,
                   Pointer     => Index,
                   Value       => Integer (Code),
                   Base        => 16,
                   Justify     => Right,
                   Fill        => '0',
                   Field       => 2
               );
            end if;               
         end loop;
      exception
         when Layout_Error | Data_Error =>
            raise Constraint_Error;
      end Put_PCT;
   begin
      Put (Text, Index, Value.Scheme);
      Put (Text, Index, ":");
      case Value.Hierarchical_Kind_Of is
         when Authority_Part =>
            declare
               Authority : IP_Authority renames
                           Value.Hierarchical.Authority;
            begin
               Put (Text, Index, "//");
               if Authority.User_Info'Length > 0 then
                  Put (Text, Index, Authority.User_Info);
                  Put (Text, Index, "@");
               end if;
               case Authority.Host.Kind_Of is
                  when IPv4_Host =>
                     Put (Text, Index, Authority.Host.V4_Address);
                  when IPv6_Host =>
                     Put (Text, Index, "[");
                     Put (Text, Index, Authority.Host.V6_Address);
                     Put (Text, Index, "]");
                  when Regular_Name_Host =>
                     Put_PCT (Authority.Host.Name, Regular_Name_Char);
               end case;
               if not Authority.Default_Port then
                  Put (Text, Index, ":");
                  Put (Text, Index, Integer (Authority.Port));
               end if;
            end;
            if Value.Hierarchical.Path.Length >= 0 then
               Put (Text, Index, "/");         
            end if;
         when Absolute_Part =>
            Put (Text, Index, "/");
         when Rootless_Part =>
            null;
      end case;
      if Value.Hierarchical.Path.Length > 0 then
         Put_PCT (Value.Hierarchical.Path.Value, Path_Char);
      end if;
      if Value.Query_Length >= 0 then
         Put (Text, Index, "?");
         Put_PCT (Value.Query.Value, Query_Char);
      end if;
      if Value.Fragment_Length >= 0 then
         Put (Text, Index, "#");
         Put_PCT (Value.Fragment.Value, Fragment_Char);
      end if;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   function Value (Source : String) return IPv4_Address is
      Result  : IPv4_Address;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   function Value (Source : String) return IPv6_Address is
      Result  : IPv6_Address;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   function Value (Source : String) return IRI is
      Pointer : aliased Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      declare
         Result : constant IRI := Get (Source, Pointer'Access);
      begin
         Get (Source, Pointer, SpaceAndTab);
         if Pointer /= Source'Last + 1 then
            raise Data_Error;
         end if;
         return Result;
      end;
   end Value;

end Strings_Edit.RFC_3897;
