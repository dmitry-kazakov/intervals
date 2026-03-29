--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_RFC_3897                               Luebeck            --
--  Test                                           Winter, 2026       --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;
with Strings_Edit.RFC_3897;  use Strings_Edit.RFC_3897;
with Strings_Edit.UTF8;      use Strings_Edit.UTF8;

procedure Test_RFC_3897 is

   Test_Error : exception;

   function "+" (Fault : Exception_Occurrence) return String is
      Result : constant String := Exception_Name (Fault);
   begin
      for Index in reverse Result'Range loop
         if Result (Index) = '.' then
            return Result (Index + 1..Result'Last);
         end if;
      end loop;
      return Result;
   end "+";

   procedure Test_IRI_Image
             (  Text  : String;
                Value : IRI;
                PCT   : Boolean := False
             )  is
   begin
      if Image (Value, PCT) /= Text then
         Put_Line
         (  Image (Value, PCT)
         &  " /= "
         &  Text
         &  " (expected)"
         );
         raise Test_Error;
      end if;
   end Test_IRI_Image;

   procedure Test_IRI_Value
             (  Text  : String;
                Data  : IRI;
                Error : String  := ""
             )  is
   begin
      declare
         Result : constant IRI := Value (Text);
      begin
         if Error'Length > 0 then
            Put_Line (Text & " /= " & Error & " (expected)");
            raise Test_Error;
         elsif Result /= Data then
            Put_Line (Image (Result) & " /= " & Text & " (expected)");
            raise Test_Error;
         end if;
      end;
   exception
      when Test_Error =>
         raise;
      when Fault : others =>
         if Error /= +Fault then
            Put_Line
            (  Text
            &  " raised "
            &  (+Fault)
            &  " /= "
            &  Error
            &  " (expected)"
            );
            raise Test_Error;
         end if;
   end Test_IRI_Value;

   procedure Test_IPv4_Image (Text : String; Value : IPv4_Address) is
   begin
      if Image (Value) /= Text then
         Put_Line
         (  Image (Value)
         &  "/="
         &  Text
         &  " (expected)"
         );
         raise Test_Error;
      end if;
   end Test_IPv4_Image;

   procedure Test_IPv4_Value
             (  Text  : String;
                Data  : IPv4_Address;
                Error : String := ""
             )  is
      Result : IPv4_Address;
   begin
      Result := Value (Text);
      if Error'Length > 0 then
         Put_Line (Text & " /= " & Error & " (expected)");
         raise Test_Error;
      elsif Result /= Data then
         Put_Line (Image (Result) & "/=" & Text & " (expected)");
         raise Test_Error;
      end if;
   exception
      when Test_Error =>
         raise;
      when Fault : others =>
         if Error /= +Fault then
            Put_Line
            (  Text
            &  " raised "
            &  (+Fault)
            &  " /= "
            &  Error
            &  " (expected)"
            );
            raise Test_Error;
         end if;
   end Test_IPv4_Value;

   procedure Test_IPv6_Image (Text : String; Value : IPv6_Address) is
   begin
      if Image (Value) /= Text then
         Put_Line
         (  Image (Value)
         &  "/="
         &  Text
         &  " (expected)"
         );
         raise Test_Error;
      end if;
   end Test_IPv6_Image;

   procedure Test_IPv6_Value
             (  Text  : String;
                Data  : IPv6_Address;
                Error : String := ""
             )  is
      Result : IPv6_Address;
   begin
      Result := Value (Text);
      if Error'Length > 0 then
         Put_Line (Text & " /= " & Error & " (expected)");
         raise Test_Error;
      elsif Result /= Data then
         Put_Line (Image (Result) & "/=" & Text & " (expected)");
         raise Test_Error;
      end if;
   exception
      when Test_Error =>
         raise;
      when Fault : others =>
         if Error /= +Fault then
            Put_Line
            (  Text
            &  " raised "
            &  (+Fault)
            &  " /= "
            &  Error
            &  " (expected)"
            );
            raise Test_Error;
         end if;
   end Test_IPv6_Value;

begin
   Test_IPv4_Image ("0.0.0.0",      (  0,  0, 0,   0));
   Test_IPv4_Image ("123.10.0.255", (123, 10, 0, 255));

   Test_IPv4_Value ("0.0.0.0",      (  0,  0, 0,   0));
   Test_IPv4_Value ("123.10.0.255", (123, 10, 0, 255));
   Test_IPv4_Value (" 23.10.0.255", ( 23, 10, 0, 255));
   Test_IPv4_Value ("",             (  0,  0, 0,   0), "END_ERROR" );
   Test_IPv4_Value ("  .0.0.0",     (  0,  0, 0,   0), "END_ERROR" );
   Test_IPv4_Value ("0.00.0.0",     (  0,  0, 0,   0), "DATA_ERROR");
   Test_IPv4_Value ("0.0.0",        (  0,  0, 0,   0), "DATA_ERROR");
   Test_IPv4_Value ("0.0.0.256",    (  0,  0, 0,   0), "DATA_ERROR");

   Test_IPv6_Image ("::",              ( 0,  0,  0,  0,  0,  0, 0, 0));
   Test_IPv6_Image ("a:b:c:d:e:f:1:2", (10, 11, 12, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("a:b:c:d:e:f:0:1", (10, 11, 12, 13, 14, 15, 0, 1));
   Test_IPv6_Image ("a:b:c:d:e:f::",   (10, 11, 12, 13, 14, 15, 0, 0));
   Test_IPv6_Image ("a:b:c:d:e::",     (10, 11, 12, 13, 14,  0, 0, 0));
   Test_IPv6_Image ("a:b:c:d::",       (10, 11, 12, 13,  0,  0, 0, 0));
   Test_IPv6_Image ("a:b:c::",         (10, 11, 12,  0,  0,  0, 0, 0));
   Test_IPv6_Image ("a:b::",           (10, 11,  0,  0,  0,  0, 0, 0));
   Test_IPv6_Image ("a::",             (10,  0,  0,  0,  0,  0, 0, 0));
   Test_IPv6_Image ("a:b:c:d:e:f:0:1", (10, 11, 12, 13, 14, 15, 0, 1));
   Test_IPv6_Image ("a:b:c:d:e::1",    (10, 11, 12, 13, 14,  0, 0, 1));
   Test_IPv6_Image ("a:b:c:d::1",      (10, 11, 12, 13,  0,  0, 0, 1));
   Test_IPv6_Image ("a:b:c::1",        (10, 11, 12,  0,  0,  0, 0, 1));
   Test_IPv6_Image ("a:b::1",          (10, 11,  0,  0,  0,  0, 0, 1));
   Test_IPv6_Image ("a::1",            (10,  0,  0,  0,  0,  0, 0, 1));
   Test_IPv6_Image ("::1",             ( 0,  0,  0,  0,  0,  0, 0, 1));
   Test_IPv6_Image ("a:b:c:d::1",      (10, 11, 12, 13,  0,  0, 0, 1));
   Test_IPv6_Image ("a:b:c::1",        (10, 11, 12,  0,  0,  0, 0, 1));
   Test_IPv6_Image ("a:b::1",          (10, 11,  0,  0,  0,  0, 0, 1));
   Test_IPv6_Image ("a:b:c::1",        (10, 11, 12,  0,  0,  0, 0, 1));
   Test_IPv6_Image ("a:b::1",          (10, 11,  0,  0,  0,  0, 0, 1));
   Test_IPv6_Image ("a:b:c:d:e:0:1:2", (10, 11, 12, 13, 14,  0, 1, 2));
   Test_IPv6_Image ("a:b:c:d::1:2",    (10, 11, 12, 13,  0,  0, 1, 2));
   Test_IPv6_Image ("a:b:c::1:2",      (10, 11, 12,  0,  0,  0, 1, 2));
   Test_IPv6_Image ("a:b::1:2",        (10, 11,  0,  0,  0,  0, 1, 2));
   Test_IPv6_Image ("a::1:2",          (10,  0,  0,  0,  0,  0, 1, 2));
   Test_IPv6_Image ("::1:2",           ( 0,  0,  0,  0,  0,  0, 1, 2));
   Test_IPv6_Image ("a:b:c:d:0:f:1:2", (10, 11, 12, 13,  0, 15, 1, 2));
   Test_IPv6_Image ("a:b:c::f:1:2",    (10, 11, 12,  0,  0, 15, 1, 2));
   Test_IPv6_Image ("a:b::f:1:2",      (10, 11,  0,  0,  0, 15, 1, 2));
   Test_IPv6_Image ("a::f:1:2",        (10,  0,  0,  0,  0, 15, 1, 2));
   Test_IPv6_Image ("::f:1:2",         ( 0,  0,  0,  0,  0, 15, 1, 2));
   Test_IPv6_Image ("a:b:c:0:e:f:1:2", (10, 11, 12,  0, 14, 15, 1, 2));
   Test_IPv6_Image ("a:b::e:f:1:2",    (10, 11,  0,  0, 14, 15, 1, 2));
   Test_IPv6_Image ("a::e:f:1:2",      (10,  0,  0,  0, 14, 15, 1, 2));
   Test_IPv6_Image ("::e:f:1:2",       ( 0,  0,  0,  0, 14, 15, 1, 2));
   Test_IPv6_Image ("a:b:0:d:e:f:1:2", (10, 11,  0, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("a::d:e:f:1:2",    (10,  0,  0, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("::d:e:f:1:2",     ( 0,  0,  0, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("a:0:c:d:e:f:1:2", (10,  0, 12, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("::c:d:e:f:1:2",   ( 0,  0, 12, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("0:b:c:d:e:f:1:2", ( 0, 11, 12, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("::c:d:e:f:1:2",   ( 0,  0, 12, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("::d:e:f:1:2",     ( 0,  0,  0, 13, 14, 15, 1, 2));
   Test_IPv6_Image ("::e:f:1:2",       ( 0,  0,  0,  0, 14, 15, 1, 2));
   Test_IPv6_Image ("::f:1:2",         ( 0,  0,  0,  0,  0, 15, 1, 2));
   Test_IPv6_Image ("::1:2",           ( 0,  0,  0,  0,  0,  0, 1, 2));
   Test_IPv6_Image ("::2",             ( 0,  0,  0,  0,  0,  0, 0, 2));
   Test_IPv6_Image ("::d:e:0:0:2",     ( 0,  0,  0, 13, 14,  0, 0, 2));
   Test_IPv6_Image ("::d:0:0:0:2",     ( 0,  0,  0, 13,  0,  0, 0, 2));
   Test_IPv6_Image ("0:0:0:d::",       ( 0,  0,  0, 13,  0,  0, 0, 0));
   Test_IPv6_Image ("a:0:0:d::2",      (10,  0,  0, 13,  0,  0, 0, 2));
   Test_IPv6_Image ("a::e:0:0:2",      (10,  0,  0,  0, 14,  0, 0, 2));

   Test_IPv6_Image ("1:2:3:4:5:6:12:34", (1, 2, 3, 4, 5, 6, 18, 52));

   Test_IPv6_Value ("1:2:3:4:5:6:7:8", (1, 2, 3, 4, 5, 6, 7, 8));
   Test_IPv6_Value ("1:2:3:4:5:6:7::", (1, 2, 3, 4, 5, 6, 7, 0));
   Test_IPv6_Value ("1:2:3:4:5:6::8",  (1, 2, 3, 4, 5, 6, 0, 8));
   Test_IPv6_Value ("1:2:3:4:5::7:8",  (1, 2, 3, 4, 5, 0, 7, 8));
   Test_IPv6_Value ("1:2:3:4::6:7:8",  (1, 2, 3, 4, 0, 6, 7, 8));
   Test_IPv6_Value ("1:2:3::5:6:7:8",  (1, 2, 3, 0, 5, 6, 7, 8));
   Test_IPv6_Value ("1:2::4:5:6:7:8",  (1, 2, 0, 4, 5, 6, 7, 8));
   Test_IPv6_Value ("1::3:4:5:6:7:8",  (1, 0, 3, 4, 5, 6, 7, 8));

   Test_IPv6_Value ("1:2:3:4:5:6::",   (1, 2, 3, 4, 5, 6, 0, 0));
   Test_IPv6_Value ("1:2:3:4:5::8",    (1, 2, 3, 4, 5, 0, 0, 8));
   Test_IPv6_Value ("1:2:3:4::7:8",    (1, 2, 3, 4, 0, 0, 7, 8));
   Test_IPv6_Value ("1:2:3::6:7:8",    (1, 2, 3, 0, 0, 6, 7, 8));
   Test_IPv6_Value ("1:2::5:6:7:8",    (1, 2, 0, 0, 5, 6, 7, 8));
   Test_IPv6_Value ("1::4:5:6:7:8",    (1, 0, 0, 4, 5, 6, 7, 8));
   Test_IPv6_Value ("::3:4:5:6:7:8",   (0, 0, 3, 4, 5, 6, 7, 8));

   Test_IPv6_Value ("1:2:3:4:5::",     (1, 2, 3, 4, 5, 0, 0, 0));
   Test_IPv6_Value ("1:2:3:4::8",      (1, 2, 3, 4, 0, 0, 0, 8));
   Test_IPv6_Value ("1:2:3::7:8",      (1, 2, 3, 0, 0, 0, 7, 8));
   Test_IPv6_Value ("1:2::6:7:8",      (1, 2, 0, 0, 0, 6, 7, 8));
   Test_IPv6_Value ("1::5:6:7:8",      (1, 0, 0, 0, 5, 6, 7, 8));
   Test_IPv6_Value ("::4:5:6:7:8",     (0, 0, 0, 4, 5, 6, 7, 8));

   Test_IPv6_Value ("1:2:3:4::",       (1, 2, 3, 4, 0, 0, 0, 0));
   Test_IPv6_Value ("1:2:3::8",        (1, 2, 3, 0, 0, 0, 0, 8));
   Test_IPv6_Value ("1:2::7:8",        (1, 2, 0, 0, 0, 0, 7, 8));
   Test_IPv6_Value ("1::6:7:8",        (1, 0, 0, 0, 0, 6, 7, 8));
   Test_IPv6_Value ("::5:6:7:8",       (0, 0, 0, 0, 5, 6, 7, 8));

   Test_IPv6_Value ("1:2:3::",         (1, 2, 3, 0, 0, 0, 0, 0));
   Test_IPv6_Value ("1:2::8",          (1, 2, 0, 0, 0, 0, 0, 8));
   Test_IPv6_Value ("1::7:8",          (1, 0, 0, 0, 0, 0, 7, 8));
   Test_IPv6_Value ("::6:7:8",         (0, 0, 0, 0, 0, 6, 7, 8));

   Test_IPv6_Value ("1:2::",           (1, 2, 0, 0, 0, 0, 0, 0));
   Test_IPv6_Value ("1::8",            (1, 0, 0, 0, 0, 0, 0, 8));
   Test_IPv6_Value ("::7:8",           (0, 0, 0, 0, 0, 0, 7, 8));

   Test_IPv6_Value ("1::",             (1, 0, 0, 0, 0, 0, 0, 0));
   Test_IPv6_Value ("::8",             (0, 0, 0, 0, 0, 0, 0, 8));

   Test_IPv6_Value ("1:2:3:4:5:6:7:8:9",  (others => 0), "DATA_ERROR");
   Test_IPv6_Value ("1:2:3:4:5:6:7:8::",  (others => 0), "DATA_ERROR");
   Test_IPv6_Value ("1:2:3:4::6:7:8::",   (others => 0), "DATA_ERROR");
   Test_IPv6_Value ("::2:3:4::6:7:8::",   (others => 0), "DATA_ERROR");

   Test_IPv6_Value ("1:2:3:4:5:6:1.2.3.4",
                                          (1, 2, 3, 4, 5, 6, 258, 772));
   Test_IPv6_Value ("1:2:3:4::1.2.3.4",   (1, 2, 3, 4, 0, 0, 258, 772));
   Test_IPv6_Value ("1:2:3::1.2.3.4",     (1, 2, 3, 0, 0, 0, 258, 772));
   Test_IPv6_Value ("1:2::1.2.3.4",       (1, 2, 0, 0, 0, 0, 258, 772));
   Test_IPv6_Value ("1::1.2.3.4",         (1, 0, 0, 0, 0, 0, 258, 772));
   Test_IPv6_Value ("::1.2.3.4",          (0, 0, 0, 0, 0, 0, 258, 772));
   Test_IPv6_Value ("1:2:3::6:1.2.3.4",   (1, 2, 3, 0, 0, 6, 258, 772));
   Test_IPv6_Value ("1:2::6:1.2.3.4",     (1, 2, 0, 0, 0, 6, 258, 772));
   Test_IPv6_Value ("1::6:1.2.3.4",       (1, 0, 0, 0, 0, 6, 258, 772));
   Test_IPv6_Value ("::6:1.2.3.4",        (0, 0, 0, 0, 0, 6, 258, 772));
   Test_IPv6_Value ("1:2::5:6:1.2.3.4",   (1, 2, 0, 0, 5, 6, 258, 772));
   Test_IPv6_Value ("1::5:6:1.2.3.4",     (1, 0, 0, 0, 5, 6, 258, 772));
   Test_IPv6_Value ("::5:6:1.2.3.4",      (0, 0, 0, 0, 5, 6, 258, 772));
   Test_IPv6_Value ("1:2::5:6:1.2.3.4",   (1, 2, 0, 0, 5, 6, 258, 772));
   Test_IPv6_Value ("1::5:6:1.2.3.4",     (1, 0, 0, 0, 5, 6, 258, 772));

   Test_IRI_Image ("HTTP:", "HTTP" + No_Authority);
   Test_IRI_Image ("telnet://192.0.2.16:80",
                          "telnet" + IPv4_Address'(192, 0, 2, 16) / 80);
   Test_IRI_Image ("tel:+1-816-555-1212",
                          "tel" - "+1-816-555-1212");
   Test_IRI_Image ("news:comp.infosystems.www.servers.unix",
                          "news" - "comp.infosystems.www.servers.unix");
   Test_IRI_Image ("news://comp.infosystems.www.servers.unix",
                          "news" + "comp.infosystems.www.servers.unix");
   Test_IRI_Image
   (  "ldap://[2001:db8::7]/c=GB?objectClass?one",
      "ldap"                                                           +
       IPv6_Address'(1 => 16#2001#, 2 => 16#db8#, 8 => 7, others => 0) /
      "c=GB"                                                           &
      "objectClass?one"
   );
   Test_IRI_Image
   (  "mailto:John.Doe@example.com",
      "mailto" - "John.Doe@example.com"
   );
   Test_IRI_Image
   (  "http://www.example.org/D%C3%BCrst",
      "http" + "www.example.org" /
       ("D" & Image (UTF8_Code_Point_Array'(16#C3#, 16#BC#)) & "rst"),
       False
   );
   Test_IRI_Image
   (  "ftp://domain.name/path(balanced_brackets)/ending.in.dot.",
      "ftp" + "domain.name" / "path(balanced_brackets)/ending.in.dot."
   );
   Test_IRI_Image
   (  "https://www.youtube.com/watch?v=7Fz3EhdgKdA",
      "https" + "www.youtube.com" / "watch" & "v=7Fz3EhdgKdA"
   );
   Test_IRI_Image
   (  "https://192.168.1.3:80/A/B?C#D",
      "https" + IPv4_Address'(192,168,1,3) / 80 / "A" / "B" & "C" + "D"
   );

   Test_IRI_Value ("HTTP:", "HTTP" + No_Authority);
   Test_IRI_Value ("telnet://192.0.2.16:80",
                          "telnet" + IPv4_Address'(192, 0, 2, 16) / 80);
   Test_IRI_Value ("tel:+1-816-555-1212",
                          "tel" - "+1-816-555-1212");
   Test_IRI_Value ("news:comp.infosystems.www.servers.unix",
                          "news" - "comp.infosystems.www.servers.unix");
   Test_IRI_Value ("news://comp.infosystems.www.servers.unix",
                          "news" + "comp.infosystems.www.servers.unix");
   Test_IRI_Value
   (  "ldap://[2001:db8::7]/c=GB?objectClass?one",
      "ldap"                                                          +
      IPv6_Address'(1 => 16#2001#, 2 => 16#db8#, 8 => 7, others => 0) /
      "c=GB"                                                          &
      "objectClass?one"
   );
   Test_IRI_Value
   (  "mailto:John.Doe@example.com",
      "mailto" - "John.Doe@example.com"
   );
   Test_IRI_Value
   (  "http://www.example.org/D%C3%BCrst",
      "http" + "www.example.org" /
       ("D" & Image (UTF8_Code_Point_Array'(16#C3#, 16#BC#)) & "rst")
   );
   Test_IRI_Value
   (  "ftp://domain.name/path(balanced_brackets)/ending.in.dot.",
      "ftp" + "domain.name" / "path(balanced_brackets)/ending.in.dot."
   );
   Test_IRI_Value
   (  "https://www.youtube.com/watch?v=7Fz3EhdgKdA",
      "https" + "www.youtube.com" / "watch" & "v=7Fz3EhdgKdA"
   );
   Test_IRI_Value
   (  "https://192.168.1.3:80/A/B?C#D",
      "https" + IPv4_Address'(192,168,1,3) / 80 / "A" / "B" & "C" + "D"
   );
   Test_IRI_Value
   (  "https://192.168.1.3/A/B?C#D",
      "https" + IPv4_Address'(192,168,1,3) / "A" / "B" & "C" + "D"
   );
   Test_IRI_Value
   (  "https:///watch?v=7Fz3EhdgKdA",
      "https" + "" / "watch" & "v=7Fz3EhdgKdA"
   );
   Test_IRI_Value
   (  "https://www.youtube.com/?v=7Fz3EhdgKdA",
      "https" + "www.youtube.com" / "" & "v=7Fz3EhdgKdA"
   );
   Test_IRI_Value
   (  "https://www.you ube.com/?v=7Fz3EhdgKdA",
      "https" + "www.you ube.com" / "" & "v=7Fz3EhdgKdA",
      "DATA_ERROR"
   );
   Test_IRI_Value
   (  "https://www.you%20ube.com/?v=7Fz3EhdgKdA",
      "https" + "www.you ube.com" / "" & "v=7Fz3EhdgKdA"
   );
   Test_IRI_Value
   (  "https://www.you%20ube.com?query",
      "https" + "www.you ube.com" & "query"
   );
   Test_IRI_Value
   (  "https://www.you%20ube.com#fragment",
      "https" + "www.you ube.com" + "fragment"
   );
   Test_IRI_Value
   (  "http://www.example.com:80/watch",
      "http" + "www.example.com" / 80 / "watch"
   );
   Test_IRI_Value
   (  "http://[2000:00da::]/watch",
      "http" + IPv6_Address'(16#2000#, 16#DA#, others => 0) / "watch"
   );
   declare
      Value : constant IRI :=
              "https" + "www.youtube.com" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 0 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 0"
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
   declare
      Value : constant IRI :=
              "https" + "www.youtube.com" / "" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 1 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 1"
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 1) /= "" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 1))
         &  " /= """""
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
   declare
      Value : constant IRI :=
              "https" + "www.youtube.com" / "A" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 1 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 1"
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 1) /= "A" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 1))
         &  " /= ""A"""
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
   declare
      Value : constant IRI :=
              "https" + "www.youtube.com" / "A" / "B" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 2 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 2"
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 1) /= "A" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 1))
         &  " /= ""A"""
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
   declare
      Value : constant IRI :=
              "https" + "www.youtube.com" / "A" / "B" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 2 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 2"
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 2) /= "B" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is"
         &  Quote (Get_Segment (Value, 2))
         &  " /= ""B"""
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
   declare
      Value : constant IRI :=
              "https" - "www.youtube.com" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 1 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 1"
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 1) /= "www.youtube.com" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 1))
         &  " /= """""
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
   declare
      Value : constant IRI :=
              "https" - "www.youtube.com" / "A" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 2 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 2"
         & " (expected)"
         );
         raise Test_Error;
      elsif Value.Hierarchical_Kind_Of /= Rootless_Part then
         Put_Line
         (  "Path is "
         &  IP_Hierarchical_Part_Type'Image (Value.Hierarchical_Kind_Of)
         &  " /= rootless"
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 1) /= "www.youtube.com" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 1))
         &  " /= """""
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 2) /= "A" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 2))
         &  " /= ""A"""
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
   declare
      Value : constant IRI :=
              "https" - "www.youtube.com" / "" / "A" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 3 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 2"
         & " (expected)"
         );
         raise Test_Error;
      elsif Value.Hierarchical_Kind_Of /= Rootless_Part then
         Put_Line
         (  "Path is "
         &  IP_Hierarchical_Part_Type'Image (Value.Hierarchical_Kind_Of)
         &  " /= rootless"
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 1) /= "www.youtube.com" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 1))
         &  " /= """""
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 2) /= "" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 2))
         &  " /= """""
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 3) /= "A" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 3))
         &  " /= ""A"""
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
   declare
      Value : constant IRI :=
              "https" - "/www.youtube.com" / "" / "A" & "v=7Fz3EhdgKdA";
   begin
      if Get_Length (Value) /= 3 then
         Put_Line
         (  "Path length of "
         &  Image (Value)
         &  " is"
         &  Integer'Image (Get_Length (Value))
         &  " /= 2"
         & " (expected)"
         );
         raise Test_Error;
      elsif Value.Hierarchical_Kind_Of /= Absolute_Part then
         Put_Line
         (  "Path is "
         &  IP_Hierarchical_Part_Type'Image (Value.Hierarchical_Kind_Of)
         &  " /= absolute"
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 1) /= "www.youtube.com" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 1))
         &  " /= """""
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 2) /= "" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 2))
         &  " /= """""
         & " (expected)"
         );
         raise Test_Error;
      elsif Get_Segment (Value, 3) /= "A" then
         Put_Line
         (  "Path item of "
         &  Image (Value)
         &  " is "
         &  Quote (Get_Segment (Value, 3))
         &  " /= ""A"""
         & " (expected)"
         );
         raise Test_Error;
      end if;
   end;
exception
   when Error : others =>
      Put ("Error: ");
      Put_Line (Exception_Information (Error));
end Test_RFC_3897;
