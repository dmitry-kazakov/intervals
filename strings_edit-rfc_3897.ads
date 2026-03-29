--                                                                    --
--  package Strings_Edit.RFC_3897   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
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

package Strings_Edit.RFC_3897 is

   type IPv4_Octet is mod 2**8;
   type IPv4_Address is array (1..4) of IPv4_Octet;
--
-- Get -- IPv4 address
--
--    Source  - The destination string
--    Pointer - The string pointer
--    Value   - The result
--
-- This procedure gets IPv4 address.
--
-- Exceptions :
--
--    Constraint_Error - Too large values
--    Data_Error       - Syntax error
--    End_Error        - No address
--    Layout_Error     - Illegal Pointer
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out IPv4_Address
             );
--
-- Image -- Text representation of IPv4 address
--
--    Value - The address
--
-- Returns :
--
--    The result
--
   function Image (Value : IPv4_Address) return String;
--
-- Put -- IPv4 address into string
--
--    Destination - The string to put object identifier into
--    Pointer     - The first element to write
--    Value       - The value
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- The parameter Pointer is advanced beyond the value output.
--
-- Exceptions :
--
--    Layout_Error - Pointer is outside bounds or no room for output
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : IPv4_Address;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Value -- Get IPv4 address
--
--    Source - The source string
--
-- Exceptions :
--
--    Constraint_Error - Too large values
--    End_Error        - No address found
--    Data_Error       - Syntax error or not all string parsed
--
   function Value (Source : String) return IPv4_Address;
------------------------------------------------------------------------
   type IPv6_Group is mod 2**16;
   type IPv6_Address is array (1..8) of IPv6_Group;
--
-- Get -- IPv6 address
--
--    Source  - The destination string
--    Pointer - The string pointer
--    Value   - The result
--
-- This procedure gets IPv6 address.
--
-- Exceptions :
--
--    Constraint_Error - Too large values
--    Data_Error       - Syntax error
--    End_Error        - No address found
--    Layout_Error     - Illegal Pointer
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out IPv6_Address
             );
--
-- Image -- Text representation of IPv6 address
--
--    Value - The address
--
-- Returns :
--
--    The result
--
   function Image (Value : IPv6_Address) return String;
--
-- Put -- IPv6 address into string
--
--    Destination - The string to put object identifier into
--    Pointer     - The first element to write
--    Value       - The value
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- The parameter Pointer is advanced beyond the value output.
--
-- Exceptions :
--
--    Layout_Error - Pointer is outside bounds or no room for output
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : IPv6_Address;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Value -- Get IPv6 address
--
--    Source - The source string
--
-- Exceptions :
--
--    Constraint_Error - Too large values
--    End_Error        - No address found
--    Data_Error       - Syntax error or not all string parsed
--
   function Value (Source : String) return IPv6_Address;
------------------------------------------------------------------------
   type IP_Port is mod 2**16;
   type IP_Host_Type is (IPv6_Host, IPv4_Host, Regular_Name_Host);
   type IP_Host (Kind_Of : IP_Host_Type; Length : Natural) is record
      case Kind_Of is
         when IPv4_Host =>
            V4_Address : IPv4_Address;
         when IPv6_Host =>
            V6_Address : IPv6_Address;
         when Regular_Name_Host =>
            Name : String (1..Length);
      end case;
   end record;
--
-- Create -- IP host construction
--
--    Address / Name - The argument
--
-- Returns :
--
--    The IP host
--
   function Create (Address : IPv4_Address) return IP_Host;
   function Create (Address : IPv6_Address) return IP_Host;
   function Create (Name    : String      ) return IP_Host;
------------------------------------------------------------------------
   type IP_Path (Length : Integer) is record
      case Length is
         when 0..Integer'Last =>
            Value : String (1..Length);
         when others =>
            null;
      end case;
   end record;
   Null_Path : constant IP_Path;
--
-- / -- Path construction
--
--    Left, Right - The directories
--
-- Returns :
--
--    The path
--
   function "/" (Left : String;  Right : String) return IP_Path;
   function "/" (Left : IP_Path; Right : String) return IP_Path;
--
-- Create -- Create a path from string
--
--    Value - The argument
--
-- Returns :
--
--    The path
--
   function Create (Value : String) return IP_Path;
--
-- Get_Segment -- A path segment
--
--    Value - The path
--    No    - The path segment position 1..Get_Length
--
-- Returns :
--
--    The path segment
--
-- Exceptions :
--
--    Constraint_Error - No out of range 1..Get_Path_Length
--
   function Get_Segment (Value : IP_Path; No : Positive) return String;
--
-- Get_Suffix -- The path following the first segment
--
--    Value - The path
--
-- Returns :
--
--    The suffix path
--
   function Get_Suffix (Value : IP_Path) return IP_Path;
--
-- Get_Length -- The path length
--
--    Value - The path
--
-- Returns :
--
--    The number of segments in the path
--
   function Get_Length (Value : IP_Path) return Natural;
------------------------------------------------------------------------
   type IP_Authority
        (  Kind_Of             : IP_Host_Type;
           Regular_Name_Length : Natural;
           User_Info_Length    : Natural;
           Default_Port        : Boolean
        )  is
   record
      User_Info : String (1..User_Info_Length);
      Host      : IP_Host (Kind_Of, Regular_Name_Length);
      case Default_Port is
         when True =>
            null;
         when False =>
            Port : IP_Port;
      end case;
   end record;
--
-- * -- IP authority construction joe@example.com
--
--    User_Info - The user information
--    Authority - The authority
--
-- If user  information  part  if prefixed  by User_Info  if  present in
-- Authority.
--
-- Returns :
--
--    The IP authority
--
   function "*" (User_Info : String; Authority : IP_Authority)
      return IP_Authority;
--
-- * -- IP authority construction
--
--    User_Info   - The user information
--    Host / Name - The host or name
--
-- Returns :
--
--    The IP authority
--
   function "*" (User_Info : String; Host : IP_Host)
      return IP_Authority;
   function "*" (User_Info : String; Name : String)
      return IP_Authority;
--
-- / -- IP authority construction example.com:80
--
--    Name - The IP host
--    Port - The IP port
--
-- Returns :
--
--    The IP authority
--
   function "/" (Name : String; Port : IP_Port)
      return IP_Authority;
--
-- / -- IP authority construction 192.168.2.1:80
--
--    Address - The IP address
--    Port    - The IP port
--
-- Returns :
--
--    The IP authority
--
   function "/" (Address : IPv4_Address; Port : IP_Port)
      return IP_Authority;
   function "/" (Address : IPv6_Address; Port : IP_Port)
      return IP_Authority;
--
-- / -- IP authority construction
--
--    Authority - The authority
--    Port      - The port
--
-- Returns :
--
--    The IP authority
--
-- Exceptions :
--
--    Constraint_Error - The port is already specified
--
   function "/" (Authority : IP_Authority; Port : IP_Port)
      return IP_Authority;
--
-- Create -- IP authority construction from host
--
--    Host - The IP host
--
-- Returns :
--
--    The IP authority
--
   function Create (Name : String) return IP_Authority;
------------------------------------------------------------------------
   type IP_Hierarchical_Part_Type is
        (  Authority_Part,
           Absolute_Part,
           Rootless_Part
        );
   type IP_Hierarchical_Part
        (  Kind_Of             : IP_Hierarchical_Part_Type;
           Authority_Kind_Of   : IP_Host_Type;
           Regular_Name_Length : Natural;
           User_Info_Length    : Natural;
           Default_Port        : Boolean;
           Path_Length         : Integer
        )  is
   record
      Path : IP_Path (Path_Length);
      case Kind_Of is
         when Authority_Part =>
            Authority : IP_Authority
                        (  Kind_Of             => Authority_Kind_Of,
                           Regular_Name_Length => Regular_Name_Length,
                           User_Info_Length    => User_Info_Length,
                           Default_Port        => Default_Port
                        );
         when Absolute_Part | Rootless_Part =>
            null;
      end case;
   end record;
--
-- / -- IP hierarchical part construction from host and path
--
--    Authority / Address - The authority or IP address
--    Path           - The path
--
-- Returns :
--
--    The IP hierarchical part
--
   function "/" (Authority : IP_Authority; Path : String)
      return IP_Hierarchical_Part;
   function "/" (Address : IPv4_Address; Path : String)
      return IP_Hierarchical_Part;
   function "/" (Address : IPv6_Address; Path : String)
      return IP_Hierarchical_Part;
--
-- / -- Add path to the hierarchical part
--
--    Hierarchical - The hierarchical part
--    Port         - The port
--
-- Returns :
--
--    The IP hierarchical part
--
   function "/" (Hierarchical : IP_Hierarchical_Part; Path : String)
      return IP_Hierarchical_Part;
--
-- Create -- IP hierarchical part construction from authority
--
--    Authority - The IP authority
--
-- Returns :
--
--    The IP hierarchical part
--
   function Create (Authority : IP_Authority)
      return IP_Hierarchical_Part;
--
-- Get_Length -- The path length in segments
--
--    Value - The IP hierarchical part
--
-- Returns :
--
--    The path length
--
   function Get_Length
            (  Value : IP_Hierarchical_Part
            )  return Natural;
--
-- Get_Segment -- Get path segment
--
--    Value - The IP hierarchical part
--    No    - The path segment position 1..Get_Length
--
-- Returns :
--
--    The path item
--
-- Exceptions :
--
--    Constraint_Error - No out of range 1..Get_Length
--
   function Get_Segment
            (  Value : IP_Hierarchical_Part;
               No    : Positive
            )  return String;
--
-- Get_Suffix -- The path following the first segment
--
--    Value - The path
--
-- Returns :
--
--    The suffix path
--
   function Get_Suffix (Value : IP_Hierarchical_Part) return IP_Path;
--
-- A hierarchical part without autority
--
   No_Authority : constant IP_Hierarchical_Part;

   type IP_Query (Length : Integer) is record
      case Length is
         when 0..Integer'Last =>
            Value : String (1..Length);
         when others =>
            null;
      end case;
   end record;
   No_Query : constant IP_Query;
--
-- Create -- IP query from string
--
--    Query - The query
--
-- Returns :
--
--    The IP query
--
   function Create (Query : String) return IP_Query;

   type IP_Fragment (Length : Integer) is record
      case Length is
         when 0..Integer'Last =>
            Value : String (1..Length);
         when others =>
            null;
      end case;
   end record;
   No_Fragment : constant IP_Fragment;
--
-- Create -- IP fragment from string
--
--    Fragment - The fragment
--
-- Returns :
--
--    The IP fragment
--
   function Create (Fragment : String) return IP_Fragment;
------------------------------------------------------------------------
   type IRI
        (  Hierarchical_Kind_Of : IP_Hierarchical_Part_Type;
           Authority_Kind_Of    : IP_Host_Type;
           Regular_Name_Length  : Natural;
           User_Info_Length     : Natural;
           Default_Port         : Boolean;
           Path_Length          : Integer;
           Scheme_Length        : Positive;
           Query_Length         : Integer;
           Fragment_Length      : Integer
        )  is
   record
      Scheme       : String (1..Scheme_Length);
      Hierarchical : IP_Hierarchical_Part
                     (  Kind_Of             => Hierarchical_Kind_Of,
                        Authority_Kind_Of   => Authority_Kind_Of,
                        Regular_Name_Length => Regular_Name_Length,
                        User_Info_Length    => User_Info_Length,
                        Default_Port        => Default_Port,
                        Path_Length         => Path_Length
                     );
      Query        : IP_Query    (Query_Length);
      Fragment     : IP_Fragment (Fragment_Length);
   end record;
--
-- + -- Constructing IRI from scheme and hierarchical part
--
--    Scheme       - The scheme
--    Hierarchical - The hierachical part
--
-- Returns :
--
--    The IRI
--
   function "+"
            (  Scheme       : String;
               Hierarchical : IP_Hierarchical_Part
            )  return IRI;
--
-- + -- Constructing IRI from scheme and authority
--
--    Scheme    - The scheme
--    Authority - The authority
--
-- Returns :
--
--    The IRI
--
   function "+" (Scheme : String; Authority : IP_Authority) return IRI;
--
-- + -- Constructing IRI from scheme and host name
--
--    ID          - The IRI
--    Host / Path - The path
--
-- Returns :
--
--    The IRI
--
   function "+" (Scheme : String; Host : String ) return IRI;
   function "+" (Scheme : String; Path : IP_Path) return IRI;
--
-- - -- Constructing IRI without authority from scheme and path
--
--    ID   - The IRI
--    Path - The path
--
-- Returns :
--
--    The IRI
--
-- Exceptions :
--
--    Constraint_Error - Invalid path
--
   function "-" (Scheme : String; Path : String ) return IRI;
   function "-" (Scheme : String; Path : IP_Path) return IRI;
--
-- & -- Adding or extending the query part
--
--    ID   - The IRI
--    Path - The query
--
-- Returns :
--
--    The IRI
--
   function "&" (ID : IRI; Query : String  ) return IRI;
   function "&" (ID : IRI; Query : IP_Query) return IRI;
--
-- + -- Adding or extending the fragment part
--
--    ID   - The IRI
--    Path - The query
--
-- Returns :
--
--    The IRI
--
   function "+" (ID : IRI; Fragment : String     ) return IRI;
   function "+" (ID : IRI; Fragment : IP_Fragment) return IRI;
--
-- Get -- IRI
--
--    Source  - The destination string
--    Pointer - The string pointer advanced after successful completion
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Data_Error   - Syntax error
--    End_Error    - No IRI found
--    Layout_Error - Illegal Pointer
--
   function Get (Source : String; Pointer : access Integer) return IRI;
--
-- Get_Length -- The path length in segments
--
--    Value - The IRI
--
-- Returns :
--
--    The path length
--
   function Get_Length (Value : IRI) return Natural;
--
-- Get_Scheme -- IRI scheme
--
--    Source    - The destination string
--  [ Pointer ] - The string pointer
--
-- Returns :
--
--    The IRI scheme or empty string
--
-- Exceptions :
--
--    Layout_Error - Illegal Pointer
--
   function Get_Scheme (Source : String) return String;
   function Get_Scheme (Source : String; Pointer : access Integer)
      return String;
--
-- Get_Segment -- Get path segment
--
--    Value - The IRI
--    No    - The path segment position 1..Get_Path_Length
--
-- Returns :
--
--    The path item
--
-- Exceptions :
--
--    Constraint_Error - No out of range 1..Get_Path_Length
--
   function Get_Segment (Value : IRI; No : Positive) return String;
--
-- Get_Suffix -- The path following the first segment
--
--    Value - The path
--
-- Returns :
--
--    The suffix path
--
   function Get_Suffix (Value : IRI) return IP_Path;
--
-- Image -- Text representation of IRI
--
--    Value   - The IRI
--    Unicode - Use Unicode characters instead of percent encoded
--
-- Returns :
--
--    The result
--
   function Image
            (  Value   : IRI;
               Unicode : Boolean := True
            )  return String;
--
-- Is_Scheme -- Test if string contains an IRI scheme
--
--    Source    - The destination string
--  [ Pointer ] - The string pointer
--
-- This  function  can be used  before  getting  IRI to check  if IRI is
-- present. If the result is true, thenn the consequent call to Get will
-- not propagate End_Error.
--
-- Returns :
--
--    True if the string contains scheme
--
-- Exceptions :
--
--    Layout_Error - Illegal Pointer
--
   function Is_Scheme (Source : String) return Boolean;
   function Is_Scheme (Source : String; Pointer : Integer)
      return Boolean;
--
-- Put -- IRI into string
--
--    Destination - The string to put object identifier into
--    Pointer     - The first element to write
--    Value       - The value
--    Unicode     - Use Unicode characters instead of percent encoded
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- The parameter Pointer is advanced beyond the value output.
--
-- Exceptions :
--
--    Layout_Error - Pointer is outside bounds or no room for output
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : IRI;
                Unicode     : Boolean   := True;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Value -- Get IRI
--
--    Source - The source string
--
-- Exceptions :
--
--    End_Error  - No IRI found
--    Data_Error - Syntax error or not all string parsed
--
   function Value (Source : String) return IRI;

private
   Null_Path    : constant IP_Path     := (Length => -1);
   No_Query     : constant IP_Query    := (Length => -1);
   No_Fragment  : constant IP_Fragment := (Length => -1);
   No_Authority : constant IP_Hierarchical_Part :=
                           (  Kind_Of             => Rootless_Part,
                              Authority_Kind_Of   => Regular_Name_Host,
                              Regular_Name_Length => 0,
                              User_Info_Length    => 0,
                              Default_Port        => True,
                              Path_Length         => -1,
                              Path                => Null_Path
                           );

end Strings_Edit.RFC_3897;
