--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Normalization             Luebeck            --
--  Interface                                      Autumn, 2025       --
--                                                                    --
--                                Last revision :  11:03 04 Dec 2025  --
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
--  This package  provides normalization as defined by UnicodeData file.
--
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

package Strings_Edit.UTF8.Normalization is
   pragma Elaborate_Body (Strings_Edit.UTF8.Normalization);
--
-- Normalization_Form -- The normalization form type
--
   type Normalization_Form      is (NFD, NFKD, NFC, NFKC);
   subtype Normalization_C_Form is Normalization_Form range NFC..NFKC;
   subtype Normalization_D_Form is Normalization_Form range NFD..NFKD;
--
-- Canonical_Combining_Class -- The canonical combining class
--
   type Canonical_Combining_Class is range 0..254;
--
-- Combining_Class
--
--    Code - The code point
--
-- Returns :
--
--    The combining class of
--
   function Combining_Class
            (  Code : UTF8_Code_Point
            )  return Canonical_Combining_Class;
--
-- Compose -- Normalization of the type C
--
--    Text    - The array of code points to compose
--    Pointer - After completion points to the first unused code point
--
-- Text  is  expected  to  be result of a normalization NFD or NFKD. The
-- canonical combining order must be respected.  After  completion  Text
-- (Text'First..Pointer - 1) is in NFC or NFKC. Note that composition is
-- same in both cases. The result depends only on the decomposition form
-- used before.
--
   procedure Compose
             (  Text    : in out UTF8_Code_Point_Array;
                Pointer : out Integer
             );
--
-- Compose -- Normalization of the type C
--
--    Destination - To put composed UTF-8 text into
--    Pointer     - After completion points to the first unused octet
--    Source      - The normalized UTF-8 source
--    Form        - The normalization form
--
-- This  procedure  puts composed Source  into Destination  starting  at
-- Pointer. After completion Pointer is advanced to the next free octet.
--
-- Exceptions :
--
--    Constraint_Error - Too many diacritic marks
--    Data_Error       - Invalid UTF-8 encoding of source
--    Layout_Error     - Pointer is not in range or else no room
--
   procedure Compose
             (  Destination : in out String;
                Pointer     : in out Integer;
                Source      : String
             );
--
-- Compare_Decomposed -- Compare normalized strings or code points
--
--    Left, Right - To compare
--  [ Map ]       - To apply before comparison, e.g. Lower_Case_Map
--    Form        - The normalization form
--
-- This function  compares Unicode strings.  The strings  are normalized
-- before comparison.  Map is applied  to the code  points  before final
-- comparison. For example, it can be Lower_Case_Map in order to perform
-- case-insensitive comparison.
--
-- Returns :
--
--    Outcome
--
-- Exceptions :
--
--    Constraint_Error - Too many diacritic marks
--    Data_Error       - Invalid UTF-8 encoding
--
   function Compare_Decomposed
            (  Left, Right : UTF8_Code_Point;
               Map         : Unicode_Mapping;
               Form        : Normalization_D_Form := NFD
            )  return Precedence;
   function Compare_Decomposed
            (  Left, Right : UTF8_Code_Point;
               Form        : Normalization_D_Form := NFD
            )  return Precedence;
   function Compare_Decomposed
            (  Left, Right : String;
               Map         : Unicode_Mapping;
               Form        : Normalization_D_Form := NFD
            )  return Precedence;
   function Compare_Decomposed
            (  Left, Right : String;
               Form        : Normalization_D_Form := NFD
            )  return Precedence;
--
-- Decompose -- Canonical D normalization
--
--    Destination - The target string
--    Pointer     - The position where to place the character
--    Code        - The code point
--    Form        - The normalization form
--
-- This   procedure   places   normalized  representation of  Code  into
-- Destination  starting  at  Destination  (Pointer).  After  completion
-- Pointer is advanced after output. The  content  left  of  Destination
-- (Pointer)  is  assumed normalized.  It  can  be  changed according to
-- canonical combining rules.  Note that upon  an exception  the content
-- left of Pointer is in an undefined state.
--
-- Exceptions :
--
--    Data_Error   - UTF-8 syntax error
--    Layout_Error - Pointer is not in Destination'First..
--                   Destination'Last + 1 or no room for output
--
   procedure Decompose
             (  Destination : in out UTF8_Code_Point_Array;
                Pointer     : in out Integer;
                Code        : UTF8_Code_Point;
                Form        : Normalization_D_Form := NFD
             );
   procedure Decompose
             (  Destination : in out String;
                Pointer     : in out Integer;
                Code        : UTF8_Code_Point;
                Form        : Normalization_D_Form := NFD
             );
--
-- Is_Normalized -- Decomposition check
--
--    Code - The code point
--    Form - The normalization form
--
-- Returns :
--
--    True if Code has the form specified
--
   function Is_Normalized
            (  Code : UTF8_Code_Point;
               Form : Normalization_D_Form := NFD
            )  return Boolean;
--
-- Is_Normalized -- Decomposition check
--
--    Source - UTF-8 encode string
--    Form   - The normalization form
--
-- Returns :
--
--    True if Code has the form specified
--
-- Exceptions :
--
--    Data_Error - Invalid string
--
   function Is_Normalized
            (  Source : String;
               Form   : Normalization_D_Form := NFD
            )  return Boolean;
--
-- Normalized_Type -- Normalization status of parameters
--
   type Normalized_Type is (First, Both, Second, None);
--
-- Is_Normalized_Prefix -- Prefix check (normalized)
--
--    Prefix     - To check
--    Source     - The string
--  [ Pointer ]  - To start at
--  [ Map     ]  - To apply before comparison, e.g. Lower_Case_Map
--    Form       - The normalization form
--    Normalized - If Prefix and Source are already normalized
--
-- These  functions  check  if  normalized Prefix is a prefix of Source.
-- When  Form  is NFC then both are normalized and then composed so that
-- comparison  is performed in NFC. When Form is NFKC then it is done in
-- NFKC.  When  Pointer  is  specified  it  indicates  where  to   start
-- comparison  and  is  advanced after the matched prefix if True is the
-- result.  When Normalized is First  or Botn then Prefix  is considered
-- already normalized. When Normalized is Second  or Both then Source is
-- consindered normalized.
--
-- Returns :
--
--    True if Prefix is a prefix of Source
--
-- Exceptions :
--
--    Constraint_Error - Too many diacritic marks
--    Data_Error       - Illegal UTF-8 strings
--    Layout_Error     - Pointer is out of range
--
   function Is_Normalized_Prefix
            (  Prefix     : String;
               Source     : String;
               Pointer    : access Integer;
               Map        : Unicode_Mapping;
               Form       : Normalization_C_Form := NFC;
               Normalized : Normalized_Type      := None
            )  return Boolean;
   function Is_Normalized_Prefix
            (  Prefix     : String;
               Source     : String;
               Map        : Unicode_Mapping;
               Form       : Normalization_C_Form := NFC;
               Normalized : Normalized_Type      := None
            )  return Boolean;
   function Is_Normalized_Prefix
            (  Prefix     : String;
               Source     : String;
               Pointer    : access Integer;
               Form       : Normalization_C_Form := NFC;
               Normalized : Normalized_Type      := None
            )  return Boolean;
   function Is_Normalized_Prefix
            (  Prefix     : String;
               Source     : String;
               Form       : Normalization_C_Form := NFC;
               Normalized : Normalized_Type      := None
            )  return Boolean;
--
-- Normalize -- Normalization of one code point
--
--    Code - The code point
--    Form - The normalization form
--
-- Returns :
--
--    The corresponding UTF-8 string or code points array
--
   function Normalize
            (  Code : UTF8_Code_Point;
               Form : Normalization_Form := NFD
            )  return UTF8_Code_Point_Array;
   function Normalize
            (  Code : UTF8_Code_Point;
               Form : Normalization_Form := NFD
            )  return String;
--
-- Normalize -- Normalization of an UTF-8 encode string
--
--    Source - The UTF-8 encoded string
--    Form - The normalization form
--
-- Returns :
--
--    The corresponding UTF-8 string
--
-- Exceptions :
--
--    Constraint_Error - Too many diacritic marks
--    Data_Error       - Invalid string
--
   function Normalize
            (  Source : String;
               Form   : Normalization_Form := NFD
            )  return String;
--
-- Normalize -- Canonical normalization
--
--    Destination - The target string
--    Pointer     - The position where to place the character
--    Source      - The source string
--    Form        - The normalization form
--
-- This  procedure  places   normalized  representation  of  Source into
-- Destination  starting  at  Destination  (Pointer).  After  completion
-- Pointer is advanced after output.
--
-- Exceptions :
--
--    Constraint_Error - Too many diacritic marks
--    Data_Error       - UTF-8 syntax error
--    Layout_Error     - Pointer is not in Destination'First..
--                       Destination'Last + 1 or no room for output
--
   procedure Normalize
             (  Destination : in out String;
                Pointer     : in out Integer;
                Source      : String;
                Form        : Normalization_Form := NFD
             );

end Strings_Edit.UTF8.Normalization;
