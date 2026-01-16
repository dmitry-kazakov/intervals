--                                                                    --
--  procedure Unit_Converter        Copyright (c)  Dmitry A. Kazakov  --
--  Sample program for Win32                       Luebeck            --
--  Implementation                                 Autumn, 2003       --
--                                                                    --
--                                Last revision :  22:59 29 May 2021  --
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

with Ada.IO_Exceptions;           use Ada.IO_Exceptions;

with Float_Measures;              use Float_Measures;
with Float_Measures_UTF8_Edit;    use Float_Measures_UTF8_Edit;
with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;
with Units;                       use Units;

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;

procedure Units_Converter is
--
-- Things related to Win32-API
--
   BN_CLICKED    : constant := 0;

   WM_CLOSE      : constant := 16#010#;
   WM_INITDIALOG : constant := 16#110#;
   WM_COMMAND    : constant := 16#111#;

   type BOOL   is new Interfaces.C.int;
   type DWORD  is new Interfaces.C.unsigned_long;
   type UINT   is new Interfaces.C.unsigned;
   type INT    is new Interfaces.C.int;
   type WORD   is new Interfaces.C.unsigned_short;
   type WPARAM is mod 2 ** Standard'Address_Size;
   type LPARAM is range -(2 ** (Standard'Address_Size - 1))
                     .. +(2 ** (Standard'Address_Size - 1) - 1);

   type LPSTR  is access all Interfaces.C.char;
   type LPWSTR is access all Interfaces.C.wchar_t;

   type HWND      is new System.Address;
   type HINSTANCE is new System.Address;

   type DLGPROC is access function
        (  Window  : HWND;
           Message : UINT;
           W_Param : WPARAM;
           L_Param : LPARAM
        )  return BOOL;
   pragma Convention (Stdcall, DLGPROC);

   function DialogBoxParamW
            (  Instance    : HINSTANCE;
               ID          : LPWSTR;
               Parent      : HWND;
               Dialog_Func : DLGPROC;
               Init_Param  : LPARAM
            )  return INT;
   pragma Import (Stdcall, DialogBoxParamW, "DialogBoxParamW");

   function EndDialog (Dialog : HWND; Result : INT) return BOOL;
   pragma Import (Stdcall, EndDialog, "EndDialog");

   function FreeLibrary
            (  Module : HINSTANCE
            )  return BOOL;
   pragma Import (Stdcall, FreeLibrary, "FreeLibrary");

   function HIWORD (Value : DWORD) return WORD is
   begin
      return WORD (Value / (2 ** 16));
   end HIWORD;

   function LOWORD (Value : DWORD) return WORD is
   begin
      return WORD (Value mod (2 ** 16));
   end LOWORD;

   function GetDlgItemTextW
            (  Dialog    : HWND;
               ID        : INT;
               Text      : LPWSTR;
               Max_Count : INT
            )  return UINT;
   pragma Import (Stdcall, GetDlgItemTextW, "GetDlgItemTextW");

   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");

   procedure InitCommonControls;
   pragma Import (Stdcall, InitCommonControls, "InitCommonControls");

   function LoadLibrary (File_Name : LPSTR) return HINSTANCE;
   pragma Import (Stdcall, LoadLibrary, "LoadLibraryA");

   function MAKEINTRESOURCEW (Value : WORD) return LPWSTR is
      function To_LPWSTR is
         new Ada.Unchecked_Conversion (WPARAM, LPWSTR);
   begin
      return To_LPWSTR (WPARAM (Value));
   end MAKEINTRESOURCEW;

   function Get_Instance return HINSTANCE;
   pragma Import (C, Get_Instance, "rts_get_hInstance");

   function SetDlgItemTextW
            (  Dialog : HWND;
               ID     : INT;
               Text   : LPWSTR
            )  return BOOL;
   pragma Import (Stdcall, SetDlgItemTextW, "SetDlgItemTextW");
--
-- The  following  are the constants defined in the resource script. See
-- Units_Converter.rc file.
--
   Dialog_ID  : constant :=  101;
   Input_ID   : constant := 1000;
   SI_ID      : constant := 1001;
   Base_ID    : constant := 1002;
   Message_ID : constant := 1003;
   Go_ID      : constant := 1004;
--
-- Useless windows return codes
--
   INT_Result  : INT;
   BOOL_Result : BOOL;
   UINT_Result : UINT;
--
-- Dialog_Proc -- Process messages to the dialog box
--
--    Window  - The window (a handle to)
--    Message - To process
--    WPar    - Its short parameter
--    LPar    - Its long parameter
--
-- Returns :
--
--    Message processing code (message specific)
--
   function Dialog_Proc
            (  Window  : HWND;
               Message : UINT;
               WPar    : WPARAM;
               LPar    : LPARAM
            )  return BOOL;
   pragma Convention (Stdcall, Dialog_Proc);
--
-- Dialog_Proc_Access -- Pointer to Dialog_Proc
--
   type Dialog_Proc_Access is access function
        (  Window  : HWND;
           Message : UINT;
           WPar    : WPARAM;
           LPar    : LPARAM
        )  return BOOL;
   pragma Convention (Stdcall, Dialog_Proc_Access);
--
-- To_DLGPROC -- Conversion from Dialog_Proc_Access to DLGPROC
--
-- This is necessary to  work  around  access  type  protection  system.
-- DLGPROC  is  declared  so  that  no  nested  function  pointer may be
-- converted to it. So we have declared another pointer type and convert
-- it to DLGPROC.
--
   function To_DLGPROC is
      new Ada.Unchecked_Conversion (Dialog_Proc_Access, DLGPROC);
--
-- Get_Text -- Wrapper around windows API function GetDlgItemText
--
--    Window - A handle to
--    ID     - Of the control
--
-- Returns :
--
--    The text of the control
--
   function Get_Text (Window : HWND; ID : UINT) return String is
      Input : Interfaces.C.wchar_array (1..256);
   begin
      UINT_Result :=
         GetDlgItemTextW
         (  Window,
            Input_ID,
            Input (Input'First)'Unchecked_Access,
            Input'Length
         );
      return To_UTF8 (Interfaces.C.To_Ada (Input));
   end Get_Text;
--
-- Set_Text -- Wrapper around windows API function SetDlgItemText
--
--    Window - A handle to
--    ID     - Of the control
--    Text   - To be put there
--
   procedure Set_Text
             (  Window : HWND;
                ID     : UINT;
                Text   : String
             )  is
      Output : Interfaces.C.wchar_array :=
               Interfaces.C.To_C (To_Wide_String (Text));
   begin
      BOOL_Result :=
         SetDlgItemTextW
         (  Window,
            INT (ID),
            Output (Output'First)'Unchecked_Access
         );
   end Set_Text;
--
-- Go -- When the Go button gets pressed
--
--    Window - A handle to
--
   procedure Go (Window : HWND) is
      Number : Measure;
   begin
      Set_Text (Window, SI_ID,      "");
      Set_Text (Window, Base_ID,    "");
      Set_Text (Window, Message_ID, "");
      Number := Value (Get_Text (Window, Input_ID));
      Set_Text (Window, SI_ID, Image (Number));
      Set_Text (Window, Base_ID, Image (Number, Derived => False));
   exception
      when Constraint_Error =>
         Set_Text (Window, Message_ID, "Numeric error");
      when Data_Error =>
         Set_Text (Window, Message_ID, "Syntax error");
      when End_Error =>
         Set_Text (Window, Message_ID, "Nothing recognized");
      when Unit_Error =>
         Set_Text (Window, Message_ID, "Unit error");
   end Go;
--
-- Dialog_Proc -- Implementation
--
   function Dialog_Proc
            (  Window  : HWND;
               Message : UINT;
               WPar    : WPARAM;
               LPar    : LPARAM
            )  return BOOL is
   begin
      case Message is
         when WM_CLOSE =>
            BOOL_Result := EndDialog (Window, 0);
         when WM_COMMAND =>
            case HIWORD (DWORD (WPar)) is
               when BN_CLICKED =>
                  case LOWORD (DWORD (WPar)) is
                     when Go_ID =>
                        Go (Window);
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when WM_INITDIALOG =>
            Set_Text
            (  Window,
               Message_ID,
               "Source: www.dmitry-kazakov.de/ada/units.htm"
            );
         when others =>
            null;
      end case;
      return 0;
   end Dialog_Proc;
   Ptr    : Dialog_Proc_Access := Dialog_Proc'Access;
   Name   : Interfaces.C.char_array :=
            Interfaces.C.To_C ("RichEd20.dll");
   Handle : HINSTANCE;
begin
   InitCommonControls; -- Needed for Unicode support
   Handle := LoadLibrary (Name (Name'First)'Unchecked_Access);
   Int_Result :=
      DialogBoxParamW
      (  Get_Instance,
         MAKEINTRESOURCEW (Dialog_ID),
         HWND (System.Null_Address),
         To_DLGPROC (Ptr),
         0
      );
   if 0 = FreeLibrary (Handle) then null; end if;
end Units_Converter;
