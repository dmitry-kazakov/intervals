This directory contains a small example program which converts units. It
uses GTK API. GTK is portable across many  platforms  including  Windows
and  Linux.

1. Ensure  that GTK is installed on your system. See http://www.gtk.org,
   but also check if there are ready to use packages for your platform. 

2. Ada  bindings  to  GTK  are called GtkAda. The home page of GtkAda is
   https://libre.adacore.com/GtkAda/main.html. 

2. Ada  bindings  to  GTK  are called GtkAda. The home page of GtkAda is
   http://libre.act-europe.fr/GtkAda/main.html.  To  build  units_mapper
   using GNAT you have to install GtkAda first.
   
3. Building:

gnatmake -XLibrary_Type=relocatable -Punits_converter
