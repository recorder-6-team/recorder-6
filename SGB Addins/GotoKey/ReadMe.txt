Recorder 6 addin
================

Go to Key addin v6.0.1.8 of 27/06/2007
======================================

Purpose: To display a given record in Recorder 6 by typing its 16-character
         identifier.

Installation: 
============  
     1.  If you are upgrading from an earlier version of Recorder and already have
         a copy of this addin installed, you should uninstall it first before installing
         the new version. Start Recorder 6 (if it is not already running) and select 
	 Tools - Install - Add-in Module from main menus. Select the addin name in the list
         and click the [Remove] button. You should then exit from Recorder 6 and restart
         it before attempting to install the same addin again.

     2.  Start Recorder 6 (if it is not already running) and select Tools - Install - 
         Add-in Module from main menus.

     3.  Press the Install button and then use the standard Windows "Open" dialog to find
	 the OCX file in the Extra addins folder on this CD.

     4.  When you have found the file, press "Open". You should see messages reporting that
         the add-in has been installed and it should appear in the list of add-ins in the
         "Add-ins Configuration" window.

     5.  If you re-installing the add-in you may see messages saying the add-in has been 
         previously installed. Just press "OK" to replace it with the newer version. If you
	 get an error message at this point:

         The addin could not be installed for the following reason: Error 5: access is denied.

         This probably means that the addin was previously installed from a CD and so the
         ocx file is read-only. In this case, the operating system won't let Recorder 6
         overwrite the file. The solution is simple: go to the \Recorder200x\Addins directory
         (using MyComputer or Windows Explorer), delete the existing CISExport.ocx file,
         and then try installing the addin again.

Using the add-on:
================

After installation, a new option "GoTo Key" appears in the Tools menu.

When you select this option, a dialog will open with an edit box into which a 16-character
key can be typed or pasted and a drop-down box indicatiing which window it applies to.

Options are:
  Survey
  Survey event
  Sample
  Taxon occurrence
  Biotope occurrence
  Location
  Organisation
  Individual
  Document
  Taxon
  Biotope
  Admin area

Click the [OK] button to open the specified window and load the row with the specified key.
If the key you specified cannot be found in the table you selected, you will see an error
message.

Stuart Ball
27/06/2007

