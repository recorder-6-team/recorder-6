Recorder 6 addin
================

Show unchecked addin v6.0.0.13 of 26/06/2008
============================================

Purpose: To find and display records that are flagged as unchecked, failed/pending verification
         or zero-abundamce.

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
         overwrite the file. The solution is simple: go to the \Recorder6\Addins directory
         (using MyComputer or Windows Explorer), delete the existing CISExport.ocx file,
         and then try installing the addin again.

This addin is automatically installed with Recorder 6

Stuart Ball
26/06/2008

