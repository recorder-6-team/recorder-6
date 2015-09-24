Recorder 6 addin
================

Bulk load map tiles addin v6.0.0.8 of 25/06/2007
================================================

Purpose: To import a set of map tiles in .GSF format into Recorder's mapping
         system in one go.

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

After installation, a new option "Bulk load maps" appears in the Maps menu.

When you select this option, a dialog appears. This has:

- a drop down box from which you can select the base map to which you want the map tiles added.
- an edit box where you enter the name of the directory from which the .GSF files will be
  imported. This has a browse button which will open a Windows directory selection window
  allowing you to choose a directory.
- a list box. Once you have selected a directory, a list of the .GSF files it contains will be 
  displayed. Select the onses you want to import. You can select multiple files by Shift-
  or Ctrl-clicking on names in the list.
- edit boxes where you specify the cut-in and cut-out scales to be applied to ALL the selected
  map tiles.

Once you have selected the map tiles to import, click the [OK] button and the selected tiles
will be copied to your \Recorder 6\Map files directory (if they are not already present) and
registered in Recorder 6's map system.

The next time you open the speciefied base map, the tiles should be available.

Stuart Ball
27/06/2007

