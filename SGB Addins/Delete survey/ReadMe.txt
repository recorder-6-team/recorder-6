Recorder 6 addin
================

Delete Survey addin v6.0.0.25 of 11/08/2009
===========================================

Purpose: To delete a survey, including the events, samples and observations it contains
         in one go.

Installation: 
============  
     1.  Unpack the files from the supplied .zip file to some convenient directory accessible
         by the computer running Recorder 6 (but NOT to the Recorder\Addins directory!).

     2.  Start Recorder 6 (if it is not already running) and select Tools - Install - 
         Add-in Module from main menus.

     3.  Press the Install button and then use the standard Windows "Open" dialog to find
	 the OCX file in the directory to which you unpacked the files in step 1.

     4.  When you have found the file, press "Open". You should see messages reporting that
         the add-in has been installed and it should appear in the list of add-ins in the
         "Add-ins Configuration" window.

     5.  If you already had an earlier version of this add-in installed you may see messages
         saying the file is already there. Just press "OK" to replace it with the newer 
         version. 


Using the addin:
================

After installing this addin, a new option "Delete survey" will appear in the Tools menu.

When you select this option, you will see a dialog box which contains a drop-down control
near the top from which you can select an existing survey. Once a survey has been selected,
the number of events, samples, species and habitat observations it contains will be displayed
in the main panel.

To delete the selected survey, click the [OK] button. You will be prompted to confirm that
you really want to permanently delete the survey and all the records associeted with it.

The "delete the Survey entry" checkbox (ticked by default) controls whether the Survey
entry itself is removed (if the checkbox is ticked) or just emptied (if it is not-ticked). 

This is useful if, for example, information has been submitted in a spreadsheet and
the contributor then sends you an updated copy. In such circumstances, the update may contain 
new observations, but may also contain those that were submitted previously - some of which 
might have been changed subsequently (e.g. to correct an identification or a grid reference).

In this circumstance it is difficult to work out what is new, what has changed and what 
you already have! The easiest course of action is to delete all the information that was 
submitted previously and replace it entirely by loading the updated version in full. 

Emptying the survey whilst leaving the Survey entry itself in place, into which to import 
the new copy, saves you having to retype this entry.

If you delete (rather than empty) a survey, it will also be delted from any Export filters
which refer to it. This could potentially leave an Export filter empty! This doesn't
cause any problems, but it might look odd to your users.

NOTE: You have to be a "System, manager" to use this addin. Users at lower security
levels will see a message informing them they cannot use the addin and the dialog 
will close immediately.

Stuart Ball
11/08/2009

