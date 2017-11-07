{ Unit DragDrop
     Defines the TDropTarget abstract base class  - an implementation of the
     IDropTarget interface for OLE drag and drop.  This class only handles
     'copy' type operations at the moment.
     The DoDrop method is abstract and must be implemented
          The DoDrop method provides an IDataObject interface from which the
          dropped data must be extracted and added to the component.
     The CheckFormats method is abstract and must be implemented
          The CheckFormats method provides an IDataObject interface and a TPoint
          which must be used to determine if the dragged data can be dropped on
          the component at that location.  The TPoint can be ignored if the drop
          is not position specific.
     Copyright 1999 Dorset Software Services Ltd.
     By John van Breda

    Revision:     $Revision: 5 $
                  $Date: 21/01/04 18:18 $
                  $Author: Ericsalmon $
}

unit DragDrop;

interface

uses
  Windows, Classes, Controls, Sysutils, ActiveX, ComObj, ApiUtils, ShellAPI,
  Dialogs;

type

  TDropTarget = class(TInterfacedObject, IDropTarget)
  protected
    {IDropTarget}
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; virtual; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; virtual; stdcall;
    function DragLeave: HResult; virtual; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; virtual; stdcall;
    { Other methods }
    procedure DoDrop(const dataObj: IDataObject); virtual; abstract;
    function CheckFormats( const dataObj : IDataObject; Pt : TPoint): boolean;
                                           virtual; abstract;
  end;

const
  Class_TDropTarget: TGUID = '{E5EC6D72-EE51-11D2-B708-0060085B710F}';
  FORMAT_UNWANTED = -1;

implementation

{ TDropTarget }


{ Handle the entering of a drag destination component }
function TDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  // no data object, no acceptance
  if (dataObj = nil) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  { call abstract method to check if available formats match those we can take }
  if CheckFormats(dataObj, Pt) then
  begin
    // set the result to accept the dropped data
    dwEffect := DROPEFFECT_COPY;
    Result := S_OK;
  end
  else
    Result := E_FAIL;
end;



{ Handle dragging out of the destination component.  Nothing to do here }
function TDropTarget.DragLeave: HResult;
begin
  Result := S_OK;
end;



{ Handle dragging around over the destination component.  Nothing to do here -
     need to implement if drag depends on which item you are over, therefore
     this method is virtual. }
function TDropTarget.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
begin
  Result := S_OK;
end;


{ Call the drop handling code }
function TDropTarget.Drop(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  DoDrop(dataObj);
  Result := S_OK;
end;






//==============================================================================

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;





end.

