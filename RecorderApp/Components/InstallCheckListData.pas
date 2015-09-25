unit InstallCheckListData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, ADODB;

type
  TdmInstallCheckList = class(TDataModule)
    qryTLISource: TJNCCQuery;
    qryTVSource: TJNCCQuery;
    qryTSource: TJNCCQuery;
    qryTFSource: TJNCCQuery;
    qryTDSource: TJNCCQuery;
    qrySSource: TJNCCQuery;
    qryRSSource: TJNCCQuery;
    qryFSSource: TJNCCQuery;
    qryTLIDest: TJNCCQuery;
    qryTVDest: TJNCCQuery;
    qryTDest: TJNCCQuery;
    qryTFDest: TJNCCQuery;
    qryTDDest: TJNCCQuery;
    qrySDest: TJNCCQuery;
    qryRSDest: TJNCCQuery;
    qryFSDest: TJNCCQuery;
    qryTLSource: TJNCCQuery;
    qryTLVSource: TJNCCQuery;
    qryTLDest: TJNCCQuery;
    qryTLVDest: TJNCCQuery;
    qryTDTSource: TJNCCQuery;
    qryTDTDest: TJNCCQuery;
    qryTRDest: TJNCCQuery;
    qryTLTSource: TJNCCQuery;
    qryTRSource: TJNCCQuery;
    qryTLTDest: TJNCCQuery;
    qryTNTSource: TJNCCQuery;
    qryTNTDest: TJNCCQuery;
    qryTSSource: TJNCCQuery;
    qryTSDest: TJNCCQuery;
  private
  public
  end;

//==============================================================================
implementation

{$R *.DFM}

//==============================================================================
end.
