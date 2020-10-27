unit GenerateRucksack;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,Registry,StrUtils, ImageListButton,DataClasses;

resourcestring

  Restr_InputFileInvalid = 'Invalid input file';
  Restr_OutputFileInvalid = 'Invalid output file';
  Restr_FilesInvalid = 'Input and output files must be specified and must not be the same';
  Restr_CreateTempTableFailed = 'Failing to create temporary tables - request support';
  Restr_Unmatched_Exist = 'Some taxa remain unmatched - find a match ?';
  Restr_Search_Column_Not_Found = 'Search column not found';
  Restr_SourceNameInvalid = 'Search column not found in file';

const
  CRLF = chr(13)+chr(10);
  SQL_CREATE_TEMPTABLE =
      'Create Table ##Temptaxa(TaxonName varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS,' +
      'TLIKey varchar(16) COLLATE SQL_Latin1_General_CP1_CI_AS,' +
      'Sort_Order varchar (60)  COLLATE SQL_Latin1_General_CP1_CI_AS, ' +
      'Original_order integer, Duplicate_Flag integer, ' +
      'SearchName varchar(30) COLLATE SQL_Latin1_General_CP1_CI_AS, ' +
      'OriginalKey varchar(16) COLLATE SQL_Latin1_General_CP1_CI_AS, ' +
      'Sort_To_Use varchar (150) COLLATE SQL_Latin1_General_CP1_CI_AS,' +
      'MatchCount integer)';
  SQL_CREATE_TEMPRUCKSACK =
      'Create Table ##TempRucksack(Rucksack_data varchar(60) COLLATE SQL_Latin1_General_CP1_CI_AS)';
  SQL_INSERT_CSV_DATA =
      'INSERT INTO ##TempTaxa (Taxonname,SearchName,Original_Order) VALUES(''%s'',''%s'',''%s'')';
  SQL_INSERT_RUCKSACK =
      'Insert Into ##TempTaxa (OriginalKey,Original_order) Values (' +
      '''%s'',%s)';
  SQL_INSERT_RUCKSACK_REST =
      'Insert Into ##TempRucksack (Rucksack_data) Values (' +
      '''%s'')';
  SQL_RUCKSACK_NAMES =
      ' Update ##Temptaxa Set MATCHCOUNT = 0, TaxonName = (select Output_Taxon_Name FROM ' +
      ' Index_Taxon_Name WHERE Index_Taxon_Name.Taxon_List_Item_Key = ' +
      ' ##Temptaxa.OriginalKey)';
  SQL_MATCHES_COUNT =
      'UPDATE ##TEMPTAXA SET MATCHCOUNT = D.CTN ' +
      ' FROM (Select ##TEMPTAXA.TAXONNAME AS TN, count(Taxon_List_Item_Key) AS CTN FROM ' +
      ' INDEX_TAXON_NAME INNER JOIN ' +
      ' ##TEMPTAXA ON ##TEMPTAXA.TaxonName ' +
      ' = Index_Taxon_Name.OUTPUT_TAXON_NAME ' +
      ' AND INDEX_TAXON_NAME.PREFERRED_TAXA = 1 ' +
      ' GROUP BY ##TEMPTAXA.TAXONNAME, OUTPUT_TAXON_NAME) AS D ' +
      ' WHERE D.TN = ##TEMPTAXA.TaxonName ';
  SQL_SINGLE_MATCH =
      'Update ##TempTaxa set TLIKey = (select Taxon_List_Item_Key FROM ' +
      'Index_taxon_Name where Preferred_Taxa = 1 and ##Temptaxa.TaxonName = ' +
      'Index_Taxon_Name.Output_Taxon_Name) WHERE ##TempTaxa.MatchCount = 1';
  SQL_SINGLE_MATCH_RECOMMENDED =
      ' Update ##TempTaxa set TLIKey = (select Recommended_Taxon_List_Item_Key FROM ' +
      ' Index_taxon_Name WHERE ##TempTaxa.TLIKey = Index_taxon_Name.Taxon_List_Item_Key)';
  SQL_RETAIN_KEY =
      'Update ##TempTaxa set TLIKey = OriginalKey';
  SQL_UPDATE_TLIKEY =
      'Update ##TempTaxa set TLIKey = ''%s'' WHERE TaxonName = ''%s''';
  SQL_SEARCH_NAME =
      'Update ##TempTaxa set SEARCHNAME = '''' WHERE SEARCHNAME IS NULL';
  SQL_TLIKEY_BLANK =
      'Update ##TempTaxa set TLIKEY = '''' WHERE TLIKEY IS NULL';
  SQL_SORT_TO_USE = 'Update ##TempTaxa Set Sort_To_Use = %s';
  SQL_SET_DUPLICATE = 'Update ##TEmptaxa set Original_Order = 0 Where Original_Order = ''%s''';
  SQL_REMOVE_DUPLICATE = 'DELETE FROM ##TempTaxa WHERE Original_Order = 0';

  SORT_ORIGINAL_ORDER = 'LTRIM(STR(Original_Order))';
  SORT_TAXON_NAME =
       '(Select OUTPUT_TAXON_NAME FROM INDEX_TAXON_NAME ITN WHERE ' +
       ' ITN.Taxon_List_Item_Key = ##TempTaxa.TLIkey )';
  SORT_GROUP_TAXON =
       '(SELECT str(TG.SORT_ORDER) + OUTPUT_TAXON_NAME FROM INDEX_TAXON_NAME ITN ' +
       ' INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ' +
       ' ITN.TAXON_LIST_ITEM_KEY INNER JOIN TAXON_VERSION TV ON ' +
       ' TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY ' +
       ' INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY ' +
       ' WHERE ITN.TAXON_LIST_ITEM_KEY = ##TEMPTAXA.TLIKEY)';
  SORT_DICT_SORT =
       '(Select Sort_Order FROM INDEX_TAXON_NAME ITN WHERE ' +
       ' ITN.Taxon_List_Item_Key = ##TempTaxa.TLIkey )';

type
TdlgGenerateRucksack = class(TForm)
    lblTaxonNameColumn: TLabel;
    lblExplainSort: TLabel;
    lblSerachColumn: TLabel;
    edInputColumnName: TEdit;
    rgSort: TRadioGroup;
    edSearchColumnName: TEdit;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    lblinputfile: TLabel;
    lblOutputFile: TLabel;
    edInputFile: TEdit;
    edOutputFile: TEdit;
    bbBrowser: TButton;
    Button1: TButton;
    btnAction: TButton;
    bbCancel: TImageListButton;
    cbTransfer: TCheckBox;
    cbRecommended: TCheckBox;
    procedure bbBrowserClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure edSearchColumnNameChange(Sender: TObject);
    procedure edInputColumnNameChange(Sender: TObject);
    procedure cbRecommendedClick(Sender: TObject);
    procedure cbTransferClick(Sender: TObject);
  private
    { Private declarations }
    function Get_RucksackPath: string;
    function Get_FirstlineInfo(TextLine: string) : boolean;
    function Get_SearchInfo(TextLine: string) : boolean;
    function Get_ColumnValues(InputValue: string):integer;
    function Get_NoOFDoubleQuotes(FieldValue:string ): integer ;
    function CreatInputTable(): boolean;
    function Get_TaxonNameFromLine(AFileLine: integer; AFullLine: string) :string;
    function Get_SearchNameFromLine(AFileLine: integer; AFullLine: string) :string;
    function FindTaxon(const iFindTaxon: string): TKeyString;
    procedure WriteRucksack();
    function Get_RuckSackOutputLine( TaxonKey: string; SearchKey :string) :string;
    function Get_AdditionalTags(): widestring;
    function Update_Sort_Order(): boolean;
    function Remove_Duplicates: boolean;
  public
    { Public declarations }
  end;

var
  dlgGenerateRucksack: TdlgGenerateRucksack;
  FRuckSackPath: string;
  FDelimiter: string;
  FStatusFlag: integer;
  FIsFromRucksack: boolean;
  FHasHeading: boolean;
  FTaxonNameColumn: integer;
  FSearchNamedColumn: string;
  FStartFlag: boolean;
  FHasSearch: boolean;
  FSearchNameColumn: integer;
  FColumnContents: array[0..100] of string;
implementation
  uses DatabaseAccessADO, ADODB, GeneralFunctions,GeneralData,Find;
{$R *.dfm}

procedure TdlgGenerateRucksack.bbBrowserClick(Sender: TObject);
var
  lLengthRucksackPath : integer;
  lTextFileVar : textfile;
  lFirstLine : string;
begin
  FRucksackPath :=  Get_RucksackPath;
  lLengthRucksackPath := length(FRucksackPath);
  with OpenDialog1 do begin
    Title := 'Input Taxon Names file';
    Filter := 'Text file(*.txt)|*.txt|CSV file(*.csv)|*.csv|Rucksack(*.ruk)|*.ruk|Card(*.crd)|*.crd';
    InitialDir :=  AnsiLeftStr(FRucksackPath,(lLengthRucksackPath-1));
    Filename := '*.csv';
  end;
  OpenDialog1.Execute;
  edInputFile.text := OpenDialog1.FileName;
  if  Uppercase(AnsiRightStr(edInputFile.text,4)) = '.CSV' then begin
    FDelimiter := ',';
    cbTransfer.enabled := false;
  end else begin
    FDelimiter := chr(9);
    cbTransfer.enabled := true;
  end;
  if edInputFile.text <> '' then begin
    Try
      AssignFile(lTextFilevar, edInputFile.text);
      Reset (lTextFilevar);
      If (Uppercase(AnsiRightStr(edInputFile.text,3)) = 'RUK')
          or (Uppercase(AnsiRightStr(edInputFile.text,3)) = 'CRD') then begin
        FIsFromRuckSack := true;
        FHasHeading := false;
        FTaxonNameColumn := 0;
        FSearchNameColumn := 0;
        FStartFlag := false;
      end else begin
        FIsFromRuckSack := false;
        If edInputColumnName.Text <>'' then
          FHasHeading := true
        else begin
          FHasHeading := false;
          FHasSearch := false;
        end;
        if FHasHeading = true then begin
          if edSearchColumnName.Text <> '' then FHasSearch := true;
          ReadLN(lTextFileVar, lFirstline);
          if Get_FirstLineInfo(lFirstLine)then begin
            If not Get_SearchInfo(lFirstLine) then begin
              edInputFile.text := '';
              MessageDlg(Restr_SourceNameInvalid, mtInformation, [mbOk], 0);
            end;
          end else begin
            FTaxonNameColumn := 0;
            FSearchNameColumn := 0;
            edInputFile.text := '';
          end;
        end;
      end;
    except
      edInputFile.text := '';
    end;
  end;
  if edInputFile.text = '' then MessageDlg(Restr_InputFileInvalid, mtWarning, [mbOk], 0);
end;

function TdlgGenerateRucksack.Get_RucksackPath: string;
var
R6Registry : TRegistry;
begin
  R6Registry := TRegistry.Create;
  try
  //r6registry.rootkey := 1
    if R6Registry.OpenKey('Software\Dorset Software\Recorder 6\Settings', FALSE) then
    begin
      Result :=  R6Registry.Readstring('Rucksack Path') ;
    end;
  finally
    R6Registry.free;
  end;
end;

function TdlgGenerateRucksack.Get_FirstlineInfo(TextLine :string) : boolean;
var
J : integer;
begin
  Result := true;
  FTaxonNameColumn := -1;
  for J :=  0 to  Get_ColumnValues(TextLine)  do
  begin
    if Uppercase(FColumnContents[J]) = Uppercase(edInputColumnName.text) then
      FTaxonNameColumn := J;
  end;
  if  FTaxonNameColumn = -1 then
    Result := false;
end;

function TdlgGenerateRucksack.Get_SearchInfo(TextLine :string) : boolean;
var
J : integer;
begin
  Result := true;
  if FHasSearch = true then begin
    FSearchNameColumn := -1;
    for J :=  0 to  Get_ColumnValues(TextLine) do begin
      if Uppercase(FColumnContents[J]) = Uppercase(edSearchColumnName.text) then
        FSearchNameColumn := J;
    end;
    if  FSearchNameColumn = -1 then
      Result:= false;
  end;
end;

function TdlgGenerateRucksack.Get_ColumnValues(
  InputValue: string):integer;
var
  TempInputValue : string;
  CommaPosition : integer;
  TempFieldValue : string;
  CurrentFieldNumber : integer;
begin
  TempInputValue := InputValue + FDelimiter;
  CommaPosition := Ansipos (FDelimiter,TempInputValue);
  CurrentFieldNumber := 0;
  While CommaPosition <> 0 do
  Begin
    TempFieldValue := ansiLeftstr(TempInputValue,CommaPosition-1);
    While Get_NoOFDoubleQuotes(TempFieldValue) = 1 do
    begin
      TempInputValue := ansiRightStr(TempInputValue,length(TempInputValue)-CommaPosition);
      CommaPosition := Ansipos (FDelimiter,TempInputValue);
      TempFieldValue := TempFieldvalue + FDelimiter + ansiLeftstr(TempInputValue,CommaPosition-1);
    end;
    FColumnContents[CurrentFieldNumber] := StringReplace(TempFieldValue,'"','',[rfReplaceAll]);
    CurrentFieldNumber :=  CurrentFieldNumber + 1;
    TempInputValue := ansiRightStr(TempInputValue,length(TempInputValue)-CommaPosition);
    CommaPosition := Ansipos (FDelimiter,TempInputValue);
  end;
  Result := CurrentFieldNumber - 1;
end;

function TdlgGenerateRucksack.Get_NoOFDoubleQuotes(FieldValue:string ): integer ;
begin
  Result := 0;
  if (ansiLeftstr(FieldValue,1) = '"') and (ansiRightStr(FieldValue,1) = '"') then Result:= 2;
  if (ansiLeftstr(FieldValue,1) = '"') and (ansiRightStr(FieldValue,1) <> '"') then Result:= 1;
end;

procedure TdlgGenerateRucksack.Button1Click(Sender: TObject);
var
 lLengthRucksackPath : integer;
 lCheckFileVar : textfile;

begin
  FRucksackPath :=  Get_RucksackPath;
  lLengthRucksackPath := length(FRucksackPath);
  edoutputfile.Text:= '';
  with SaveDialog1 do
  begin
    Title := 'Rucksack Name';
    Filter := 'Rucksack file (*.ruk)|*.ruk';
    InitialDir :=  AnsiLeftStr(FRucksackPath,(lLengthRucksackPath-1));
    Filename := '*.ruk';
  end;
  Try
    SaveDialog1.Execute;
    edoutputfile.text := SaveDialog1.FileName;
    if leftstr(edoutputfile.text,1) = '*' then
      edoutputfile.text := '';
    AssignFile(lCheckFilevar,edoutputfile.text);
    Rewrite(lCheckFileVar);
    Closefile (lCheckFileVar);
  Except
    begin
      MessageDlg(Restr_OutputFileInvalid, mtWarning, [mbOk], 0);
      edinputfile.text := '';
    end;
  end;

end;

procedure TdlgGenerateRucksack.bbCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TdlgGenerateRucksack.btnActionClick(Sender: TObject);
var
  lFileVar: textfile;
  lTaxonName: string;
  lSearchName: string;
  lFileLine: integer;
  lFullLine: string;
  lIsTLIKey: boolean;
  lOrigTLIKey: string;
  lOrigSortOrder: integer;
  lTLIKey: string;
  rs2: _Recordset;
  rs: _Recordset;
  lRetry: boolean;
  lOrigCursor: TCursor;
begin
  lIsTLIKey:=  false;
  lOrigCursor:= Screen.Cursor;
  Screen.Cursor := crHourglass;
  lRetry:= true;
  lOrigSortOrder:= 0;
  If (edInputFile.text = '') or (edOutputFile.text = '') or (UPPERCASE(edInputFile.text) =
        UPPERCASE(edOutputFile.text)) then
    MessageDlg(Restr_FilesInvalid, mtWarning, [mbOk], 0)
  else begin
    if CreatInputTable()then begin
      AssignFile(lFileVar, edInputFile.text);
      Reset (lFileVar);
      if FIsFromRucksack then begin
        While not eof(lFileVar) do begin
          ReadLN(lFileVar, lFullLine );
          if lFullLine = '<TAXON>' then lIsTLiKey := true
          else
          if lFullLine = '</TAXON>' then lIsTLiKey := false;
          If (lIsTLiKey) AND  (lFullLine <> '<TAXON>') then begin
            lOrigTLIKey:= AnsiLeftStr(lFullLine,16);
            lOrigSortOrder :=  lOrigSortOrder + 1;
            dmDataBase.ExecuteSQl(Format(SQL_INSERT_RUCKSACK,[lOrigTLiKey,inttostr(lOrigSortOrder)]),true);
          end else begin
            // if it is rucksack save anything not Taxa related
            If (Uppercase(AnsiRightStr(edInputFile.text,3)) = 'RUK')then begin
              lOrigTLIKey:= AnsiLeftStr(lFullLine,50);
              dmDataBase.ExecuteSQl(Format(SQL_INSERT_RUCKSACK_REST,[lOrigTLIKey]),true);
            end;
          end;
        end;
        Closefile(lFilevar);
        dmDataBase.ExecuteSQl(SQL_RUCKSACK_NAMES,true);
      end else begin
        // This is the main routine which reads the input file and poulates the main temp table
        Reset (lFileVar);
        lFileLine := 0;
        While not eof(lFileVar) do begin
          lOrigSortOrder :=  lOrigSortOrder + 1;
          lFileline := lFileline + 1;
          ReadLN(lFileVar, lFullLine );
          lTaxonName := Get_TaxonNameFromLine(lFileLine,lFullLine);
          lSearchName := Get_SearchNameFromLine(lFileLine,lFullLine);
          if lTaxonName <> '' then
            dmDatabase.ExecuteSQL(Format(SQL_INSERT_CSV_DATA,[lTaxonName,lSearchName,inttostr(lOrigSortOrder)]),true);
        end;
        Closefile(lFileVar);
      end;
    end;
    dmDatabase.ExecuteSQL(SQL_MATCHES_COUNT);
    if (cbTransfer.Checked) AND (FIsFromRucksack) then
      dmDatabase.ExecuteSQL(SQL_RETAIN_KEY)
    else begin
      dmDatabase.ExecuteSQL(SQL_SEARCH_NAME);
      dmDatabase.ExecuteSQL(SQL_SINGLE_MATCH);
      While lretry do begin
        lRetry := true;
        rs := dmDatabase.ExecuteSQL('SELECT * FROM ##TempTaxa WHERE TLIKey IS NULL',true);
        if not rs.EOF then begin
          if MessageDlg(Restr_Unmatched_Exist, mtInformation, [mbYes,MbNo], 0)=mrNo then
            lRetry:= false
        end else
          lRetry := false;
        if lRetry = true then begin
          rs2 := dmDatabase.ExecuteSQL('SELECT * FROM ##TempTaxa WHERE MatchCount <> 1',true);
          while not rs2.Eof do begin
            lTaxonName := rs2.Fields['TaxonName'].Value;
            lTLIKey := FindTaxon(lTaxonName);
            if lTLIKey <> '' then dmDatabase.ExecuteSQL(format(SQL_UPDATE_TLIKEY,[lTLIKey,rs2.Fields['TaxonName'].Value]));
            rs2.MoveNext;
          end;
        end;
      end;
    end;
    if cbRecommended.checked then
      dmDatabase.ExecuteSQL(SQL_SINGLE_MATCH_RECOMMENDED);
    WriteRucksack;
    dmDatabase.ExecuteSQL('Drop Table ##TempTaxa');
    dmDatabase.ExecuteSQL('Drop Table ##TempRucksack');
    MessageDlg('Process complete', mtInformation, [mbok], 0);

  end;
  Screen.Cursor := lOrigCursor;
  ModalResult:= mrOK;
end;

Function TdlgGenerateRucksack.CreatInputTable() : boolean;
begin
  Result := true;
  Try
    dmDatabase.ExecuteSQL(SQL_CREATE_TEMPTABLE,true);
    dmDatabase.ExecuteSQL(SQL_CREATE_TEMPRUCKSACK,true);
  Except
    begin
      Result:= false;
      MessageDlg(Restr_CreateTemPTableFailed, mtWarning, [mbOk], 0)
    end;
  end;
end;

function TdlgGenerateRucksack.Get_TaxonNameFromLine(AFileLine: integer; AFullLine:string) :string;
var
  lTempTaxonName : string;
begin
  if (AFileLine = 1) and (FHasHeading = true )   then
    Result:= ''
  else
  begin
    Get_ColumnValues(AFullLine);
    lTempTaxonName := FColumnContents[FTaxonNameColumn];
    Result :=   lTempTaxonName;
  end;
end;

function TdlgGenerateRucksack.Get_SearchNameFromLine(AFileLine: integer; AFullLine: string) :string;
begin
  if (AFileLine = 1) and (FHasSearch = true ) then
    Result:= ''
  else
  begin
    if FHasSearch = true then
    begin
      Get_ColumnValues(AFullLine);
      Result := FColumnContents[FSearchNameColumn];
    end
    else
      Result := '';
  end;
end;

function TdlgGenerateRucksack.FindTaxon(const iFindTaxon: string): TKeyString;
var ldlgFind: TdlgFind;
begin
  ldlgFind := TdlgFind.CreateDialog(nil, True, 'Find Species', ftTaxon);
  with ldlgFind do begin
    try
      if iFindTaxon <> '' then StoreSearchText(iFindTaxon)
                          else StoreSearchText('');
      // If there is a unique item, get it, otherwise, we have to show the dialog
      if FindUnique(iFindTaxon) then
        Result := ItemKey
      else
      if ShowModal = mrOK then Result := ItemKey
                          else Result := '';
    finally
      Release;
    end;
  end; // with lFind
end; // FindTaxon

procedure TdlgGenerateRucksack.edSearchColumnNameChange(Sender: TObject);
begin
  edInputFile.Text := '';
end;

procedure TdlgGenerateRucksack.edInputColumnNameChange(Sender: TObject);
begin
  edInputFile.text := '';
end;

procedure TdlgGenerateRucksack.WriteRucksack();
var
  rs : _Recordset;
  RuckVar : textfile;
begin
  AssignFile(RuckVar, edOutputFile.text);
  Rewrite(RuckVar);
  Update_Sort_Order;
  Remove_Duplicates;
  dmDatabase.ExecuteSQL(SQL_TLIKEY_BLANK,true);
  rs := dmDatabase.ExecuteSQL('SELECT * FROM ##TempTaxa Order By Sort_To_Use',true);
  {Start writing Rucksack here }
  write (RuckVar,'<TAXON>',CRLF);
  while not rs.Eof do begin
    If rs.fields[1].value <> '' then
      write (RuckVar,rs.fields[1].value,CRLF);
    rs.MoveNext;
  end;
  if FIsFromRuckSack then begin
    write (RuckVar,'</TAXON>',CRLF);
    write (RuckVar,Get_AdditionalTags());
  end else begin
    write (RuckVar,'</TAXON>',CRLF);
    write (RuckVar,'<BIOTOPE>',CRLF);
    write (RuckVar,'</BIOTOPE>',CRLF);
    write (RuckVar,'<LOCATION>',CRLF);
    write (RuckVar,'</LOCATION>',CRLF);
    write (RuckVar,'<PEOPLE>',CRLF);
    write (RuckVar,'</PEOPLE>',CRLF);
    write (RuckVar,'<REFERENCE>',CRLF);
    write (RuckVar,'</REFERENCE>',CRLF);
    write (RuckVar,'<TAXONSEARCHCODE>',CRLF);
  end;
  if edSearchColumnName.text <> ''  then begin
    rs.MoveFirst;
    While Not (rs.eof) do begin
      if (rs.fields[1].value <> '') and  (rs.fields[4].value <> '') then
         write(RuckVar,Get_RucksackOutputLine(rs.fields[1].value,rs.fields[5].value),CRLF);
        rs.MoveNext;
     end;
  end;
  if not FIsFromRuckSack then write(RuckVar,'</TAXONSEARCHCODE>',CRLF);
  closefile (RuckVar);
end;

function TdlgGenerateRucksack.Get_RuckSackOutputLine( TaxonKey :string; SearchKey :string) :string;
begin
   if SearchKey = '' then Result := Taxonkey
     else Result := TaxonKey +  '=' + Searchkey;
end;

function  TdlgGenerateRucksack.Get_AdditionalTags() : widestring;
Var
lAllOutput: string;
rsTag : _Recordset;

begin
  rsTag := dmDatabase.ExecuteSQL('SELECT * FROM ##TempRucksack',true);
  While not rsTag.Eof do begin
    if (rsTag.Fields[0].value = '<TAXONSEARCHCODE>')
       and (cbTransfer.checked) then Break;
    lAllOutput := lAllOutput + rsTag.Fields[0].value + CRLF;
    rsTag.MoveNext;
  end;
  if cbTransfer.checked then
      lAllOutput := lAllOutput + '<TAXONSEARCHCODE>' + CRLF  +  '</TAXONSEARCHCODE>';
  Result := lAllOutput;
end;

function TdlgGenerateRucksack.Update_Sort_Order(): boolean;
var lSort: string;
begin
  Case rgSort.ItemIndex of
    0 : lSort := SORT_ORIGINAL_ORDER;
    1 : lSort := SORT_TAXON_NAME;
    2 : lSort := SORT_GROUP_TAXON;
    3 : lSort := SORT_DICT_SORT;

   else lSort := 'SEARCHNAME';
   end;
   dmDatabase.ExecuteSQL(Format(SQL_SORT_TO_USE,[lSort]),true);
   Result:= true;
end;

procedure TdlgGenerateRucksack.cbRecommendedClick(Sender: TObject);
begin
  if cbRecommended.Checked then
    cbTransfer.Checked := false;

end;

procedure TdlgGenerateRucksack.cbTransferClick(Sender: TObject);
begin
  if cbTransfer.checked then
    cbRecommended.Checked := false;
end;

// R6 does like the same key being in a rucksack twice
function TdlgGenerateRucksack.Remove_Duplicates: boolean;
var
  rs: _Recordset;
  lPrevious: string;
begin
  lPrevious := '';
  rs := dmDatabase.ExecuteSQL('SELECT * FROM ##TempTaxa Order by TLIKey',true);
  while not rs.eof do begin
    if rs.fields[1].value = lPrevious then begin
      dmDatabase.ExecuteSQL(Format(SQL_SET_DUPLICATE,[rs.fields[3].value]),true);
    end;
    lPrevious := rs.fields[1].value;
    rs.MoveNext;
  end;
  dmDatabase.ExecuteSQL(SQL_REMOVE_DUPLICATE,true);
  Result := true;
end;

end.
