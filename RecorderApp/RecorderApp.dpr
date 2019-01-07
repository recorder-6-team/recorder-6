program RecorderApp;

uses
  LanguagesDEPfix in 'Source\LanguagesDEPfix.pas',
  madListHardware,
  madListProcesses,
  madListModules,
  madExcept,
  LocOnFly,
  KLocales,
  Forms,
  Windows,
  Messages,
  Controls,
  Dialogs,
  SysUtils,
  MidasLib,
  Recorder2000_TLB in 'Recorder2000_TLB.pas',
  RecorderAddInInstaller_TLB in '..\RecorderAddInInstaller\RecorderAddInInstaller_TLB.pas',
  COMClasses in 'Components\COMClasses.pas',
  ComformControls in 'Components\ComFormControls.pas',
  CompositeComponent in 'Components\CompositeComponent.pas',
  DataClasses in 'Components\DataClasses.pas',
  DatabaseOutput in 'Components\DatabaseOutput.pas',
  DatabaseUtilities in 'Components\DatabaseUtilities.pas',
  DataOutput in 'Components\DataOutput.pas',
  DataStringGrid in 'Components\DataStringGrid.pas',
  DBBrowser in 'Components\DBBrowser.pas',
  DBBrowserRapTree in 'Components\DBBrowserRapTree.pas',
  DBFilterGrid in 'Components\DBFilterGrid.pas',
  DBListCombo in 'Components\DBListCombo.pas',
  DLLPeek in 'Components\DLLPeek.pas',
  DropSource in 'Components\DropSource.pas',
  DropStruct in 'Components\DropStruct.pas',
  DropTarget in 'Components\DropTarget.pas',
  ExportFilters in 'Components\ExportFilters.pas',
  ExportFilterSQL in 'Components\ExportFilterSQL.pas',
  FileOutput in 'Components\FileOutput.pas',
  Finder in 'Components\Finder.pas',
  GenFuncs in 'Components\GenFuncs.pas',
  GridSquareExtractor in 'Components\GridSquareExtractor.pas',
  GridSquareItem in 'Components\GridSquareItem.pas',
  HierarchyNodes in 'Components\HierarchyNodes.pas',
  HTMLDisplayFuncs in 'Components\HTMLDisplayFuncs.pas',
  ImportWizardDBGrid in 'Components\ImportWizardDBGrid.pas',
  JNCCDatasets in 'Components\JNCCDatasets.pas',
  JNCCGrid in 'Components\JNCCGrid.pas',
  JnccQRDesign in 'Components\JnccQRDesign.pas',
  JNCCRelationships in 'Components\JNCCRelationships.pas',
  JNCCXMLDoc in 'Components\JNCCXMLDoc.pas',
  JRO_TLB in 'Components\JRO_TLB.pas',
  MapPolygonInteraction in 'Components\MapPolygonInteraction.pas',
  MapPolygonScanner in 'Components\MapPolygonScanner.pas',
  MarkupDocs in 'Components\MarkupDocs.pas',
  Measurements in 'Components\Measurements.pas',
  MeasurementsData in 'Components\MeasurementsData.pas' {dmMeasurements: TDataModule},
  RecordingSchemes in 'Components\RecordingSchemes.pas',
  ReportExport in 'Components\ReportExport.pas',
  ReportGenerator in 'Components\ReportGenerator.pas',
  ReportSQLData in 'Components\ReportSQLData.pas',
  Sources in 'Components\Sources.pas',
  SourcesData in 'Components\SourcesData.pas' {dmSources: TDataModule},
  SpatialRef in 'Components\SpatialRef.pas',
  SpatialRefFuncs in 'Components\SpatialRefFuncs.pas',
  SQLConverter in 'Components\SQLConverter.pas',
  TempData_ADO in 'Components\TempData_ADO.pas',
  ThreadedDatabaseOutput in 'Components\ThreadedDatabaseOutput.pas',
  ThreadedDataOutput in 'Components\ThreadedDataOutput.pas',
  VagueDate in 'Components\VagueDate.pas',
  VagueDateEdit in 'Components\VagueDateEdit.pas',
  VagueDateLatLong in 'Components\VagueDateLatLong.pas',
  XMLData in 'Components\XMLData.pas' {dmNBNXML: TDataModule},
  xmlErrorReport in 'Components\xmlErrorReport.pas' {frmXMLReport: TfrmXMLReport},
  XMLTypes in 'Components\XmlTypes.pas',
  JnccQRDesignReg7 in 'Packages\JnccQRDesignReg7.pas',
  About in 'Source\About.pas' {dlgAbout},
  AddFilterContainer in 'Source\AddfilterContainer.pas',
  AddIn in 'Source\AddIn.pas' {dlgAddIn},
  AdminAreaDictBrowser in 'Source\AdminAreaDictBrowser.pas' {frmAdminAreaDictBrowser},
  AdminAreaDictBrowserData in 'Source\AdminAreaDictBrowserData.pas' {dmAdminAreaDictBrowser: TDataModule},
  ApplicationSettings in 'Source\ApplicationSettings.pas' {AutoApplicationSettings: CoClass},
  BaseBiotopeDictDataUnit in 'Source\BaseBiotopeDictDataUnit.pas' {BaseBiotopeDictData: TDataModule},
  BaseBiotopeDictUnit in 'Source\BaseBiotopeDictUnit.pas' {BaseBiotopeDict},
  BaseChildUnit in 'Source\BaseChildUnit.pas' {BaseChild},
  BaseData in 'Source\BaseData.pas' {BaseDataModule: TDataModule},
  BaseDictionaryDataUnit in 'Source\BaseDictionaryDataUnit.pas' {BaseDictionaryData: TDataModule},
  BaseDictionaryUnit in 'Source\BaseDictionaryUnit.pas' {BaseDictionary},
  BaseDockedForm in 'Source\BaseDockedForm.pas' {frmBaseDockedForm},
  BaseDragFormUnit in 'Source\BaseDragFormUnit.pas' {BaseDragForm},
  BaseExportableUnit in 'Source\BaseExportableUnit.pas' {BaseExportable},
  BaseFormUnit in 'Source\BaseFormUnit.pas' {BaseForm},
  BaseLegend in 'Source\BaseLegend.pas',
  BaseOccurrenceDetail in 'Source\BaseOccurrenceDetail.pas' {frmBaseOccurrenceDetail},
  BaseReportSelectUnit in 'Source\BaseReportSelectUnit.pas' {BaseReportSelect},
  BaseSampleEventData in 'Source\BaseSampleEventData.pas' {BaseSampleEventDataModule: TDataModule},
  BaseTaxonDictDataUnit in 'Source\BaseTaxonDictDataUnit.pas' {BaseTaxonDictData: TDataModule},
  BaseTaxonDictUnit in 'Source\BaseTaxonDictUnit.pas' {BaseTaxonDict},
  BatchUpdates in 'Source\BatchUpdates.pas' {dlgBatchUpdate},
  BiotopeDictBrowser in 'Source\BiotopeDictBrowser.pas' {frmBiotopeDictBrowser},
  BiotopeDictBrowserData in 'Source\BiotopeDictBrowserData.pas' {dmBiotopeDictBrowser: TDataModule},
  BiotopeDictEditor in 'Source\BiotopeDictEditor.pas' {frmBiotopeDictEditor},
  BiotopeDictEditorData in 'Source\BiotopeDictEditorData.pas' {dmBiotopeDictEditor: TDataModule},
  BiotopeOccur in 'Source\BiotopeOccur.pas' {frmBiotopeOccurrences},
  BiotopeOccurData in 'Source\BiotopeOccurData.pas' {dmBiotopeOccurrences: TDataModule},
  BoundaryFieldSelect in 'Source\BoundaryFieldSelect.pas' {dlgBoundaryFieldSelect},
  BoundaryImport in 'Source\BoundaryImport.pas' {dlgBoundaryImportDialog},
  BoundaryLocationMatch in 'Source\BoundaryLocationMatch.pas' {dlgBoundaryLocationMatch},
  Cascade in 'Source\Cascade.pas' {dlgCascade},
  ChangeCustodianDb in 'Source\ChangeCustodianDb.pas' {TdlgChangeCustodianDB},
  ChangePassword in 'Source\ChangePassword.pas' {dlgChangePassword},
  ComAddinUnit in 'Source\ComAddinUnit.pas',
  ComplexFilter in 'Source\ComplexFilter.pas' {dlgComplexFilter},
  ConceptTerm in 'Source\ConceptTerm.pas',
  ContributionProgress in 'Source\ContributionProgress.pas' {frmContributionProgress},
  Constants in 'Source\Constants.pas',
  CRColumns in 'Source\CRColumns.pas',
  CRCommonClasses in 'Source\CRCommonClasses.pas',
  CRConstants in 'Source\CRConstants.pas',
  CRCSVTables in 'Source\CRCSVTables.pas',
  CRExternalFilterFiles in 'Source\CRExternalFilterFiles.pas',
  CRFrmParametersUnit in 'Source\CRFrmParametersUnit.pas' {frmParameters},
  CRReportIndex in 'Source\CRReportIndex.pas',
  CRReportSQL in 'Source\CRReportSQL.pas',
  Custody in 'Source\Custody.pas',
  CustodyTransfer in 'Source\CustodyTransfer.pas' {dlgCustodyTransfer},
  CustodyTransferExport in 'Source\CustodyTransferExport.pas' {dlgCustodyTransferExport},
  DatabaseAccessADO in 'Source\DatabaseAccessADO.pas' {dmDatabase: TDataModule},
  DataImport in 'Source\DataImport.pas',
  DataExport in 'Source\DataExport.pas' {dlgDataExport},
  DatasetLegend in 'Source\DatasetLegend.pas',
  DBMerger in 'Source\DBMerger.pas',
  DefaultPaths in 'Source\DefaultPaths.pas',
  DeleteDatasets in 'Source\DeleteDatasets.pas' {dlgDeleteDatasets},
  DeterminationData in 'Source\DeterminationData.pas' {dmDetermination: TDataModule},
  DialogContainer in 'Source\DialogContainer.pas' {dlgContainer},
  DictionaryDataGeneric in 'Source\DictionaryDataGeneric.pas',
  DictionaryHTMLDetails in 'Source\DictionaryHTMLDetails.pas',
  DictionaryUpgrade in 'Source\DictionaryUpgrade.pas' {TdlgDictionaryUpgrade},
  EnhancedTermLists in 'Source\EnhancedTermLists.pas' {frmEnhancedTermLists},
  EnhancedTermListsData in 'Source\EnhancedTermListsData.pas' {dmEnhancedTermLists: TDataModule},
  EventData in 'Source\EventData.pas' {dmEvent: TDataModule},
  EventDetails in 'Source\EventDetails.pas' {frmEventDetails},
  ExportField in 'Source\ExportField.pas' {dlgExportField},
  ExternalFilter in 'Source\ExternalFilter.pas',
  ExtractGridSquares in 'Source\ExtractGridSquares.pas' {dlgExtractGridSquares},
  FeatureDetails in 'Source\FeatureDetails.pas' {frmFeatureDetails},
  FeatureDetailsData in 'Source\FeatureDetailsData.pas' {dmFeatureDetails: TDataModule},
  Filter in 'Source\Filter.pas' {dlgFilter},
  FilterManager in 'Source\FilterManager.pas' {frmFilterManager},
  FilterResult in 'Source\FilterResult.pas' {frmFilterResult},
  Find in 'Source\Find.pas' {dlgFind},
  FirstRun in 'Source\FirstRun.pas' {dlgFirstRun},
  FormActions in 'Source\FormActions.pas' {dmFormActions: TDataModule},
  GeneralData in 'Source\GeneralData.pas' {dmGeneralData: TDataModule},
  GoogleEarthExport in 'Source\GoogleEarthExport.pas' {dlgGoogleEarthExport},
  IndOrg in 'Source\IndOrg.pas' {frmIndOrg},
  InstallProgress in 'Source\InstallProgress.pas' {frmInstallProgress},
  LastSearchedRucksack in 'Source\LastSearchedRucksack.pas',
  LayerLegend in 'Source\LayerLegend.pas',
  LocationData in 'Source\LocationData.pas' {dmLocation: TDataModule},
  LocationDetails in 'Source\LocationDetails.pas' {frmLocationDetails},
  LocationDetailsData in 'Source\LocationDetailsData.pas' {dmLocationDetails: TDataModule},
  LocationInfoFrame in 'Source\LocationInfoFrame.pas' {fraLocationInfo: TFrame},
  Locations in 'Source\Locations.pas' {frmLocations},
  LogIn in 'Source\Login.pas' {dlgLogin},
  Maintbar in 'Source\Maintbar.pas' {frmMain},
  Map in 'Source\Map.pas' {frmMap},
  MapBrowser in 'Source\MapBrowser.pas' {frmMapBrowser},
  MapClasses in 'Source\MapClasses.pas',
  MapData in 'Source\MapData.pas' {dmMap: TDataModule},
  MapLocationOptions in 'Source\MapLocationOptions.pas' {dlgMapLocationOptions},
  MapOptions in 'Source\MapOptions.pas' {dlgMapOptions},
  MapSelection in 'Source\MapSelection.pas' {dlgMapSelection},
  MapServerLink in 'Source\MapServerLink.pas',
  MergeData in 'Source\MergeData.pas' {frmMergeData},
  MergeDataData in 'Source\MergeDataData.pas' {dmMergeData: TDataModule},
  Metadata in 'Source\Metadata.pas' {dlgMetadata},
  MetaDataPopup in 'Source\MetaDataPopup.pas' {dlgMetaDataPopup},
  MultipleMatches in 'Source\MultipleMatches.pas' {TdlgMultipleMatches},
  NagScreen in 'Source\NagScreen.pas' {dlgNagScreen},
  NameData in 'Source\NameData.pas' {dmName: TDataModule},
  NewPlaceCard in 'Source\NewPlaceCard.pas' {dlgNewPlaceCard},
  ObservationData in 'Source\ObservationData.pas' {dmObservation: TDataModule},
  Observations in 'Source\Observations.pas' {frmObservations},
  OLEContainer in 'Source\OLEContainer.pas' {frmMDIContainer},
  OnlineHelp in 'Source\OnlineHelp.pas',
  OpenReport in 'Source\OpenReport.pas' {dlgOpenReport},
  Options in 'Source\Options.pas' {dlgOptions},
  Password in 'Source\Password.pas' {dlgPassword},
  PatternMatching in 'Source\PatternMatching.pas',
  PlaceCard in 'Source\PlaceCard.pas' {frmPlaceCard},
  PlaceCardData in 'Source\PlaceCardData.pas' {dmPlaceCard: TDataModule},
  PolygonLayerDetails in 'Source\PolygonLayerDetails.pas' {dlgPolygonLayerDetails},
  PrintTitle in 'Source\PrintTitle.pas' {dlgPrintTitle},
  References in 'Source\References.pas' {frmReferences},
  ReferencesData in 'Source\ReferencesData.pas' {dmReferences: TDataModule},
  RegisterMap in 'Source\RegisterMap.pas' {dlgRegisterMap},
  ReportDesigner in 'Source\ReportDesigner.pas',
  ReportPreview in 'Source\ReportPreview.pas' {frmReportPreview},
  Reports in 'Source\Reports.pas' {qrReports: TQuickRep},
  ReportsData in 'Source\ReportsData.pas' {dmReports: TDataModule},
  ReportToSHPExport in 'Source\ReportToSHPExport.pas' {dlgReportToSHPExport},
  ReportWizardSettings in 'Source\ReportWizardSettings.pas',
  ResourceStrings in 'Source\ResourceStrings.pas',
  Rucksack in 'Source\Rucksack.pas' {frmRuckSack},
  RucksackSave in 'Source\RucksackSave.pas' {dlgRucksackSave},
  RunReportOptions in 'Source\RunReportOptions.pas' {dlgRunReportOptions},
  SampleData in 'Source\SampleData.pas' {dmSample: TDataModule},
  SampleDetails in 'Source\SampleDetails.pas' {frmSampleDetails},
  SchemeManager in 'Source\SchemeManager.pas' {dlgSchemeManager},
  SchemeManagerData in 'Source\SchemeManagerData.pas' {dmSchemeManager},
  Search in 'Source\Search.pas' {dmSearch: TDataModule},
  SelectDistributionPoint in 'Source\SelectDistributionPoint.pas' {dlgSelectDistributionPoint},
  SelectLocationSearchColumns in 'Source\SelectLocationSearchColumns.pas' {dlgSelectLocationSearchColumns},
  ServerFolders in 'Source\ServerFolders.pas' {dlgServerFolders},
  Snapshot in 'Source\Snapshot.pas' {frmSnapshot},
  SpeciesCard in 'Source\SpeciesCard.pas' {frmSpeciesCard},
  SQLConstants in 'Source\SQLConstants.pas',
  SubFormActions in 'Source\SubFormActions.pas' {dmSubFormActions: TDataModule},
  SurveyData in 'Source\SurveyData.pas' {dmSurvey: TDataModule},
  SurveyDataOptions in 'Source\SurveyDataOptions.pas' {dlgSurveyDataOptions},
  SurveyDetails in 'Source\SurveyDetails.pas' {frmSurveyDetails},
  SurveyTagDetails in 'Source\SurveyTagDetails.pas' {frmSurveyTagDetails},
  TaxonDictBrowser in 'Source\TaxonDictBrowser.pas' {frmTaxonDictBrowser},
  TaxonDictBrowserData in 'Source\TaxonDictBrowserData.pas' {dmTaxonDictBrowser: TDataModule},
  TaxonDictEditor in 'Source\TaxonDictEditor.pas' {frmTaxonDictEditor},
  TaxonDictEditorData in 'Source\TaxonDictEditorData.pas' {dmTaxonDictEditor: TDataModule},
  TaxonOccur in 'Source\TaxonOccur.pas' {frmTaxonOccurrences},
  TaxonOccurrData in 'Source\TaxonOccurrData.pas' {dmTaxonOccurrences: TDataModule},
  TermLists in 'Source\TermLists.pas' {frmTermLists},
  TermListsData in 'Source\TermListsData.pas' {dmTermLists: TDataModule},
  TextInput in 'Source\TextInput.pas' {dlgTextInput},
  UserConfig in 'Source\UserConfig.pas' {dlgUserConfig},
  ValidationData in 'Source\ValidationData.pas' {dmValidation: TDataModule},
  ValidationResults in 'Source\ValidationResults.pas' {dlgValidationResults},
  Welcome in 'Source\Welcome.pas' {dlgWelcome},
  Wizard in 'Source\Wizard.pas' {dlgWizard},
  WizardData in 'Source\WizardData.pas' {dmWizard: TDataModule},
  XmlReportList in 'Source\XmlReportList.pas',
  BaseMatchPage in 'Import Wizard\BaseMatchPage.pas' {BaseMatch: TFrame},
  ColumnType in 'Import Wizard\ColumnType.pas' {dlgColumnType},
  ColumnTypes in 'Import Wizard\ColumnTypes.pas' {fraColumnTypes: TFrame},
  FileSelect in 'Import Wizard\FileSelect.pas' {fraFileSelect: TFrame},
  FixedWidths in 'Import Wizard\FixedWidths.pas' {fraFixedWidths: TFrame},
  FastColumnTypes in 'Import Wizard\FastColumnTypes.pas',
  Import in 'Import Wizard\Import.pas' {fraImport: TFrame},
  ImportAnalysis in 'Import Wizard\ImportAnalysis.pas' {fraImportAnalysis: TFrame},
  ImportComplete in 'Import Wizard\ImportComplete.pas' {dlgImportComplete},
  ImportWizard in 'Import Wizard\ImportWizard.pas' {frmImportWizard},
  IWBasePage in 'Import Wizard\IWBasePage.pas' {BasePage: TFrame},
  IWColumnMappingClasses in 'Import Wizard\IWColumnMappingClasses.pas',
  IWColumnTypeKeys in 'Import Wizard\IWColumnTypeKeys.pas',
  IWConstants in 'Import Wizard\IWConstants.pas',
  IWOutputFieldGenerators in 'Import Wizard\IWOutputFieldGenerators.pas',
  IWParsers in 'Import Wizard\IWParsers.pas',
  IWResourceStrings in 'Import Wizard\IWResourceStrings.pas',
  IWSearchManager in 'Import Wizard\IWSearchManager.pas',
  IWSettings in 'Import Wizard\IWSettings.pas' {dmIWSettings: TDataModule},
  IWTableRule in 'Import Wizard\IWTableRule.pas',
  IWUserSuppliedData in 'Import Wizard\IWUserSuppliedData.pas',
  IWValidation in 'Import Wizard\IWValidation.pas',
  MatchGeneric in 'Import Wizard\MatchGeneric.pas' {fraMatchGeneric: TFrame},
  MatchLocations in 'Import Wizard\MatchLocations.pas' {fraMatchLocations: TFrame},
  MissingData in 'Import Wizard\MissingData.pas' {fraMissingData: TFrame},
  ProjectSpecificAccess in 'Import Wizard\ProjectSpecificAccess.pas',
  IWLargeImportFile in 'Import Wizard\IWLargeImportFile.pas',
  splist in '..\Third Party\Dorset Software Services\DssVcl\splist.pas',
  KeyList in '..\Third Party\Dorset Software Services\DssVcl\KeyList.pas',
  ADOConnStringEditor in '..\Third Party\Dorset Software Services\DssVcl32\ADOConnStringEditor.pas' {dlgConnStringEditor: TForm},
  ADODB_TLB in '..\Third Party\Dorset Software Services\DssVcl32\ADODB_TLB.pas',
  ADOX_TLB in '..\Third Party\Dorset Software Services\DssVcl32\ADOX_TLB.pas',
  ApiUtils in '..\Third Party\Dorset Software Services\DssVcl32\ApiUtils.pas',
  BaseADODataModule in '..\Third Party\Dorset Software Services\DssVcl32\BaseADODataModule.pas',
  ComboListID in '..\Third Party\Dorset Software Services\DssVcl32\ComboListID.pas',
  ColorBtn in '..\Third Party\Dorset Software Services\DssVcl32\ColorBtn.pas',
  ColPal in '..\Third Party\Dorset Software Services\DssVcl32\ColPal.pas',
  ControlStringGrid in '..\Third Party\Dorset Software Services\DssVcl32\ControlStringGrid.pas',
  CustomOleCtrls7 in '..\Third Party\Dorset Software Services\DssVcl32\CustomOleCtrls7.pas',
  DBGlyphCtrls in '..\Third Party\Dorset Software Services\DssVcl32\DBGlyphCtrls.pas',
  DirectDraw in '..\Third Party\Dorset Software Services\DssVcl32\DirectDraw.pas',
  DisplayFuncs in '..\Third Party\Dorset Software Services\DssVcl32\DisplayFuncs.pas',
  DragDrop in '..\Third Party\Dorset Software Services\DssVcl32\DragDrop.pas',
  DssStringGrid in '..\Third Party\Dorset Software Services\DssVcl32\DssStringGrid.pas',
  easyshell in '..\Third Party\Dorset Software Services\DssVcl32\EasyShell.pas',
  ExceptionForm in '..\Third Party\Dorset Software Services\DssVcl32\ExceptionForm.pas' {frmException: TfrmException},
  FileUtils in '..\Third Party\Dorset Software Services\DssVcl32\FileUtils.pas',
  FolderBrowser in '..\Third Party\Dorset Software Services\DssVcl32\FolderBrowser.pas',
  GeneralFunctions in '..\Third Party\Dorset Software Services\DssVcl32\GeneralFunctions.pas',
  GDIPAPI in '..\Third Party\Dorset Software Services\DssVcl32\GDIPAPI.pas',
  GDIPObj in '..\Third Party\Dorset Software Services\DssVcl32\GDIPObj.pas',
  html2rtf in '..\Third Party\Dorset Software Services\DssVcl32\Html2RTF.pas',
  HintPointer in '..\Third Party\Dorset Software Services\DssVcl32\HintPointer.pas',
  IDComboBox in '..\Third Party\Dorset Software Services\DssVcl32\IDComboBox.pas',
  ImageListButton in '..\Third Party\Dorset Software Services\DssVcl32\ImageListButton.pas',
  KeyboardRapidTree in '..\Third Party\Dorset Software Services\DssVcl32\KeyboardRapidTree.pas',
  ListDlls in '..\Third Party\Dorset Software Services\DssVcl32\ListDlls.pas',
  MapiDLLFunctions in '..\Third Party\Dorset Software Services\DssVcl32\MapiDLLFunctions.pas',
  OLETools in '..\Third Party\Dorset Software Services\DssVcl32\OLETools.pas',
  PrivilegedComObj in '..\Third Party\Dorset Software Services\DssVcl32\PrivilegedComObj.pas',
  Relationships_ADO in '..\Third Party\Dorset Software Services\DssVcl32\Relationships_ADO.pas',
  RestrictedEdits in '..\Third Party\Dorset Software Services\DssVcl32\RestrictedEdits.pas',
  RTFGrid in '..\Third Party\Dorset Software Services\DssVcl32\RTFGrid.pas',
  SortListView in '..\Third Party\Dorset Software Services\DssVcl32\SortListView.pas',
  Sql in '..\Third Party\Dorset Software Services\DssVcl32\Sql.pas',
  SQLList in '..\Third Party\Dorset Software Services\DssVcl32\SQLList.pas',
  StockIcons in '..\Third Party\Dorset Software Services\DssVcl32\StockIcons.pas',
  streamhtml2rtf in '..\Third Party\Dorset Software Services\DssVcl32\streamhtml2rtf.pas',
  TablePriorityList_ADO in '..\Third Party\Dorset Software Services\DssVcl32\TablePriorityList_ADO.pas',
  TaskProgress in '..\Third Party\Dorset Software Services\DssVcl32\TaskProgress.pas',
  VersionInfo in '..\Third Party\Dorset Software Services\DssVcl32\VersionInfo.pas',
  XPToolButton in '..\Third Party\Dorset Software Services\DssVcl32\XPToolButton.pas',
  AddinIDComboBox in '..\Third Party\Dorset Software Services\Recorder Addins\Components\AddinIDComboBox.pas',
  AddinLinkedControls in '..\Third Party\Dorset Software Services\Recorder Addins\Components\AddinLinkedControls.pas',
  AddinCompositeComponent in '..\Third Party\Dorset Software Services\Recorder Addins\Components\AddinCompositeComponent.pas',
  BaseCompositeComponent in '..\Third Party\Dorset Software Services\Recorder Addins\Components\BaseCompositeComponent.pas',
  DSSDataTypes in '..\Third Party\Dorset Software Services\Recorder Addins\Components\DSSDataTypes.pas',
  LinkedControls in '..\Third Party\Dorset Software Services\Recorder Addins\Components\LinkedControls.pas',
  AddinConstants in '..\Third Party\Dorset Software Services\Recorder Addins\Source\AddinConstants.pas',
  AddinFind in '..\Third Party\Dorset Software Services\Recorder Addins\Source\AddinFind.pas' {dlgAddinFind},
  AddinGeneralData in '..\Third Party\Dorset Software Services\Recorder Addins\Source\AddinGeneralData.pas' {dmAddinGeneral: TDataModule},
  AddinInterfaceDataModule in '..\Third Party\Dorset Software Services\Recorder Addins\Source\AddinInterfaceDataModule.pas' {dmAddinInterface: TDataModule},
  AddinResourceStrings in '..\Third Party\Dorset Software Services\Recorder Addins\Source\AddinResourceStrings.pas',
  AddinSearchManager in '..\Third Party\Dorset Software Services\Recorder Addins\Source\AddinSearchManager.pas',
  DSSLocalizerUtils in '..\Third Party\Dorset Software Services\Recorder Addins\Source\DSSLocalizerUtils.pas',
  ExternalSourceDetails in 'Components\ExternalSourceDetails.pas' {dlgExternalSourceDetails},
  HierarchyFunctions in 'Source\HierarchyFunctions.pas';

{$R 'RecorderApp.manifest.res' 'RecorderApp.manifest.rc'}

{$R RecorderApp.KLR}

{$R Recorder2000.TLB}

{$R *.RES}

var
  lModalResult : TModalResult;
  MutHandle    : THandle=0;
begin
  TDSSLocUtils.Init;
  {$IFDEF madExcept}
  RegisterExceptionHandler(MadExceptionHandler, stTrySyncCallOnSuccess);
  {$ENDIF}
  // Check for previous instance of Recorder 2000
  MutHandle:=OpenMutex(MUTEX_ALL_ACCESS, false, 'Recorder 2000');
  // If not running, all fine, else exit
  if MutHandle=0 then
    MutHandle:=CreateMutex(nil,true,'Recorder 2000')
  else
  begin
    MessageDlg(ResStr_AlreadyRunningOnThisMachine, mtWarning, [mbOk], 0);
    if ParamCount>=1 then
      PostMessage(StrToInt64(Copy(ParamStr(1), 2, 255)), WM_CLOSE, 0, 0);
    Exit;
  end;

  Application.Initialize;
  MstErrorMessage := ResStr_UnhandledErrorOccur;
  NonCriticalExceptions.Add('EPrinter');
  Application.BringToFront;
  { Ensure the bitmap displays }
  Application.ProcessMessages;
  { Prepare the rucksack and application settings instances }
  AppSettings:=TApplicationSettings.Create;
  try
    dmDatabase := TdmDatabase.Create(Application,
                                     AppSettings.ServerName,
                                     AppSettings.DatabaseName,
                                     AppSettings.TrustedSecurity);
    dmGeneralData := TdmGeneralData.Create(Application);
    dmDatabase.DisplayCommonNames := AppSettings.DisplayTaxonCommonNames;
    dmDatabase.UserId := AppSettings.UserId;
    dmDatabase.SessionID := AppSettings.SessionId;
    Application.Title := 'Recorder 6';
    dlgLogin:= TdlgLogin.Create(Application);
    { Online Help Setup }
    OHelp := TOnlineHelp.Create(dlgLogin.Handle);
    Application.OnHelp := OHelp.OnHelpReplacement;
    dlgLogin.HelpContext := IDH_LOGIN;
    try
      if dlgLogin.CancelledFirstRun then
        lModalResult := mrCancel
      else
      if dlgLogin.BypassLogin then
        lModalResult := mrOk
      else
        lModalResult := dlgLogin.ShowModal;

      { Reshow the splash screen correctly }
      Application.ProcessMessages;
    finally
      dlgLogin.Release;
    end;

    { Only create forms if required }
    if lModalResult = mrOK then
    begin
      if TdlgNagScreen.MustShow then
        with TdlgNagScreen.Create(nil) do
          try
            ShowModal;
          finally
            Free;
          end;
      dmFormActions := TdmFormActions.Create(Application);
      Application.CreateForm(TdmSearch, dmSearch);
  Application.CreateForm(TfrmMain, frmMain);
  //Init menus controlled by addins
      AppSettings.ComAddins.MainMenu := frmMain.mnuMain;
      AppSettings.ComAddins.InitialiseDynamicMenus;
      Application.CreateForm(TdmValidation, dmValidation);
      dmValidation.SetDatabase(dmDatabase.LocalDatabase);
    end;
    // close the splash screen from the handle passed as a parameter
    if ParamCount>=1 then
      PostMessage(StrToInt64(Copy(ParamStr(1), 2, 255)), WM_CLOSE, 0, 0);
    try
      Application.Run;
    finally
      AppSettings.Free;
      OHelp.Free;
    end;
  except on E:Exception do
    begin
      MessageDlg(ResStr_ApplicationStartingError + #13#10 + 
                 E.Classname + ': ' + E.Message, mtError, [mbOk], 0);
      if ParamCount>=1 then
        PostMessage(StrToInt64(Copy(ParamStr(1), 2, 255)), WM_CLOSE, 0, 0);
    end;
  end;
end.

