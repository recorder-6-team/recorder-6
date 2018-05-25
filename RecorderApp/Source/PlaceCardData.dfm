inherited dmPlaceCard: TdmPlaceCard
  OldCreateOrder = True
  Left = 99
  Top = 134
  Height = 301
  Width = 540
  object qryCheckEvent: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Survey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT SE.SURVEY_EVENT_KEY, SE.VAGUE_DATE_START, SE.VAGUE_DATE_E' +
        'ND, SE.VAGUE_DATE_TYPE'
      'FROM SURVEY_EVENT SE'
      'Where SE.SURVEY_KEY = :Survey'
      '')
    ParseSQL = True
    Left = 268
    Top = 60
  end
  object qryCheckRecorders: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT SE_RECORDER_KEY, NAME_KEY'
      'FROM SURVEY_EVENT_RECORDER'
      'WHERE SURVEY_EVENT_KEY = :Key')
    ParseSQL = True
    Left = 364
    Top = 60
  end
  object dsSampleType: TDataSource
    DataSet = qrySampleType
    Left = 176
    Top = 56
  end
  object tblRecordType: TJNCCTable
    CommandTimeout = 0
    TableName = 'RECORD_TYPE'
    Left = 96
    Top = 8
  end
  object dsRecordType: TDataSource
    DataSet = tblRecordType
    Left = 96
    Top = 56
  end
  object tblSubstrate: TJNCCTable
    CommandTimeout = 0
    TableName = 'SUBSTRATE'
    Left = 24
    Top = 8
  end
  object dsSubstrate: TDataSource
    DataSet = tblSubstrate
    Left = 24
    Top = 56
  end
  object qryAdminAreas: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT AA.ITEM_NAME, AA.SHORT_CODE'
      'FROM ADMIN_AREA As AA'
      'INNER JOIN'
      'LOCATION_ADMIN_AREAS As LAA'
      'ON AA.ADMIN_AREA_KEY = LAA.ADMIN_AREA_KEY'
      'WHERE LAA.LOCATION_KEY = :Key')
    ParseSQL = True
    Left = 452
    Top = 8
  end
  object qryTaxonTLIKey: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'TaxonKey'
        Size = -1
        Value = Null
      end
      item
        Name = 'TaxonListKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TLI.Taxon_List_Item_Key'
      
        'FROM Taxon_List_Version AS TLV, Taxon_List_Item AS TLI, Taxon_Ve' +
        'rsion AS TV'
      'WHERE  TV.Taxon_Key =:TaxonKey'
      'AND  TLI.Taxon_Version_Key = TV.Taxon_Version_Key'
      'AND TLV.Taxon_list_Version_key = TLI.Taxon_list_Version_key'
      'AND TLV.Taxon_List_Key =:TaxonListKey')
    ParseSQL = True
    Left = 268
    Top = 8
  end
  object qryBiotopeBLIKey: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Biotope_List_Item_Key'
      'FROM   Biotope_List_Item '
      'WHERE  Biotope_Key = :KeyParameter')
    ParseSQL = True
    Left = 364
    Top = 8
  end
  object qryTaxaNames: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT TLI.taxon_list_Item_Key AS ListItemKey,'
      '       TLI2.Taxon_List_Item_Key AS OriginalKey,'
      '       ITN.Preferred_Name AS ItemName,'
      '       TV.Validation_Level AS Critical,'
      '       TLI.Lst_Itm_Code AS CodeNumber,'
      '       ITN.Common_Name AS CommonName, ITN.Abbreviation'
      'FROM ((Taxon_List_Item TLI'
      
        'INNER JOIN Taxon_List_Item TLI2 ON TLI.Taxon_List_Item_Key = TLI' +
        '2.Preferred_Name)'
      
        'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI' +
        '.Taxon_List_Item_Key)'
      
        'INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_' +
        'Version_Key'
      'WHERE TLI2.Taxon_List_Item_Key IN ('
      ''
      
        ') ORDER BY TLI.Taxon_List_Item_Key, ITN.Preferred_Name, TV.Valid' +
        'ation_Level')
    ParseSQL = True
    Left = 364
    Top = 112
  end
  object qryCleanup: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 452
    Top = 112
  end
  object qrySampleType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'Select * From SAMPLE_TYPE')
    ParseSQL = True
    Left = 176
    Top = 8
  end
  object qryTaxaDetails: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT ITN.Taxon_List_Item_Key'
      'FROM Index_Taxon_Name ITN'
      
        'INNER JOIN Taxon_List_Item TLI ON TLI.Preferred_Name = ITN.Taxon' +
        '_List_Item_Key'
      'WHERE ITN.Taxon_List_Version_Key = ('
      
        'Select Top 1 Taxon_List_Version_Key from Taxon_List_Version wher' +
        'e Taxon_List_Key = '
      #39#39
      'order by version desc)'
      'AND TLI.Taxon_List_Item_Key = TLI.Preferred_Name'
      'ORDER BY TLI.Sort_Code, ITN.Actual_Name')
    ParseSQL = True
    Left = 268
    Top = 112
  end
  object qryInsertSample: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Sample_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Sample_Reference'
        Size = -1
        Value = Null
      end
      item
        Name = 'Vague_Date_Start'
        Size = -1
        Value = Null
      end
      item
        Name = 'Vague_Date_End'
        Size = -1
        Value = Null
      end
      item
        Name = 'Vague_Date_Type'
        Size = -1
        Value = Null
      end
      item
        Name = 'Spatial_Ref'
        Size = -1
        Value = Null
      end
      item
        Name = 'Spatial_Ref_System'
        Size = -1
        Value = Null
      end
      item
        Name = 'Spatial_Ref_Qualifier'
        Size = -1
        Value = Null
      end
      item
        Name = 'Lat'
        Size = -1
        Value = Null
      end
      item
        Name = 'Long'
        Size = -1
        Value = Null
      end
      item
        Name = 'Outstanding_Card'
        Size = -1
        Value = Null
      end
      item
        Name = 'Sample_Type_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Location_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Survey_Event_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Location_Name'
        Size = -1
        Value = Null
      end
      item
        Name = 'Comment'
        Size = -1
        Value = Null
      end
      item
        Name = 'Entered_By'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'INSERT INTO Sample (Sample_Key, Sample_Reference,'
      'Vague_Date_Start, Vague_Date_End, Vague_Date_Type,'
      
        'Spatial_Ref, Spatial_Ref_System, Spatial_Ref_Qualifier, "Lat", "' +
        'Long",'
      
        'Outstanding_Card, Sample_Type_Key, Location_Key, Survey_Event_Ke' +
        'y,'
      'Location_Name, Comment, Entered_By)'
      'VALUES'
      
        '(:Sample_Key, :Sample_Reference, :Vague_Date_Start, :Vague_Date_' +
        'End, :Vague_Date_Type,'
      
        ':Spatial_Ref, :Spatial_Ref_System, :Spatial_Ref_Qualifier, :Lat,' +
        ' :Long,'
      
        ':Outstanding_Card, :Sample_Type_Key, :Location_Key, :Survey_Even' +
        't_Key,'
      ':Location_Name, :Comment, :Entered_By)'
      '')
    ParseSQL = True
    Left = 48
    Top = 160
  end
  object qryInsertBiotopeOccur: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      ''
      '')
    ParseSQL = True
    Left = 48
    Top = 212
  end
  object qryInsertTaxonOccur: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 164
    Top = 160
  end
  object qryInsertTaxOccurData: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SiteID'
        Size = -1
        Value = Null
      end
      item
        Name = 'Taxon_Occurrence_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Accuracy'
        Size = -1
        Value = Null
      end
      item
        Name = 'Data'
        Size = -1
        Value = Null
      end
      item
        Name = 'Measurement_Qualifier_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Measurement_Unit_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Entered_By'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'declare @NextKey char(16)'
      ''
      'exec spNextKey '#39'TAXON_OCCURRENCE_DATA'#39', @NextKey output, :SiteID'
      ''
      'Insert into Taxon_Occurrence_Data('
      '    Taxon_Occurrence_Data_Key,  Taxon_Occurrence_Key, Accuracy,'
      
        '    Data, Measurement_Qualifier_Key, Measurement_Unit_Key, Enter' +
        'ed_By'
      ')'
      'Values('
      '    @NextKey, :Taxon_Occurrence_Key, :Accuracy,'
      
        '    :Data, :Measurement_Qualifier_Key, :Measurement_Unit_Key, :E' +
        'ntered_By'
      ')')
    ParseSQL = True
    Left = 164
    Top = 212
  end
  object qryInsertSurveyEvent: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Survey_Event_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Vague_Date_Start'
        Size = -1
        Value = Null
      end
      item
        Name = 'Vague_Date_End'
        Size = -1
        Value = Null
      end
      item
        Name = 'Vague_Date_Type'
        Size = -1
        Value = Null
      end
      item
        Name = 'Spatial_Ref'
        Size = -1
        Value = Null
      end
      item
        Name = 'Spatial_Ref_System'
        Size = -1
        Value = Null
      end
      item
        Name = 'Spatial_Ref_Qualifier'
        Size = -1
        Value = Null
      end
      item
        Name = 'Lat'
        Size = -1
        Value = Null
      end
      item
        Name = 'Long'
        Size = -1
        Value = Null
      end
      item
        Name = 'Location_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Survey_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Location_Name'
        Size = -1
        Value = Null
      end
      item
        Name = 'Entered_By'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'INSERT INTO Survey_Event (Survey_Event_Key, Vague_Date_Start, Va' +
        'gue_Date_End, Vague_Date_Type,'
      
        'Spatial_Ref, Spatial_Ref_System, Spatial_Ref_Qualifier, "Lat", "' +
        'Long",'
      'Location_Key, Survey_Key, Location_Name,  Entered_By)'
      'VALUES'
      
        '(:Survey_Event_Key,  :Vague_Date_Start, :Vague_Date_End, :Vague_' +
        'Date_Type,'
      
        ':Spatial_Ref, :Spatial_Ref_System, :Spatial_Ref_Qualifier, :Lat,' +
        ' :Long,'
      ':Location_Key, :Survey_Key,:Location_Name, :Entered_By)'
      ' ')
    ParseSQL = True
    Left = 48
    Top = 108
  end
  object qryCheckDictionary: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'TaxonList'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Allow_Data_Entry'
      'FROM TAXON_LIST_TYPE AS TLT, TAXON_LIST AS TL'
      'WHERE TLT.TAXON_LIST_TYPE_KEY = TL.TAXON_LIST_TYPE_KEY AND'
      'TL.TAXON_LIST_KEY = :TaxonList')
    ParseSQL = True
    Left = 268
    Top = 160
  end
  object qryCheckSample: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Start'
        Size = -1
        Value = Null
      end
      item
        Name = 'End'
        Size = -1
        Value = Null
      end
      item
        Name = 'DateType'
        Size = -1
        Value = Null
      end
      item
        Name = 'SurveyKey'
        Size = -1
        Value = Null
      end
      item
        Name = 'SampleType'
        Size = -1
        Value = Null
      end
      item
        Name = 'SessionStart'
        Size = -1
        Value = Null
      end
      item
        Name = 'Lat'
        Size = -1
        Value = Null
      end
      item
        Name = 'Long'
        Size = -1
        Value = Null
      end
      item
        Name = 'LocationName'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT S.SAMPLE_KEY'
      'FROM SAMPLE S'
      'WHERE S.VAGUE_DATE_START = :Start AND S.VAGUE_DATE_END = :End'
      
        'AND S.VAGUE_DATE_TYPE = :DateType AND S.SURVEY_EVENT_KEY = :Surv' +
        'eyKey'
      
        'AND S.SAMPLE_TYPE_KEY = :SampleType AND S.ENTRY_DATE >= :Session' +
        'Start'
      
        'AND S.LAT = :Lat AND S.LONG = :Long AND S.LOCATION_NAME = :Locat' +
        'ionName')
    ParseSQL = True
    Left = 164
    Top = 112
  end
  object qryInsertSampleData: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SiteID'
        Size = -1
        Value = Null
      end
      item
        Name = 'Sample_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Accuracy'
        Size = -1
        Value = Null
      end
      item
        Name = 'Data'
        Size = -1
        Value = Null
      end
      item
        Name = 'Measurement_Qualifier_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Measurement_Unit_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Entered_By'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'declare @NextKey char(16)'
      ''
      'exec spNextKey '#39'SAMPLE_DATA'#39', @NextKey output, :SiteID'
      ''
      'Insert into Sample_Data('
      '    Sample_Data_Key,  Sample_Key, Accuracy,'
      
        '    Data, Measurement_Qualifier_Key, Measurement_Unit_Key, Enter' +
        'ed_By'
      '  )'
      'Values('
      '    @NextKey, :Sample_Key, :Accuracy,'
      
        '    :Data, :Measurement_Qualifier_Key, :Measurement_Unit_Key, :E' +
        'ntered_By'
      ')')
    ParseSQL = True
    Left = 268
    Top = 212
  end
end
