object dmWizard: TdmWizard
  OldCreateOrder = True
  Left = 480
  Top = 124
  Height = 336
  Width = 320
  object qrySurveyTypes: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Survey_Type_Key AS KeyField, Short_Name AS DisplayField'
      'FROM Survey_Type')
    ParseSQL = True
    Left = 24
    Top = 8
  end
  object qrySurveys: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SelectedListKey'
        Size = -1
        Value = Null
      end
      item
        Name = 'UserID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT S.Survey_Key AS ListKey, S.Item_Name AS DisplayField'
      'FROM Survey S'
      'LEFT JOIN User_Survey_Restriction USR'
      '             ON USR.Survey_Key = S.Survey_Key'
      '           AND USR.Name_Key = :UserID'
      'WHERE S.Survey_Type_Key = :SelectedListKey'
      '      AND USR.Name_Key IS NULL')
    ParseSQL = True
    Left = 108
    Top = 8
  end
  object qrySearch: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SourceParam'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Location_Name_Key, Item_Name'
      'FROM Location_Name'
      
        'WHERE Location_Key IN (SELECT Location_Key FROM Location_Name WH' +
        'ERE Item_Name LIKE :SourceParam)'
      ' AND Preferred = 1'
      'ORDER BY Item_Name')
    ParseSQL = True
    Left = 24
    Top = 60
  end
  object tblInfoFinder: TJNCCTable
    CommandTimeout = 0
    Left = 192
    Top = 8
  end
  object qryFieldFinder: TJNCCQuery
    CommandTimeout = 0
    ParamCheck = False
    Parameters = <>
    ParseSQL = True
    Left = 192
    Top = 112
  end
  object qryUsableTables: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT * '
      'FROM Usable_Table'
      'WHERE Apply_To IN '
      '('#39'A'#39','#39'T'#39','#39'B'#39')')
    ParseSQL = True
    Left = 108
    Top = 112
  end
  object qryOccSamples: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT S.Sample_Key'
      'FROM Sample S, Taxon_Occurrence XO'
      'WHERE XO.Taxon_Occurrence_Key in ()'
      'AND S.Sample_Key = XO.Sample_Key')
    ParseSQL = True
    Left = 192
    Top = 164
  end
  object qrySampleRec: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT IIF(ISNULL(I.Forename),IIF(ISNULL(I.Initials),I.Surname,I' +
        '.Initials+'#39' '#39'+I.Surname),I.Forename+'#39' '#39'+I.Surname) AS [Recorder ' +
        'Name]'
      'FROM Individual I, Sample_Recorder SR, Survey_Event_Recorder SER'
      'WHERE I.Name_Key = SER.Name_Key'
      'AND SER.SE_Recorder_Key = SR.SE_Recorder_Key'
      'AND SR.Sample_Key = :Key')
    ParseSQL = True
    Left = 108
    Top = 164
  end
  object qryResults: TJNCCQuery
    Connection = connReport
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'select * from #Report_Output')
    ParseSQL = False
    ParamCheck = False
    Left = 108
    Top = 60
  end
  object qryMapSheets: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT * FROM Map_Sheet WHERE Sheet_Type = 3')
    ParseSQL = True
    Left = 20
    Top = 116
  end
  object qryMapObjects: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 108
    Top = 216
  end
  object dsResults: TDataSource
    DataSet = qryResults
    Left = 192
    Top = 60
  end
  object connReport: TADOConnection
    LoginPrompt = False
    Left = 20
    Top = 168
  end
  object qryTaxonDesignationSets: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      
        'SELECT Taxon_Designation_Set_Key AS KeyField, Title AS DisplayFi' +
        'eld'
      'FROM Taxon_Designation_Set')
    ParseSQL = True
    Left = 52
    Top = 244
  end
  object qryTaxonDesignationSetItems: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SelectedSetKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TDSI.Taxon_Designation_Type_Key AS ListKey, '
      '              TDT.Short_Name AS DisplayField'
      'FROM Taxon_Designation_Set_Item TDSI'
      'JOIN   Taxon_Designation_Type TDT'
      
        'ON TDT.Taxon_Designation_Type_Key = TDSI.Taxon_Designation_Type_' +
        'Key'
      'WHERE TDSI.Taxon_Designation_Set_Key = :SelectedSetKey')
    ParseSQL = True
    Left = 192
    Top = 244
  end
end
