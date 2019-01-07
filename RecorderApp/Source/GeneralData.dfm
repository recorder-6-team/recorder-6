inherited dmGeneralData: TdmGeneralData
  OldCreateOrder = True
  Left = 589
  Top = 326
  Height = 520
  Width = 428
  object qryIDGenSelect: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'TableName'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT LAST_KEY_TEXT'
      'FROM LAST_KEY'
      'WHERE TABLE_NAME = :TableName')
    ParseSQL = True
    Left = 48
    Top = 16
  end
  object qryIDGenInsert: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'TableName'
        Size = -1
        Value = Null
      end
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'INSERT INTO LAST_KEY (TABLE_NAME, LAST_KEY_TEXT)'
      'VALUES(:TableName, :Key)')
    ParseSQL = True
    Left = 136
    Top = 16
  end
  object qryGridSquares: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM GRID_SQUARE'
      'WHERE LOCATION_KEY = :Key')
    ParseSQL = True
    Left = 288
    Top = 128
  end
  object qrySurvey: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * '
      'FROM Survey'
      'WHERE Survey_Key = :KeyParameter')
    ParseSQL = True
    Left = 48
    Top = 72
  end
  object qryReference: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT R.Year_Vague_Date_Start, R.Year_Vague_Date_End, R.Year_Va' +
        'gue_Date_Type,'
      ' R.Title, RA.Author'
      
        'FROM Reference AS R INNER JOIN VW_Reference_Authors AS RA ON R.S' +
        'ource_Key = RA.Source_Key'
      'WHERE R.Source_Key = :Key')
    ParseSQL = True
    Left = 136
    Top = 128
  end
  object qryEvent: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * FROM Survey_Event AS SE '
      'WHERE SE.Survey_Event_Key = :KeyParameter')
    ParseSQL = True
    Left = 136
    Top = 72
  end
  object qryPreferred: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 216
    Top = 128
  end
  object qryAllPurpose: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 288
    Top = 188
  end
  object qryRecordStrings: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 136
    Top = 188
  end
  object qryUsers: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT'
      #9'CASE'
      #9#9'WHEN I.Forename IS NOT NULL THEN I.Forename + '#39' '#39' + I.Surname'
      #9#9'WHEN I.Initials IS NOT NULL THEN I.Initials+'#39' '#39'+I.Surname'
      #9#9'WHEN I.Title    IS NOT NULL THEN I.Title+'#39' '#39'+I.Surname'
      #9#9'ELSE I.Surname'
      #9'END  AS DISPLAY_NAME, U.*'
      'FROM "USER" AS U, INDIVIDUAL AS I'
      'WHERE I.NAME_KEY = U.NAME_KEY'
      'ORDER BY I.FORENAME, I.SURNAME')
    ParseSQL = True
    Left = 216
    Top = 188
  end
  object qryLatestListVersion: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 136
    Top = 244
  end
  object qryMetadata: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT NAME, DATA FROM SPECIAL_XML_ELEMENT '
      'WHERE TYPE='#39'M'#39' '
      'AND DATA IS NULL')
    ParseSQL = True
    Left = 48
    Top = 188
  end
  object qryTaxonName: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 48
    Top = 244
  end
  object dsUsers: TDataSource
    AutoEdit = False
    DataSet = qryUsers
    Left = 216
    Top = 244
  end
  object qryOrganisation: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'NameKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT      *'
      'FROM        ORGANISATION'
      'WHERE       NAME_KEY        =   :NameKey')
    ParseSQL = True
    Left = 216
    Top = 72
  end
  object qryIndividual: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'NameKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT      *'
      'FROM        INDIVIDUAL'
      'WHERE       NAME_KEY    =   :NameKey')
    ParseSQL = True
    Left = 216
    Top = 16
  end
  object spNextKey: TADOStoredProc
    ProcedureName = 'spNextKey'
    Parameters = <
      item
        Name = '@TableName'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = '@NextKey'
        DataType = ftString
        Direction = pdOutput
        Size = 16
        Value = Null
      end
      item
        Name = '@SiteID'
        DataType = ftString
        Size = 8
        Value = Null
      end>
    Left = 48
    Top = 356
  end
  object spRepairLastKey: TADOStoredProc
    ProcedureName = 'dbo.spRepairLastKey'
    Parameters = <
      item
        Name = '@TableName'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = '@KeyField'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = '@SiteID'
        DataType = ftString
        Size = 8
        Value = Null
      end>
    Left = 136
    Top = 356
  end
  object qryTempLoc: TJNCCQuery
    CommandTimeout = 0
    ParamCheck = False
    Parameters = <>
    SQL.Strings = (
      'SELECT      *'
      'FROM        SETTING'
      'WHERE     NAME    =  '#39'TempLic'#39)
    ParseSQL = False
    Left = 224
    Top = 304
  end
  object qryTempRecorders: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SurveyKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT      COUNT(*) AS RECORDS'
      'FROM        SAMPLE'
      'INNER JOIN SURVEY_EVENT ON'
      'SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY'
      'WHERE  isnull(SAMPLE.RECORDERS,'#39#39') <> '#39#39
      'AND SURVEY_EVENT.SURVEY_KEY  =  :SurveyKey')
    ParseSQL = True
    Left = 128
    Top = 296
  end
end
