inherited dmEvent: TdmEvent
  Left = 669
  Top = 130
  Height = 373
  Width = 232
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
    Left = 36
    Top = 16
  end
  object dsEvent: TDataSource
    AutoEdit = False
    DataSet = qryEvent
    Left = 120
    Top = 16
  end
  object qryRecorder: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Survey_Event_Recorder '
      'WHERE Survey_Event_Key = :KeyParameter')
    ParseSQL = True
    Left = 36
    Top = 120
  end
  object qryLocationName: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * '
      'FROM Location_Name'
      'WHERE Location_Key = :KeyParameter'
      'AND Preferred = True')
    ParseSQL = True
    Left = 36
    Top = 68
  end
  object tblRole: TJNCCTable
    CommandTimeout = 0
    TableName = 'RECORDER_ROLE'
    Left = 120
    Top = 68
  end
  object qrySurveyDates: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT '
      'S.FROM_VAGUE_DATE_START, S.FROM_VAGUE_DATE_END, '
      'S.FROM_VAGUE_DATE_TYPE, S.TO_VAGUE_DATE_START, '
      'S.TO_VAGUE_DATE_END, S.TO_VAGUE_DATE_TYPE'
      'FROM SURVEY AS S'
      'WHERE S.SURVEY_KEY = :Key;')
    ParseSQL = True
    Left = 120
    Top = 120
  end
  object qryOwnership: TJNCCQuery
    CursorType = ctStatic
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 36
    Top = 180
    object qryOwnershipDisplayName: TStringField
      DisplayLabel = 'Name'
      FieldName = 'DisplayName'
      Required = True
      Size = 100
    end
    object qryOwnershipSurvey_Event_Owner_Type_Key: TStringField
      DisplayLabel = 'Type'
      FieldName = 'Survey_Event_Owner_Type_Key'
      LookupDataSet = qryOwnershipTypes
      LookupKeyFields = 'Survey_Event_Owner_Type_Key'
      LookupResultField = 'Short_Name'
      Required = True
      Visible = False
      Size = 16
    end
    object qryOwnershipSurvey_Event_Owner_Key: TStringField
      FieldName = 'Survey_Event_Owner_Key'
      Visible = False
      Size = 16
    end
    object qryOwnershipName_Key: TStringField
      DisplayLabel = 'Name'
      FieldName = 'Name_Key'
      Required = True
      Visible = False
      Size = 16
    end
    object qryOwnershipCustodian: TStringField
      FieldName = 'Custodian'
      Visible = False
      Size = 8
    end
    object qryOwnershipShort_Name: TStringField
      DisplayLabel = 'Type'
      FieldName = 'Short_Name'
      LookupDataSet = qryOwnershipTypes
      LookupKeyFields = 'Survey_Event_Owner_Type_Key'
      LookupResultField = 'Short_Name'
      Required = True
    end
    object qryOwnershipEntered_By: TStringField
      FieldName = 'Entered_By'
      Visible = False
      FixedChar = True
      Size = 16
    end
  end
  object qryOwnershipTypes: TJNCCNamesQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'EXEC usp_SurveyEventOwnerTypes_Select')
    ParseSQL = True
    DisplayCommonNames = False
    Left = 120
    Top = 180
  end
  object qrySampleRecorder: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT Sample_Recorder.SE_Recorder_Key,Survey_Event_Recorder.Nam' +
        'e_Key  '
      'FROM Sample_Recorder'
      'INNER JOIN Survey_Event_Recorder  ON '
      'Sample_Recorder.SE_Recorder_Key ='
      'Survey_Event_Recorder.SE_Recorder_Key'
      'WHERE Survey_Event_Recorder.Survey_Event_Key = :KeyParameter')
    ParseSQL = True
    Left = 36
    Top = 248
  end
  object qryRole: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * FROM Recorder_Role Order By Long_Name ')
    ParseSQL = True
    Left = 132
    Top = 256
  end
end
