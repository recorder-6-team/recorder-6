inherited dmSurvey: TdmSurvey
  OldCreateOrder = True
  Left = 858
  Top = 239
  Height = 392
  Width = 246
  object qrySurveyType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Survey_Type_Key, Short_Name'
      'FROM Survey_Type'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 40
    Top = 124
  end
  object qrySurveyStatus: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Survey_Status_Key, Short_Name'
      'FROM Survey_Status'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 40
    Top = 12
  end
  object qrySurveyMedia: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Survey_Media_Key, Short_Name'
      'FROM Survey_Media'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 40
    Top = 68
  end
  object dsSurveyMedia: TDataSource
    AutoEdit = False
    DataSet = qrySurveyMedia
    Left = 132
    Top = 68
  end
  object dsSurveyStatus: TDataSource
    AutoEdit = False
    DataSet = qrySurveyStatus
    Left = 132
    Top = 12
  end
  object dsSurveyType: TDataSource
    AutoEdit = False
    DataSet = qrySurveyType
    Left = 132
    Top = 124
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
    Left = 40
    Top = 180
  end
  object dsSurvey: TDataSource
    AutoEdit = False
    DataSet = qrySurvey
    Left = 132
    Top = 180
  end
  object qrySurveyTags: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = '@Key'
        Attributes = [paNullable]
        DataType = ftString
        NumericScale = 255
        Precision = 255
        Size = 16
        Value = Null
      end>
    SQL.Strings = (
      'EXEC usp_SurveyTags_Select_ForSurvey :Key')
    ParseSQL = True
    Left = 40
    Top = 236
    object qrySurveyTagsSurvey_Tag_Key: TStringField
      FieldName = 'Survey_Tag_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySurveyTagsConcept_Key: TStringField
      FieldName = 'Concept_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySurveyTagsPreferred_Concept_Key: TStringField
      FieldName = 'Preferred_Concept_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySurveyTagsPlaintext: TWideStringField
      DisplayLabel = 'Tag'
      DisplayWidth = 325
      FieldName = 'Plaintext'
      Size = 150
    end
    object qrySurveyTagsEntered_By: TStringField
      FieldName = 'Entered_By'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySurveyTagsCustodian: TStringField
      FieldName = 'Custodian'
      Visible = False
      FixedChar = True
      Size = 8
    end
  end
  object qrySurveyLicence: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Licence_Key,  Long_Name'
      'FROM Licences'
      'ORDER BY Long_Name')
    ParseSQL = True
    Left = 40
    Top = 296
  end
  object dsSurveyLicence: TDataSource
    AutoEdit = False
    DataSet = qrySurveyLicence
    Left = 136
    Top = 240
  end
end
