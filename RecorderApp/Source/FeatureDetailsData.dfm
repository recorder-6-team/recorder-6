inherited dmFeatureDetails: TdmFeatureDetails
  OldCreateOrder = True
  Left = 361
  Top = 107
  Height = 273
  Width = 269
  object qryFeature: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Location_Feature'
      'WHERE Location_Feature_Key = :KeyParameter')
    ParseSQL = True
    Left = 28
    Top = 12
  end
  object dsFeature: TDataSource
    AutoEdit = False
    DataSet = qryFeature
    Left = 108
    Top = 12
  end
  object tblType: TJNCCTable
    CommandTimeout = 0
    TableName = 'LOCATION_FEATURE_TYPE'
    Left = 28
    Top = 68
  end
  object dsType: TDataSource
    AutoEdit = False
    DataSet = tblType
    Left = 108
    Top = 68
  end
  object qryGrading: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM LOCATION_FEATURE_GRADING'
      'WHERE LOCATION_FEATURE_TYPE_KEY = :Key'
      'ORDER BY SHORT_NAME')
    ParseSQL = True
    Left = 28
    Top = 124
  end
  object dsGrading: TDataSource
    AutoEdit = False
    DataSet = qryGrading
    Left = 108
    Top = 124
  end
  object qryTypeLocate: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT LOCATION_FEATURE_TYPE_KEY'
      'FROM LOCATION_FEATURE_GRADING'
      'WHERE FEATURE_GRADING_KEY = :Key')
    ParseSQL = True
    Left = 184
    Top = 12
  end
  object qryManagementAim: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM MANAGEMENT_AIM'
      'WHERE LOCATION_FEATURE_KEY = :Key')
    ParseSQL = True
    Left = 184
    Top = 68
  end
  object tblThreatType: TJNCCTable
    CommandTimeout = 0
    TableName = 'THREAT_TYPE'
    Left = 28
    Top = 180
  end
  object dsThreatType: TDataSource
    AutoEdit = False
    DataSet = tblThreatType
    Left = 108
    Top = 180
  end
  object qryThreats: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM POTENTIAL_THREAT'
      'WHERE LOCATION_FEATURE_KEY = :Key')
    ParseSQL = True
    Left = 184
    Top = 124
  end
  object qryDamages: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM DAMAGE_OCCURRENCE'
      'WHERE LOCATION_FEATURE_KEY = :Key')
    ParseSQL = True
    Left = 184
    Top = 180
  end
end
