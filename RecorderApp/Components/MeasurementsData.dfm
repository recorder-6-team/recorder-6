object dmMeasurements: TdmMeasurements
  OldCreateOrder = False
  Left = 526
  Top = 215
  Height = 247
  Width = 296
  object qryMeasure: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT DT.[Data_Key] AS Data_Key, '
      
        '     DT.Custodian, DT.Data, DT.Accuracy, DT.Changed_By, DT.Chang' +
        'ed_Date,'
      
        '     DT.Measurement_Qualifier_Key, MQ.Short_Name AS Qualifier_Sh' +
        'ort_Name,'
      '     DT.Measurement_Unit_Key,  MU.Short_Name AS Unit_Short_Name,'
      
        '     MT.Measurement_Type_Key, MT.Short_Name AS Type_Short_Name, ' +
        'DT.Entered_By'
      'FROM ((Measurement_Type AS MT'
      '  INNER JOIN Measurement_Qualifier AS MQ'
      '  ON MT.Measurement_Type_Key = MQ.Measurement_Type_Key)'
      '  INNER JOIN Measurement_Unit AS MU'
      '  ON MT.Measurement_Type_Key = MU.Measurement_Type_Key)'
      '  INNER JOIN [DataTable] AS DT'
      '  ON (MU.Measurement_Unit_Key = DT.Measurement_Unit_Key)'
      
        '     AND (MQ.Measurement_Qualifier_Key = DT.Measurement_Qualifie' +
        'r_Key)'
      'WHERE DT.[Master_Key]'
      '= :KeyParameter'
      
        'ORDER BY MT.Short_Name, MQ.Short_Name, DT.Data, MU.Short_Name, D' +
        'T.Accuracy')
    ParseSQL = True
    Left = 24
    Top = 12
  end
  object qryMeasureType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'DataTableName'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT MT.*'
      'FROM ((Measurement_Type AS MT'
      
        'LEFT JOIN Measurement_Type_Context AS MTC ON MT.Measurement_Type' +
        '_Key = MTC.Measurement_Type_Key)'
      
        'LEFT JOIN Measurement_Context AS MC ON MC.Measurement_Context_Ke' +
        'y = MTC.Measurement_Context_Key)'
      'WHERE MC.Data_Table = :DataTableName or MC.Data_Table is null'
      'ORDER BY MT.Short_Name')
    ParseSQL = True
    Left = 108
    Top = 12
  end
  object qryMeasureUnit: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * '
      'FROM Measurement_Unit'
      'WHERE Measurement_Type_Key = :KeyParameter'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 108
    Top = 60
  end
  object dsMeasureType: TDataSource
    AutoEdit = False
    DataSet = qryMeasureType
    Left = 212
    Top = 12
  end
  object dsMeasureUnit: TDataSource
    AutoEdit = False
    DataSet = qryMeasureUnit
    Left = 212
    Top = 60
  end
  object qryMeasureQualifier: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * '
      'FROM Measurement_Qualifier'
      'WHERE Measurement_Type_Key = :KeyParameter'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 108
    Top = 108
  end
  object dsMeasureQualifier: TDataSource
    AutoEdit = False
    DataSet = qryMeasureQualifier
    Left = 212
    Top = 108
  end
  object qryUpdate: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 24
    Top = 60
  end
  object qryMeasureData: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'EXEC usp_MeasurementUnitValue_Select_ForMeasurementUnit :KeyPara' +
        'meter')
    ParseSQL = True
    Left = 108
    Top = 156
  end
  object dsMeasureData: TDataSource
    AutoEdit = False
    DataSet = qryMeasureData
    Left = 212
    Top = 156
  end
end
