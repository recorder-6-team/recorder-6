inherited dmSample: TdmSample
  Left = 645
  Top = 189
  Height = 203
  Width = 373
  object qrySample: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Sample'
      'WHERE Sample_Key = :KeyParameter')
    ParseSQL = True
    Left = 36
    Top = 8
  end
  object dsSample: TDataSource
    AutoEdit = False
    DataSet = qrySample
    Left = 124
    Top = 8
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
    Left = 212
    Top = 8
  end
  object qryRelatedSample: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT SR.Sample_Relation_Key, SR.Sample_Key_2, S.Sample_Referen' +
        'ce,'
      '       S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type,'
      '       ST.Sample_Type_Key, ST.Short_Name, SR.Custodian'
      'FROM Sample_Type AS ST INNER JOIN'
      '       (Sample AS S INNER JOIN Sample_Relation AS SR ON'
      '         S.Sample_Key = SR.Sample_Key_2) ON'
      '       ST.Sample_Type_Key = S.Sample_Type_Key'
      'WHERE SR.Sample_Key_1 = :KeyParameter'
      ' ')
    ParseSQL = True
    Left = 212
    Top = 60
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
      'SELECT I.Forename ,I.Surname, I.Initials, R.Short_Name,'
      '       S.Sample_Key, SER.SE_Recorder_Key'
      'FROM ((Survey_Event_Recorder AS SER LEFT JOIN'
      '         SampleRecorders AS S ON'
      '     SER.SE_Recorder_Key = S.SE_Recorder_Key) INNER JOIN'
      '         Individual AS I ON'
      '     SER.Name_Key = I.Name_Key) INNER JOIN'
      '         Recorder_Role AS R ON'
      '     SER.Recorder_Role_Key = R.Recorder_Role_Key'
      'WHERE SER.Survey_Event_Key = :KeyParameter'
      '')
    ParseSQL = True
    Left = 36
    Top = 112
  end
  object qrySampleType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Sample_Type_Key, Short_Name, Image'
      'FROM Sample_Type'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 36
    Top = 60
  end
  object dsSampleType: TDataSource
    AutoEdit = False
    DataSet = qrySampleType
    Left = 124
    Top = 60
  end
  object qryRelSampProps: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT S.Sample_Reference, S.Vague_Date_Start, S.Vague_Date_End,' +
        ' S.Vague_Date_Type, ST.Sample_Type_Key, ST.Short_Name '
      'FROM Sample_Type AS ST, Sample AS S '
      'WHERE ST.Sample_Type_Key = S.Sample_Type_Key'
      'AND S.Sample_Key = :KeyParameter')
    ParseSQL = True
    Left = 212
    Top = 112
  end
  object qryEvent: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * FROM Survey_Event'
      'WHERE Survey_Event_Key = :Key')
    ParseSQL = True
    Left = 124
    Top = 112
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
      'SELECT *'
      'FROM SAMPLE_ADMIN_AREAS'
      'WHERE SAMPLE_KEY = :Key')
    ParseSQL = True
    Left = 300
    Top = 8
  end
end
