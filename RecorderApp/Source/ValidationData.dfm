object dmValidation: TdmValidation
  OldCreateOrder = True
  Left = 791
  Top = 235
  Height = 270
  Width = 180
  object tblEntity: TJNCCTable
    CommandTimeout = 0
    Left = 32
    Top = 16
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
      'Select * From Survey Where Survey_Key=:KeyParameter')
    ParseSQL = True
    Left = 32
    Top = 68
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
      'Select * From Survey_Event Where Survey_Event_Key=:KeyParameter')
    ParseSQL = True
    Left = 32
    Top = 120
  end
  object qryLocation: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Location'
      'WHERE Location_Key = :KeyParameter')
    ParseSQL = True
    Left = 104
    Top = 68
  end
  object qrySample: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'Select * From Sample Where Sample_Key=:KeyParameter')
    ParseSQL = True
    Left = 104
    Top = 120
  end
  object qryDetermination: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 104
    Top = 16
  end
  object qryGridSquares: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * FROM GRID_SQUARE WHERE LOCATION_KEY = :KeyParameter')
    ParseSQL = True
    Left = 32
    Top = 176
  end
end
