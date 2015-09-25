inherited dmBiotopeOccurrences: TdmBiotopeOccurrences
  OldCreateOrder = True
  Left = 277
  Top = 110
  Width = 279
  object qryBiotopeOcc: TJNCCQuery
    Parameters = <
      item
        Name = 'KeyParameter'
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Biotope_Occurrence'
      'WHERE Biotope_Occurrence_Key = :KeyParameter')
    Left = 32
    Top = 12
    ParamData = <
      item
        Name = 'KeyParameter'
        Size = 0
        Value = Null
        ParamType = ptInput
      end>
  end
  object dsBiotopeOcc: TDataSource
    AutoEdit = False
    DataSet = qryBiotopeOcc
    Left = 120
    Top = 12
  end
  object qryBLIKey: TJNCCQuery
    Parameters = <
      item
        Name = 'KeyParameter'
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Biotope_List_Item_Key'
      'FROM   Biotope_List_Item '
      'WHERE  Biotope_Key = :KeyParameter'
      'AND BT_CL_Version_To IS NULL')
    Left = 196
    Top = 12
    ParamData = <
      item
        Name = 'KeyParameter'
        Size = 0
        Value = Null
        ParamType = ptInput
      end>
  end
  object qryClassificationFromBLI: TJNCCQuery
    Parameters = <
      item
        Name = 'KeyParameter'
        Value = Null
      end>
    SQL.Strings = (
      'SELECT BC.Short_Name'
      'FROM Biotope_Classification AS BC INNER JOIN '
      '         (Biotope_Classification_Version AS BCV INNER JOIN '
      '             Biotope_List_Item AS BLI ON '
      '         BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key) ON '
      
        '     BC.Biotope_Classification_Key = BCV.Biotope_Classification_' +
        'Key'
      'WHERE BLI.Biotope_List_Item_Key = :KeyParameter'
      '')
    Left = 68
    Top = 64
    ParamData = <
      item
        Name = 'KeyParameter'
        Size = 0
        Value = Null
        ParamType = ptInput
      end>
  end
end
