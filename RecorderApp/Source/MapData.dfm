inherited dmMap: TdmMap
  OldCreateOrder = True
  Left = 419
  Top = 231
  Height = 277
  Width = 366
  object qryMapSheets: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 248
    Top = 16
  end
  object qryObjectDetails: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT'
      '  B.OBJECT_ID,'
      '  L.LOCATION_KEY,'
      '  N.PREFERRED,'
      '  N.ITEM_NAME'
      'FROM (LOCATION As L INNER JOIN LOCATION_BOUNDARY As B'
      'ON L.LOCATION_KEY = B.LOCATION_KEY)'
      'INNER JOIN LOCATION_NAME As N ON L.LOCATION_KEY = N.LOCATION_KEY'
      'Where'
      '  N.PREFERRED = true')
    ParseSQL = True
    Left = 152
    Top = 16
  end
  object qryLocationName: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'ObjectSheetName'
        Size = -1
        Value = Null
      end
      item
        Name = 'ObjectID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'Select'
      '  N.ITEM_NAME,'
      '  N.PREFERRED,'
      '  L.LOCATION_KEY,'
      '  B.OBJECT_ID,'
      '  B.MAP_FILE'
      'From'
      '(LOCATION As L INNER JOIN LOCATION_BOUNDARY As B'
      'ON L.LOCATION_KEY = B.LOCATION_KEY)'
      'INNER JOIN LOCATION_NAME As N'
      'ON L.LOCATION_KEY = N.LOCATION_KEY'
      'Where'
      '  (N.PREFERRED=-1'
      '  AND B.MAP_FILE=:ObjectSheetName'
      '  AND B.OBJECT_ID=:ObjectID);'
      ' ')
    ParseSQL = True
    Left = 152
    Top = 72
  end
  object qryLocationBoundary: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'New_Map_File'
        Size = -1
        Value = Null
      end
      item
        Name = 'New_ID'
        Size = -1
        Value = Null
      end
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Map_File'
        Size = -1
        Value = Null
      end
      item
        Name = 'Original_ID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'UPDATE LOCATION_BOUNDARY'
      'SET MAP_FILE=:New_Map_File,'
      '        OBJECT_ID=:New_ID'
      'WHERE LOCATION_KEY=:Key'
      'and MAP_FILE=:Map_File'
      'and OBJECT_ID=:Original_ID')
    ParseSQL = True
    Left = 52
    Top = 16
  end
  object qrySampleType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT * FROM SAMPLE_TYPE')
    ParseSQL = True
    Left = 252
    Top = 72
  end
  object qryTaxonKeys: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'ListItemKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TLI1.Taxon_List_Item_Key'
      'FROM (Taxon_List_Item AS TLI1'
      'INNER JOIN Taxon_List_Item AS TLI2'
      'ON TLI1.Preferred_Name = TLI2.Preferred_Name)'
      'INNER JOIN Taxon_List_Item AS TLI3'
      'ON TLI3.Taxon_Version_Key = TLI2.Taxon_Version_Key'
      'WHERE TLI3.Taxon_List_Item_Key = :ListItemKey'
      ' ')
    ParseSQL = True
    Left = 252
    Top = 132
  end
  object qryLocName: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'iLocKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'Select ITEM_NAME '
      'From LOCATION_NAME'
      'Where PREFERRED =True and LOCATION_KEY =:iLocKey')
    ParseSQL = True
    Left = 52
    Top = 76
  end
  object qryDeleteBoundary: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Map_Sheet_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Object_ID'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'delete from '
      'LOCATION_BOUNDARY'
      'where MAP_SHEET_KEY=:Map_Sheet_Key '
      'and OBJECT_ID=:Object_ID')
    ParseSQL = True
    Left = 52
    Top = 136
  end
  object spAdminAreaGetRootType: TADOStoredProc
    ExecuteOptions = [eoExecuteNoRecords]
    ProcedureName = 'spAdminAreaGetRootType'
    Parameters = <
      item
        Name = 'AdminAreaKey'
        DataType = ftString
        Size = 16
        Value = Null
      end
      item
        Name = 'RootTypeKey'
        DataType = ftString
        Direction = pdOutput
        Size = 16
        Value = Null
      end>
    Left = 152
    Top = 180
  end
end
