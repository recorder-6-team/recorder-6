inherited dmLocationDetails: TdmLocationDetails
  OldCreateOrder = True
  Left = 474
  Top = 142
  Height = 276
  Width = 462
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
    Left = 28
    Top = 16
  end
  object dsLocation: TDataSource
    AutoEdit = False
    DataSet = qryLocation
    Left = 112
    Top = 16
  end
  object dsLocationType: TDataSource
    AutoEdit = False
    DataSet = qryLocationType
    Left = 112
    Top = 68
  end
  object qryLocNames: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Location_Name'
      'WHERE Location_Key = :KeyParameter'
      'ORDER BY Item_Name')
    ParseSQL = True
    Left = 204
    Top = 16
  end
  object qryDesignation: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT S.Short_Name, LD.*'
      'FROM Location_Designation AS LD INNER JOIN '
      
        '          Site_Status AS S ON LD.Site_Status_Key = S.Site_Status' +
        '_Key'
      'WHERE LD.Location_Key = :KeyParameter'
      '')
    ParseSQL = True
    Left = 204
    Top = 72
  end
  object qrySiteStatus: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT *'
      'FROM Site_Status'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 28
    Top = 120
  end
  object dsSiteStatus: TDataSource
    AutoEdit = False
    DataSet = qrySiteStatus
    Left = 112
    Top = 120
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
      'FROM LOCATION_ADMIN_AREAS'
      'WHERE LOCATION_KEY = :Key')
    ParseSQL = True
    Left = 204
    Top = 124
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
    Left = 280
    Top = 16
  end
  object qryLandParcels: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM LAND_PARCEL'
      'WHERE LOCATION_KEY = :Key')
    ParseSQL = True
    Left = 204
    Top = 176
  end
  object qryBoundaries: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM LOCATION_BOUNDARY'
      'WHERE LOCATION_KEY = :Key')
    ParseSQL = True
    Left = 280
    Top = 72
  end
  object qryRelations: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM LOCATION_RELATION'
      'WHERE LOCATION_KEY_1 = :Key')
    ParseSQL = True
    Left = 372
    Top = 16
  end
  object qryUses: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM LOCATION_USE'
      'WHERE LOCATION_KEY = :Key')
    ParseSQL = True
    Left = 280
    Top = 124
  end
  object tblTenureType: TJNCCTable
    CommandTimeout = 0
    TableName = 'TENURE_TYPE'
    Left = 28
    Top = 176
  end
  object dsTenureType: TDataSource
    AutoEdit = False
    DataSet = tblTenureType
    Left = 112
    Top = 176
  end
  object qryTenures: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM TENURE'
      'WHERE LOCATION_KEY = :Key')
    ParseSQL = True
    Left = 280
    Top = 176
  end
  object qryCheckForeignKey: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT COUNT(*) FROM'
      'TableName'
      'WHERE'
      'KeyField'
      '= :Key')
    ParseSQL = True
    Left = 372
    Top = 72
  end
  object qryLocationType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'select * '
      'from Location_Type '
      'order By SHORT_NAME')
    ParseSQL = True
    Left = 360
    Top = 144
  end
end
