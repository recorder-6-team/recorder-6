inherited dmLocation: TdmLocation
  OldCreateOrder = True
  Left = 707
  Top = 112
  Height = 215
  Width = 178
  object qryFindParentLoc: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Parent_Key FROM Location WHERE Location_Key = :Key')
    ParseSQL = True
    Left = 60
    Top = 72
  end
  object qryAddBoundaryLink: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Location_Boundary_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Location_Key'
        Size = -1
        Value = Null
      end
      item
        Name = 'Date_Start'
        Size = -1
        Value = Null
      end
      item
        Name = 'Date_End'
        Size = -1
        Value = Null
      end
      item
        Name = 'Date_Type'
        Size = -1
        Value = Null
      end
      item
        Name = 'Version'
        Size = -1
        Value = Null
      end
      item
        Name = 'map_file'
        Size = -1
        Value = Null
      end
      item
        Name = 'object_id'
        Size = -1
        Value = Null
      end
      item
        Name = 'entered_by'
        Size = -1
        Value = Null
      end
      item
        Name = 'entry_date'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'insert into LOCATION_BOUNDARY'
      
        'values (:Location_Boundary_Key, :Location_Key, :Date_Start, :Dat' +
        'e_End,'
      ':Date_Type, null, null, null, :Version, :map_file, :object_id, '
      ':entered_by, :entry_date, null, null, 0)')
    ParseSQL = True
    Left = 60
    Top = 128
  end
  object cmdFetch: TADOCommand
    Parameters = <>
    Left = 56
    Top = 24
  end
end
