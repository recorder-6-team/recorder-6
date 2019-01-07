inherited dmTaxonOccurrences: TdmTaxonOccurrences
  OldCreateOrder = True
  Left = 836
  Top = 181
  Height = 303
  Width = 403
  object qryTaxonOcc: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Taxon_Occurrence '
      'WHERE Taxon_Occurrence_Key = :KeyParameter')
    ParseSQL = True
    Left = 32
    Top = 12
  end
  object dsTaxonOcc: TDataSource
    AutoEdit = False
    DataSet = qryTaxonOcc
    Left = 112
    Top = 12
  end
  object qryRecordType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Record_Type_Key, Short_Name FROM Record_Type')
    ParseSQL = True
    Left = 32
    Top = 60
  end
  object qrySubstrate: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Substrate_Key, Short_Name FROM Substrate')
    ParseSQL = True
    Left = 112
    Top = 60
  end
  object dsRecordType: TDataSource
    AutoEdit = False
    DataSet = qryRecordType
    Left = 32
    Top = 108
  end
  object dsSubstrate: TDataSource
    AutoEdit = False
    DataSet = qrySubstrate
    Left = 112
    Top = 108
  end
  object qryTLIKeyDELTHIS: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'TaxonKey'
        Size = -1
        Value = Null
      end
      item
        Name = 'TaxonListKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TLI.Taxon_List_Item_Key'
      
        'FROM Taxon_List_Version AS TLV, Taxon_List_Item AS TLI, Taxon_Ve' +
        'rsion AS TV'
      'WHERE  TV.Taxon_Key =:TaxonKey'
      'AND  TLI.Taxon_Version_Key = TV.Taxon_Version_Key'
      'AND TLV.Taxon_list_Version_key = TLI.Taxon_list_Version_key'
      'AND TLV.Taxon_List_Key =:TaxonListKey')
    ParseSQL = True
    Left = 112
    Top = 160
  end
  object qryRelationType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Relationship_Type_Key, Short_Name'
      'FROM Relationship_Type')
    ParseSQL = True
    Left = 196
    Top = 60
  end
  object dsRelationType: TDataSource
    AutoEdit = False
    DataSet = qryRelationType
    Left = 196
    Top = 108
  end
  object qryRelOcc: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TOR.*, RT.Short_Name'
      'FROM Taxon_Occurrence_Relation AS TOR INNER JOIN'
      '         Relationship_Type AS RT ON'
      '     TOR.Relationship_Type_Key = RT.Relationship_Type_Key'
      'WHERE TOR.Taxon_Occurrence_Key_1 = :KeyParameter')
    ParseSQL = True
    Left = 196
    Top = 12
  end
  object qrySpecimen: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT S.*, ST.Short_Name'
      'FROM Specimen AS S INNER JOIN'
      '         Specimen_Type AS ST ON'
      '     S.Specimen_Type_Key = ST.Specimen_Type_Key'
      'WHERE Taxon_Occurrence_Key = :KeyParameter')
    ParseSQL = True
    Left = 276
    Top = 12
  end
  object qrySpecType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT Specimen_Type_Key, Short_Name'
      'FROM Specimen_Type')
    ParseSQL = True
    Left = 268
    Top = 60
  end
  object dsSpecType: TDataSource
    AutoEdit = False
    DataSet = qrySpecType
    Left = 276
    Top = 108
  end
  object qryNameFromOcc: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT TAXON_LIST_ITEM_KEY '
      'FROM TAXON_DETERMINATION '
      'WHERE (TAXON_DETERMINATION_KEY = :KeyParameter) '
      'AND (PREFERRED = TRUE) ')
    ParseSQL = True
    Left = 196
    Top = 160
  end
  object qryTaxonPrivateDetail: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT P.*,PT.Short_Name'
      'FROM TAXON_PRIVATE_DATA  P'
      'INNER JOIN TAXON_PRIVATE_TYPE PT'
      'ON PT.TAXON_PRIVATE_TYPE_KEY = P.TAXON_PRIVATE_TYPE_KEY   '
      'WHERE Taxon_Occurrence_Key = :KeyParameter'
      '')
    ParseSQL = True
    Left = 28
    Top = 164
  end
  object dsPrivateType: TDataSource
    AutoEdit = False
    DataSet = qryPrivateType
    Left = 36
    Top = 220
  end
  object qryPrivateType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      
        'SELECT Taxon_Private_Type_Key, Short_Name,dbo.ufn_RtfToPlaintext' +
        '(Description) AS Description '
      'FROM Taxon_Private_Type')
    ParseSQL = True
    Left = 280
    Top = 168
  end
end
