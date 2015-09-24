inherited dmDetermination: TdmDetermination
  OldCreateOrder = True
  Left = 190
  Top = 107
  Height = 260
  Width = 208
  object qryTaxonDet: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT DT.Short_Name AS Det_Type, DR.Short_Name AS Det_Role, T.I' +
        'tem_Name, TD.*,'
      '       R.Full_Reference, DT.Verified'
      'FROM Taxon AS T INNER JOIN'
      '         (Taxon_Version AS TV INNER JOIN'
      '             (Taxon_List_Item AS TLI INNER JOIN'
      '                 (Determiner_Role AS DR INNER JOIN'
      '                     (Determination_Type AS DT INNER JOIN'
      '                         (Taxon_Determination AS TD LEFT JOIN'
      '                             Reference AS R ON'
      '                         TD.Source_Key = R.Source_Key) ON'
      
        '                     DT.Determination_Type_Key = TD.Determinatio' +
        'n_Type_Key) ON'
      
        '                 DR.Determiner_Role_Key = TD.Determiner_Role_Key' +
        ') ON'
      
        '             TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Key) O' +
        'N'
      '         TV.Taxon_Version_Key = TLI.Taxon_Version_Key) ON'
      '     T.Taxon_Key = TV.Taxon_Key'
      'WHERE TD.Taxon_Occurrence_Key = :KeyParameter'
      'ORDER BY T.Item_Name;')
    ParseSQL = True
    Left = 44
    Top = 8
  end
  object qryDetRole: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Determiner_Role'
      'WHERE Hide = 0'
      'OR Determiner_Role_Key = :KeyParameter'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 44
    Top = 64
  end
  object qryDetType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Determination_Type'
      'WHERE Hide = 0'
      'OR Determination_Type_Key = :KeyParameter'
      'ORDER BY Short_Name')
    ParseSQL = True
    Left = 44
    Top = 120
  end
  object dsDetRole: TDataSource
    DataSet = qryDetRole
    Left = 120
    Top = 64
  end
  object dsDetType: TDataSource
    DataSet = qryDetType
    Left = 120
    Top = 120
  end
  object qryBiotopeDet: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT DT.Short_Name AS Det_Type, DR.Short_Name AS Det_Role, '
      
        '       B.Short_Term, B.Original_Code, BD.*, R.Full_Reference, DT' +
        '.Verified'
      'FROM (Determiner_Role AS DR INNER JOIN'
      '             (Determination_Type AS DT INNER JOIN'
      '                 (Biotope AS B INNER JOIN'
      '                     (Biotope_List_Item AS BLI INNER JOIN'
      '                         Biotope_Determination AS BD ON'
      
        '                     BLI.Biotope_List_Item_Key = BD.Biotope_List' +
        '_Item_Key) ON'
      '                 B.Biotope_Key = BLI.Biotope_Key) ON'
      
        '             DT.Determination_Type_Key = BD.Determination_Type_K' +
        'ey) ON'
      
        '         DR.Determiner_Role_Key = BD.Determiner_Role_Key) LEFT J' +
        'OIN'
      '             Reference AS R ON'
      '         BD.Source_Key = R.Source_Key'
      'WHERE BD.Biotope_Occurrence_Key = :KeyParameter'
      'ORDER BY B.SHORT_TERM;')
    ParseSQL = True
    Left = 124
    Top = 8
  end
  object qrySampleDate: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Vague_Date_Start, Vague_Date_End, Vague_Date_Type'
      'FROM Sample'
      'WHERE Sample_Key = :Key')
    ParseSQL = True
    Left = 120
    Top = 176
  end
end
