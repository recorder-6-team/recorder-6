inherited dmName: TdmName
  OldCreateOrder = True
  Left = 216
  Top = 112
  Height = 282
  Width = 360
  object qryIndividual: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM INDIVIDUAL'
      'WHERE NAME_KEY = :Key')
    ParseSQL = True
    Left = 200
    Top = 8
  end
  object qryPopulate: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 32
    Top = 8
  end
  object qryOrganisation: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM ORGANISATION'
      'WHERE NAME_KEY = :Key')
    ParseSQL = True
    Left = 280
    Top = 8
  end
  object dsIndividual: TDataSource
    AutoEdit = False
    DataSet = qryIndividual
    Left = 200
    Top = 64
  end
  object dsOrganisation: TDataSource
    AutoEdit = False
    DataSet = qryOrganisation
    Left = 280
    Top = 64
  end
  object qryPrefAddress: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM ADDRESS'
      'WHERE NAME_KEY = :Key'
      'AND PREFERRED = TRUE')
    ParseSQL = True
    Left = 32
    Top = 124
  end
  object qryPrefContact: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM CONTACT_NUMBER'
      'WHERE NAME_KEY = :Key'
      'AND PREFERRED = TRUE')
    ParseSQL = True
    Left = 116
    Top = 124
  end
  object dsOrganisationType: TDataSource
    AutoEdit = False
    DataSet = qryOrganisationType
    Left = 116
    Top = 64
  end
  object qryAddresses: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM ADDRESS'
      'WHERE NAME_KEY = :Key')
    ParseSQL = True
    Left = 200
    Top = 124
  end
  object qryContacts: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM CONTACT_NUMBER'
      'WHERE NAME_KEY = :Key')
    ParseSQL = True
    Left = 32
    Top = 184
  end
  object qryComms: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM COMMUNICATION'
      'WHERE NAME_KEY_1 = :Key')
    ParseSQL = True
    Left = 116
    Top = 184
  end
  object qryAssocs: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM NAME_RELATION'
      'WHERE NAME_KEY_1 = :Key')
    ParseSQL = True
    Left = 200
    Top = 184
  end
  object qryAddDelete: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 32
    Top = 64
  end
  object qryOrganisationType: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'Select Organisation_Type_Key, Short_Name '
      'from Organisation_Type'
      'Order by Short_Name')
    ParseSQL = True
    Left = 112
    Top = 8
  end
  object qryDepartments: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 280
    Top = 124
  end
end
