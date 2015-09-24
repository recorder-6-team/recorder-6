inherited dmReferences: TdmReferences
  OldCreateOrder = True
  Left = 413
  Top = 194
  Height = 276
  Width = 312
  object qryReference: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM REFERENCE'
      'WHERE SOURCE_KEY = :Key')
    ParseSQL = True
    Left = 88
    Top = 16
  end
  object qryPopulate: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 24
    Top = 16
  end
  object dsReference: TDataSource
    AutoEdit = False
    DataSet = qryReference
    Left = 88
    Top = 72
  end
  object tblReferenceType: TJNCCTable
    CommandTimeout = 0
    TableName = 'REFERENCE_TYPE'
    Left = 168
    Top = 16
  end
  object dsReferenceType: TDataSource
    AutoEdit = False
    DataSet = tblReferenceType
    Left = 168
    Top = 72
  end
  object qryAuthors: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM Reference_Author'
      'WHERE Source_Key = :Key'
      'ORDER BY SORT_ORDER')
    ParseSQL = True
    Left = 24
    Top = 180
  end
  object dsJournal: TDataSource
    AutoEdit = False
    DataSet = qryJournal
    Left = 244
    Top = 72
  end
  object qryEditors: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM REFERENCE_EDITOR'
      'WHERE SOURCE_KEY = :Key'
      'ORDER BY SORT_ORDER')
    ParseSQL = True
    Left = 24
    Top = 124
  end
  object qryISBNs: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT *'
      'FROM REFERENCE_NUMBER'
      'WHERE SOURCE_KEY = :Key')
    ParseSQL = True
    Left = 88
    Top = 124
  end
  object qrySource: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'INSERT INTO SOURCE (SOURCE_KEY, INTERNAL)'
      'VALUES (:Key, 1)')
    ParseSQL = True
    Left = 24
    Top = 72
  end
  object qryJournal: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'Select *'
      'from Journal'
      'order by Short_Name')
    ParseSQL = True
    Left = 88
    Top = 180
  end
  object qryReferenceWithCustodian: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT      r.*,'
      '            s.CUSTODIAN'
      'FROM        REFERENCE       AS  r'
      
        'INNER JOIN  SOURCE          AS  s   ON  s.SOURCE_KEY    =   r.SO' +
        'URCE_KEY'
      'WHERE       r.SOURCE_KEY    = :Key')
    ParseSQL = True
    Left = 204
    Top = 124
  end
  object qryKeywords: TJNCCQuery
    CursorType = ctStatic
    CommandTimeout = 0
    Parameters = <
      item
        Name = '@Key'
        Attributes = [paNullable]
        DataType = ftString
        NumericScale = 255
        Precision = 255
        Size = 16
        Value = Null
      end>
    SQL.Strings = (
      'EXEC usp_ReferenceKeywords_Get :Key')
    ParseSQL = True
    Left = 204
    Top = 176
    object qryKeywordsReference_Keyword_Key: TStringField
      FieldName = 'Reference_Keyword_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qryKeywordsConcept_Key: TStringField
      FieldName = 'Concept_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qryKeywordsPlaintext: TWideStringField
      DisplayLabel = 'Keyword'
      DisplayWidth = 284
      FieldName = 'Plaintext'
      Size = 150
    end
    object qryKeywordsCustodian: TStringField
      FieldName = 'Custodian'
      Visible = False
      FixedChar = True
      Size = 8
    end
    object qryKeywordsEntered_By: TStringField
      FieldName = 'Entered_By'
      Visible = False
      FixedChar = True
      Size = 16
    end
  end
end
