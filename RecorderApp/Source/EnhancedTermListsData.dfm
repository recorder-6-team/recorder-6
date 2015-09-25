inherited dmEnhancedTermLists: TdmEnhancedTermLists
  OldCreateOrder = True
  Height = 123
  Width = 202
  object qryLanguages: TJNCCQuery
    CursorType = ctStatic
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'EXEC usp_Languages_Select')
    ParseSQL = True
    Left = 120
    Top = 16
    object qryLanguagesLanguage_Key: TStringField
      FieldName = 'Language_Key'
      ReadOnly = True
      Size = 4
    end
    object qryLanguagesItem_Name: TStringField
      FieldName = 'Item_Name'
      Size = 50
    end
  end
  object qrySynonyms: TJNCCQuery
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
        Value = 'DSS0039400000Z1I'
      end>
    SQL.Strings = (
      'EXEC usp_ListSynonyms_Select_ForConcept :ConceptKey')
    ParseSQL = True
    Left = 36
    Top = 16
    object qrySynonymsIndicator: TBooleanField
      DisplayLabel = ' '
      DisplayWidth = 11
      FieldKind = fkCalculated
      FieldName = 'Indicator'
      ReadOnly = True
      Calculated = True
    end
    object qrySynonymsItem_Key: TStringField
      DisplayLabel = 'Synonym'
      FieldName = 'Item_Key'
      Visible = False
      FixedChar = True
      Size = 16
    end
    object qrySynonymsItem_Name: TWideStringField
      DisplayLabel = 'Synonym'
      FieldName = 'Item_Name'
      Required = True
      Size = 251
    end
    object qrySynonymsLanguage_Key: TStringField
      FieldName = 'Language_Key'
      Visible = False
      Size = 4
    end
    object qrySynonymsLanguage: TStringField
      DisplayWidth = 100
      FieldName = 'Language'
      LookupDataSet = qryLanguages
      LookupKeyFields = 'Language_Key'
      LookupResultField = 'Item_Name'
      Required = True
      Size = 50
    end
    object qrySynonymsCustodian: TStringField
      FieldName = 'Custodian'
      Visible = False
      FixedChar = True
      Size = 8
    end
    object qrySynonymsEntered_By: TStringField
      FieldName = 'Entered_By'
      Visible = False
      FixedChar = True
      Size = 16
    end
  end
end
