inherited dmSchemeManager: TdmSchemeManager
  OldCreateOrder = True
  Height = 257
  Width = 233
  object qrySchemes: TJNCCQuery
    Parameters = <>
    SQL.Strings = (
      
        'SELECT RS.RECORDING_SCHEME_KEY, RS.ITEM_NAME AS SCHEME_NAME, RS.' +
        'EMAIL,'
      '    EF.ITEM_NAME AS FILTER_NAME,'
      '    EF.EXPORT_FILTER_KEY, '
      '    RS.EXPORT_FORMAT_ID, '
      '    EXPORT_FORMAT_LK.ITEM_NAME AS FORMAT_NAME, '
      '    RS.LAST_CONTRIBUTION_DATE'
      'FROM (RECORDING_SCHEME AS RS LEFT JOIN EXPORT_FILTER EF '
      '      ON EF.EXPORT_FILTER_KEY = RS.EXPORT_FILTER_KEY)'
      
        '     LEFT JOIN EXPORT_FORMAT_LK ON EXPORT_FORMAT_LK.EXPORT_FORMA' +
        'T_ID = RS.EXPORT_FORMAT_ID'
      'ORDER BY RS.ITEM_NAME;')
    Left = 40
    Top = 36
    ParamData = <>
    object qrySchemesIndicator: TBooleanField
      DisplayLabel = ' '
      DisplayWidth = 11
      FieldKind = fkCalculated
      FieldName = 'Indicator'
      Calculated = True
    end
    object qrySchemesRECORDING_SCHEME_KEY: TStringField
      FieldName = 'RECORDING_SCHEME_KEY'
      Visible = False
      Size = 16
    end
    object qrySchemesSCHEME_NAME: TStringField
      DisplayLabel = 'Recording Scheme'
      DisplayWidth = 120
      FieldName = 'SCHEME_NAME'
      Required = True
      Size = 30
    end
    object qrySchemesEMAIL: TStringField
      DisplayLabel = 'EMail'
      DisplayWidth = 130
      FieldName = 'EMAIL'
      Required = True
      Size = 50
    end
    object qrySchemesSITEM_NAME: TStringField
      DisplayLabel = 'Filter Name'
      DisplayWidth = 120
      FieldName = 'FILTER_NAME'
      LookupDataSet = qryExportFilterLookup
      LookupKeyFields = 'EXPORT_FILTER_KEY'
      LookupResultField = 'FILTER_NAME'
      Size = 40
    end
    object qrySchemesEXPORT_FILTER_KEY: TStringField
      FieldName = 'EXPORT_FILTER_KEY'
      Visible = False
      Size = 16
    end
    object qrySchemesFORMAT_NAME: TStringField
      DisplayLabel = 'Export Format'
      DisplayWidth = 140
      FieldName = 'FORMAT_NAME'
      LookupDataSet = qryExportFormatLookup
      LookupKeyFields = 'EXPORT_FORMAT_ID'
      LookupResultField = 'ITEM_NAME'
      Size = 30
    end
    object qrySchemesEXPORT_FORMAT_ID: TIntegerField
      FieldName = 'EXPORT_FORMAT_ID'
      Visible = False
    end
    object qrySchemesLAST_CONTRIBUTION_DATE: TDateTimeField
      DisplayLabel = 'Last Contribution Date'
      DisplayWidth = 140
      FieldName = 'LAST_CONTRIBUTION_DATE'
      ReadOnly = True
      Required = True
    end
  end
  object dsSchemes: TDataSource
    DataSet = qrySchemes
    Left = 144
    Top = 36
  end
  object qryExportFilterLookup: TJNCCQuery
    Parameters = <>
    SQL.Strings = (
      'SELECT EXPORT_FILTER_KEY, ITEM_NAME AS FILTER_NAME '
      'FROM EXPORT_FILTER '
      'ORDER BY ITEM_NAME;')
    Left = 40
    Top = 96
    ParamData = <>
  end
  object qryExportFormatLookup: TJNCCQuery
    Parameters = <>
    SQL.Strings = (
      'SELECT EXPORT_FORMAT_ID, ITEM_NAME'
      'FROM EXPORT_FORMAT_LK')
    Left = 44
    Top = 156
    ParamData = <>
  end
end
