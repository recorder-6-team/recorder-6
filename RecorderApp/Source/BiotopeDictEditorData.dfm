inherited dmBiotopeDictEditor: TdmBiotopeDictEditor
  Height = 322
  inherited qryGeneral: TJNCCQuery
    SQL.Strings = (
      'SELECT '
      'BLI.BIOTOPE_LIST_ITEM_KEY AS BLI_BIOTOPE_LIST_ITEM_KEY, '
      'BLI.BIOTOPE_KEY AS BLI_BIOTOPE_KEY, '
      'BLI.PARENT AS BLI_PARENT,'
      'BLI.SORT_CODE AS BLI_SORT_CODE,'
      'BLI.ENTERED_BY AS BLI_ENTERED_BY,'
      'BLI.ENTRY_DATE AS BLI_ENTRY_DATE,'
      'BLI.CHANGED_BY AS BLI_CHANGED_BY,'
      'BLI.CHANGED_DATE AS BLI_CHANGED_DATE,'
      'BLI.SYSTEM_SUPPLIED_DATA AS BLI_SYSTEM_SUPPLIED_DATA,'
      'BLI.BT_CL_VERSION_KEY AS BLI_BT_CL_VERSION_KEY,'
      'BLI.CUSTODIAN,'
      'B.BIOTOPE_KEY AS B_BIOTOPE_KEY, '
      'B.SHORT_TERM AS B_SHORT_TERM,'
      'B.FULL_TERM AS B_FULL_TERM, '
      'B.ORIGINAL_CODE AS B_ORIGINAL_CODE,'
      'B.ENTERED_BY AS B_ENTERED_BY,'
      'B.ENTRY_DATE AS B_ENTRY_DATE,'
      'B.CHANGED_BY AS B_CHANGED_BY,'
      'B.CHANGED_DATE AS B_CHANGED_DATE,'
      'B.SYSTEM_SUPPLIED_DATA AS B_SYSTEM_SUPPLIED_DATA,'
      'B.TERM_CURRENT AS B_TERM_CURRENT'
      ''
      
        'FROM BIOTOPE_LIST_ITEM AS BLI INNER JOIN BIOTOPE AS B ON BLI.BIO' +
        'TOPE_KEY = B.BIOTOPE_KEY'
      'WHERE (((BLI.BIOTOPE_LIST_ITEM_KEY)=:Key));')
  end
  object dsGeneral: TDataSource
    AutoEdit = False
    DataSet = qryGeneral
    Left = 20
    Top = 236
  end
  object qrySortcode: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      '')
    ParseSQL = True
    Left = 172
    Top = 180
  end
end
