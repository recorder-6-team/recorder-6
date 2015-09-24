inherited dmTermLists: TdmTermLists
  OldCreateOrder = True
  Left = 618
  Top = 192
  Height = 218
  Width = 255
  object qryTermLists: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      
        'Select * From TERM_LIST Where (LINKED_TABLE = '#39'Yes'#39' OR LINKED_TA' +
        'BLE is NULL) Order By Description Asc ')
    ParseSQL = True
    Left = 24
    Top = 24
  end
  object dsTermList: TDataSource
    AutoEdit = False
    DataSet = qryTermList
    Left = 180
    Top = 76
  end
  object qryLinkedList: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 104
    Top = 24
  end
  object qryContexts: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      
        'SELECT S.Screen_Key, S.Screen_Name, IIF(MTC.Measurement_Type_Key' +
        ' = '
      #39'NBNSYS0000000001'#39
      ', 1, 0) AS [Available] '
      'FROM Screen S LEFT JOIN Measurement_Type_Context MTC ON '
      '  S.Screen_Key = MTC.Screen_Key  '
      'ORDER BY Screen.Screen_Name')
    ParseSQL = True
    Left = 24
    Top = 124
  end
  object tblMeasurementContexts: TJNCCTable
    CommandTimeout = 0
    TableName = 'MEASUREMENT_TYPE_CONTEXT'
    Left = 120
    Top = 124
  end
  object qryLinkedTables: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 24
    Top = 76
  end
  object qryTermList: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 180
    Top = 24
  end
end
