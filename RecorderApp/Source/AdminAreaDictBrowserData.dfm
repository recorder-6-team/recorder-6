inherited dmAdminAreaDictBrowser: TdmAdminAreaDictBrowser
  Left = 268
  Top = 144
  Height = 282
  Width = 381
  inherited qryList: TJNCCQuery
    SQL.Strings = (
      'SELECT      ADMIN_TYPE.ADMIN_TYPE_KEY AS KeyField,'
      '            ISNULL(ADMIN_TYPE.LONG_NAME,'
      '                   ADMIN_TYPE.SHORT_NAME) As DisplayField'
      'FROM        ADMIN_TYPE'
      
        'INNER JOIN  ADMIN_AREA ON ADMIN_TYPE.ADMIN_TYPE_KEY = ADMIN_AREA' +
        '.ADMIN_TYPE_KEY'
      'WHERE       ADMIN_AREA.PARENT Is Null'
      
        'GROUP BY    ADMIN_TYPE.ADMIN_TYPE_KEY, ADMIN_TYPE.LONG_NAME, ADM' +
        'IN_TYPE.SHORT_NAME'
      'ORDER BY    DisplayField;')
  end
  inherited qryParent: TJNCCQuery
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Parent '
      'FROM Admin_Area'
      'WHERE Admin_Area_Key = :Key'
      '')
  end
  inherited ppTopLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT IsNull(A.Short_Code + '#39', '#39' + A.Item_Name, A.Item_Name) AS' +
        ' DisplayField,'
      '        A.Admin_Area_Key AS ListKey, A.Item_Name AS ItemName,'
      
        '        A.Short_Code As ShortCode, COUNT(ACount.Admin_Area_Key) ' +
        'AS ChildrenCount'
      'FROM Admin_Area AS A'
      
        'LEFT JOIN Admin_Area AS ACount ON A.Admin_Area_Key = ACount.Pare' +
        'nt'
      '<#JoinFilter>'
      'WHERE A.Admin_Type_Key IN (<#ListKeys>)'
      'AND A.Parent Is NULL'
      '<#Filter>'
      'GROUP BY IsNull(A.Short_Code + '#39', '#39' + A.Item_Name, A.Item_Name),'
      'A.Admin_Area_Key, A.Short_Code, A.Item_Name'
      '<#OrderBy>')
  end
  inherited ppChildLevel: TPageProducer
    HTMLDoc.Strings = (
      
        'SELECT IsNull(A.Short_Code + '#39', '#39' + A.Item_Name, A.Item_Name) AS' +
        ' DisplayField,'
      '        A.Admin_Area_Key AS ListKey, A.Item_Name AS ItemName,'
      
        '        A.Short_Code As ShortCode, COUNT(ACount.Admin_Area_Key) ' +
        'AS ChildrenCount'
      'FROM Admin_Area AS A'
      
        'LEFT JOIN Admin_Area AS ACount ON A.Admin_Area_Key = ACount.Pare' +
        'nt'
      '<#JoinFilter>'
      'WHERE A.Parent = '#39'<#ParentKey>'#39
      '<#Filter>'
      'GROUP BY IsNull(A.Short_Code + '#39', '#39' + A.Item_Name, A.Item_Name),'
      'A.Admin_Area_Key, A.Short_Code, A.Item_Name'
      '<#OrderBy>')
  end
  object qryDictDetailsSources: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Admin_Area_Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT ADMIN_AREA.ADMIN_AREA_KEY, SOURCE.SOURCE_KEY, '
      'SOURCE_FILE.FILE_NAME, REFERENCE.TITLE'
      
        'FROM ((SOURCE INNER JOIN (ADMIN_AREA INNER JOIN ADMIN_AREA_SOURC' +
        'ES'
      
        ' ON ADMIN_AREA.ADMIN_AREA_KEY = ADMIN_AREA_SOURCES.ADMIN_AREA_KE' +
        'Y) '
      
        'ON SOURCE.SOURCE_KEY = ADMIN_AREA_SOURCES.SOURCE_KEY) LEFT JOIN ' +
        'REFERENCE '
      
        'ON SOURCE.SOURCE_KEY = REFERENCE.SOURCE_KEY) LEFT JOIN SOURCE_FI' +
        'LE '
      'ON SOURCE.SOURCE_KEY = SOURCE_FILE.SOURCE_KEY'
      'WHERE ADMIN_AREA.ADMIN_AREA_KEY=:Admin_Area_Key')
    ParseSQL = True
    Left = 168
    Top = 124
  end
  object qryDictDetails: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Admin_Area_Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT ADMIN_AREA.ITEM_NAME, ADMIN_TYPE.SHORT_NAME, '
      'ADMIN_TYPE.LONG_NAME, ADMIN_TYPE.DESCRIPTION, '
      'Convert(Char(10),ADMIN_AREA.ENTRY_DATE,103) AS ENTRY_DATE, '
      'ADMIN_AREA.CHANGED_DATE'
      
        'FROM ADMIN_TYPE INNER JOIN ADMIN_AREA ON ADMIN_TYPE.ADMIN_TYPE_K' +
        'EY = ADMIN_AREA.ADMIN_TYPE_KEY'
      'WHERE ADMIN_AREA.ADMIN_AREA_KEY=:Admin_Area_Key')
    ParseSQL = True
    Left = 20
    Top = 128
  end
  object ppAdminAreaDetails: TPageProducer
    HTMLDoc.Strings = (
      '<#ADMIN_AREA_NAME>'
      '<#LAST_UPDATED>'
      '<#ADMIN_AREA_TYPE>'
      '<#SOURCES>')
    OnHTMLTag = ppAdminAreaDetailsHTMLTag
    Left = 88
    Top = 180
  end
end
