inherited dmObservation: TdmObservation
  OldCreateOrder = True
  Left = 721
  Top = 173
  Height = 321
  Width = 213
  object qrySurvey: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    SQL.Strings = (
      'SELECT DISTINCT'
      '       SU.Survey_Key,'
      '       SU.Item_Name AS SurveyName,'
      '       CASE N.Organisation'
      
        '         WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname' +
        ' ELSE I.Forename + '#39' '#39' + I.Surname END'
      '         WHEN 1 THEN O.Full_Name'
      '         ELSE '#39#39
      '       END AS FullName,'
      
        '       CASE WHEN ST.Survey_Key IS NULL THEN 0 ELSE 1 END AS HasT' +
        'ag'
      'FROM      Survey       SU'
      'JOIN      Name         N  ON  N.Name_Key     = SU.Run_By'
      'LEFT JOIN Individual   I  ON  I.Name_Key     = N.Name_Key'
      '                          AND N.Organisation = 0'
      'LEFT JOIN Organisation O  ON  O.Name_Key     = N.Name_Key'
      '                          AND N.Organisation = 1'
      'LEFT JOIN Survey_Tag   ST ON  ST.Survey_Key  = SU.Survey_Key'
      
        'LEFT JOIN User_Survey_Restriction USR ON USR.Survey_Key = SU.Sur' +
        'vey_Key'
      '                          AND USR.Name_Key = :UserID'
      'WHERE USR.Name_Key IS NULL'
      ''
      'ORDER BY  SurveyName')
    ParseSQL = True
    Left = 36
    Top = 12
  end
  object qryEvent: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SurveyKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT E.Survey_Event_Key, E.Vague_Date_Start, E.Vague_Date_End,' +
        ' E.Vague_Date_Type, '
      'LN.Item_Name, E.Spatial_Ref, E.Location_Name, E.Location_Key'
      
        'FROM   Survey_Event As E LEFT JOIN Location_Name AS LN ON E.Loca' +
        'tion_Key = LN.Location_Key '
      'WHERE E.Survey_Key = :SurveyKey '
      'AND  (LN.Preferred = 1 or LN.Preferred is null)'
      ''
      'ORDER BY Vague_Date_Start')
    ParseSQL = True
    Left = 116
    Top = 12
  end
  object qrySample: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'EventKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT S.Sample_Key, S.Sample_Reference, S.Vague_Date_Start, S.V' +
        'ague_Date_End,'
      
        '       S.Vague_Date_Type, S.Outstanding_Card, T.Short_Name, T.Sa' +
        'mple_Type_Key, S.Location_Key,'
      
        '       LN.Item_Name, S.Spatial_Ref, S.Location_Name, S.Private_L' +
        'ocation, S.Private_Code'
      
        'FROM Sample As S INNER JOIN Sample_Type As T ON S.Sample_Type_Ke' +
        'y = T.Sample_Type_Key'
      
        'LEFT JOIN Location_Name AS LN ON LN.Location_Key = S.Location_Ke' +
        'y'
      
        'WHERE S.Survey_Event_Key = :EventKey AND (LN.Preferred = 1 OR LN' +
        '.Preferred IS NULL)'
      ''
      'ORDER BY Sample_Reference')
    ParseSQL = True
    Left = 40
    Top = 64
  end
  object qryTaxonOcc: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SampleKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT DISTINCT TXO.Sample_Key, TXO.Taxon_Occurrence_Key,'
      '       ITN.Taxon_List_Item_Key,'
      '       ITN.Actual_Name,'
      '       ITN.Actual_Name_Italic,'
      '       ITN.Actual_Name_Attribute,'
      '       ITN.Authority,'
      '       ITN.Preferred_Name,'
      '       ITN.Preferred_Name_Italic,'
      '       ITN.Preferred_Name_Attribute,'
      '       ITN.Preferred_Name_Authority,'
      '       ITN.Common_Name,'
      '       ITN.Common_Name_Italic,'
      '       ITN.Common_Name_Attribute,ITN.Sort_Order,'
      '       TXO.Checked,'
      '       TXO.Confidential, TXO.Zero_Abundance,TXO.Verified'
      'FROM Taxon_Occurrence TXO'
      
        'INNER JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key = T' +
        'XO.Taxon_Occurrence_Key'
      
        'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.' +
        'Taxon_List_Item_Key'
      ''
      'AND'
      'TD.Preferred = 1'
      ''
      'AND ITN.SYSTEM_SUPPLIED_DATA=1'
      ''
      'ORDER BY ITN.Preferred_Name')
    ParseSQL = True
    Left = 36
    Top = 116
  end
  object qryBiotopeOcc: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'SampleKey'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT BO.Biotope_Occurrence_Key,'
      
        '       CASE WHEN B.Original_Code IS NULL THEN B.Short_Term ELSE ' +
        'B.Original_Code+'#39', '#39'+B.Short_Term END AS Item_Name,'
      '       BO.Checked, BO.Verified'
      'FROM Biotope_Occurrence AS BO INNER JOIN'
      '         ((Biotope AS B INNER JOIN'
      '             Biotope_List_Item AS BLI ON'
      '         B.Biotope_Key = BLI.Biotope_Key) INNER JOIN'
      '             Biotope_Determination AS BD ON'
      
        '         BLI.Biotope_List_Item_Key = BD.Biotope_List_Item_Key) O' +
        'N'
      '     BO.Biotope_Occurrence_Key = BD.Biotope_Occurrence_Key'
      'WHERE BO.Sample_Key = :SampleKey'
      'AND BD.Preferred = 1'
      ''
      'ORDER BY B.Original_Code')
    ParseSQL = True
    Left = 116
    Top = 116
  end
  object qrySampleState: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * '
      'FROM Sample'
      'WHERE Sample_Key = :KeyParameter')
    ParseSQL = True
    Left = 116
    Top = 64
  end
  object qrySurveyByKey: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'Key'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT S.Survey_Key, S.Item_Name AS SurveyName, O.Full_Name AS F' +
        'ullName'
      
        'FROM (Organisation O RIGHT JOIN Name N ON O.Name_Key = N.Name_Ke' +
        'y) INNER JOIN Survey S ON N.Name_Key = S.Run_By'
      'WHERE (N.Organisation)=Yes'
      'UNION'
      
        'SELECT S.Survey_Key, S.Item_Name AS SurveyName, I.Forename+'#39' '#39'+I' +
        '.Surname AS FullName'
      
        'FROM (Individual I RIGHT JOIN Name N ON I.Name_Key = N.Name_Key)' +
        ' INNER JOIN Survey S ON N.Name_Key = S.Run_By'
      'WHERE (N.Organisation)=No and S.Survey_Key = :Key'
      'ORDER BY SurveyName'
      ' ')
    ParseSQL = True
    Left = 36
    Top = 172
  end
  object qryMultiPurpose: TJNCCQuery
    CommandTimeout = 0
    Parameters = <>
    ParseSQL = True
    Left = 36
    Top = 228
  end
end
