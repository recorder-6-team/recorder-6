object dmSources: TdmSources
  OldCreateOrder = False
  Left = 370
  Top = 117
  Height = 216
  Width = 290
  object qryGetExtSources: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      
        'SELECT SF.File_Name, SF.Title, SF.Source_Key, JN.Source_Link_Key' +
        ', JN.Custodian'
      
        'FROM (Source AS S INNER JOIN Source_File AS SF ON S.Source_Key =' +
        ' SF.Source_Key) INNER JOIN'
      'Taxon_Occurrence_Sources'
      'AS JN ON S.Source_Key = JN.Source_Key'
      'WHERE'
      'JN.Taxon_Occurrence_Key'
      '= :KeyParameter'
      ' ')
    ParseSQL = True
    Left = 48
    Top = 60
  end
  object qryGetIntSources: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT R.*, JN.Original, JN.Source_Link_Key, JN.Custodian'
      
        'FROM (Source AS S INNER JOIN Reference AS R ON S.Source_Key = R.' +
        'Source_Key) INNER JOIN'
      '     Survey_Sources'
      '     AS JN ON S.Source_Key = JN.Source_Key'
      'WHERE'
      'JN.Survey_Key'
      '= :KeyParameter'
      ' ')
    ParseSQL = True
    Left = 48
    Top = 8
  end
  object qryGetRef: TJNCCQuery
    CommandTimeout = 0
    Parameters = <
      item
        Name = 'KeyParameter'
        Size = -1
        Value = Null
      end>
    SQL.Strings = (
      'SELECT Full_Reference, Reference_Type, Original_File'
      ' FROM Reference'
      'WHERE Source_Key = :KeyParameter')
    ParseSQL = True
    Left = 136
    Top = 8
  end
end
