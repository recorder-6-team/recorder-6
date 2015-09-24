object dlgServerFolders: TdlgServerFolders
  Left = 399
  Top = 216
  BorderStyle = bsDialog
  Caption = 'Server Folders'
  ClientHeight = 295
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 409
    Height = 249
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 40
    Width = 28
    Height = 13
    Caption = 'Drive:'
  end
  object lblInstruct: TLabel
    Left = 16
    Top = 16
    Width = 204
    Height = 13
    Caption = 'Please select a database backup directory.'
  end
  object lblPath: TLabel
    Left = 16
    Top = 60
    Width = 389
    Height = 17
    AutoSize = False
  end
  object cmbDrive: TComboBox
    Left = 60
    Top = 36
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbDriveChange
  end
  object tvDirectories: TKeyboardRapidTree
    Left = 16
    Top = 80
    Width = 393
    Height = 169
    SmoothExpandCollapse = False
    FitColumnToClientWidth = True
    FitToHeight = False
    DoubleBuffered = False
    TransparentMode = False
    DefaultRowHeight = 16
    RowCount = 1
    FixedRows = 0
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 1
    OnSelectCell = tvDirectoriesSelectCell
    OnExpanding = tvDirectoriesExpanding
    HideSelection = False
    Showlines = True
    ShowRoot = True
    ShowButtons = True
    ShowImages = True
    ShowLogic = False
    Images = dmFormActions.ilMenuOn
    SortType = stNone
    WordWrap = False
    AutoMove = False
    ToolTips = False
    AutoExpand = False
    TooltipColor = clInfoBk
    ToolTipPause = 1000
    StatesDrawed = True
    Data = {
      9C00000001000000060854466C794E6F64658A000000060943656C6C73546578
      741300000006020D0A060648696464656E2000000000060A496D616765496E64
      65783400000000000000060D53656C6563746564496E6465784B000000000000
      00060A5374617465496E6465785F000000000000000604546578747300000006
      0854466C794E6F64650609556E6971756554616786000000FFFFFFFF00000000}
  end
  object btnOK: TImageListButton
    Left = 258
    Top = 264
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    ImageList = dmFormActions.ilButtons
    ImageIndex = 0
  end
  object btnCancel: TImageListButton
    Left = 338
    Top = 264
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ImageList = dmFormActions.ilButtons
    ImageIndex = 1
  end
  object qryMoveBackup: TADOQuery
    Parameters = <
      item
        Name = 'Name'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'NewLocation'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'Move'
        DataType = ftBoolean
        Value = True
      end>
    SQL.Strings = (
      'declare @Name as sysname, '
      '@NewLocation as nvarchar(260),'
      '@Move as bit '
      ''
      'set @Name = :Name'
      'set @NewLocation = :NewLocation'
      'set @Move = :Move'
      ''
      'declare @OldLocation as nvarchar(260),'
      '@cmdString as varchar(1000)'
      ''
      'if @Move = 1 '
      
        #9'set @OldLocation = (Select phyName from master..sysdevices wher' +
        'e name = @name) '
      ''
      ''
      'exec master.dbo.sp_dropdevice @Name'
      ''
      ''
      'if @Move = 1 '
      'begin'
      
        #9'set @cmdString = '#39'move '#39' + @OldLocation + '#39' "'#39' + @NewLocation +' +
        ' '#39'"'#39
      ''
      #9'exec master.dbo.xp_cmdShell @cmdString'
      'end'
      ''
      'exec master.dbo.sp_addumpdevice '#39'disk'#39', @Name, @NewLocation')
    Left = 336
    Top = 8
  end
end
