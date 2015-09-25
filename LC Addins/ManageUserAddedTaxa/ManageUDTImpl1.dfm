object ManageUDT: TManageUDT
  Left = 331
  Top = 217
  Width = 844
  Height = 684
  ActiveControl = ListBox2
  Caption = 
    '----------------------------------------------------------------' +
    '----------------------------------------------------------------' +
    '-------------------------------'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = ActiveFormCreate
  OnDestroy = ActiveFormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 512
    Top = 152
    Width = 305
    Height = 81
    Brush.Color = clYellow
  end
  object Label1: TLabel
    Left = 520
    Top = 160
    Width = 121
    Height = 25
    AutoSize = False
    Caption = 'Current Parent Organism '
    Color = clYellow
    ParentColor = False
  end
  object Label2: TLabel
    Left = 520
    Top = 192
    Width = 281
    Height = 25
    AutoSize = False
  end
  object Label4: TLabel
    Left = 648
    Top = 160
    Width = 153
    Height = 25
    AutoSize = False
    Caption = 'Not Available'
  end
  object Shape2: TShape
    Left = 512
    Top = 264
    Width = 305
    Height = 81
  end
  object Label5: TLabel
    Left = 520
    Top = 272
    Width = 120
    Height = 13
    Caption = 'Manage Parent Organism'
  end
  object Label9: TLabel
    Left = 512
    Top = 240
    Width = 61
    Height = 20
    Caption = 'Actions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Shape3: TShape
    Left = 512
    Top = 8
    Width = 305
    Height = 129
    Brush.Color = clSkyBlue
  end
  object Label3: TLabel
    Left = 520
    Top = 40
    Width = 289
    Height = 25
    AutoSize = False
    Color = clMedGray
    ParentColor = False
  end
  object Label6: TLabel
    Left = 520
    Top = 64
    Width = 289
    Height = 25
    AutoSize = False
    Color = clMedGray
    ParentColor = False
  end
  object Label7: TLabel
    Left = 520
    Top = 16
    Width = 81
    Height = 13
    Caption = 'Current Selection'
  end
  object Shape4: TShape
    Left = 512
    Top = 360
    Width = 305
    Height = 137
  end
  object Label8: TLabel
    Left = 520
    Top = 368
    Width = 66
    Height = 13
    Caption = 'Manage Taxa'
  end
  object Label10: TLabel
    Left = 520
    Top = 104
    Width = 164
    Height = 13
    Caption = 'Number of  records usiing the taxa '
  end
  object Label11: TLabel
    Left = 696
    Top = 104
    Width = 65
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Color = clMedGray
    ParentColor = False
  end
  object Label12: TLabel
    Left = 648
    Top = 16
    Width = 53
    Height = 13
    Caption = 'Version 2.0'
  end
  object Label13: TLabel
    Left = 768
    Top = 16
    Width = 3
    Height = 13
  end
  object Shape5: TShape
    Left = 512
    Top = 504
    Width = 305
    Height = 121
  end
  object Label14: TLabel
    Left = 520
    Top = 512
    Width = 71
    Height = 13
    Caption = 'Add New Taxa'
  end
  object Label15: TLabel
    Left = 520
    Top = 536
    Width = 49
    Height = 13
    Caption = 'Sci. Name'
  end
  object Label16: TLabel
    Left = 520
    Top = 560
    Width = 26
    Height = 13
    Caption = 'Rank'
  end
  object ListBox1: TListBox
    Left = 24
    Top = 80
    Width = 465
    Height = 545
    ExtendedSelect = False
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object Button2: TButton
    Left = 720
    Top = 296
    Width = 89
    Height = 25
    Hint = 'Remove from Organism table'
    Caption = 'Delete Organism'
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 520
    Top = 392
    Width = 145
    Height = 25
    Caption = 'Select Replacement'
    Enabled = False
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 520
    Top = 296
    Width = 97
    Height = 25
    Hint = 'Select suitale parent from dictionary'
    Caption = 'Change Parent'
    Enabled = False
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button4: TButton
    Left = 624
    Top = 296
    Width = 89
    Height = 25
    Hint = 'Let system find suitable parent. '
    Caption = 'Auto Allocate'
    Enabled = False
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 520
    Top = 424
    Width = 145
    Height = 25
    Caption = 'Auto Replace All  Listed '
    Enabled = False
    TabOrder = 5
    OnClick = Button5Click
  end
  object ListBox2: TListBox
    Left = 32
    Top = 16
    Width = 465
    Height = 57
    ItemHeight = 13
    TabOrder = 6
    OnClick = ListBox2Click
  end
  object Button6: TButton
    Left = 672
    Top = 424
    Width = 129
    Height = 25
    Caption = 'Delete Selected'
    Enabled = False
    TabOrder = 7
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 672
    Top = 392
    Width = 129
    Height = 25
    Caption = 'Auto Replace Selected'
    Enabled = False
    TabOrder = 8
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 520
    Top = 456
    Width = 145
    Height = 25
    Caption = 'Auto Delete Unused'
    Enabled = False
    TabOrder = 9
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 672
    Top = 456
    Width = 129
    Height = 25
    Caption = 'Refresh'
    Enabled = False
    TabOrder = 10
    OnClick = Button9Click
  end
  object Edit1: TEdit
    Left = 600
    Top = 528
    Width = 209
    Height = 21
    TabOrder = 11
  end
  object ComboBox1: TComboBox
    Left = 600
    Top = 560
    Width = 209
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 12
  end
  object Button10: TButton
    Left = 520
    Top = 592
    Width = 57
    Height = 25
    Caption = 'Add'
    TabOrder = 13
    OnClick = Button10Click
  end
end
