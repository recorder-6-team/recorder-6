object ManageDesignationSetsX: TManageDesignationSetsX
  Left = 282
  Top = 232
  Width = 854
  Height = 694
  Caption = 'ManageDesignationSetsX'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = ActiveFormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Shape3: TShape
    Left = 296
    Top = 8
    Width = 49
    Height = 25
    Brush.Color = clBtnHighlight
  end
  object Shape2: TShape
    Left = 240
    Top = 8
    Width = 49
    Height = 25
    Brush.Color = clYellow
  end
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 116
    Height = 13
    Caption = 'Taxon Designation Sets '
  end
  object Shape1: TShape
    Left = 504
    Top = 8
    Width = 321
    Height = 105
    Brush.Color = clYellow
  end
  object Label5: TLabel
    Left = 304
    Top = 16
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label7: TLabel
    Left = 248
    Top = 16
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label4: TLabel
    Left = 16
    Top = 120
    Width = 384
    Height = 13
    Caption = 
      'Available Types (Highlight types on this list to add to the set.' +
      '  Use > to add them ) '
  end
  object Label2: TLabel
    Left = 432
    Top = 120
    Width = 221
    Height = 13
    Caption = 'Designation Types in Selected Set (Current list)'
  end
  object Label3: TLabel
    Left = 512
    Top = 16
    Width = 304
    Height = 26
    Caption = 
      'To create a new set or clone the selected set, first enter a nam' +
      'e for the new set,  then click the appropriate button below.  '
    WordWrap = True
  end
  object Label6: TLabel
    Left = 512
    Top = 56
    Width = 79
    Height = 13
    Caption = 'New set - Name '
  end
  object ListBox2: TListBox
    Left = 16
    Top = 40
    Width = 473
    Height = 73
    Color = clYellow
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox2Click
  end
  object ListBox1: TListBox
    Left = 432
    Top = 136
    Width = 401
    Height = 505
    ExtendedSelect = False
    ItemHeight = 13
    TabOrder = 1
  end
  object Button1: TButton
    Left = 144
    Top = 8
    Width = 33
    Height = 25
    Caption = 'All'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 184
    Top = 8
    Width = 41
    Height = 25
    Caption = 'Local'
    TabOrder = 3
    OnClick = Button2Click
  end
  object ListBox3: TListBox
    Left = 16
    Top = 136
    Width = 377
    Height = 505
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 4
  end
  object Button3: TButton
    Left = 400
    Top = 184
    Width = 25
    Height = 25
    Caption = '<'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 400
    Top = 152
    Width = 25
    Height = 25
    Caption = '>'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 512
    Top = 80
    Width = 105
    Height = 25
    Caption = 'Add New Set'
    Enabled = False
    TabOrder = 7
    OnClick = Button5Click
  end
  object Edit1: TEdit
    Left = 600
    Top = 56
    Width = 217
    Height = 21
    MaxLength = 100
    TabOrder = 8
    Text = '[Name of New Set]'
  end
  object Button6: TButton
    Left = 624
    Top = 80
    Width = 105
    Height = 25
    Caption = 'Clone to New Set'
    Enabled = False
    TabOrder = 9
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 376
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Delete Selected Set'
    TabOrder = 10
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 400
    Top = 224
    Width = 25
    Height = 25
    Caption = '<<'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
    OnClick = Button8Click
  end
end
