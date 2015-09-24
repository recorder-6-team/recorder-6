inherited frmMergeData: TfrmMergeData
  Left = 326
  Top = 155
  Width = 416
  Height = 351
  Caption = 'Merge Data Items'
  Constraints.MinHeight = 246
  Constraints.MinWidth = 416
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnlInstruct: TPanel [0]
    Left = 0
    Top = 0
    Width = 408
    Height = 121
    Align = alTop
    TabOrder = 0
    object lblInstruct: TLabel
      Left = 5
      Top = 5
      Width = 397
      Height = 39
      AutoSize = False
      Caption = 
        'First, drag or copy the item you wish to reallocate into the sou' +
        'rce item box on the left. After the merge is complete, this item' +
        ' is deleted and all data referring to the item is reallocated to' +
        ' the item you drag or copy to the second box.'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 8
      Top = 104
      Width = 60
      Height = 13
      Caption = 'Source Item:'
    end
    object lblDestItem: TLabel
      Left = 184
      Top = 104
      Width = 79
      Height = 13
      Caption = 'Destination Item:'
    end
    object lblInstruct2: TLabel
      Left = 5
      Top = 49
      Width = 397
      Height = 39
      AutoSize = False
      Caption = 
        'Once selected, check that the data items you have chosen are cor' +
        'rect by double clicking on items to see the data they contain.  ' +
        'Double click foreign key data to view the record which the key l' +
        'inks to.  If correct, click Merge to perform the operation.'
      WordWrap = True
    end
  end
  object Panel2: TPanel [1]
    Tag = 2
    Left = 0
    Top = 121
    Width = 179
    Height = 133
    Align = alLeft
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = 'Panel2'
    TabOrder = 1
    object tvSourceItem: TTreeView
      Left = 1
      Top = 1
      Width = 177
      Height = 131
      Align = alLeft
      Images = dmMergeData.ilTreeImages
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnClick = ItemClick
      OnCustomDrawItem = CustomDrawItem
    end
  end
  object pnlDest: TPanel [2]
    Tag = 2
    Left = 179
    Top = 121
    Width = 229
    Height = 133
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = 'Panel2'
    TabOrder = 2
    object tvDestItem: TTreeView
      Left = 1
      Top = 1
      Width = 227
      Height = 131
      Align = alClient
      Images = dmMergeData.ilTreeImages
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnClick = ItemClick
      OnCustomDrawItem = CustomDrawItem
    end
  end
  object pnlButtons: TPanel [3]
    Left = 0
    Top = 254
    Width = 408
    Height = 43
    Align = alBottom
    TabOrder = 3
    object bbMerge: TImageListButton
      Left = 240
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Merge'
      Default = True
      Enabled = False
      TabOrder = 0
      OnClick = bbMergeClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 0
    end
    object bbCancel: TImageListButton
      Left = 324
      Top = 9
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Clear'
      ModalResult = 2
      TabOrder = 1
      OnClick = bbCancelClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 1
    end
  end
  inherited mnuChildMerge: TMainMenu
    Images = dmFormActions.ilMenuOn
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      GroupIndex = 2
      object mnuPaste: TMenuItem
        Action = dmFormActions.actPaste
      end
    end
  end
end
