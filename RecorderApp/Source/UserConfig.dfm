inherited frmUserConfig: TfrmUserConfig
  Left = 519
  AutoScroll = False
  Caption = 'User Configuration'
  ClientHeight = 516
  ClientWidth = 608
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel [0]
    Left = 0
    Top = 482
    Width = 608
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      608
      34)
    object bbUserAdd: TImageListButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Add'
      TabOrder = 0
      OnClick = bbUserAddClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 2
      Spacing = -1
    end
    object bbUserEdit: TImageListButton
      Left = 88
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Edit'
      TabOrder = 1
      OnClick = bbUserEditClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
      Spacing = -1
    end
    object bbUserDel: TImageListButton
      Left = 168
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Delete'
      TabOrder = 2
      OnClick = bbUserDelClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
      Spacing = -1
    end
  end
  object pnlConfig: TPanel [1]
    Left = 0
    Top = 0
    Width = 608
    Height = 482
    Align = alClient
    Anchors = []
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object spltUserOptions: TSplitter
      Left = 311
      Top = 4
      Width = 4
      Height = 474
      Align = alRight
      AutoSnap = False
      MinSize = 180
      ResizeStyle = rsUpdate
    end
    object pnlSettings: TPanel
      Left = 315
      Top = 4
      Width = 289
      Height = 474
      Align = alRight
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        289
        474)
      object Label4: TLabel
        Left = 8
        Top = 35
        Width = 67
        Height = 13
        Caption = 'Access Level:'
      end
      object Label1: TLabel
        Left = 8
        Top = 11
        Width = 51
        Height = 13
        Caption = 'Username:'
      end
      object Shape2: TShape
        Tag = 2
        Left = 79
        Top = 5
        Width = 181
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Pen.Color = clRed
      end
      object lblSurvey: TLabel
        Left = 8
        Top = 59
        Width = 36
        Height = 13
        Caption = 'Survey:'
      end
      object bbUserFind: TImageListButton
        Left = 261
        Top = 7
        Width = 21
        Height = 21
        Hint = 'Get name'
        Anchors = [akTop, akRight]
        Enabled = False
        TabOrder = 1
        OnClick = bbUserFindClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 5
      end
      object bbEditSave: TImageListButton
        Left = 127
        Top = 443
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Save'
        Enabled = False
        TabOrder = 6
        OnClick = bbEditSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
        Spacing = -1
      end
      object bbEditCancel: TImageListButton
        Left = 207
        Top = 443
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Cancel'
        Enabled = False
        TabOrder = 7
        OnClick = bbEditCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
        Spacing = -1
      end
      object eUserName: TEdit
        Left = 80
        Top = 6
        Width = 179
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 0
        OnKeyPress = eUserNameKeyPress
      end
      object cmbAccessLevel: TComboBox
        Left = 80
        Top = 31
        Width = 202
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          'Read Only'
          'Record Cards Only'
          'Add Only'
          'Full Edit (Own data only)'
          'Full Edit'
          'System Manager')
      end
      object bbSelectAll: TButton
        Left = 207
        Top = 75
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Select All'
        Enabled = False
        TabOrder = 4
        OnClick = bbSelectAllClick
      end
      object bbUnselectAll: TButton
        Left = 207
        Top = 103
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Unselect All'
        Enabled = False
        TabOrder = 5
        OnClick = bbUnselectAllClick
      end
      object clbSurveys: TCheckListBox
        Left = 8
        Top = 75
        Width = 194
        Height = 362
        OnClickCheck = clbSurveysOnClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 3
        OnMouseMove = clbSurveysMouseMove
      end
    end
    object pnlUsers: TPanel
      Left = 4
      Top = 4
      Width = 307
      Height = 474
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object lvUsers: TListView
        Left = 0
        Top = 0
        Width = 307
        Height = 474
        Align = alClient
        Anchors = []
        BevelInner = bvNone
        BevelOuter = bvNone
        Columns = <
          item
            Caption = 'Username'
            Width = 150
          end
          item
            Caption = 'Access Level'
            Width = 150
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnChanging = lvUsersChanging
        OnClick = lvUsersClick
        OnSelectItem = lvUsersSelectItem
      end
    end
  end
  inherited mnuChildMerge: TMainMenu
    Tag = 999
  end
end
