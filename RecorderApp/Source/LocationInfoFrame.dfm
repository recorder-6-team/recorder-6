object fraLocationInfo: TfraLocationInfo
  Left = 0
  Top = 0
  Width = 340
  Height = 88
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  TabOrder = 0
  OnResize = FrameResize
  DesignSize = (
    340
    88)
  object Label6: TLabel
    Left = 30
    Top = 4
    Width = 44
    Height = 13
    Caption = 'Location:'
  end
  object Label7: TLabel
    Left = 19
    Top = 62
    Width = 55
    Height = 13
    Caption = 'Spatial Ref:'
  end
  object Label8: TLabel
    Left = 0
    Top = 31
    Width = 75
    Height = 13
    Caption = 'Location Name:'
  end
  object eSpatialRef: TSpatialRef
    Tag = 2
    Left = 82
    Top = 52
    Width = 258
    Height = 33
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
    DropDownMenu = pmMaps
    ImageIndex = 5
    ImageList = dmFormActions.ilButtons
    OnChange = DataChange
    OnGetFromMap = eSpatialRefGetFromMap
  end
  object eLocationName: TEdit
    Left = 82
    Top = 28
    Width = 258
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    OnChange = DataChange
  end
  object eLocation: TAddinLinkedEdit
    Tag = 2
    Left = 82
    Top = 0
    Width = 258
    Height = 23
    TabOrder = 0
    BorderStyle = bsSingle
    ImageIndex = 5
    ImageList = dmFormActions.ilButtons
    OnChange = DataChange
    OnFindData = eLocationFindData
    OnGetData = eLocationGetData
  end
  object pmMaps: TPopupMenu
    Images = dmFormActions.ilMenuOn
    Left = 130
    Top = 57
  end
end
