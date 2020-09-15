inherited frmTaxonDictEditor: TfrmTaxonDictEditor
  Left = 307
  Top = 202
  Width = 855
  Height = 494
  Caption = 'Taxon Dictionary Details'
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited DictSplitter: TSplitter
    Left = 447
    Height = 385
  end
  inherited pnlSelection: TPanel
    Width = 847
    object Label4: TLabel [1]
      Left = 16
      Top = 16
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    inherited cmbList: TComboBox
      Width = 720
    end
    inherited btnShowAll: TButton
      Left = 763
    end
  end
  inherited pcBrowser: TPageControl
    Width = 447
    Height = 385
    inherited tsTree: TTabSheet
      inherited tvDictionary: TKeyboardRapidTree
        Width = 437
        Height = 373
        OnKeyDown = tvDictionaryKeyDown
        Data = {0400000000000000}
      end
    end
  end
  inherited pnlDetails: TPanel
    Left = 457
    Width = 390
    Height = 385
    Constraints.MinHeight = 377
    Constraints.MinWidth = 388
    OnResize = pnlDetailsResize
    object scbTaxonDetails: TScrollBox
      Left = 0
      Top = 0
      Width = 390
      Height = 377
      Constraints.MinHeight = 377
      Constraints.MinWidth = 388
      TabOrder = 0
      DesignSize = (
        386
        373)
      object lblNamePrompt: TLabel
        Left = 8
        Top = 8
        Width = 76
        Height = 13
        Caption = 'Selected Name:'
      end
      object lblSelectedName: TLabel
        Left = 90
        Top = 8
        Width = 3
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object btnCancel: TImageListButton
        Left = 306
        Top = 342
        Width = 75
        Height = 25
        Hint = 'Cancel changes'
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Cancel'
        Enabled = False
        TabOrder = 2
        OnClick = btnCancelClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 1
      end
      object btnSave: TImageListButton
        Left = 222
        Top = 342
        Width = 75
        Height = 25
        Hint = 'Save taxon details'
        Anchors = [akRight, akBottom]
        Caption = '&Save'
        Enabled = False
        TabOrder = 1
        OnClick = btnSaveClick
        ImageList = dmFormActions.ilButtons
        ImageIndex = 0
      end
      object pcTaxonDetails: TPageControl
        Left = 8
        Top = 27
        Width = 371
        Height = 310
        ActivePage = tsOrganism
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnChange = pcTaxonDetailsChange
        OnChanging = pcTaxonDetailsChanging
        object tsGeneral: TTabSheet
          Caption = 'General'
          DesignSize = (
            363
            282)
          object bvlGeneralFram: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 273
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object lblRank: TLabel
            Left = 16
            Top = 152
            Width = 29
            Height = 13
            Caption = 'Rank:'
          end
          object lblAuthority: TLabel
            Left = 16
            Top = 100
            Width = 44
            Height = 13
            Caption = 'Authority:'
          end
          object lblName: TLabel
            Left = 16
            Top = 44
            Width = 31
            Height = 13
            Caption = 'Name:'
          end
          object lblEntryByPrompt: TLabel
            Left = 152
            Top = 227
            Width = 15
            Height = 13
            Caption = 'By:'
          end
          object lblEntryDatePrompt: TLabel
            Left = 16
            Top = 227
            Width = 51
            Height = 13
            Caption = 'Entry date:'
          end
          object lblChangeByPrompt: TLabel
            Left = 152
            Top = 251
            Width = 15
            Height = 13
            Caption = 'By:'
          end
          object lblChangeDatePrompt: TLabel
            Left = 16
            Top = 251
            Width = 64
            Height = 13
            Caption = 'Change date:'
          end
          object dblblEntryDate: TDBText
            Left = 84
            Top = 227
            Width = 61
            Height = 13
            DataField = 'TLI_ENTRY_DATE'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object dblblChangedDate: TDBText
            Left = 84
            Top = 251
            Width = 61
            Height = 13
            DataField = 'TLI_CHANGED_DATE'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lblEnteredBy: TLabel
            Left = 172
            Top = 227
            Width = 178
            Height = 13
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lblChangedBy: TLabel
            Left = 172
            Top = 251
            Width = 178
            Height = 13
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object dbeName: TDBEdit
            Left = 80
            Top = 40
            Width = 229
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'T_ITEM_NAME'
            TabOrder = 0
          end
          object dbeAuthority: TDBEdit
            Left = 80
            Top = 96
            Width = 229
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DataField = 'T_AUTHORITY'
            TabOrder = 1
          end
          object dbcmbRank: TDBGlyphLookupComboBox
            Left = 80
            Top = 150
            Width = 201
            Height = 22
            DataField = 'TLI_TAXON_RANK_KEY'
            GlyphField = 'IMAGE'
            ItemHeight = 16
            KeyField = 'TAXON_RANK_KEY'
            ListField = 'LONG_NAME'
            TabOrder = 2
            OnClick = dbComboClick
            OnKeyDown = dbComboKeyDown
            OnKeyUp = dbComboKeyUp
          end
        end
        object tsPersonalNames: TTabSheet
          Caption = 'Taxon Names'
          ImageIndex = 4
          DesignSize = (
            363
            282)
          object sgPersonalNames: TStringGrid
            Left = 4
            Top = 24
            Width = 353
            Height = 253
            Anchors = [akLeft, akTop, akRight, akBottom]
            ColCount = 3
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goColSizing]
            PopupMenu = pmTaxonNames
            TabOrder = 1
            OnClick = sgPersonalNamesClick
            OnDrawCell = sgPersonalNamesDrawCell
            OnKeyDown = sgPersonalNamesKeyDown
            OnKeyPress = sgPersonalNamesKeyPress
            OnTopLeftChanged = sgPersonalNamesTopLeftChanged
            ColWidths = (
              19
              168
              139)
          end
          object cmbEnterLanguage: TComboBox
            Left = 196
            Top = 44
            Width = 141
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            Sorted = True
            TabOrder = 2
            Visible = False
            OnChange = cmbEnterLanguageChange
            OnKeyPress = cmbEnterLanguageKeyPress
            Items.Strings = (
              'a - Hausa'
              'aa - Afar'
              'ab - Abkhazian'
              'af - Afrikaans'
              'am - Amharic'
              'ar - Arabic'
              'as - Assamese'
              'ay - Aymara'
              'az - Azerbaijani'
              'ba - Bashkir'
              'be - Byelorussian'
              'bg - Bulgarian'
              'bh - Bihari'
              'bi - Bislama'
              'bn - Bengali;Bangla'
              'bo - Tibetan'
              'br - Breton'
              'ca - Catalan'
              'co - Corsican'
              'cs - Czech'
              'cy - Welsh'
              'cy - Welsh'
              'da - Danish'
              'de - German'
              'dz - Bhutani'
              'el - Greek'
              'en - English'
              'eo - Esperanto'
              'es - Spanish'
              'es - Spanish'
              'et - Estonian'
              'eu - Basque'
              'fa - Persian (Farsi)'
              'fi - Finnish'
              'fj -Fiji'
              'fo - Faroese'
              'fr - French'
              'fy - Frisian'
              'ga - Gaelic (Irish)'
              'gd - Gaelic (Scottish)'
              'gl - Galician'
              'gnv - Guarani'
              'he - Hebrew'
              'hi - Hindi'
              'hr - Croatian'
              'hu - Hungarian'
              'hu - Hungarian'
              'hy - Armenian'
              'ia - Interlingua'
              'id - Indonesian'
              'ie - Interlingue'
              'ikv - Inupiak'
              'is - Icelandic'
              'it - Italian'
              'it - Italian'
              'iu - Inuktitut'
              'ja - Japanese'
              'jv - Javanese'
              'ka - Georgian'
              'kk - Kazakh'
              'klv - Greenlandic'
              'km - Cambodian'
              'kn - Kannada'
              'ko - Korean'
              'ks - Kashmiri'
              'ku - Kurdish'
              'ky - Kirghiz'
              'la - Latin'
              'ln - Lingala'
              'lo - Laothian'
              'lt - Lithuanian'
              'lt - Lithuanian'
              'lv - Latvian;Lettish '
              'mg - Malagasy'
              'mi - Maori'
              'mk - Macedonian'
              'mk - Macedonian'
              'ml - Malayalam'
              'mn - Mongolian'
              'mo - Moldavian'
              'mr - Marathi'
              'ms - Malay'
              'mt - Maltese'
              'my - Burmese'
              'na - Nauru'
              'ne - Nepali'
              'nl - Dutch'
              'no - Norwegian'
              'no - Norwegian'
              'oc - Occitan'
              'om - Afan (Oromo)'
              'or - Oriya'
              'pa - Punjabi'
              'pl - Polish'
              'pl - Polish'
              'ps - Pashto;Pushto'
              'pt - Portuguese'
              'pt - Portuguese'
              'qu - Quechua'
              'rm - Rhaeto-Romance'
              'rn - Kurundi'
              'ro - Romanian'
              'ro - Romanian'
              'ru - Russian'
              'rw - Kinyarwanda'
              'sa - Sanskrit'
              'sd - Sindhi'
              'sg - Sangho'
              'sh - Serbo-Croatian'
              'sh - Serbo-Croatian'
              'si - Singhalese'
              'sk - Slovak'
              'sk - Slovak'
              'sl - Slovenian'
              'sl - Slovenian'
              'sm - Samoan'
              'sn - Shona'
              'so - Somali'
              'sq - Albanian'
              'sr - Serbian'
              'sr - Serbian'
              'ss - Siswati'
              'st - Sesotho'
              'su - Sundanese'
              'sv - Swedish'
              'sv - Swedish'
              'sw - Swahili'
              'ta - Tamil'
              'te - Telugu'
              'tg - Tajik'
              'th - Thai'
              'ti - Tigrinya'
              'tk - Turkmen'
              'tl - Tagalog'
              'tn - Setswana'
              'to - Tonga'
              'tr - Turkish'
              'ts - Tsonga'
              'tt - Tatar'
              'tw - Twi'
              'ug - Uigur'
              'uk - Ukrainian'
              'ur - Urdu'
              'uz - Uzbek'
              'vi - Vietnamese'
              'vo - Volapuk'
              'vu - Gujarati'
              'wo - Wolof'
              'xh - Xhosa'
              'yi - Yiddish'
              'yo - Yoruba'
              'za - Zhuang'
              'zh - Chinese'
              'zu - Zulu')
          end
          object chkStandardCommonName: TCheckBox
            Left = 6
            Top = 4
            Width = 351
            Height = 17
            Caption = 'Use standard common name '
            TabOrder = 0
            OnClick = chkStandardCommonNameClick
          end
        end
        object tsOrganism: TTabSheet
          Caption = 'Organism'
          ImageIndex = 6
          DesignSize = (
            363
            282)
          object bvOrganism: TBevel
            Left = 4
            Top = 4
            Width = 357
            Height = 273
            Anchors = [akLeft, akTop, akRight, akBottom]
            Shape = bsFrame
          end
          object Label1: TLabel
            Left = 16
            Top = 24
            Width = 47
            Height = 13
            Caption = 'Organism '
          end
          object Label2: TLabel
            Left = 16
            Top = 56
            Width = 78
            Height = 13
            Caption = 'Organism Parent'
          end
          object Label3: TLabel
            Left = 16
            Top = 88
            Width = 65
            Height = 13
            Caption = 'Taxon Group '
          end
          object lblInfo: TLabel
            Left = 24
            Top = 128
            Width = 283
            Height = 52
            Caption = 
              'Organism is the recommended name for the taxon. For user added t' +
              'axa the Organism Parent may be changed. Taxon Group is normally ' +
              'system supplied, but for user added taxa it will be the same as ' +
              'the parent.    '
            WordWrap = True
          end
          object lblOrganismName: TLabel
            Left = 128
            Top = 24
            Width = 193
            Height = 25
            AutoSize = False
            Caption = 'LblOrganism'
          end
          object lblTaxonGroup: TLabel
            Left = 128
            Top = 88
            Width = 217
            Height = 13
            AutoSize = False
            Caption = 'lblTaxonGroup'
          end
          object edParent: TEdit
            Left = 128
            Top = 56
            Width = 193
            Height = 21
            ReadOnly = True
            TabOrder = 0
            OnClick = edParentClick
            OnKeyDown = edParentKeyDown
          end
          object btnParent: TButton
            Left = 328
            Top = 56
            Width = 25
            Height = 25
            Caption = '---'
            TabOrder = 1
            Visible = False
            OnClick = btnParentClick
          end
        end
        object tsStatuses: TTabSheet
          BorderWidth = 4
          Caption = 'Statuses'
          ImageIndex = 1
          object splStatuses: TSplitter
            Left = 0
            Top = 76
            Width = 355
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbStatusDetails: TGroupBox
            Left = 0
            Top = 79
            Width = 355
            Height = 195
            Align = alBottom
            Caption = 'Details'
            Constraints.MinHeight = 195
            TabOrder = 0
            DesignSize = (
              355
              195)
            object lblStatusType: TLabel
              Left = 10
              Top = 16
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object lblStatusFrom: TLabel
              Left = 10
              Top = 40
              Width = 26
              Height = 13
              Caption = 'From:'
            end
            object lblStatusTo: TLabel
              Left = 228
              Top = 40
              Width = 16
              Height = 13
              Caption = 'To:'
            end
            object lblStatusGeoArea: TLabel
              Left = 10
              Top = 64
              Width = 83
              Height = 13
              Caption = 'Geographic Area:'
            end
            object lblStatusConstraints: TLabel
              Left = 10
              Top = 88
              Width = 55
              Height = 13
              Caption = 'Constraints:'
            end
            object lblStatusDetails: TLabel
              Left = 10
              Top = 112
              Width = 35
              Height = 13
              Caption = 'Details:'
            end
            object Shape4: TShape
              Tag = 2
              Left = 99
              Top = 134
              Width = 227
              Height = 23
              Anchors = [akLeft, akRight, akBottom]
              Pen.Color = clRed
            end
            object lblGeneralReference: TLabel
              Left = 10
              Top = 138
              Width = 52
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Document:'
            end
            object btnStatusCancel: TImageListButton
              Left = 323
              Top = 163
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              TabOrder = 9
              OnClick = btnStatusCancelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object btnStatusOK: TImageListButton
              Left = 299
              Top = 163
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              TabOrder = 8
              OnClick = btnStatusOKClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object btnStatusesReferenceFind: TImageListButton
              Left = 326
              Top = 134
              Width = 20
              Height = 22
              Hint = 'Get document'
              Anchors = [akRight, akBottom]
              TabOrder = 7
              OnClick = btnStatusesReferenceFindClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 5
            end
            object eStatusGeoArea: TEdit
              Left = 100
              Top = 60
              Width = 247
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 3
            end
            object eStatusConstraints: TEdit
              Left = 100
              Top = 84
              Width = 247
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 4
            end
            object reStatusDetails: TRichEdit
              Left = 100
              Top = 108
              Width = 247
              Height = 21
              Anchors = [akLeft, akTop, akRight, akBottom]
              PopupMenu = dmFormActions.pmRTF
              ScrollBars = ssVertical
              TabOrder = 5
              OnEnter = reEnterRTF
              OnExit = reExitRTF
            end
            object eStatusFrom: TEdit
              Left = 100
              Top = 36
              Width = 93
              Height = 21
              TabOrder = 1
              OnExit = eStatusFromExit
            end
            object eStatusTo: TEdit
              Left = 250
              Top = 36
              Width = 94
              Height = 21
              TabOrder = 2
              OnExit = eStatusToExit
            end
            object eStatusReference: TEdit
              Tag = 1
              Left = 100
              Top = 135
              Width = 225
              Height = 21
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 6
              OnDblClick = eStatusReferenceDblClick
              OnKeyPress = eStatusReferenceKeyPress
            end
            object cmbStatusType: TDBListCombo
              Left = 100
              Top = 12
              Width = 247
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              ItemHeight = 0
              Sorted = True
              TabOrder = 0
              ListField = 'SHORT_NAME'
              KeyField = 'TAXON_DESIGNATION_TYPE_KEY'
              Active = False
              EmptyItem = False
              ReadOnly = False
            end
          end
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 355
            Height = 76
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              355
              76)
            object sgStatuses: TStringGrid
              Left = 0
              Top = 0
              Width = 331
              Height = 72
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 3
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgStatusesClick
              ColWidths = (
                75
                114
                110)
            end
            object btnStatusAdd: TImageListButton
              Left = 331
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new status'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = btnStatusAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
            object btnStatusEdit: TImageListButton
              Left = 331
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected status'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = btnStatusEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object btnStatusDel: TImageListButton
              Left = 331
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected status'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = btnStatusDelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
          end
        end
        object tsFacts: TTabSheet
          BorderWidth = 4
          Caption = 'Facts'
          ImageIndex = 2
          object splFacts: TSplitter
            Left = 0
            Top = 90
            Width = 355
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            AutoSnap = False
            Beveled = True
            MinSize = 73
          end
          object gbFactDetails: TGroupBox
            Left = 0
            Top = 93
            Width = 355
            Height = 181
            Align = alBottom
            Caption = 'Details:'
            Constraints.MinHeight = 147
            TabOrder = 0
            DesignSize = (
              355
              181)
            object Shape1: TShape
              Tag = 2
              Left = 69
              Top = 123
              Width = 257
              Height = 23
              Anchors = [akLeft, akRight, akBottom]
              Pen.Color = clRed
            end
            object lblFactsReference: TLabel
              Left = 10
              Top = 127
              Width = 52
              Height = 13
              Anchors = [akLeft, akBottom]
              Caption = 'Document:'
            end
            object lblFactTitle: TLabel
              Left = 10
              Top = 16
              Width = 23
              Height = 13
              Caption = 'Title:'
            end
            object lblFactType: TLabel
              Left = 10
              Top = 40
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object lblFact: TLabel
              Left = 10
              Top = 60
              Width = 36
              Height = 26
              Caption = 'Facts: (HTML)'
              WordWrap = True
            end
            object lblFactDate: TLabel
              Left = 192
              Top = 40
              Width = 26
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Date:'
            end
            object btnFactOK: TImageListButton
              Left = 299
              Top = 151
              Width = 24
              Height = 23
              Hint = 'Accept changes'
              Anchors = [akRight, akBottom]
              TabOrder = 7
              OnClick = btnFactOKClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 6
            end
            object btnFactCancel: TImageListButton
              Left = 323
              Top = 151
              Width = 24
              Height = 23
              Hint = 'Discard changes'
              Anchors = [akRight, akBottom]
              TabOrder = 6
              OnClick = btnFactCancelClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 7
            end
            object btnFactReferenceFind: TImageListButton
              Left = 326
              Top = 124
              Width = 20
              Height = 21
              Hint = 'Get document'
              Anchors = [akRight, akBottom]
              TabOrder = 5
              OnClick = btnFactReferenceFindClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 5
            end
            object eFactReference: TEdit
              Tag = 1
              Left = 70
              Top = 124
              Width = 255
              Height = 21
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 4
              OnDblClick = eFactReferenceDblClick
              OnKeyPress = eFactReferenceKeyPress
            end
            object cmbFactType: TComboBox
              Left = 48
              Top = 36
              Width = 85
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              ItemHeight = 13
              Sorted = True
              TabOrder = 1
              Items.Strings = (
                'AVI Movie'
                'Bitmap Image'
                'JPEG Image'
                'Text'
                'WAV Sound')
            end
            object eFactTitle: TEdit
              Left = 48
              Top = 12
              Width = 299
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
            end
            object vdeFactDate: TVagueDateEdit
              Left = 224
              Top = 36
              Width = 123
              Height = 21
              Anchors = [akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clGreen
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              TabOrder = 2
              OnExit = vdeFactDateExit
            end
            object reFact: TMemo
              Left = 48
              Top = 60
              Width = 299
              Height = 58
              Anchors = [akLeft, akTop, akRight, akBottom]
              ScrollBars = ssVertical
              TabOrder = 3
            end
          end
          object pnlFactsTop: TPanel
            Left = 0
            Top = 0
            Width = 355
            Height = 90
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              355
              90)
            object sgFacts: TStringGrid
              Left = 0
              Top = 0
              Width = 331
              Height = 86
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 3
              DefaultRowHeight = 18
              FixedCols = 0
              RowCount = 2
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goRowSelect]
              TabOrder = 0
              OnClick = sgFactsClick
              ColWidths = (
                64
                70
                191)
            end
            object btnFactDelete: TImageListButton
              Left = 331
              Top = 46
              Width = 24
              Height = 23
              Hint = 'Delete selected fact'
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = btnFactDeleteClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 4
            end
            object btnFactEdit: TImageListButton
              Left = 331
              Top = 23
              Width = 24
              Height = 23
              Hint = 'Edit selected fact'
              Anchors = [akTop, akRight]
              TabOrder = 2
              OnClick = btnFactEditClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 3
            end
            object btnFactAdd: TImageListButton
              Left = 331
              Top = 0
              Width = 24
              Height = 23
              Hint = 'Add new fact'
              Anchors = [akTop, akRight]
              TabOrder = 3
              OnClick = btnFactAddClick
              ImageList = dmFormActions.ilButtons
              ImageIndex = 2
            end
          end
        end
        object tsSources: TTabSheet
          Caption = 'Sources'
          ImageIndex = 3
          object Sources: TSources
            Left = 4
            Top = 4
            Width = 355
            Height = 273
            SourceCol = clBlue
            DestCol = clRed
            TabOrder = 0
            DesignSize = (
              355
              273)
          end
        end
        object tsTaxonGroups: TTabSheet
          Caption = 'Ad hoc Group'
          ImageIndex = 5
          object Splitter1: TSplitter
            Left = 0
            Top = 173
            Width = 363
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            Beveled = True
          end
          object pnlArbitraryGroup: TPanel
            Left = 0
            Top = 0
            Width = 363
            Height = 173
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              363
              173)
            object shpTaxa: TShape
              Tag = 3
              Left = 3
              Top = 32
              Width = 357
              Height = 138
              Anchors = [akLeft, akTop, akRight, akBottom]
              Brush.Style = bsClear
              Pen.Color = clRed
            end
            object lblArbitraryGroup: TLabel
              Left = 4
              Top = 16
              Width = 211
              Height = 13
              Caption = 'Arbitrary taxa in this ad hoc taxonomic group:'
            end
            object lbArbitraryGroup: TListBox
              Left = 4
              Top = 33
              Width = 355
              Height = 136
              Style = lbOwnerDrawFixed
              Anchors = [akLeft, akTop, akRight, akBottom]
              DragMode = dmAutomatic
              ItemHeight = 14
              MultiSelect = True
              TabOrder = 0
              OnDrawItem = lbArbitraryGroupDrawItem
              OnKeyDown = lbArbitraryGroupKeyDown
            end
            object btnLoadFromRucksack: TBitBtn
              Left = 271
              Top = 4
              Width = 89
              Height = 25
              Anchors = [akTop, akRight]
              Caption = '&Use Rucksack'
              PopupMenu = pmRucksacks
              TabOrder = 1
              OnClick = btnLoadFromRucksackClick
            end
          end
          object pnlSystemGroup: TPanel
            Left = 0
            Top = 176
            Width = 363
            Height = 106
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              363
              106)
            object lblSystemGroup: TLabel
              Left = 4
              Top = 6
              Width = 250
              Height = 13
              Caption = 'System Supplied taxa contained in this ad hoc group:'
            end
            object lbSystemGroup: TListBox
              Left = 4
              Top = 21
              Width = 355
              Height = 76
              Style = lbOwnerDrawFixed
              Anchors = [akLeft, akTop, akRight, akBottom]
              Color = clBtnFace
              DragMode = dmAutomatic
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ItemHeight = 13
              MultiSelect = True
              ParentFont = False
              TabOrder = 0
              OnDrawItem = lbArbitraryGroupDrawItem
            end
          end
        end
      end
    end
  end
  object pnlButtons: TPanel [4]
    Left = 0
    Top = 414
    Width = 847
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object btnEdit: TImageListButton
      Left = 88
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Edit the selected taxon'
      Caption = 'Ed&it'
      TabOrder = 1
      OnClick = btnEditClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 3
    end
    object btnAdd: TImageListButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Add a new taxon to the selected list'
      Caption = '&Add'
      TabOrder = 0
      OnClick = btnAddClick
      ImageList = dmFormActions.ilMenuOn
      ImageIndex = 2
    end
    object btnDelete: TImageListButton
      Left = 168
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Delete the selected taxon'
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteClick
      ImageList = dmFormActions.ilButtons
      ImageIndex = 4
    end
  end
  inherited mnuChildMerge: TMainMenu
    inherited mnuEdit: TMenuItem
      object mnuEditAdd: TMenuItem [0]
        Caption = '&Add'
        Hint = 'Add a new taxon to the selected list'
        object mnuEditAddSibling: TMenuItem
          Action = actAddSibling
        end
        object mnuEditAddChild: TMenuItem
          Action = actAddChild
        end
      end
      object mnuEditEdit: TMenuItem [1]
        Caption = '&Edit'
        Hint = 'Edit the selected taxon'
        OnClick = btnEditClick
      end
      object mnuEditDelete: TMenuItem [2]
        Caption = '&Delete'
        Hint = 'Delete the selected taxon'
        OnClick = btnDeleteClick
      end
      object N2: TMenuItem [3]
        Caption = '-'
      end
      object mnuEditCut: TMenuItem [4]
        Action = dmFormActions.actCut
      end
      object mnuEditPaste: TMenuItem [6]
        Action = dmFormActions.actPaste
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuEditBold: TMenuItem
        Action = dmFormActions.actBold
      end
      object mnuEditItalic: TMenuItem
        Action = dmFormActions.actItalic
      end
      object mnuEditUnderline: TMenuItem
        Action = dmFormActions.actUnderline
      end
    end
  end
  inherited alTaxonDict: TActionList
    object actAddSibling: TAction [0]
      Caption = '&Sibling'
      Hint = 'Add a taxon on the same level as the selected one'
      OnExecute = actAddSiblingExecute
    end
    object actAddChild: TAction [1]
      Caption = '&Child'
      Hint = 'Add a taxon as a child to the same level as the selected one'
      OnExecute = actAddChildExecute
    end
  end
  inherited pmHierarchy: TPopupMenu
    object pmHAdd: TMenuItem [0]
      Caption = '&Add'
      object pmHAddSibling: TMenuItem
        Action = actAddSibling
      end
      object pmHAddChild: TMenuItem
        Action = actAddChild
      end
    end
  end
  object pmAdd: TPopupMenu
    Left = 44
    Top = 268
    object pmAddSibling: TMenuItem
      Action = actAddSibling
    end
    object pmAddChild: TMenuItem
      Action = actAddChild
    end
  end
  object pmTaxonNames: TPopupMenu
    Left = 140
    Top = 268
    object pmTaxonNamesInsert: TMenuItem
      Caption = '&Insert New Taxon Name'
      ShortCut = 45
      OnClick = pmTaxonNamesInsertClick
    end
    object pmTaxonNamesRename: TMenuItem
      Caption = '&Rename Taxon Name'
      OnClick = pmTaxonNamesRenameClick
    end
    object pmTaxonNameDelete: TMenuItem
      Caption = '&Delete Taxon Name'
      ShortCut = 46
      OnClick = pmTaxonNameDeleteClick
    end
  end
  object pmRucksacks: TPopupMenu
    AutoHotkeys = maManual
    Left = 240
    Top = 242
  end
end
