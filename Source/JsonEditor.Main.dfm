object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Simple JSON Editor'
  ClientHeight = 640
  ClientWidth = 1028
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 694
    Top = 0
    Height = 621
    Align = alRight
  end
  object SynEdit: TSynEdit
    Left = 0
    Top = 0
    Width = 694
    Height = 621
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    SearchEngine = SynEditSearch
    TabWidth = 2
    OnChange = SynEditChange
    OnStatusChange = SynEditStatusChange
    FontSmoothing = fsmNone
  end
  object TreeItems: TVirtualStringTree
    Left = 697
    Top = 0
    Width = 331
    Height = 621
    Align = alRight
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 22
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.SelectionOptions = [toExtendedFocus]
    OnCreateEditor = TreeItemsCreateEditor
    OnEdited = TreeItemsEdited
    OnEditing = TreeItemsEditing
    OnGetText = TreeItemsGetText
    OnPaintText = TreeItemsPaintText
    Columns = <
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coEditable]
        Position = 0
        Width = 169
        WideText = 'Name'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coEditable]
        Position = 1
        Width = 158
        WideText = 'Value'
      end>
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 621
    Width = 1028
    Height = 19
    Panels = <
      item
        Width = 96
      end
      item
        Width = 50
      end>
  end
  object MainMenu: TMainMenu
    Left = 368
    Top = 264
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemFileNew: TMenuItem
        Action = ActionFileNew
      end
      object MenuItemFileOpen: TMenuItem
        Action = ActionFileOpen
      end
      object MenuItemFileRecent: TMenuItem
        Caption = 'Recent'
        Visible = False
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuItemFileSave: TMenuItem
        Action = ActionFileSave
      end
      object MenuItemFileSaveAs: TMenuItem
        Action = ActionFileSaveAs
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MenuItemFileExportToHTML: TMenuItem
        Action = ActionFileExportAs
      end
      object MenuItemFilePrint: TMenuItem
        Action = ActionFilePrint
        Caption = 'Print'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemFileExit: TMenuItem
        Action = ActionFileExit
      end
    end
    object MenuItemEdit: TMenuItem
      Caption = '&Edit'
      object MenuItemEditUndo: TMenuItem
        Action = ActionEditUndo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MenuItemEditCut: TMenuItem
        Action = ActionEditCut
      end
      object MenuItemEditCopy: TMenuItem
        Action = ActionEditCopy
      end
      object MenuItemEditPaste: TMenuItem
        Action = ActionEditPaste
      end
      object MenuItemEditDelete: TMenuItem
        Action = ActionEditDelete
      end
      object MenuItemEditSelectAll: TMenuItem
        Action = ActionEditSelectAll
      end
    end
    object MenuItemSearch: TMenuItem
      Caption = '&Search'
      object MenuItemSearchFind: TMenuItem
        Action = ActionSearchFind
      end
      object MenuItemSearchFindNext: TMenuItem
        Action = ActionSearchFindNext
      end
      object MenuItemSearchReplace: TMenuItem
        Action = ActionSearchReplace
      end
    end
    object MenuItemView: TMenuItem
      Caption = '&View'
      object MenuItemViewEditor: TMenuItem
        Action = ActionViewEditor
        AutoCheck = True
      end
      object MenuItemViewTree: TMenuItem
        Action = ActionViewTree
        AutoCheck = True
      end
    end
    object MenuItemTools: TMenuItem
      Caption = '&Tools'
      object MenuItemToolsPreferences: TMenuItem
        Action = ActionToolsPreferences
      end
    end
    object MenuItemHelp: TMenuItem
      Caption = '&Help'
      object MenuItemHelpAbout: TMenuItem
        Action = ActionHelpAbout
      end
    end
  end
  object ActionList: TActionList
    Left = 272
    Top = 264
    object ActionFileNew: TAction
      Category = 'File'
      Caption = '&New'
      OnExecute = ActionFileNewExecute
    end
    object ActionFileOpen: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.DefaultExt = '.json'
      Dialog.Filter = 'JSON (*.json)|*.json'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
      OnAccept = ActionFileOpenAccept
    end
    object ActionFileSaveAs: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Dialog.DefaultExt = '.json'
      Dialog.Filter = 'JSON (*.json)|*.json'
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 30
      OnAccept = ActionFileSaveAsAccept
    end
    object ActionEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object ActionEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object ActionEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object ActionEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object ActionEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object ActionEditDelete: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
    object ActionSearchFind: TSearchFind
      Category = 'Search'
      Caption = '&Find...'
      Hint = 'Find|Finds the specified text'
      ImageIndex = 34
      ShortCut = 16454
    end
    object ActionSearchFindNext: TSearchFindNext
      Category = 'Search'
      Caption = 'Find &Next'
      Hint = 'Find Next|Repeats the last find'
      ImageIndex = 33
      ShortCut = 114
    end
    object ActionSearchReplace: TSearchReplace
      Category = 'Search'
      Caption = '&Replace'
      Hint = 'Replace|Replaces specific text with different text'
      ImageIndex = 32
    end
    object ActionFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      OnExecute = ActionFileSaveExecute
      OnUpdate = ActionFileSaveUpdate
    end
    object ActionFileExportAs: TFileSaveAs
      Category = 'File'
      Caption = '&Export to HTML...'
      Dialog.DefaultExt = '.html'
      Dialog.Filter = 'HTML (*.html)|*.html'
      Hint = 'Export As|Exports the active file as HTML'
      ImageIndex = 30
      OnAccept = ActionFileExportAsAccept
    end
    object ActionFilePrint: TAction
      Category = 'File'
      Caption = 'Print...'
      OnExecute = ActionFilePrintExecute
    end
    object ActionFileExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
    object ActionViewTree: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Tree'
      Checked = True
      OnExecute = ActionViewTreeExecute
    end
    object ActionViewEditor: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Editor'
      Checked = True
      OnExecute = ActionViewEditorExecute
    end
    object ActionToolsPreferences: TAction
      Category = 'Tools'
      Caption = '&Preferences'
      OnExecute = ActionToolsPreferencesExecute
    end
    object ActionHelpAbout: TAction
      Category = 'Help'
      Caption = '&About'
      OnExecute = ActionHelpAboutExecute
    end
  end
  object SynEditSearch: TSynEditSearch
    Left = 272
    Top = 320
  end
  object SynMacroRecorder: TSynMacroRecorder
    Editor = SynEdit
    RecordShortCut = 24658
    PlaybackShortCut = 24656
    Left = 368
    Top = 320
  end
  object SynExporterHTML: TSynExporterHTML
    Color = clWindow
    DefaultFilter = 'HTML Documents (*.htm;*.html)|*.htm;*.html'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Title = 'Untitled'
    UseBackground = False
    Left = 272
    Top = 376
  end
  object SynEditPrint: TSynEditPrint
    Copies = 1
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Arial'
    Header.DefaultFont.Style = []
    Footer.DefaultFont.Charset = DEFAULT_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Arial'
    Footer.DefaultFont.Style = []
    Margins.Left = 25.000000000000000000
    Margins.Right = 15.000000000000000000
    Margins.Top = 25.000000000000000000
    Margins.Bottom = 25.000000000000000000
    Margins.Header = 15.000000000000000000
    Margins.Footer = 15.000000000000000000
    Margins.LeftHFTextIndent = 2.000000000000000000
    Margins.RightHFTextIndent = 2.000000000000000000
    Margins.HFInternalMargin = 0.500000000000000000
    Margins.MirrorMargins = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    TabWidth = 8
    Color = clWhite
    Left = 368
    Top = 376
  end
  object SynEditOptionsDialog: TSynEditOptionsDialog
    UseExtendedStrings = False
    Left = 272
    Top = 432
  end
end
