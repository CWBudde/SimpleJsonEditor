unit JsonEditor.Main;

interface

uses
  (* Delphi *)
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.ActnList, Vcl.StdActns, Vcl.ComCtrls, Vcl.ImgList, Vcl.ToolWin,

  (* SynEdit *)
  SynEdit, SynHighlighterJSON, SynEditMiscClasses, SynEditPrint, SynEditSearch,
  SynEditPlugins, SynMacroRecorder, SynEditExport, SynExportHTML,
  SynEditOptionsDialog,

  (* DWS *)
  dwsJSON, dwsJSONConnector,

  (* Virtual Treeview *)
  VirtualTrees,

  (* Custom *)
  JsonEditor.EditLink, JsonEditor.AddDialog, SynEditHighlighter, System.Actions;

type
  TVirtualStringTree = class(VirtualTrees.TVirtualStringTree)
  public
    // override the flawed standard action handling
    function ExecuteAction(Action: TBasicAction): Boolean; override;
  end;

  TFormMain = class(TForm)
    ActionEditCopy: TEditCopy;
    ActionEditCut: TEditCut;
    ActionEditDelete: TEditDelete;
    ActionEditPaste: TEditPaste;
    ActionEditSelectAll: TEditSelectAll;
    ActionEditUndo: TEditUndo;
    ActionFileExit: TFileExit;
    ActionFileExportAs: TFileSaveAs;
    ActionFileNew: TAction;
    ActionFileOpen: TFileOpen;
    ActionFilePrint: TAction;
    ActionFileSave: TAction;
    ActionFileSaveAs: TFileSaveAs;
    ActionHelpAbout: TAction;
    ActionList: TActionList;
    ActionNodeAddArray: TAction;
    ActionNodeAddObject: TAction;
    ActionNodeAddValue: TAction;
    ActionNodeDelete: TAction;
    ActionSearchFind: TSearchFind;
    ActionSearchFindNext: TSearchFindNext;
    ActionSearchReplace: TSearchReplace;
    ActionToolsPreferences: TAction;
    ActionViewEditor: TAction;
    ActionViewTree: TAction;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItemAddArray: TMenuItem;
    MenuItemAddNode: TMenuItem;
    MenuItemAddObject: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemEditCopy: TMenuItem;
    MenuItemEditCut: TMenuItem;
    MenuItemEditDelete: TMenuItem;
    MenuItemEditPaste: TMenuItem;
    MenuItemEditSelectAll: TMenuItem;
    MenuItemEditUndo: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFileExportToHTML: TMenuItem;
    MenuItemFileNew: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFilePrint: TMenuItem;
    MenuItemFileRecent: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    MenuItemSearch: TMenuItem;
    MenuItemSearchFind: TMenuItem;
    MenuItemSearchFindNext: TMenuItem;
    MenuItemSearchReplace: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemToolsPreferences: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemViewEditor: TMenuItem;
    MenuItemViewTree: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    PopupMenu: TPopupMenu;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit;
    SynEditOptionsDialog: TSynEditOptionsDialog;
    SynEditPrint: TSynEditPrint;
    SynEditSearch: TSynEditSearch;
    SynExporterHTML: TSynExporterHTML;
    SynMacroRecorder: TSynMacroRecorder;
    ToolBar: TToolBar;
    ToolButtonAbout: TToolButton;
    ToolButtonCopy: TToolButton;
    ToolButtonCut: TToolButton;
    ToolButtonDelete: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButtonExport: TToolButton;
    ToolButtonFind: TToolButton;
    ToolButtonFindNext: TToolButton;
    ToolButtonFindReplace: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonPaste: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonSelectAll: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonSeparator2: TToolButton;
    ToolButtonSeparator3: TToolButton;
    ToolButtonUndo: TToolButton;
    TreeItems: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileExportAsAccept(Sender: TObject);
    procedure ActionFilePrintExecute(Sender: TObject);
    procedure ActionFileSaveAsAccept(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionFileSaveUpdate(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionNodeAddObjectArrayExecute(Sender: TObject);
    procedure ActionNodeAddValueExecute(Sender: TObject);
    procedure ActionNodeDeleteExecute(Sender: TObject);
    procedure ActionToolsPreferencesExecute(Sender: TObject);
    procedure ActionViewEditorExecute(Sender: TObject);
    procedure ActionViewTreeExecute(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure TreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeItemsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure TreeItemsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure TreeItemsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeItemsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure TreeItemsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FHighlighter: TSynJSONSyn;
    FCurrentFileName: TFileName;
    FRecentFiles: TStringList;
    FFormAdd: TFormAddDialog;
    FBase: TdwsJSONValue;
    procedure SetCurrentFileName(const Value: TFileName);
    procedure MenuItemRecentClicked(Sender: TObject);
  public
    procedure UpdateTree;
    procedure UpdateEditor;

    procedure RebuildJSON;

    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);

    procedure UpdateCaption;

    property CurrentFileName: TFileName read FCurrentFileName write SetCurrentFileName;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  dwsXPlatform, System.Win.Registry, JsonEditor.About;

resourcestring
  RStrFileModified = 'The file has been modified.';
  RStrNeedSave = 'Do you want to saved it first?';

const
  CBaseCaption = 'Simple JSON Editor';
  CRegistryKey = 'Software\SimpleJsonEditor\';

{ TVirtualStringTree }

function TVirtualStringTree.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := True;

  if Action is TEditDelete then
    FormMain.ActionNodeDelete.Execute
  else
    Result := inherited ExecuteAction(Action);
end;


{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FHighlighter := TSynJSONSyn.Create(SynEdit);
  SynEdit.Highlighter := FHighlighter;

  TreeItems.NodeDataSize := SizeOf(TJsonNode);

  FRecentFiles := TStringList.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FRecentFiles.Free;
  TreeItems.Clear;
  FFormAdd.Free;
  FBase.Free;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  FileName: TFileName;
  Index: Integer;
  MenuItem: TMenuItem;
begin
  with TRegistry.Create do
  try
    if OpenKeyReadOnly(CRegistryKey) then
    begin
      // read recent file (if present)
      if ValueExists('RecentFile') then
      begin
        FileName := ReadString('RecentFile');
        if FileExists(FileName) then
          LoadFromFile(FileName);
      end;

      // read recent caret position
      if ValueExists('CaretX') then
        SynEdit.CaretX := ReadInteger('CaretX');
      if ValueExists('CaretY') then
        SynEdit.CaretY := ReadInteger('CaretY');

      // read recent file list
      FRecentFiles.Clear;
      if KeyExists('Recent') then
      begin
        OpenKeyReadOnly('Recent');
        Index := 0;
        while ValueExists(IntToStr(Index)) do
        begin
          FRecentFiles.Add(ReadString(IntToStr(Index)));
          Inc(Index);
        end;
      end;
    end;
    CloseKey;
  finally
    Free;
  end;

  // only allow 10 entries
  while FRecentFiles.Count > 10 do
    FRecentFiles.Delete(10);

  for Index := 0 to FRecentFiles.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(MenuItemFileRecent);
    MenuItem.Caption := ExtractFileName(FRecentFiles[Index]);
    MenuItem.Tag := Index;
    MenuItem.OnClick := MenuItemRecentClicked;
    MenuItemFileRecent.Add(MenuItem);
  end;
  MenuItemFileRecent.Visible := MenuItemFileRecent.Count > 0;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Index: Integer;
begin
  with TRegistry.Create do
  try
    if OpenKey(CRegistryKey, True) then
    begin
      WriteString('RecentFile', CurrentFileName);
      WriteInteger('CaretX', SynEdit.CaretX);
      WriteInteger('CaretY', SynEdit.CaretY);

      if FRecentFiles.Count > 0 then
      begin
        OpenKey('Recent', True);

        for Index := 0 to FRecentFiles.Count - 1 do
          WriteString(IntToStr(Index), FRecentFiles[Index]);
      end;
    end;
    CloseKey;
  finally
    Free;
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if SynEdit.Modified then
    case MessageDlg(RStrFileModified + #13#10#13#10 + RStrNeedSave,
      mtConfirmation, [mbYes, mbNo, mbAbort], 0) of
      mrYes:
        ActionFileSaveAs.Execute;
      mrAbort:
        CanClose := False;
    end;
end;

procedure TFormMain.LoadFromFile(FileName: TFileName);
begin
  SynEdit.Text := LoadTextFromFile(FileName);
  SynEdit.Modified := False;
  CurrentFileName := FileName;
  UpdateCaption;
  UpdateTree;
end;

procedure TFormMain.MenuItemRecentClicked(Sender: TObject);
var
  FileName: TFileName;
begin
  FileName := FRecentFiles[TMenuItem(Sender).Tag];
  if FileExists(FileName) then
    LoadFromFile(FileName);
end;

procedure TFormMain.RebuildJSON;
var
  NewJSON: TdwsJSONObject;

  procedure IterateNodes(ParentNode: PVirtualNode; ParentValue: TdwsJSONValue);
  var
    Node: PVirtualNode;
    JsonNode: PJsonNode;
    NewValue: TdwsJSONValue;
  begin
    for Node in TreeItems.ChildNodes(ParentNode) do
    begin
      JsonNode := TreeItems.GetNodeData(Node);

      // handle leafs
      if JsonNode.Value is TdwsJSONImmediate then
        NewValue := JsonNode^.Value.Clone
      else
        NewValue := TdwsJSONValue(JsonNode.Value.ClassType.Create);

      Assert(ParentValue is TdwsJSONValue);
      Assert(not (ParentValue is TdwsJSONImmediate));
      if ParentValue is TdwsJSONArray then
        TdwsJSONArray(ParentValue).Add(NewValue)
      else
        TdwsJSONObject(ParentValue).Add(JsonNode^.Name, NewValue);

      IterateNodes(Node, NewValue);
      JsonNode^.Value := NewValue;
    end;
  end;

begin
  NewJSON := TdwsJSONObject.Create;
  IterateNodes(TreeItems.RootNode, NewJson);
  FBase.Free;
  FBase := NewJSON;
end;

procedure TFormMain.SaveToFile(FileName: TFileName);
begin
  SaveTextToUTF8File(FileName, SynEdit.Text);
  SynEdit.Modified := False;
  CurrentFileName := FileName;
end;

procedure TFormMain.SetCurrentFileName(const Value: TFileName);
var
  Index: Integer;
begin
  if FCurrentFileName <> Value then
  begin
    FCurrentFileName := Value;
    if FCurrentFileName <> '' then
    begin
      ActionFileExportAs.Dialog.FileName := ChangeFileExt(FCurrentFileName, '.html');
      Index := FRecentFiles.IndexOf(FCurrentFileName);
      if Index >= 0 then
        FRecentFiles.Delete(Index);

      FRecentFiles.Insert(0, FCurrentFileName);
    end
    else
      ActionFileExportAs.Dialog.FileName := '';
  end;
end;

procedure TFormMain.SynEditChange(Sender: TObject);
begin
  if TreeItems.Visible then
    UpdateTree;
end;

procedure TFormMain.SynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if [scCaretX, scCaretY] * Changes <> [] then
    StatusBar.Panels[0].Text := Format('X: %d Y: %d',
      [SynEdit.CaretX, SynEdit.CaretY]);
end;

procedure TFormMain.TreeItemsCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TPropertyEditLink.Create;
end;

procedure TFormMain.TreeItemsEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  UpdateEditor;
end;

procedure TFormMain.TreeItemsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  JsonNode: PJsonNode;
begin
  with Sender do
  begin
    JsonNode := GetNodeData(Node);
    Allowed := (Column = 0) or (JsonNode.Value is TdwsJSONImmediate);
  end;
end;

procedure TFormMain.TreeItemsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PJsonNode;
begin
  NodeData := Sender.GetNodeData(Node);
  Finalize(NodeData^);
end;

procedure TFormMain.TreeItemsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  JsonNode: PJsonNode;
begin
  CellText := '';
  if not Assigned(Node) then
    Exit;

  JsonNode := TreeItems.GetNodeData(Node);
  case Column of
    0:
      CellText := JsonNode^.Name;
    1:
      if JsonNode^.Value.ClassType = TdwsJSONImmediate then
        CellText := JsonNode^.Value.AsString
      else
        CellText := JsonNode^.Value.ToString;
  end;
end;

procedure TFormMain.TreeItemsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
      ActionNodeDelete.Execute;
  end;
end;

procedure TFormMain.TreeItemsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  JsonNode: PJsonNode;
begin
  if (Node = Sender.FocusedNode) and (Column = Sender.FocusedColumn) then
    Exit;

  case Column of
    0:
      TargetCanvas.Font.Color := FHighlighter.AttributeAttri.Foreground;
    1:
      begin
        JsonNode := TreeItems.GetNodeData(Node);
        if JsonNode^.Value is TdwsJSONImmediate then
          case JsonNode^.Value.Value.ValueType of
            jvtNumber:
              TargetCanvas.Font.Color := FHighlighter.NumberAttri.Foreground;
            jvtString:
              TargetCanvas.Font.Color := FHighlighter.ValueAttri.Foreground;
            jvtBoolean, jvtNull:
            begin
              TargetCanvas.Font.Color := FHighlighter.KeywordAttribute.Foreground;
              TargetCanvas.Font.Style := FHighlighter.KeywordAttribute.Style;
            end;
          end
        else
          TargetCanvas.Font.Color := clGrayText;
      end;
  end;
end;

procedure TFormMain.UpdateCaption;
begin
  if FCurrentFileName <> '' then
    Caption := CBaseCaption + ' - ' + ExtractFileName(FCurrentFileName)
  else
    Caption := CBaseCaption;
end;

procedure TFormMain.ActionFileExportAsAccept(Sender: TObject);
begin
  SynExporterHTML.Highlighter := SynEdit.Highlighter;
  SynExporterHTML.Title := ExtractFileName(FCurrentFileName);
  SynExporterHTML.ExportAsText := True;
  SynExporterHTML.ExportAll(SynEdit.Lines);
  SynExporterHTML.SaveToFile(TFileSaveAs(Sender).Dialog.FileName);
end;

procedure TFormMain.ActionFileNewExecute(Sender: TObject);
begin
  if SynEdit.Modified then
    case MessageDlg(RStrFileModified + #13#10#13#10 + RStrNeedSave,
      mtConfirmation, [mbYes, mbNo, mbAbort], 0) of
      mrYes:
        ActionFileSaveAs.Execute;
      mrAbort:
        Exit;
    end;

  SynEdit.Clear;
  SynEdit.Modified := False;
  CurrentFileName := '';
  UpdateCaption;
  UpdateTree;
end;

procedure TFormMain.ActionFileOpenAccept(Sender: TObject);
begin
  if SynEdit.Modified then
    case MessageDlg(RStrFileModified + #13#10#13#10 + RStrNeedSave,
      mtConfirmation, [mbYes, mbNo, mbAbort], 0) of
      mrYes:
        ActionFileSaveAs.Execute;
      mrAbort:
        Exit;
    end;

  LoadFromFile(TFileSaveAs(Sender).Dialog.FileName);
end;

procedure TFormMain.ActionFilePrintExecute(Sender: TObject);
begin
  SynEditPrint.Title := ExtractFileName(FCurrentFileName);
  SynEditPrint.DocTitle := SynEditPrint.Title;
  SynEditPrint.Highlighter := SynEdit.Highlighter;
  SynEditPrint.Print;
end;

procedure TFormMain.ActionFileSaveAsAccept(Sender: TObject);
begin
  SaveToFile(TFileSaveAs(Sender).Dialog.FileName);
end;

procedure TFormMain.ActionFileSaveExecute(Sender: TObject);
begin
  SaveToFile(CurrentFileName);
end;

procedure TFormMain.ActionFileSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := CurrentFileName <> '';
end;

procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
  with TFormAbout.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormMain.ActionNodeAddObjectArrayExecute(Sender: TObject);

  function CreateArrayOrObject: TdwsJSONValue;
  begin
    Result := nil;
    Assert(TAction(Sender).Tag in [1, 2]);
    case TAction(Sender).Tag of
      1:
        Result := TdwsJSONArray.Create;
      2:
        Result := TdwsJSONObject.Create;
    end;
  end;

var
  Node: PVirtualNode;
  JsonNode: PJsonNode;
  Value, ParentValue: TdwsJSONValue;
begin
  Node := TreeItems.FocusedNode;
  if Node = nil then
  begin
    Node := TreeItems.RootNode;
    ParentValue := FBase;
  end
  else
  begin
    JsonNode := TreeItems.GetNodeData(Node);
    ParentValue := JsonNode.Value;

    // check for immediate value
    if ParentValue is TdwsJSONImmediate then
    begin
      Node := Node.Parent;
      JsonNode := TreeItems.GetNodeData(Node);
      if Assigned(JsonNode) then
        ParentValue := JsonNode.Value
      else
        ParentValue := FBase;
    end;
  end;

  // handle array parent value
  if ParentValue is TdwsJSONArray then
  begin
    Value := CreateArrayOrObject;
    TdwsJSONArray(ParentValue).Add(Value);

    // add node and link node to object
    Node := TreeItems.AddChild(Node);
    JsonNode := TreeItems.GetNodeData(Node);
    JsonNode^.Value := Value;
    JsonNode^.Name := IntToStr(TdwsJSONArray(Parent).ElementCount - 1);

    UpdateEditor;
    Exit;
  end;

  // the parent value must be an object
  Assert(ParentValue is TdwsJSONObject);

  // eventually create add dialog
  if not Assigned(FFormAdd) then
    FFormAdd := TFormAddDialog.Create(Self);

  with FFormAdd do
  begin
    PanelName.Visible := True;
    PanelValue.Visible := False;
    ComboBoxName.Text := '';
    ComboBoxValue.Text := '';

    if ShowModal = mrOk then
    begin
      Value := CreateArrayOrObject;
      TdwsJSONObject(ParentValue).Add(ComboBoxName.Text, Value);

      // add node and link node to object
      Node := TreeItems.AddChild(Node);
      JsonNode := TreeItems.GetNodeData(Node);
      JsonNode^.Value := Value;
      JsonNode^.Name := ComboBoxName.Text;
      UpdateEditor;
    end;
  end;
end;

procedure TFormMain.ActionNodeAddValueExecute(Sender: TObject);
var
  Node: PVirtualNode;
  JsonNode: PJsonNode;
  Value, ParentValue: TdwsJSONValue;
begin
  Node := TreeItems.FocusedNode;
  if Node = nil then
  begin
    Node := TreeItems.RootNode;
    ParentValue := FBase;
  end
  else
  begin
    JsonNode := TreeItems.GetNodeData(Node);
    ParentValue := JsonNode.Value;

    // check for immediate value
    if ParentValue is TdwsJSONImmediate then
    begin
      Node := Node.Parent;
      JsonNode := TreeItems.GetNodeData(Node);
      if Assigned(JsonNode) then
        ParentValue := JsonNode.Value
      else
        ParentValue := FBase;
    end;
  end;

  // eventually create add dialog
  if not Assigned(FFormAdd) then
    FFormAdd := TFormAddDialog.Create(Self);

  with FFormAdd do
  begin
    ComboBoxName.Text := '';
    ComboBoxValue.Text := '';

    PanelName.Visible := not (ParentValue is TdwsJSONArray);

    if ShowModal = mrOk then
    begin
      // add create value
      Value := TdwsJSONImmediate.Create;
      if RadioButtonString.Checked then
        Value.AsString := ComboBoxValue.Text
      else if RadioButtonNumber.Checked then
      try
        Value.AsNumber := StrToFloat(ComboBoxValue.Text)
      except
        Value.Free;
        Exit;
      end
      else if RadioButtonBoolean.Checked then
        Value.AsBoolean := Boolean(ComboBoxValue.ItemIndex)
      else
        Value.IsNull := True;

      if ParentValue is TdwsJSONArray then
        TdwsJSONArray(ParentValue).Add(Value)
      else
        TdwsJSONObject(ParentValue).Add(ComboBoxName.Text, Value);

      // add node and link node to object
      Node := TreeItems.AddChild(Node);
      JsonNode := TreeItems.GetNodeData(Node);
      JsonNode^.Value := Value;
      JsonNode^.Name := ComboBoxName.Text;
      UpdateEditor;
    end;
  end;
end;

procedure TFormMain.ActionNodeDeleteExecute(Sender: TObject);
begin
  if not Assigned(TreeItems.FocusedNode) then
    Exit;

  TreeItems.DeleteNode(TreeItems.FocusedNode);

  if SynEdit.Visible then
    UpdateEditor;
end;

procedure TFormMain.ActionToolsPreferencesExecute(Sender: TObject);
var
  SynEditorOptionsContainer: TSynEditorOptionsContainer;
begin
  SynEditorOptionsContainer := TSynEditorOptionsContainer.Create(nil);
  SynEditorOptionsContainer.Assign(SynEdit);
  SynEditOptionsDialog.Form.Position := poMainFormCenter;
  SynEditOptionsDialog.Execute(SynEditorOptionsContainer);
  SynEdit.Assign(SynEditorOptionsContainer);
end;

procedure TFormMain.ActionViewEditorExecute(Sender: TObject);
begin
  SynEdit.Visible := TAction(Sender).Checked;
  if SynEdit.Visible then
  begin
    SynEdit.Text := FBase.ToBeautifiedString;
    if TreeItems.Visible then
    begin
      TreeItems.Align := alRight;
      TreeItems.Width := 330;
      Splitter.Visible := True;
    end;
  end
  else
  if TreeItems.Visible then
  begin
    Splitter.Visible := False;
    TreeItems.Align := alClient;
  end;
end;

procedure TFormMain.ActionViewTreeExecute(Sender: TObject);
begin
  TreeItems.Visible := TAction(Sender).Checked;
  if TreeItems.Visible then
  begin
    UpdateTree;
    if SynEdit.Visible then
    begin
      TreeItems.Align := alRight;
      TreeItems.Width := 330;
      Splitter.Visible := True;
    end
    else
    begin
      Splitter.Visible := False;
      TreeItems.Align := alClient;
    end;
  end;
end;

procedure TFormMain.UpdateEditor;
begin
  RebuildJSON;
  SynEdit.Text := FBase.ToBeautifiedString;
end;

procedure TFormMain.UpdateTree;

  procedure IterateValue(Value: TdwsJSONValue; ParentNode: PVirtualNode);
  var
    Index: Integer;
    Node: PVirtualNode;
    JsonNode: PJsonNode;
  begin
    for Index := 0 to Value.ElementCount - 1 do
    begin
      Node := TreeItems.AddChild(ParentNode);
      JsonNode := TreeItems.GetNodeData(Node);
      JsonNode^.Value := Value.Elements[Index];
      JsonNode^.Name := Value.Names[Index];
      IterateValue(Value.Elements[Index], Node);
    end;
  end;

var
  Msg, Str: string;
  CharPos: Integer;
begin
  TreeItems.BeginUpdate;
  try
    TreeItems.Clear;
    StatusBar.Panels[1].Text := '';

    FreeAndNil(FBase);

    try
      FBase := TdwsJSONValue.ParseString(SynEdit.Text);
    except
      on E: EdwsJSONParseError do
      begin
        Msg := E.Message;
        StatusBar.Panels[1].Text := Msg;

        // now locate line
        CharPos := Pos('line', Msg);
        if CharPos > 0 then
        begin
          Str := '';
          CharPos := CharPos + 5;
          if CharInSet(Msg[CharPos], ['1'..'9']) then
          begin
            Str := Msg[CharPos];
            Inc(CharPos);
            while CharInSet(Msg[CharPos], ['0'..'9']) and (CharPos < Length(Msg)) do
            begin
              Str := Str + Msg[CharPos];
              Inc(CharPos);
            end;

            SynEdit.GotoLineAndCenter(StrToInt(Str));
          end;
        end;

        FBase := nil;
        Exit;
      end;
    end;

    IterateValue(FBase, TreeItems.RootNode);
  finally
    TreeItems.EndUpdate;
  end;
end;

end.
