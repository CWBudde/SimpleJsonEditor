unit JsonEditor.Main;

interface

uses
  (* Delphi *)
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.ActnList, Vcl.StdActns, Vcl.ComCtrls,

  (* SynEdit *)
  SynEdit, SynHighlighterJSON, SynEditMiscClasses, SynEditPrint, SynEditSearch,
  SynEditPlugins, SynMacroRecorder, SynEditExport, SynExportHTML,
  SynEditOptionsDialog,

  (* DWS *)
  dwsJSON, dwsJSONConnector,

  (* Virtual Treeview *)
  VirtualTrees,

  (* Custom *)
  JsonEditor.EditLink;

type
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
    ActionList: TActionList;
    ActionSearchFind: TSearchFind;
    ActionSearchFindNext: TSearchFindNext;
    ActionSearchReplace: TSearchReplace;
    ActionViewEditor: TAction;
    ActionViewTree: TAction;
    MainMenu: TMainMenu;
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
    MenuItemSearch: TMenuItem;
    MenuItemSearchFind: TMenuItem;
    MenuItemSearchFindNext: TMenuItem;
    MenuItemSearchReplace: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemViewEditor: TMenuItem;
    MenuItemViewTree: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit;
    SynEditPrint: TSynEditPrint;
    SynEditSearch: TSynEditSearch;
    SynExporterHTML: TSynExporterHTML;
    SynMacroRecorder: TSynMacroRecorder;
    TreeItems: TVirtualStringTree;
    MenuItemTools: TMenuItem;
    MenuItemToolsPreferences: TMenuItem;
    ActionToolsPreferences: TAction;
    ActionHelpAbout: TAction;
    MenuItemHelp: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    SynEditOptionsDialog: TSynEditOptionsDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileSaveAsAccept(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionFileSaveUpdate(Sender: TObject);
    procedure ActionFileExportAsAccept(Sender: TObject);
    procedure ActionFilePrintExecute(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
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
    procedure ActionToolsPreferencesExecute(Sender: TObject);
  private
    FHighlighter: TSynJSON;
    FCurrentFileName: TFileName;
    FBase: TdwsJSONValue;
    procedure SetCurrentFileName(const Value: TFileName);
  public
    procedure BuildTree;

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

const
  CBaseCaption = 'Simple JSON Editor';
  CRegistryKey = 'Software\SimpleJsonEditor\';

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FHighlighter := TSynJSON.Create(SynEdit);
  SynEdit.Highlighter := FHighlighter;

  TreeItems.NodeDataSize := SizeOf(TJsonNode);

  BuildTree;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TreeItems.Clear;
  FBase.Free;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  FileName: TFileName;
begin
  with TRegistry.Create do
  try
    if OpenKey(CRegistryKey, False) then
    begin
      if ValueExists('Recent') then
      begin
        FileName := ReadString('Recent');
        if FileExists(FileName) then
          LoadFromFile(FileName);
      end;

      if ValueExists('CaretX') then
        SynEdit.CaretX := ReadInteger('CaretX');
      if ValueExists('CaretY') then
        SynEdit.CaretY := ReadInteger('CaretY');
    end;
    CloseKey;
  finally
    Free;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TRegistry.Create do
  try
    if OpenKey(CRegistryKey, True) then
    begin
      WriteString('Recent', CurrentFileName);
      WriteInteger('CaretX', SynEdit.CaretX);
      WriteInteger('CaretY', SynEdit.CaretY);
    end;
    CloseKey;
  finally
    Free;
  end;
end;

procedure TFormMain.LoadFromFile(FileName: TFileName);
begin
  SynEdit.Text := LoadTextFromFile(FileName);
  CurrentFileName := FileName;
  UpdateCaption;
  BuildTree;
end;

procedure TFormMain.SaveToFile(FileName: TFileName);
begin
  SaveTextToUTF8File(FileName, SynEdit.Text);

  CurrentFileName := FileName;
end;

procedure TFormMain.SetCurrentFileName(const Value: TFileName);
begin
  if FCurrentFileName <> Value then
  begin
    FCurrentFileName := Value;
    if FCurrentFileName <> '' then
      ActionFileExportAs.Dialog.FileName := ChangeFileExt(FCurrentFileName, '.html')
    else
      ActionFileExportAs.Dialog.FileName := '';
  end;
end;

procedure TFormMain.SynEditChange(Sender: TObject);
begin
  if TreeItems.Visible then
    BuildTree;
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

procedure TFormMain.TreeItemsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  if SynEdit.Visible then
    SynEdit.Text := FBase.ToBeautifiedString;
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
          end;
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
  SynEdit.Clear;
  CurrentFileName := '';
  UpdateCaption;
  BuildTree;
end;

procedure TFormMain.ActionFileOpenAccept(Sender: TObject);
begin
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
    BuildTree;
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

procedure TFormMain.BuildTree;

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
      JsonNode^.Index := Index;
      JsonNode^.Parent := Value;
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
