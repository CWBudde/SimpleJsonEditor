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
    MenuItemFileSave: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    MenuItemSearch: TMenuItem;
    MenuItemSearchFind: TMenuItem;
    MenuItemSearchFindNext: TMenuItem;
    MenuItemSearchReplace: TMenuItem;
    MenuItemTreeView: TMenuItem;
    MenuItemView: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    ree1: TMenuItem;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit;
    SynEditPrint: TSynEditPrint;
    SynEditSearch: TSynEditSearch;
    SynExporterHTML: TSynExporterHTML;
    SynMacroRecorder: TSynMacroRecorder;
    TreeItems: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileSaveAsAccept(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionFileSaveUpdate(Sender: TObject);
    procedure ActionViewEditorExecute(Sender: TObject);
    procedure ActionViewTreeExecute(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure TreeItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeItemsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure TreeItemsEnter(Sender: TObject);
    procedure TreeItemsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure TreeItemsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeItemsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure ActionFilePrintExecute(Sender: TObject);
    procedure ActionFileExportAsAccept(Sender: TObject);
  private
    FHighlighter: TSynJSON;
    FFileName: TFileName;
    FBase: TdwsJSONValue;
  public
    procedure BuildTree;

    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);

    procedure UpdateCaption;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  dwsXPlatform;

const
  CBaseCaption = 'Simple JSON Editor';

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

procedure TFormMain.LoadFromFile(FileName: TFileName);
begin
  SynEdit.Text := LoadTextFromFile(FileName);
  FFileName := FileName;
  UpdateCaption;
  BuildTree;
end;

procedure TFormMain.SaveToFile(FileName: TFileName);
begin
  SaveTextToUTF8File(FileName, SynEdit.Text);

  FFileName := FileName;
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

procedure TFormMain.TreeItemsEnter(Sender: TObject);
begin
//  BuildTree;
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
  if FFileName <> '' then
    Caption := CBaseCaption + ' - ' + ExtractFileName(FFileName)
  else
    Caption := CBaseCaption;
end;

procedure TFormMain.ActionFileExportAsAccept(Sender: TObject);
begin
  SynExporterHTML.ExportAll(SynEdit.Lines);
  SynExporterHTML.SaveToFile(TFileSaveAs(Sender).Dialog.FileName);
end;

procedure TFormMain.ActionFileNewExecute(Sender: TObject);
begin
  SynEdit.Clear;
  FFileName := '';
  UpdateCaption;
  BuildTree;
end;

procedure TFormMain.ActionFileOpenAccept(Sender: TObject);
begin
  LoadFromFile(TFileSaveAs(Sender).Dialog.FileName);
end;

procedure TFormMain.ActionFilePrintExecute(Sender: TObject);
begin
  SynEditPrint.DocTitle := ExtractFileName(FFileName);
  SynEditPrint.Title := ExtractFileName(FFileName);
  SynEditPrint.Highlighter := FHighlighter;
  SynEditPrint.Print;
end;

procedure TFormMain.ActionFileSaveAsAccept(Sender: TObject);
begin
  SaveToFile(TFileSaveAs(Sender).Dialog.FileName);
end;

procedure TFormMain.ActionFileSaveExecute(Sender: TObject);
begin
  SaveToFile(FFileName);
end;

procedure TFormMain.ActionFileSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FFileName <> '';
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

    IterateValue(FBase, TreeItems.RootNode)
  finally
    TreeItems.EndUpdate;
  end;
end;

end.
