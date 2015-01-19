unit JsonEditor.EditLink;

interface

uses
  (* Delphi *)
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Forms,
  Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs,
  Vcl.ImgList, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Mask,

  (* DWS *)
  dwsJSON,

  (* Virtual Treeview *)
  VirtualTrees;

type
  TJsonNode = record
(*
  private
    function GetName: string;
    function GetValue: TdwsJSONValue;
  public
    Parent: TdwsJSONValue;
    Index: Integer;

    property Value: TdwsJSONValue read GetValue;
    property Name: string read GetName;
*)
    Value: TdwsJSONValue;
    Name: string;
  end;
  PJsonNode = ^TJsonNode;

  TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FControl: TWinControl;
    FTree: TVirtualStringTree;
    FNode: PVirtualNode;
    FColumn: Integer;
  protected
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    procedure SetBounds(R: TRect); stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
  end;

implementation

(*
{ TJsonNode }

function TJsonNode.GetName: string;
begin
  Result := Parent.Names[Index];
end;

function TJsonNode.GetValue: TdwsJSONValue;
begin
  Result := Parent.Elements[Index];
end;
*)


{ TPropertyEditLink }

destructor TPropertyEditLink.Destroy;
begin
  if FControl.HandleAllocated then
    PostMessage(FControl.Handle, CM_RELEASE, 0, 0);
  inherited;
end;

procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  CanAdvance := True;

  case Key of
    VK_ESCAPE:
      Key := 0;

    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;

    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        if FControl is TComboBox then
          CanAdvance := CanAdvance and not TComboBox(FControl).DroppedDown;
        if FControl is TDateTimePicker then
          CanAdvance :=  CanAdvance and not TDateTimePicker(FControl).DroppedDown;

        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

procedure TPropertyEditLink.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, [#1..#31, '0'..'9', DecimalSeparator]) then
    Key := #0;
end;

procedure TPropertyEditLink.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;
  end;
end;

function TPropertyEditLink.BeginEdit: Boolean;
begin
  Result := True;
  FControl.Show;
  FControl.SetFocus;
end;

function TPropertyEditLink.CancelEdit: Boolean;
begin
  Result := True;
  FControl.Hide;
end;

function TPropertyEditLink.EndEdit: Boolean;
var
  JsonNode: PJsonNode;
  Buffer: array[0..1024] of Char;
  S: UnicodeString;
begin
  Result := True;

  JsonNode := FTree.GetNodeData(FNode);
  if FControl is TComboBox then
    S := TComboBox(FControl).Text
  else
  begin
    GetWindowText(FControl.Handle, Buffer, 1024);
    S := Buffer;
  end;

  case FColumn of
    0:
      begin
        if S <> JsonNode.Name then
        begin
          JsonNode.Name := S;
          FTree.InvalidateNode(FNode);
        end;
      end;
    1:
      begin
        case JsonNode.Value.ValueType of
          jvtString:
            begin
              JsonNode.Value.AsString := S;
              FTree.InvalidateNode(FNode);
            end;
          jvtNumber:
            begin
              JsonNode.Value.AsNumber := StrToFloat(S);
              FTree.InvalidateNode(FNode);
            end;
          jvtBoolean:
            begin
              JsonNode.Value.AsBoolean := SameText(S, 'true');
              FTree.InvalidateNode(FNode);
            end;
        end;
      end;
  end;
  FControl.Hide;
  FTree.SetFocus;
end;

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex): Boolean;
var
  Data: PJsonNode;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // determine what edit type actually is needed
  FControl.Free;
  FControl := nil;
  Data := FTree.GetNodeData(Node);

  case Column of
    0:
      begin
        FControl := TEdit.Create(nil);
        with FControl as TEdit do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Name;
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    1:
      begin
        Assert(Data.Value is TdwsJSONImmediate);
        case Data.Value.ValueType of
          jvtString:
            begin
              FControl := TEdit.Create(nil);
              with FControl as TEdit do
              begin
                Visible := False;
                Parent := Tree;
                Text := Data.Value.AsString;
                OnKeyDown := EditKeyDown;
                OnKeyUp := EditKeyUp;
              end;
            end;
          jvtNumber:
            begin
              FControl := TEdit.Create(nil);
              with FControl as TEdit do
              begin
                Visible := False;
                Parent := Tree;
                Text := Data.Value.AsString;
                OnKeyPress := EditKeyPress;
                OnKeyDown := EditKeyDown;
                OnKeyUp := EditKeyUp;
              end;
            end;
          jvtBoolean:
            begin
              FControl := TComboBox.Create(nil);
              with FControl as TComboBox do
              begin
                Visible := False;
                Parent := Tree;
                Style := csDropDownList;
                Items.Add('false');
                Items.Add('true');
                ItemIndex := Integer(Data.Value.AsBoolean);
                OnKeyDown := EditKeyDown;
                OnKeyUp := EditKeyUp;
              end;
            end;
        end;
      end;
  else
    Result := False;
  end;
end;

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);
begin
  FControl.WindowProc(Message);
end;

function TPropertyEditLink.GetBounds: TRect;
begin
  Result := FControl.BoundsRect;
end;

procedure TPropertyEditLink.SetBounds(R: TRect);
var
  Dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would
  // influence how the selection is drawn) we have to set the edit's width
  // explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FControl.BoundsRect := R;
end;

end.
