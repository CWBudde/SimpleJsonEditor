unit JsonEditor.AddDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormAddDialog = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ComboBoxName: TComboBox;
    ComboBoxValue: TComboBox;
    LabelName: TLabel;
    LabelValue: TLabel;
    PanelName: TPanel;
    PanelValue: TPanel;
    RadioButtonBoolean: TRadioButton;
    RadioButtonNull: TRadioButton;
    RadioButtonNumber: TRadioButton;
    RadioButtonString: TRadioButton;
    procedure RadioButtonNumberClick(Sender: TObject);
    procedure ComboBoxValueNumberKeyPressed(Sender: TObject; var Key: Char);
    procedure RadioButtonBooleanClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioButtonStringClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioButtonNullClick(Sender: TObject);
  private
    FNumberValueHistory: TStringList;
    FStringValueHistory: TStringList;
  end;

implementation

{$R *.dfm}

{ TFormAddDialog }

procedure TFormAddDialog.FormCreate(Sender: TObject);
begin
  FStringValueHistory := TStringList.Create;
  FNumberValueHistory := TStringList.Create;
end;

procedure TFormAddDialog.FormDestroy(Sender: TObject);
begin
  FNumberValueHistory.Free;
  FStringValueHistory.Free;
end;

procedure TFormAddDialog.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Dummy: Single;
begin
  if ModalResult = mrOk then
  begin
    if RadioButtonString.Checked then
    begin
      ComboBoxValue.Items.Add(ComboBoxValue.Text);
      FStringValueHistory.Add(ComboBoxValue.Text);
    end;
    if RadioButtonNumber.Checked then
      if TryStrToFloat(ComboBoxValue.Text, Dummy) then
      begin
        ComboBoxValue.Items.Add(ComboBoxValue.Text);
        FNumberValueHistory.Add(ComboBoxValue.Text);
      end;

    // store name
    ComboBoxName.Items.Add(ComboBoxName.Text);
  end;
end;

procedure TFormAddDialog.ComboBoxValueNumberKeyPressed(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, [#1..#31, 'e', 'E', '0'..'9', FormatSettings.DecimalSeparator]) then
    Key := #0;
end;

procedure TFormAddDialog.RadioButtonBooleanClick(Sender: TObject);
begin
  with ComboBoxValue do
  begin
    OnKeyPress := nil;
    Style := csDropDownList;
    Items.Clear;
    Items.Add('false');
    Items.Add('true');
    ItemIndex := 0;
    Visible := True;
  end;
end;

procedure TFormAddDialog.RadioButtonNullClick(Sender: TObject);
begin
  ComboBoxValue.Visible := False;
end;

procedure TFormAddDialog.RadioButtonNumberClick(Sender: TObject);
begin
  with ComboBoxValue do
  begin
    OnKeyPress := ComboBoxValueNumberKeyPressed;
    Style := csDropDown;
    Text := '';
    Items.Assign(FNumberValueHistory);
    Visible := True;
  end;
end;

procedure TFormAddDialog.RadioButtonStringClick(Sender: TObject);
begin
  with ComboBoxValue do
  begin
    OnKeyPress := nil;
    Style := csDropDown;
    Text := '';
    Items.Assign(FStringValueHistory);
    Visible := True;
  end;
end;

end.

