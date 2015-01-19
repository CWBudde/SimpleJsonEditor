unit JsonEditor.AddDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormAddDialog = class(TForm)
    EditName: TEdit;
    ButtonOK: TButton;
    LabelName: TLabel;
    ButtonCancel: TButton;
    EditValue: TEdit;
    LabelValue: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.

