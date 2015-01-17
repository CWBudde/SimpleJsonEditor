unit JsonEditor.About;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage;

type
  TFormAbout = class(TForm)
    Label1: TLabel;
    LabelCopyright: TLabel;
    ImageLogo: TImage;
    procedure FormClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TFormAbout.FormClick(Sender: TObject);
begin
  Close;
end;

end.

