program JsonEditor;

uses
  Vcl.Forms,
  JsonEditor.Main in 'JsonEditor.Main.pas' {FormMain},
  JsonEditor.EditLink in 'JsonEditor.EditLink.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

