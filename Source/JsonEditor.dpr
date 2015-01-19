program JsonEditor;

uses
  Vcl.Forms,
  JsonEditor.Main in 'JsonEditor.Main.pas' {FormMain},
  JsonEditor.EditLink in 'JsonEditor.EditLink.pas',
  JsonEditor.About in 'JsonEditor.About.pas' {FormAbout},
  JsonEditor.AddDialog in 'JsonEditor.AddDialog.pas' {FormAddDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

