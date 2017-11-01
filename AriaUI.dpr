program AriaUI;

uses
  SysSfIni, Windows, AvL, MainForm, AddForm;

{$R *.res}
{$R AriaUIRes.res}

begin
  InitCommonControls;
  Randomize;
  FormMain := TMainForm.Create;
  FormAdd := TAddForm.Create(FormMain);
  FormMain.Run;
  FormMain.Free;
end.
