program AriaUI;

uses
  SysSfIni, Windows, AvL, MainForm;

{$R *.res}
{$R AriaUIRes.res}

begin
  InitCommonControls;
  Randomize;
  FormMain := TMainForm.Create;
  FormMain.Run;
  FormMain.Free;
end.
