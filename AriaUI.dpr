program AriaUI;

uses
  SysSfIni, Windows, AvL, avlOneInstance, MainForm, AddForm, ServerOptionsForm;

{$R *.res}
{$R AriaUIRes.res}

var
  PrevWnd: THandle;

begin
  if IsRunning(AppName) then
  begin
    PrevWnd := FindWindow('TMainForm', AppCaption);
    ShowWindow(PrevWnd, SW_SHOW);
    SetForegroundWindow(PrevWnd);
    Exit;
  end;
  InitCommonControls;
  Randomize;
  IsMultiThread := true;
  FormMain := TMainForm.Create;
  FormAdd := TAddForm.Create(FormMain);
  FormServerOptions := TServerOptionsForm.Create(FormMain);
  FormMain.Run;
  FormMain.Free;
end.
