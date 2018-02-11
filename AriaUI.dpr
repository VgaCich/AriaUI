program AriaUI;

uses
  SysSfIni, Windows, AvL, avlOneInstance, MainForm, OptionsForm, AddForm,
  ServerOptionsForm, RPCRequestForm;

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
  FormOptions := TOptionsForm.Create(FormMain);
  FormAdd := TAddForm.Create(FormMain);
  FormServerOptions := TServerOptionsForm.Create(FormMain);
  FormRPCRequest := TRPCRequestForm.Create(FormMain);
  FormMain.Run;
  FormMain.Free;
end.
