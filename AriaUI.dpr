program AriaUI;

uses
  LeakDetect, Windows, AvL, avlOneInstance, MainForm, OptionsForm, AddForm,
  ServerOptionsForm, RPCRequestForm;

{$R *.res}
{$R AriaUIRes.res}

var
  PrevWnd: THandle;

begin
  if IsRunning(AppName) then //TODO: Pass options like files for open
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
  LeakMessageEnabled := FormMain.Settings.ReadBool(SDebug, 'ReportLeaks', false);
  FormOptions := TOptionsForm.Create(FormMain); //TODO: Dialogs must close on Escape
  FormAdd := TAddForm.Create(FormMain);
  FormServerOptions := TServerOptionsForm.Create(FormMain);
  FormRPCRequest := TRPCRequestForm.Create(FormMain);
  FormMain.Run;
  FormMain.Free;
end.
