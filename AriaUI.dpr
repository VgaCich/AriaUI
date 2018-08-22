program AriaUI;

uses
  {$IFDEF FASTMM}FastMM4{$ELSE}LeakDetect{$ENDIF}, Windows, AvL, avlOneInstance,
  Utils, MainForm, OptionsForm, AddForm, ServerOptionsForm, RPCRequestForm;

{$R *.res}
{$R AriaUIRes.res}

var
  PrevWnd: THandle;

begin
  Set8087CW($133F); //Disable FPU exceptions
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
  {$IFNDEF FASTMM}LeakMessageEnabled := Settings.ReadBool(SDebug, 'ReportLeaks', false);{$ENDIF}
  FormOptions := TOptionsForm.Create(FormMain); //TODO: Dialogs must close on Escape
  FormAdd := TAddForm.Create(FormMain);
  FormServerOptions := TServerOptionsForm.Create(FormMain);
  FormRPCRequest := TRPCRequestForm.Create(FormMain);
  FormMain.Run;
  FormMain.Free;
end.
