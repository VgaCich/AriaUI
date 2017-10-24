unit MainForm;

interface

uses
  Windows, CommCtrl, Messages, ShellAPI, AvL, avlUtils, avlSettings, avlSplitter,
  avlTrayIcon, avlJSON, Aria2, RequestTransport, TransfersList, InfoPane;

type
  TMainForm = class(TForm)
    MainMenu, TrayMenu: TMenu;
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    Splitter: TSplitter;
    TrayIcon: TAvLTrayIcon;
    Transfers: TTransfersList;
    Info: TInfoPane;
  private
    FMinWidth, FMinHeight: Integer;
    FAccelTable: HAccel;
    FTBImages: TImageList;
    FRequestTransport: TRequestTransport;
    FAria: TAria2;
    procedure ExitProgram;
    function FormClose(Sender: TObject): Boolean;
    procedure FormResize(Sender: TObject);
    procedure TrayIconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function QueryEndSession(var Msg: TMessages): Boolean;
    function FormProcessMsg(var Msg: TMsg): Boolean;
    function GetSettings: TSettings;
    procedure RepaintAll;
    procedure ShowAbout;
    procedure SplitterMove(Sender: TObject);
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    //procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMSizing(var Msg: TWMMoving); message WM_SIZING;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  FormMain: TMainForm;

const
  AppCaption = 'Aria UI';
  AppName = 'AriaUI';

implementation

type
  TTBButtons = (tbAddURL, tbAddTorrent, tbAddMetalink, tbOptions, tbExit);
  TMenuID = (IDMenuFile = 1000, IDAddURL, IDAddTorrent, IDAddMetalink, IDOptions, IDMenuFileSep0, IDExit,
             IDMenuServer = 2000, IDServerOptions, IDShutdownServer, IDServerVersion,
             IDMenuHelp = 5000, IDAbout,
             IDMenuTray = 10000, IDTrayShow, IDTrayAbout, IDTrayMenuSep0, IDTrayExit);

const
  CRLF = #13#10;
  AboutIcon = 'MAINICON';
  AboutCaption = 'About ';
  AboutText = 'Aria UI 1.0'+CRLF+CRLF+
              'Copyright '#169' VgaSoft, 2017'+CRLF+
              'vgasoft@gmail.com';
  MenuFileCapt = '&File';
  MenuFile: array[0..6] of PChar = ('1001',
    'Add &URL...'#9'Ctrl-U',
    'Add .&torrent...'#9'Ctrl-O',
    'Add .&metalink...'#9'Ctrl-M',
    'Op&tions...',
    '-',
    'E&xit'#9'Alt-X');
  MenuServerCapt = '&Server';
  MenuServer: array[0..3] of PChar = ('2001',
    'Server &options...',
    '&Shutdown server',
    'Server &version...');
  MenuHelpCapt = '&Help';
  MenuHelp: array[0..1] of PChar = ('5001',
    '&About...'#9'F1');
  MenuTray: array[0..4] of PChar = ('10001',
    '&Show',
    '&About...',
    '-',
    'E&xit');
  TBButtons: array[0..5] of record Caption: string; ImageIndex: Integer; end = (
    (Caption: 'Add URL...'; ImageIndex: 0),
    (Caption: 'Add .torrent'; ImageIndex: 1),
    (Caption: 'Add .metalink'; ImageIndex: 2),
    (Caption: '-'; ImageIndex: -1),
    (Caption: 'Options'; ImageIndex: 8),
    (Caption: 'Exit'; ImageIndex: 7));
  TBMenuIDs: array[TTBButtons] of TMenuID = (IDAddURL, IDAddTorrent, IDAddMetalink, IDOptions, IDExit);

var
  Accels: array[0..4] of TAccel = (
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('U'); Cmd: Ord(IDAddURL)),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('O'); Cmd: Ord(IDAddTorrent)),
    (fVirt: FCONTROL or FVIRTKEY; Key: Ord('M'); Cmd: Ord(IDAddMetalink)),
    (fVirt: FALT or FVIRTKEY; Key: Ord('X'); Cmd: Ord(IDExit)),
    (fVirt: FVIRTKEY; Key: VK_F1; Cmd: Ord(IDAbout)));

constructor TMainForm.Create;

  procedure AddMenu(const Name: string; ID: Cardinal; const Template: array of PChar);
  var
    Menu: TMenu;
  begin
    Menu := TMenu.Create(Self, false, Template);
    InsertMenu(MainMenu.Handle, ID, MF_BYCOMMAND or MF_POPUP, Menu.Handle, PChar(Name));
  end;

var
  i: Integer;
begin
  inherited Create(nil, AppCaption);
  OnClose := FormClose;
  OnProcessMsg := FormProcessMsg;
  SetSize(800, 600);
  Position := poScreenCenter;
  FMinHeight := 300;
  FMinWidth := 400;
  MainMenu := TMenu.Create(Self, true, ['0']);
  AddMenu(MenuFileCapt, Ord(IDMenuFile), MenuFile);
  AddMenu(MenuServerCapt, Ord(IDMenuServer), MenuServer);
  AddMenu(MenuHelpCapt, Ord(IDMenuHelp), MenuHelp);
  SetMenu(Handle, MainMenu.Handle);
  TrayMenu := TMenu.Create(Self, false, MenuTray);
  TrayIcon := TAvLTrayIcon.Create;
  TrayIcon.ToolTip := Caption;
  TrayIcon.Icon := LoadImage(hInstance, 'MAINICON', IMAGE_ICON, 16, 16, LR_SHARED);
  TrayIcon.OnQueryEndSession := QueryEndSession;
  TrayIcon.OnMouseUp := TrayIconMouseUp;
  TrayIcon.Active := true;
  FAccelTable := CreateAcceleratorTable(Accels[0], Length(Accels));
  FTBImages := TImageList.Create;
  FTBImages.AddMasked(LoadImage(hInstance, 'TBMAIN', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  ToolBar := TToolBar.Create(Self, true);
  ToolBar.Style := ToolBar.Style or TBSTYLE_TOOLTIPS or CCS_TOP;
  ToolBar.ExStyle := ToolBar.ExStyle or TBSTYLE_EX_MIXEDBUTTONS;
  ToolBar.Perform(TB_SETMAXTEXTROWS, 0, 0);
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  ToolBar.Images := FTBImages;
  for i := Low(TBButtons) to High(TBButtons) do
    ToolBar.ButtonAdd(TBButtons[i].Caption, TBButtons[i].ImageIndex);
  StatusBar := TStatusBar.Create(Self, '');
  Splitter := TSplitter.Create(Self, false);
  Splitter.SetBounds(0, 300, ClientWidth, Splitter.Height);
  Splitter.OnMove := SplitterMove;
  Transfers := TTransfersList.Create(Self);
  Transfers.SetBounds(0, ToolBar.Height, ClientWidth, Splitter.Top - ToolBar.Height);
  Info := TInfoPane.Create(Self);
  Info.SetBounds(0, Splitter.Bottom, ClientWidth, StatusBar.Top - Splitter.Bottom);
  //DragAcceptFiles(Handle, true);
  OnResize := FormResize;
  FormResize(Self);
  FRequestTransport := TRequestTransport.Create;
  FRequestTransport.Connect('localhost', 6800, '', '');
  FAria := TAria2.Create(FRequestTransport.SendRequest, LoadFile('secret.txt'));
end;

procedure TMainForm.TrayIconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    if Visible then
      Hide
    else
      Show
  else
    TrayMenu.Popup(X, Y);
end;

function TMainForm.QueryEndSession(var Msg: TMessages): Boolean;
begin
  Msg.Result:=1;
  ExitProgram;
  Result := true;
end;

function TMainForm.GetSettings: TSettings;
begin
  Result := TSettings.Create(AppName);
end;

procedure TMainForm.ShowAbout;
var
  Version: TOSVersionInfo;
  MsgBoxParamsW: TMsgBoxParamsW;
  MsgBoxParamsA: TMsgBoxParamsA;
begin
  Version.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Version);
  if Version.dwPlatformId = VER_PLATFORM_WIN32_NT then
  begin
    FillChar(MsgBoxParamsW, SizeOf(MsgBoxParamsW), #0);
    with MsgBoxParamsW do
    begin
      cbSize := SizeOf(MsgBoxParamsW);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := PWideChar(WideString(AboutText));
      lpszCaption := PWideChar(WideString(AboutCaption+Caption));
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectW(MsgBoxParamsW);
  end
  else begin
    FillChar(MsgBoxParamsA, SizeOf(MsgBoxParamsA), #0);
    with MsgBoxParamsA do
    begin
      cbSize := SizeOf(MsgBoxParamsA);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := PAnsiChar(AboutText);
      lpszCaption := PAnsiChar(AboutCaption+Caption);
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectA(MsgBoxParamsA);
  end;
end;

procedure TMainForm.SplitterMove(Sender: TObject);
begin
  Transfers.SetBounds(Splitter.Left, ToolBar.Height, Splitter.Width, Splitter.Top - ToolBar.Height);
  Info.SetBounds(Splitter.Left, Splitter.Bottom, Splitter.Width, StatusBar.Top - Splitter.Bottom);
end;

procedure TMainForm.WMCommand(var Msg: TWMCommand);
begin
  if (Msg.Ctl = 0) and (Msg.NotifyCode in [0, 1]) then
    try
      case TMenuID(Msg.ItemID) of
        IDExit, IDTrayExit: ExitProgram;
        IDAbout, IDTrayAbout: ShowAbout;
        IDTrayShow: if Visible then Hide else Show;
        IDServerOptions: MessageDlg(JsonToStr(FAria.GetGlobalOptions), 'Aria2 Options', MB_ICONINFORMATION);
        IDShutdownServer: FAria.Shutdown;
        IDServerVersion: MessageDlg('Aria2 ' + FAria.GetVersion(true), 'Aria2 version', MB_ICONINFORMATION);
      end;
    except
      on E: Exception do MessageDlg(E.Message, 'Error', MB_ICONERROR);
    end;
  if Assigned(ToolBar) and (Msg.Ctl = ToolBar.Handle) then
    Perform(WM_COMMAND, Ord(TBMenuIDs[TTBButtons(Msg.ItemID)]), 0);
end;

{procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  FileName: array[0..MAX_PATH] of Char;
  i: Integer;
begin
  for i := 0 to DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0) - 1 do
  begin
    DragQueryFile(Msg.Drop, i, FileName, MAX_PATH + 1);
    //Process file here
  end;
  DragFinish(Msg.Drop);
end;}

procedure TMainForm.FormResize(Sender: TObject);
begin
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  StatusBar.Perform(WM_SIZE, 0, 0);
  Splitter.Width := ClientWidth;
  Splitter.Top := StatusBar.Top - Info.Height - Splitter.Height;
  Splitter.MinPos := ToolBar.Height + 70;
  if ClientHeight > 0 then
    Splitter.MaxPos := StatusBar.Top - Splitter.Height;
  SplitterMove(Splitter);
end;

procedure TMainForm.WMSizing(var Msg: TWMMoving);
begin
  with Msg do
  begin
    if DragRect.Right - DragRect.Left < FMinWidth then
      if (Edge = WMSZ_LEFT) or (Edge = WMSZ_TOPLEFT) or (Edge = WMSZ_BOTTOMLEFT)
        then DragRect.Left := DragRect.Right - FMinWidth
        else DragRect.Right := DragRect.Left + FMinWidth;
    if DragRect.Bottom - DragRect.Top < FMinHeight then
      if (Edge = WMSZ_TOP) or (Edge = WMSZ_TOPLEFT) or (Edge = WMSZ_TOPRIGHT)
        then DragRect.Top := DragRect.Bottom - FMinHeight
        else DragRect.Bottom := DragRect.Top + FMinHeight;
  end;
end;

procedure TMainForm.RepaintAll;
var
  Cur: TWinControl;
begin
  Cur := NextControl;
  while Assigned(Cur) do
  begin
    Cur.Invalidate;
    UpdateWindow(Cur.Handle);
    Cur := Cur.NextControl;
  end;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FAria);
  FreeAndNil(FRequestTransport);
  DestroyAcceleratorTable(FAccelTable);
  FreeAndNil(TrayIcon);
  FreeAndNil(MainMenu);
  FreeAndNil(TrayMenu);
  FreeAndNil(FTBImages);
  inherited;
end;

procedure TMainForm.ExitProgram;
begin
  if Visible then Hide;
  Close;
end;

function TMainForm.FormClose(Sender: TObject): Boolean;
begin
  Result := not Visible;
  if Visible then Hide;
end;

function TMainForm.FormProcessMsg(var Msg: TMsg): Boolean;
begin
  Result := TranslateAccelerator(Handle, FAccelTable, Msg) <> 0;
end;

end.
