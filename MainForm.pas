unit MainForm;

interface

uses
  Windows, CommCtrl, Messages, ShellAPI, AvL, avlUtils, avlSettings, avlSplitter,
  avlListViewEx, avlTrayIcon, avlJSON, Aria2, RequestTransport, InfoPane;

type
  TMainForm = class(TForm)
    Settings: TSettings;
    MainMenu, TrayMenu: TMenu;
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    Splitter: TSplitter;
    TrayIcon: TAvLTrayIcon;
    ServersList: TComboBox;
    TransfersList: TListViewEx;
    Info: TInfoPane;
  private
    FMinWidth, FMinHeight: Integer;
    FAccelTable: HAccel;
    FTBImages, FTransferIcons: TImageList;
    FRefreshTimer: TTimer;
    FRequestTransport: TRequestTransport;
    FAria2: TAria2;
    FTransfersUpdate: record
      Item: Integer;
      Selected: TAria2GID;
    end;
    procedure BeginTransfersUpdate;
    procedure EndTransfersUpdate;
    procedure ExitProgram;
    function FormClose(Sender: TObject): Boolean;
    procedure FormResize(Sender: TObject);
    procedure TrayIconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function QueryEndSession(var Msg: TMessages): Boolean;
    function FormProcessMsg(var Msg: TMsg): Boolean;
    function GetGID(Index: Integer): TAria2GID;
    procedure Refresh(Sender: TObject);
    procedure UpdateTransfers(List: TAria2Struct);
    procedure RepaintAll;
    procedure ServerChange(Sender: TObject);
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
  SServers = 'Servers';
  SServer = 'Server.';
  SCount = 'Count';
  SCurrent = 'Current';
  SHost = 'Host';
  SPort = 'Port';
  SUsername = 'Username';
  SPassword = 'Password';
  SToken = 'Token';
  SSplitter = 'Splitter';

implementation

type
  TTBButtons = (tbAddURL, tbAddTorrent, tbAddMetalink, tbResume, tbPause, tbRemove, tbMoveUp, tbMoveDown, tbOptions, tbExit);
  TSBPart = (sbConnection, sbDownSpeed, sbUpSpeed, sbStats);
  TMenuID = (IDMenuFile = 1000, IDAddURL, IDAddTorrent, IDAddMetalink, IDOptions, IDFileSep0, IDExit,
             IDMenuTransfers = 2000, IDResume, IDPause, IDRemove, IDProperties, IDTransfersSep0, IDMoveUp, IDMoveDown, IDTransfersSep1, IDResumeAll, IDPauseAll, IDPurge,
             IDMenuServer = 3000, IDServerOptions, IDShutdownServer, IDServerVersion,
             IDMenuHelp = 5000, IDAbout,
             IDMenuTray = 10000, IDTrayShow, IDTraySep0, IDTrayResumeAll, IDTrayPauseAll, IDTraySep1, IDTrayOptions, IDTrayAbout, IDTraySep2, IDTrayExit);
  TTransferFieldType = (tftString, tftName, tftStatus, tftSize, tftSpeed, tftPercent, tftETA);

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
  MenuTransfersCapt = '&Transfers';
  MenuTransfers: array[0..11] of PChar = ('2001',
    '&Resume',
    '&Pause',
    'R&emove',
    'Pr&operties...',
    '-',
    'Move &up',
    'Move &down',
    '-',
    'Resume all',
    'Pause all',
    'Purge &completed');
  MenuServerCapt = '&Server';
  MenuServer: array[0..3] of PChar = ('3001',
    'Server &options...',
    '&Shutdown server',
    'Server &version...');
  MenuHelpCapt = '&Help';
  MenuHelp: array[0..1] of PChar = ('5001',
    '&About...'#9'F1');
  MenuTray: array[0..9] of PChar = ('10001',
    '&Show',
    '-',
    '&Resume all',
    '&Pause all',
    '-',
    '&Options...',
    '&About...',
    '-',
    'E&xit');
  TBButtons: array[0..12] of record Caption: string; ImageIndex: Integer; end = (
    (Caption: 'Add URL...'; ImageIndex: 0),
    (Caption: 'Add .torrent...'; ImageIndex: 1),
    (Caption: 'Add .metalink...'; ImageIndex: 2),
    (Caption: '-'; ImageIndex: -1),
    (Caption: 'Resume'; ImageIndex: 3),
    (Caption: 'Pause'; ImageIndex: 4),
    (Caption: 'Remove'; ImageIndex: 5),
    (Caption: '-'; ImageIndex: -1),
    (Caption: 'Move up'; ImageIndex: 6),
    (Caption: 'Move down'; ImageIndex: 7),
    (Caption: '-'; ImageIndex: -1),
    (Caption: 'Options...'; ImageIndex: 8),
    (Caption: 'Exit'; ImageIndex: 9));
  TBMenuIDs: array[TTBButtons] of TMenuID = (IDAddURL, IDAddTorrent, IDAddMetalink, IDResume, IDPause, IDRemove, IDMoveUp, IDMoveDown, IDOptions, IDExit);
  Keys: array[0..13] of string = (sfGID, sfBittorrent, sfFiles, sfStatus, sfErrorMessage, sfSeeder, sfVerifyPending, sfTotalLength, sfCompletedLength, sfUploadLength, sfDownloadSpeed, sfUploadSpeed, sfConnections, sfNumSeeders);
  TransferColumns: array[0..9] of record Caption: string; Width: Integer; FType: TTransferFieldtype; Field: string; end = ( //TODO: User-customizable columns
    (Caption: 'Name'; Width: 200; FType: tftName; Field: ''),
    (Caption: 'Size'; Width: 80; FType: tftSize; Field: sfTotalLength),
    (Caption: 'Progress'; Width: 60; FType: tftPercent; Field: sfCompletedLength + ':' + sfTotalLength),
    (Caption: 'ETA'; Width: 60; FType: tftETA; Field: sfDownloadSpeed + ':' + sfTotalLength + ':' + sfCompletedLength),
    (Caption: 'Status'; Width: 100; FType: tftStatus; Field: ''),
    (Caption: 'Uploaded'; Width: 80; FType: tftSize; Field: sfUploadLength),
    (Caption: 'DL speed'; Width: 80; FType: tftSpeed; Field: sfDownloadSpeed),
    (Caption: 'UL speed'; Width: 80; FType: tftSpeed; Field: sfUploadSpeed),
    (Caption: 'Conns.'; Width: 50; FType: tftString; Field: sfConnections),
    (Caption: 'Seeds'; Width: 50; FType: tftString; Field: sfNumSeeders));
  SBParts: array[TSBPart] of Integer = (100, 250, 400, -1);

var
  Accels: array[0..4] of TAccel = ( //TODO: More accels
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
  Settings := TSettings.Create(AppName);
  OnClose := FormClose;
  OnProcessMsg := FormProcessMsg;
  SetSize(800, 600);
  Position := poScreenCenter;
  Settings.RestoreFormState(ClassName, Self);
  FMinHeight := 300;
  FMinWidth := 400;
  MainMenu := TMenu.Create(Self, true, ['0']);
  AddMenu(MenuFileCapt, Ord(IDMenuFile), MenuFile);
  AddMenu(MenuTransfersCapt, Ord(IDMenuTransfers), MenuTransfers);
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
  FTransferIcons := TImageList.Create;
  FTransferIcons.AddMasked(LoadImage(hInstance, 'TLICONS', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  ServersList := TComboBox.Create(Self, csDropDownList);
  for i := 0 to Settings.ReadInteger(SServers, SCount, 0) - 1 do
    ServersList.ItemAdd(Settings.ReadString(SServers, IntToStr(i), '###'));
  ServersList.ItemIndex := Settings.ReadInteger(SServers, SCurrent, 0);
  ServersList.OnChange := ServerChange;
  ToolBar := TToolBar.Create(Self, true);
  ToolBar.Style := ToolBar.Style or TBSTYLE_TOOLTIPS or CCS_TOP;
  ToolBar.ExStyle := ToolBar.ExStyle or TBSTYLE_EX_MIXEDBUTTONS;
  ToolBar.Perform(TB_SETMAXTEXTROWS, 0, 0);
  ToolBar.Perform(TB_AUTOSIZE, 0, 0);
  Toolbar.Indent := 100;
  ToolBar.Images := FTBImages;
  for i := Low(TBButtons) to High(TBButtons) do
    ToolBar.ButtonAdd(TBButtons[i].Caption, TBButtons[i].ImageIndex);
  ServersList.SetBounds(ToolBar.Left + 2, ToolBar.Top + (ToolBar.Height - ServersList.Height) div 2, ToolBar.Indent - 4, ServersList.Height);
  StatusBar := TStatusBar.Create(Self, '');
  StatusBar.SetParts(Length(SBParts), SBParts);
  Splitter := TSplitter.Create(Self, false);
  Splitter.SetBounds(0, Settings.ReadInteger(ClassName, SSplitter, 300), ClientWidth, Splitter.Height);
  Splitter.OnMove := SplitterMove;
  TransfersList := TListViewEx.Create(Self);
  TransfersList.Style := TransfersList.Style and not LVS_SINGLESEL or LVS_SHOWSELALWAYS {or LVS_EDITLABELS} or LVS_NOSORTHEADER; //TODO: switches for sorting & etc
  //TransfersList.ExStyle := TransfersList.ExStyle or WS_EX_STATICEDGE and not WS_EX_CLIENTEDGE;
  TransfersList.ViewStyle := LVS_REPORT;
  TransfersList.OptionsEx := TransfersList.OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  TransfersList.SmallImages := FTransferIcons;
  TransfersList.SetBounds(0, ToolBar.Height, ClientWidth, Splitter.Top - ToolBar.Height);
  for i := 0 to High(TransferColumns) do
    with TransferColumns[i] do
      TransfersList.ColumnAdd(Caption, Width);
  Info := TInfoPane.Create(Self);
  Info.SetBounds(0, Splitter.Bottom, ClientWidth, StatusBar.Top - Splitter.Bottom);
  //DragAcceptFiles(Handle, true);
  OnResize := FormResize;
  FormResize(Self);
  FRequestTransport := TRequestTransport.Create;
  //FRequestTransport.Connect('localhost', 6800, '', '');
  //FAria2 := TAria2.Create(FRequestTransport.SendRequest, LoadFile('secret.txt'));
  FAria2 := TAria2.Create(FRequestTransport.SendRequest);
  ServerChange(ServersList);
  FRefreshTimer := TTimer.CreateEx(1000, true);
  FRefreshTimer.OnTimer := Refresh;
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
  TransfersList.SetBounds(Splitter.Left, ToolBar.Height, Splitter.Width, Splitter.Top - ToolBar.Height);
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
        IDOptions, IDTrayOptions: ShowMessage('Under construction');
        IDResumeAll, IDTrayResumeAll: FAria2.UnpauseAll;
        IDPauseAll, IDTrayPauseAll: FAria2.PauseAll(GetKeyState(VK_SHIFT) < 0);
        IDPurge: FAria2.PurgeDownloadResult;
        IDServerOptions: MessageDlg(JsonToStr(FAria2.GetGlobalOptions.Raw), 'Aria2 options', MB_ICONINFORMATION);
        IDShutdownServer: FAria2.Shutdown(GetKeyState(VK_SHIFT) < 0);
        IDServerVersion: MessageDlg('Aria2 ' + FAria2.GetVersion(true), 'Aria2 version', MB_ICONINFORMATION);
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
  if (ClientWidth <= 0) or (ClientHeight <= 0) then Exit;
  Splitter.Width := ClientWidth;
  Splitter.Top := StatusBar.Top - Info.Height - Splitter.Height;
  Splitter.MinPos := ToolBar.Height + 70;
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
  FreeAndNil(FRefreshTimer);
  FreeAndNil(FAria2);
  FreeAndNil(FRequestTransport);
  DestroyAcceleratorTable(FAccelTable);
  FreeAndNil(TrayIcon);
  FreeAndNil(MainMenu);
  FreeAndNil(TrayMenu);
  FreeAndNil(FTransferIcons);
  FreeAndNil(FTBImages);
  FreeAndNil(Settings);
  inherited;
end;

procedure TMainForm.BeginTransfersUpdate;
begin
  TransfersList.BeginUpdate;
  FTransfersUpdate.Item := 0;
  FTransfersUpdate.Selected := GetGID(TransfersList.SelectedIndex);
end;

procedure TMainForm.EndTransfersUpdate;
var
  i: Integer;
begin
  while TransfersList.ItemCount > FTransfersUpdate.Item do
  begin
    FreeMem(PAria2GID(TransfersList.ItemObject[TransfersList.ItemCount - 1]));
    TransfersList.ItemDelete(TransfersList.ItemCount - 1);
  end;
  if (FTransfersUpdate.Selected <> -1) and (GetGID(TransfersList.SelectedIndex) <> FTransfersUpdate.Selected) then
    for i := 0 to TransfersList.ItemCount - 1 do
      if GetGID(i) = FTransfersUpdate.Selected then
      begin
        TransfersList.SelectedIndex := i;
        Break;
      end;
  TransfersList.EndUpdate;
end;

procedure TMainForm.ExitProgram;
begin
  if Visible then Hide;
  Close;
end;

function TMainForm.FormClose(Sender: TObject): Boolean;
begin
  Result := not Visible;
  if Visible then
    Hide
  else begin
    Settings.SaveFormState(ClassName, Self);
    Settings.WriteInteger(ClassName, SSplitter, Splitter.Top);
  end;
end;

function TMainForm.FormProcessMsg(var Msg: TMsg): Boolean;
begin
  Result := TranslateAccelerator(Handle, FAccelTable, Msg) <> 0;
end;

function TMainForm.GetGID(Index: Integer): TAria2GID;
begin
  if (Index < 0) or (Index >= TransfersList.ItemCount) then
    Result := -1
  else
    Result := PAria2GID(TransfersList.ItemObject[Index])^;
end;

procedure TMainForm.Refresh(Sender: TObject);
var
  Stats: TAria2Struct;
  i: Integer;
begin
  try
    Stats := FAria2.GetGlobalStats;
    FRefreshTimer.Interval := 1000; //TODO: lower refresh when minimized
    StatusBar.SetPartText(Ord(sbConnection), 0, 'OK (Aria2 ' + FAria2.GetVersion + ')');
  except
    FRefreshTimer.Interval := 10000;
    for i := Ord(Low(SBParts)) to Ord(High(SBParts)) do
      StatusBar.SetPartText(i, 0, '');
    StatusBar.SetPartText(Ord(sbConnection), 0, 'No connection');
    TrayIcon.ToolTip := Caption + CRLF + 'No connection';
    Exit;
  end;
  try
    BeginTransfersUpdate;
    UpdateTransfers(FAria2.TellActive(Keys));
    UpdateTransfers(FAria2.TellWaiting(0, Stats.Int[sfNumWaiting], Keys));
    UpdateTransfers(FAria2.TellStopped(0, Stats.Int[sfNumStopped], Keys));
    StatusBar.SetPartText(Ord(sbDownSpeed), 0, 'Down: ' + SizeToStr(Stats.Int[sfDownloadSpeed]) + '/s');
    StatusBar.SetPartText(Ord(sbUpSpeed), 0, 'Up: ' + SizeToStr(Stats.Int[sfUploadSpeed]) + '/s');
    StatusBar.SetPartText(Ord(sbStats), 0, Format('Active: %d; Waiting: %d; Stopped: %d', [Stats.Int[sfNumActive], Stats.Int[sfNumWaiting], Stats.Int[sfNumStopped]]));
    TrayIcon.ToolTip := Format('%s' + CRLF + 'Active: %d; Waiting: %d; Stopped: %d' + CRLF + 'Down: %s/s; Up: %s/s',
      [Caption, Stats.Int[sfNumActive], Stats.Int[sfNumWaiting], Stats.Int[sfNumStopped], SizeToStr(Stats.Int[sfDownloadSpeed]), SizeToStr(Stats.Int[sfUploadSpeed])]);
  finally
    EndTransfersUpdate;
    FreeAndNil(Stats);
  end;
end;

procedure TMainForm.ServerChange(Sender: TObject);
var
  Section: string;
  i: Integer;
begin
  Section := SServer + IntToStr(ServersList.ItemIndex);
  FRequestTransport.Connect(Settings.ReadString(Section, SHost, 'localhost'), Settings.ReadInteger(Section, SPort, 6800), Settings.ReadString(Section, SUsername, ''), Settings.ReadString(Section, SPassword, ''));
  FAria2.RPCSecret := Settings.ReadString(Section, SToken, '');
  Settings.WriteInteger(SServers, SCurrent, ServersList.ItemIndex);
  for i := 0 to TransfersList.ItemCount - 1 do
    FreeMem(PAria2GID(TransfersList.ItemObject[i]));
  TransfersList.Clear;
end;

procedure TMainForm.UpdateTransfers(List: TAria2Struct);

  function First(const Pair: string): string;
  begin
    Result := Copy(Pair, 1, FirstDelimiter(':', Pair) - 1);
  end;

  function Second(const Pair: string): string;
  begin
    Result := Copy(Pair, FirstDelimiter(':', Pair) + 1, MaxInt);
  end;

  function GetValue(FType: TTransferFieldType; const Field: string): string;
  begin
    case FType of
      tftString: Result := List[Field];
      tftName: if List.Has[sfBittorrent] then
                 Result := List[sfBTName]
               else
                 Result := ExtractFileName(List[sfFiles + '.0.' + sfPath]);
      tftStatus: begin
                   Result := List[sfStatus];
                   if Boolean(StrToEnum(List[sfSeeder], sfBoolValues)) then
                     Result := Result + '; seeding';
                   if Boolean(StrToEnum(List[sfVerifyPending], sfBoolValues)) then
                     Result := '; verifying';
                   if List[sfErrorMessage] <> '' then
                     Result := Result + ' (' + List[sfErrorMessage] + ')';
                 end;
      tftSize: Result := SizeToStr(List.Int64[Field]);
      tftSpeed: Result := SizeToStr(List.Int64[Field]) + '/s';
      tftPercent: Result := FloatToStr2(100 * List.Int64[First(Field)] / List.Int64[Second(Field)], 1, 2) + '%';
      tftETA: Result := EtaToStr(List.Int64[First(Second(Field))] - List.Int64[Second(Second(Field))], List.Int64[First(Field)]);
    end
  end;

var
  i, j, Image: Integer;
  P: PAria2GID;
begin
  for i := 0 to List.Length[''] - 1 do
  begin
    List.Index := i;
    if FTransfersUpdate.Item >= TransfersList.ItemCount then
    begin
      FTransfersUpdate.Item := TransfersList.ItemAdd('');
      GetMem(P, SizeOf(TAria2GID));
      TransfersList.ItemObject[FTransfersUpdate.Item] := TObject(P);
    end;
    PAria2GID(TransfersList.ItemObject[FTransfersUpdate.Item])^ := StrToGID(List[sfGID]);
    for j := Low(TransferColumns) to High(TransferColumns) do
      TransfersList.Items[FTransfersUpdate.Item, j] := GetValue(TransferColumns[j].FType, TransferColumns[j].Field);
    Image := StrToEnum(List[sfStatus], sfStatusValues);
    if (TAria2Status(Image) in [asActive, asWaiting]) and Boolean(StrToEnum(List[sfSeeder], sfBoolValues)) then
      Inc(Image, 6);
    if Boolean(StrToEnum(List[sfVerifyPending], sfBoolValues)) then
      Image := 8;
    TransfersList.ItemImageIndex[FTransfersUpdate.Item] := Image;
    Inc(FTransfersUpdate.Item);
  end;
  List.Free;
end;

end.
