library SampleTransport;

uses
  SysSfIni, Windows, WinInet, AvL, avlSyncObjs;

type
  PExternalTransportResponce = ^ TExternalTransportResponce;
  TExternalTransportResponce = record
    Data: Pointer;
    Length: Integer;
    Free: procedure(P: PExternalTransportResponce); stdcall;
  end;
  TWininetRequestTransport = class
  private
    FSession, FConnection: HINTERNET;
    FRequestFlags: Cardinal;
    FRequestLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const Server: string; Port: Word; const UserName, Password: string; UseSSL: Boolean);
    procedure Disconnect;
    function SendRequest(Sender: TObject; const Request: string): string;
  end;

procedure FreeResponce(P: PExternalTransportResponce); stdcall;
begin
  FreeMem(P.Data);
  Dispose(P);
end;

function Create: Pointer; stdcall;
begin
  Result := TWininetRequestTransport.Create;
end;

procedure Free(Inst: Pointer); stdcall;
begin
  if Assigned(Inst) then
    TObject(Inst).Free;
end;

function Connect(Inst: Pointer; Server: PChar; Port: Word; UserName, Password: PChar; UseSSL: LongBool): LongBool; stdcall;
begin
  Result := false;
  if Assigned(Inst) then
  try
    TWininetRequestTransport(Inst).Connect(Server, Port, UserName, Password, UseSSL);
    Result := true;
  except
  end;
end;

procedure Disconnect(Inst: Pointer); stdcall;
begin
  if Assigned(Inst) then
    TWininetRequestTransport(Inst).Disconnect;
end;

function SendRequest(Inst, Data: Pointer; Length: Integer): PExternalTransportResponce; stdcall;
var
  S: string;
begin
  Result := nil;
  if Assigned(Inst) then
  try
    SetLength(S, Length);
    Move(Data^, S[1], Length);
    S := TWininetRequestTransport(Inst).SendRequest(nil, S);
    New(Result);
    Result.Length := System.Length(S);
    GetMem(Result.Data, Result.Length);
    Move(S[1], Result.Data^, Result.Length);
    Result.Free := FreeResponce;
  except
    FreeResponce(Result);
    Result := nil;
  end
end;

{ TWininetRequestTransport }

constructor TWininetRequestTransport.Create;
begin
  inherited Create;
  FRequestLock := TCriticalSection.Create;
  FSession := InternetOpen('AriaUI', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  //INTERNET_OPTION_CONNECT_TIMEOUT
  //INTERNET_OPTION_CONNECTED_STATE
end;

destructor TWininetRequestTransport.Destroy;
begin
  Disconnect;
  if Assigned(FSession) then
    InternetCloseHandle(FSession);
  FreeAndNil(FRequestLock);
  inherited;
end;

procedure TWininetRequestTransport.Connect(const Server: string; Port: Word; const UserName, Password: string; UseSSL: Boolean);
begin
  if FSession = nil then
    raise Exception.Create('WinInet not initialized');
  Disconnect;
  FRequestLock.Acquire;
  try
    FConnection := InternetConnect(FSession, PChar(Server), Port, PChar(UserName), PChar(Password), INTERNET_SERVICE_HTTP, INTERNET_FLAG_EXISTING_CONNECT, 0);
    if FConnection = nil then
      raise Exception.Create('Can''t connect to Aria2 server');
    FRequestFlags := INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD;
    if UseSSL then
      FRequestFlags := FRequestFlags or INTERNET_FLAG_SECURE{ or INTERNET_FLAG_IGNORE_CERT_CN_INVALID or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID};
  finally
    FRequestLock.Release;
  end;
end;

procedure TWininetRequestTransport.Disconnect;
begin
  FRequestLock.Acquire;
  try
    if Assigned(FConnection) then
      InternetCloseHandle(FConnection);
    FConnection := nil;
  finally
    FRequestLock.Release;
  end;
end;

function TWininetRequestTransport.SendRequest(Sender: TObject; const Request: string): string;
var
  Req: HINTERNET;
  Len, Avail, Read: Cardinal;
begin
  Result := '';
  if FConnection = nil then
    raise Exception.Create('No connection to Aria2 server');
  FRequestLock.Acquire;
  try
    Req := HttpOpenRequest(FConnection, 'POST', '/jsonrpc', nil, nil, nil, FRequestFlags, 0);
    if Req = nil then
      raise Exception.Create('Can''t open request');
    try
      if not HttpSendRequest(Req, PChar('Content-Length: ' + IntToStr(Length(Request))), Cardinal(-1), PChar(Request), Length(Request)) then
        raise Exception.Create('Can''t send request');
      while InternetQueryDataAvailable(Req, Avail, 0, 0) do
      begin
        if Avail = 0 then Break;
        Len := Length(Result);
        SetLength(Result, Len + Avail);
        if not InternetReadFile(Req, @Result[Len + 1], Avail, Read) then Break;
        if Read < Avail then
          SetLength(Result, Length(Result) - Avail + Read);
      end;
    finally
      InternetCloseHandle(Req);
    end;
  finally
    FRequestLock.Release;
  end;
end;

exports
  Create, Free, Connect, Disconnect, SendRequest;

begin
end.