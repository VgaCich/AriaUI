unit RequestTransport;

interface

uses
  Windows, WinInet, AvL;

type
  TRequestTransport = class
  private
    FSession, FConnection: HINTERNET;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const Server: string; Port: Word; const UserName, Password: string);
    procedure Disconnect;
    function SendRequest(Sender: TObject; const Request: string): string;
  end;

implementation

{ TRequestTransport }

constructor TRequestTransport.Create;
begin
  inherited Create;
  FSession := InternetOpen('AriaUI', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  //INTERNET_OPTION_CONNECT_TIMEOUT
  //INTERNET_OPTION_CONNECTED_STATE
end;

destructor TRequestTransport.Destroy;
begin
  Disconnect;
  if Assigned(FSession) then
    InternetCloseHandle(FSession);
  inherited;
end;

procedure TRequestTransport.Connect(const Server: string; Port: Word; const UserName, Password: string);
begin
  if FSession = nil then
    raise Exception.Create('WinInet not initialized');
  FConnection := InternetConnect(FSession, PChar(Server), Port, PChar(UserName), PChar(Password), INTERNET_SERVICE_HTTP, INTERNET_FLAG_EXISTING_CONNECT, 0);
  if FConnection = nil then
    raise Exception.Create('Can''t connect to Aria2 server');
end;

procedure TRequestTransport.Disconnect;
begin
  if Assigned(FConnection) then
    InternetCloseHandle(FConnection);
  FConnection := nil;
end;

function TRequestTransport.SendRequest(Sender: TObject; const Request: string): string;
var
  Req: HINTERNET;
  Len, Avail, Read: Cardinal;
begin
  Result := '';
  if FConnection = nil then
    raise Exception.Create('No connection to Aria2 server');
  Req := HttpOpenRequest(FConnection, 'POST', '/jsonrpc', nil, nil, nil, INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD {or INTERNET_FLAG_SECURE or INTERNET_FLAG_IGNORE_CERT_CN_INVALID or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID}, 0);
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
end;

end.