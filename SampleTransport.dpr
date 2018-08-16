library SampleTransport;

uses
  Windows, WinInet;

{$I ExternalTransport.inc}

type
  PTransportInstance = ^TTransportInstance;
  TTransportInstance = record
    BaseInstance: TExternalTransportInstance;
    Session, Connection: HINTERNET;
    RequestFlags: Cardinal;
    RequestLock: TRTLCriticalSection;
  end;

function IntToStr(I: Integer): string;
begin
  Str(I, Result);
end;

procedure FreeResponse(Self: PExternalTransportResponse); stdcall;
begin
  if not Assigned(Self) then Exit;
  FreeMem(Self.Data);
  Dispose(Self);
end;

function Connect(Self: PExternalTransportInstance; Server: PChar; Port: Word; UserName, Password: PChar; UseSSL: LongBool): LongBool; stdcall;
begin
  Result := false;
  if not Assigned(Self) then Exit;
  with PTransportInstance(Self)^ do
  begin
    if not Assigned(Session) then Exit;
    BaseInstance.Disconnect(Self);
    EnterCriticalSection(RequestLock);
    try
      Connection := InternetConnect(Session, Server, Port, UserName, Password, INTERNET_SERVICE_HTTP, INTERNET_FLAG_EXISTING_CONNECT, 0);
      if not Assigned(Connection) then Exit;
      RequestFlags := INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_NO_COOKIES or INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD;
      if UseSSL then
        RequestFlags := RequestFlags or INTERNET_FLAG_SECURE;
      Result := true;
    finally
      LeaveCriticalSection(RequestLock);
    end;
  end;
end;

procedure Disconnect(Self: PExternalTransportInstance); stdcall;
begin
  if not Assigned(Self) then Exit;
  with PTransportInstance(Self)^ do
  begin
    EnterCriticalSection(RequestLock);
    try
      if Assigned(Connection) then
        InternetCloseHandle(Connection);
      Connection := nil;
    finally
      LeaveCriticalSection(RequestLock);
    end;
  end;
end;

function SendRequest(Self: PExternalTransportInstance; Data: Pointer; Length: Integer): PExternalTransportResponse; stdcall;
var
  Req: HINTERNET;
  Avail, Read: Cardinal;
begin
  Result := nil;
  if not Assigned(Self) then Exit;
  with PTransportInstance(Self)^ do
  begin
    if not Assigned(Connection) then Exit;
    EnterCriticalSection(RequestLock);
    try
      Req := HttpOpenRequest(Connection, 'POST', '/jsonrpc', nil, nil, nil, RequestFlags, 0);
      if not Assigned(Req) then Exit;
      try
        if not HttpSendRequest(Req, PChar('Content-Length: ' + IntToStr(Length)), Cardinal(-1), Data, Length) then Exit;
        New(Result);
        ZeroMemory(Result, SizeOf(TExternalTransportResponse));
        Result.Free := FreeResponse;
        while InternetQueryDataAvailable(Req, Avail, 0, 0) do
        begin
          if Avail = 0 then Break;
          ReallocMem(Result.Data, Cardinal(Result.Length) + Avail);
          if not InternetReadFile(Req, Pointer(Cardinal(Result.Data) + Cardinal(Result.Length)), Avail, Read) then Break;
          Inc(Result.Length, Read);
        end;
      finally
        InternetCloseHandle(Req);
      end;
    finally
      LeaveCriticalSection(RequestLock);
    end;
  end
end;

procedure FreeInstance(Self: PExternalTransportInstance); stdcall;
begin
  if not Assigned(Self) then Exit;
  with PTransportInstance(Self)^ do
  begin
    BaseInstance.Disconnect(Self);
    if Assigned(Session) then
      InternetCloseHandle(Session);
    DeleteCriticalSection(RequestLock);
  end;
  Dispose(PTransportInstance(Self));
end;

function TransportCreate: PExternalTransportInstance; stdcall;
begin
  New(PTransportInstance(Result));
  ZeroMemory(Result, SizeOf(TTransportInstance));
  with PTransportInstance(Result)^ do
  begin
    BaseInstance.Connect := Connect;
    BaseInstance.Disconnect := Disconnect;
    BaseInstance.SendRequest := SendRequest;
    BaseInstance.Free := FreeInstance;
    InitializeCriticalSection(RequestLock);
    Session := InternetOpen('AriaUI Sample Transport', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  end;
end;

exports
  TransportCreate;

begin
  IsMultiThread := true;
end.