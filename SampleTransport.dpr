library SampleTransport;

uses
  Windows, WinInet;

type
  PExternalTransportResponce = ^TExternalTransportResponce;
  TExternalTransportResponce = record
    Data: Pointer;
    Length: Integer;
    Free: procedure(P: PExternalTransportResponce); stdcall;
  end;
  PTransportInstance = ^TTransportInstance;
  TTransportInstance = record
    Session, Connection: HINTERNET;
    RequestFlags: Cardinal;
    RequestLock: TRTLCriticalSection;
  end;

function IntToStr(I: Integer): string;
begin
  Str(I, Result);
end;

procedure FreeResponce(P: PExternalTransportResponce); stdcall;
begin
  if not Assigned(P) then Exit;
  FreeMem(P.Data);
  Dispose(P);
end;

function Create: Pointer; stdcall;
begin
  New(PTransportInstance(Result));
  ZeroMemory(Result, SizeOf(TTransportInstance));
  with PTransportInstance(Result)^ do
  begin
    InitializeCriticalSection(RequestLock);
    Session := InternetOpen('AriaUI Sample Transport', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  end;
end;

procedure Disconnect(Inst: Pointer); stdcall; forward;

procedure Free(Inst: Pointer); stdcall;
begin
  if not Assigned(Inst) then Exit;
  with PTransportInstance(Inst)^ do
  begin
    Disconnect(Inst);
    if Assigned(Session) then
      InternetCloseHandle(Session);
    DeleteCriticalSection(RequestLock);
  end;
  Dispose(Inst);
end;

function Connect(Inst: Pointer; Server: PChar; Port: Word; UserName, Password: PChar; UseSSL: LongBool): LongBool; stdcall;
begin
  Result := false;
  if not Assigned(Inst) then Exit;
  with PTransportInstance(Inst)^ do
  begin
    if not Assigned(Session) then Exit;
    Disconnect(Inst);
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

procedure Disconnect(Inst: Pointer); stdcall;
begin
  if not Assigned(Inst) then Exit;
  with PTransportInstance(Inst)^ do
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

function SendRequest(Inst, Data: Pointer; Length: Integer): PExternalTransportResponce; stdcall;
var
  Req: HINTERNET;
  Avail, Read: Cardinal;
begin
  Result := nil;
  if not Assigned(Inst) then Exit;
  with PTransportInstance(Inst)^ do
  begin
    if not Assigned(Connection) then Exit;
    EnterCriticalSection(RequestLock);
    try
      Req := HttpOpenRequest(Connection, 'POST', '/jsonrpc', nil, nil, nil, RequestFlags, 0);
      if not Assigned(Req) then Exit;
      try
        if not HttpSendRequest(Req, PChar('Content-Length: ' + IntToStr(Length)), Cardinal(-1), Data, Length) then Exit;
        New(Result);
        ZeroMemory(Result, SizeOf(TExternalTransportResponce));
        Result.Free := FreeResponce;
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

exports
  Create, Free, Connect, Disconnect, SendRequest;

begin
  IsMultiThread := true;
end.