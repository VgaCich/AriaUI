program Aria2RPC;

{$APPTYPE CONSOLE}

uses AvL, avlUtils, avlJson, Aria2, RequestTransport;

type
  TAria2G = class(TAria2)
  public
    function SendRequest(const Method, Params: string): PJsonValue;
  end;

var
  A2: TAria2G;
  RT: TRequestTransport;
  Opt: array[0..5] of record Key, Value: string; end = (
    (Key: 'h'; Value: 'localhost'),
    (Key: 'p'; Value: '6800'),
    (Key: 'u'; Value: ''),
    (Key: 'pw'; Value: ''),
    (Key: 't'; Value: ''),
    (Key: 's'; Value: ''));
  Method: string = '';
  Params: string = '';

{ TAria2G }

function TAria2G.SendRequest(const Method, Params: string): PJsonValue;
begin
  Result := GetResult(inherited SendRequest(Method, Params));
end;

procedure SetOption(const Key, Value: string);
var
  i: Integer;
begin
  for i := 0 to High(Opt) do
    if Opt[i].Key = Key then
    begin
      Opt[i].Value := Value;
      Exit;
    end;
  WriteLn('Unknown option: ' + Key);
end;

function GetOption(const Key: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(Opt) do
    if Opt[i].Key = Key then
    begin
      Result := Opt[i].Value;
      Exit;
    end;
end;

var
  i: Integer;
  Res: PJsonValue;

begin
  WriteLn('Aria2 RPC 1.1 (c) Vga 2017-2018');
  if ParamCount = 0 then
  begin
    WriteLn('Usage: aria2rpc [options] method params');
    WriteLn('Options: -h host, -p port, -u username, -pw password, -t token -s secure');
  end;
  i := 1;
  while i <= ParamCount do
  begin
    if (ParamStr(i)[1] = '-') and (ParamStr(i) <> '-') then
    begin
      SetOption(Copy(ParamStr(i), 2, MaxInt), ParamStr(i + 1));
      Inc(i);
    end
    else if Method = '' then
      Method := ParamStr(i)
    else if Params = '' then
      Params := ParamStr(i)
    else
      WriteLn('Ignored: ' + ParamStr(i));
    Inc(i);
  end;
  if Method = '' then
  begin
    WriteLn('Method not specified');
    Exit;
  end;
  if FileExists(GetOption('t')) then
    SetOption('t', LoadFile(GetOption('t')));
  if Method = '-' then
    ReadLn(Method);
  if Params = '-' then
    ReadLn(Params);
  RT := TRequestTransport.Create;
  A2 := TAria2G.Create(RT.SendRequest, GetOption('t'));
  try
    try
      RT.Connect(GetOption('h'), StrToInt(GetOption('p')), GetOption('u'), GetOption('pw'), GetOption('s') <> '');
      Res := A2.SendRequest(Method, Params);
      try
        WriteLn(JsonToStr(Res));
      finally
        JsonFree(Res);
      end;
    except
      on E: Exception do WriteLn('Error: ' + E.Message);
    end;
  finally
    A2.Free;
    RT.Free;
  end;
end.
