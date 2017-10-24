unit Aria2;

interface

uses
  AvL, avlUtils, avlJSON, Base64;

type
  TOnRPCRequest = function(Sender: TObject; const Request: string): string of object;
  TAria2GID = Int64;
  TAria2Options = class
  private
    FJson: string;
  public
    property Json: string read FJson;
  end;
  {TAria2Transfer = class
  private

  public

  end;}
  TAria2 = class
  private
    FOnRequest: TOnRPCRequest;
    FRPCSecret: string;
    function AddToken(const Params: string): string;
    function NewId: string;
    function SendRequest(const Method, Params: string): PJsonValue;
    function SendRequestStr(const Method, Params: string): string;
  public
    constructor Create(OnRequest: TOnRPCRequest; const RPCSecret: string = '');
    function AddUri(const Uris: array of string; Options: TAria2Options = nil; Position: Integer = -1): TAria2GID;
    function AddTorrent(const Torrent: string; const Uris: array of string; Options: TAria2Options = nil; Position: Integer = -1): TAria2GID;
    function AddMetalink(const Metalink: string; Options: TAria2Options = nil; Position: Integer = -1): TAria2GID;
    function Remove(GID: TAria2GID; Force: Boolean = false): TAria2GID;
    function Pause(GID: TAria2GID; Force: Boolean = false): TAria2GID;
    function PauseAll(Force: Boolean): Boolean;
    function Unpause(GID: TAria2GID): TAria2GID;
    function UnpauseAll: Boolean;
    function GetVersion: string;
    function Shutdown(Force: Boolean = false): Boolean;
    property OnRequest: TOnRPCRequest read FOnRequest write FOnRequest;
    property RPCSecret: string read FRPCSecret write FRPCSecret;
  end;

function StrToGID(const S: string): TAria2GID;
function GIDToStr(GID: TAria2GID): string;

implementation

function StrToGID(const S: string): TAria2GID;
begin
  Result := StrToInt64('$' + S);
end;

function GIDToStr(GID: TAria2GID): string;
var
  G: packed record Lo, Hi: Integer; end absolute GID;
begin
  Result := IntToHex(G.Hi, 8) + IntToHex(G.Lo, 8);
end;

function Select(Exp: Boolean; const STrue, SFalse: string): string;
begin
  if Exp then
    Result := STrue
  else
    Result := SFalse;
end;

function Check(Exp: Boolean; const STrue: string): string;
begin
  if Exp then
    Result := STrue
  else
    Result := '';
end;

function UrisToJson(const Uris: array of string): string;
var
  i: Integer;
begin
  if Length(Uris) < 1 then
  begin
    Result := '';
    Exit;
  end;
  Result := '[';
  for i := 0 to High(Uris) do
    Result := Result + '"' + Uris[i] + '",';
  Result[Length(Result)] := ']';
end;

function MakeParams(const Placeholders, Params: array of string): string;
var
  i, ActualParams: Integer;
begin
  Result := '';
  if (Length(Params) = 0) or (Length(Placeholders) <> Length(Params)) then Exit;
  ActualParams := 0;
  for i := 0 to High(Params) do
    if Params[i] <> '' then
      ActualParams := i;
  for i := 0 to ActualParams do
    Result := Result + Select(Params[i] <> '', Params[i], Placeholders[i]) + ',';
  Delete(Result, Length(Result), 1);
end;

{ TAria2 }

constructor TAria2.Create(OnRequest: TOnRPCRequest; const RPCSecret: string = '');
begin
  inherited Create;
  FOnRequest := OnRequest;
  FRPCSecret := RPCSecret;
end;

function TAria2.AddUri(const Uris: array of string; Options: TAria2Options; Position: Integer): TAria2GID;
begin
  Result := StrToGID(SendRequestStr('aria2.addUri', MakeParams(['', '{}', ''],
    [UrisToJson(Uris), Check(Assigned(Options), Options.Json), Check(Position >= 0, IntToStr(Position))])));
end;

function TAria2.AddTorrent(const Torrent: string; const Uris: array of string; Options: TAria2Options = nil; Position: Integer = -1): TAria2GID;
begin
  Result := StrToGID(SendRequestStr('aria2.addTorrent', MakeParams(['', '[]', '{}', ''],
    [Base64Encode(Torrent), UrisToJson(Uris), Check(Assigned(Options), Options.Json), Check(Position >= 0, IntToStr(Position))])));
end;

function TAria2.AddMetalink(const Metalink: string; Options: TAria2Options = nil; Position: Integer = -1): TAria2GID;
begin
  Result := StrToGID(SendRequestStr('aria2.addMetalink', MakeParams(['', '{}', ''],
    [Base64Encode(Metalink), Check(Assigned(Options), Options.Json), Check(Position >= 0, IntToStr(Position))])));
end;

function TAria2.Remove(GID: TAria2GID; Force: Boolean = false): TAria2GID;
const
  Method: array[Boolean] of string = ('aria2.remove', 'aria2.forceRemove');
begin
  Result := StrToGID(SendRequestStr(Method[Force], GIDToStr(GID)));
end;

function TAria2.Pause(GID: TAria2GID; Force: Boolean = false): TAria2GID;
const
  Method: array[Boolean] of string = ('aria2.pause', 'aria2.forcePause');
begin
  Result := StrToGID(SendRequestStr(Method[Force], GIDToStr(GID)));
end;

function TAria2.PauseAll(Force: Boolean): Boolean;
const
  Method: array[Boolean] of string = ('aria2.pauseAll', 'aria2.forcePauseAll');
begin
  Result := SendRequestStr(Method[Force], '') = 'OK';
end;

function TAria2.Unpause(GID: TAria2GID): TAria2GID;
begin
  Result := StrToGID(SendRequestStr('aria2.unpause', GIDToStr(GID)));
end;

function TAria2.UnpauseAll: Boolean;
begin
  Result := SendRequestStr('aria2.unpauseAll', '') = 'OK';
end;

function TAria2.GetVersion: string;
const
  Term: array[Boolean] of string = (', ', ')');
var
  i: Integer;
  Res: PJsonValue;
begin
  Result := '';
  Res := SendRequest('aria2.getVersion', '');
  try
    if Assigned(Res) then
    begin
      Result := JsonStr(JsonItem(Res, 'version')) + ' (features: ';
      with JsonItem(Res, 'enabledFeatures')^ do
        for i := 0 to Arr.Length - 1 do
          Result := Result + JsonStr(Arr.Values[i]) + Term[i = Integer(Arr.Length) - 1];
    end;
  finally
    JsonFree(Res);
  end;
end;

function TAria2.Shutdown(Force: Boolean): Boolean;
const
  Method: array[Boolean] of string = ('aria2.shutdown', 'aria2.forceShutdown');
begin
  Result := SendRequestStr(Method[Force], '') = 'OK';
end;

function TAria2.AddToken(const Params: string): string;
begin
  Result := '';
  if FRPCSecret <> '' then
  begin
    Result := '"token:' + FRPCSecret + '"';
    if Params <> '' then
      Result := Result + ','
  end;
  Result := Result + Params;
end;

function TAria2.NewId: string;
begin
  Result := IntToHex(Random(MaxInt), 8);
end;

function TAria2.SendRequest(const Method, Params: string): PJsonValue;
const
  RequestTemplate = '{"jsonrpc":"2.0","id":"%s","method":"%s","params":[%s]}';
var
  Id: string;
  Reply: PJsonValue;
begin
  Result := nil;
  if not Assigned(FOnRequest) then
    raise Exception.Create('Aria2: no transport provided');
  Id := NewId;
  Reply := JsonParse(FOnRequest(Self, Format(RequestTemplate, [Id, Method, AddToken(Params)])));
  if not Assigned(Reply) then
    raise Exception.Create('Aria2: invalid reply');
  try
    if JsonStr(JsonItem(Reply, 'id')) <> Id then
      raise Exception.Create('Aria2: reply id mismatch');
    if Assigned(JsonItem(Reply, 'error')) then
      raise Exception.CreateFmt('Aria2: request error %d: %s',
        [JsonInt(JsonItem(JsonItem(Reply, 'error'), 'code')),
         JsonStr(JsonItem(JsonItem(Reply, 'error'), 'message'))]);
    Result := JsonExtractItem(Reply, 'result');
    if not Assigned(Result) then
      raise Exception.Create('Aria2: invalid reply');
  finally
    JsonFree(Reply);
  end;
end;

function TAria2.SendRequestStr(const Method, Params: string): string;
var
  Res: PJsonValue;
begin
  Res := SendRequest(Method, Params);
  try
    Result := JsonStr(Res);
  finally
    JsonFree(Res);
  end;
end;

end.
