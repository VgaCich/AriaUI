unit Aria2;

interface

uses
  AvL, avlUtils, avlJSON, Base64;

type
  TOnRPCRequest = function(Sender: TObject; const Request: string): string of object;
  TAria2GID = Int64;
  TAria2GIDArray = array of TAria2GID;
  TAria2PosOrigin = (poFromBeginning, poFromCurrent, poFromEnd);
  TAria2Status = (asActive, asWaiting, asPaused, asError, asComplete, asRemoved);
  TAria2TorrentMode = (atmSingle, atmMulti);
  TAria2UriStatus = (ausUsed, ausWaiting);
  TAria2 = class
  private
    FOnRequest: TOnRPCRequest;
    FRPCSecret: string;
    function AddToken(const Params: string): string;
    function NewId: string;
  protected
    function SendRequest(const Method, Params: string): PJsonValue;
    function SendRequestStr(const Method, Params: string): string;
  public
    constructor Create(OnRequest: TOnRPCRequest; const RPCSecret: string = '');
    function AddUri(const Uris: array of string; const Options: string = ''; Position: Integer = -1): TAria2GID;
    function AddTorrent(const Torrent: string; const Uris: array of string; const Options: string = ''; Position: Integer = -1): TAria2GID;
    function AddMetalink(const Metalink: string; const Options: string = ''; Position: Integer = -1): TAria2GIDArray;
    function Remove(GID: TAria2GID; Force: Boolean = false): TAria2GID;
    function Pause(GID: TAria2GID; Force: Boolean = false): TAria2GID;
    function PauseAll(Force: Boolean = false): Boolean;
    function Unpause(GID: TAria2GID): TAria2GID;
    function UnpauseAll: Boolean;
    function TellStatus(GID: TAria2GID; const Keys: array of string): PJsonValue;
    function GetUris(GID: TAria2GID): PJsonValue;
    function GetFiles(GID: TAria2GID): PJsonValue;
    function GetPeers(GID: TAria2GID): PJsonValue;
    function GetServers(GID: TAria2GID): PJsonValue;
    function TellActive(const Keys: array of string): PJsonValue;
    function TellWaiting(Offset, Num: Integer; const Keys: array of string): PJsonValue;
    function TellStopped(Offset, Num: Integer; const Keys: array of string): PJsonValue;
    function ChangePosition(GID: TAria2GID; Pos: Integer; Origin: TAria2PosOrigin): Integer;
    function ChangeUri(GID: TAria2GID; FileIndex: Integer; const DelUris, AddUris: string; Position: Integer = -1): Cardinal;
    function GetOptions(GID: TAria2GID): PJsonValue;
    function ChangeOptions(GID: TAria2GID; const Options: string): Boolean;
    function GetGlobalOptions: PJsonValue;
    function ChangeGlobalOptions(const Options: string): Boolean;
    function GetGlobalStats: PJsonValue;
    function PurgeDownloadResult: Boolean;
    function RemoveDownloadResult(GID: TAria2GID): Boolean;
    function GetVersion(Features: Boolean = false): string;
    function GetSessionInfo: PJsonValue;
    function Shutdown(Force: Boolean = false): Boolean;
    function SaveSession: Boolean;
    property OnRequest: TOnRPCRequest read FOnRequest write FOnRequest;
    property RPCSecret: string read FRPCSecret write FRPCSecret;
  end;

function StrToGID(const S: string): TAria2GID;
function GIDToStr(GID: TAria2GID): string;
function StrToEnum(const S: string; const Values: array of string): Integer;

const
  sfGID = 'gid';
  sfStatus = 'status';
  sfTotalLength = 'totalLength';
  sfCompletedLength = 'completedLength';
  sfUploadLength = 'uploadLength';
  sfBitfield = 'bitfield';
  sfDownloadSpeed = 'downloadSpeed';
  sfUploadSpeed = 'uploadSpeed';
  sfInfoHash = 'infoHash';
  sfNumSeeders = 'numSeeders';
  sfSeeder = 'seeder';
  sfPieceLength = 'pieceLength';
  sfNumPieces = 'numPieces';
  sfConnections = 'connections';
  sfErrorCode = 'errorCode';
    //The error codes are defined in the `EXIT STATUS`_ section.
  sfErrorMessage = 'errorMessage';
  sfFollowedBy = 'followedBy';
  sfFollowing = 'following';
  sfBelongsTo = 'belongsTo';
  sfDir = 'dir';
  sfFiles = 'files';
  sfBittorrent = 'bittorrent';
  sfAnnounceList = 'announceList';
  sfComment = 'comment';
  sfCreationDate = 'creationDate';
  sfMode = 'mode';
  sfInfo = 'info';
  sfName = 'name';
  sfVerifiedLength = 'verifiedLength';
  sfVerifyPenfing = 'verifyIntegrityPending';
  sfUri = 'uri';
  sfUris = 'uris';
  sfIndex = 'index';
  sfPath = 'path';
  sfLength = 'length';
  sfSelected = 'selected';
  sfPeerId = 'peerId';
  sfIP = 'ip';
  sfPorts = 'port';
  sfAmChoking = 'amChoking';
  sfPeerChoking = 'peerChoking';
  sfServers = 'servers';
  sfCurrentUri = 'currentUri';
  sfNumActive = 'numActive';
  sfNumWaiting = 'numWaiting';
  sfNumStopped = 'numStopped';
  sfNumStoppedTotal = 'numStoppedTotal';
  sfSessionId = 'sessionId';
  sfBoolValues: array[Boolean] of string = ('false', 'true');
  sfStatusValues: array[TAria2Status] of string = ('active', 'waiting', 'paused', 'error', 'complete', 'removed');
  sfbtModeValues: array[TAria2TorrentMode] of string = ('single', 'multi');
  sfUriStatusValues: array[TAria2UriStatus] of string = ('used', 'waiting');

implementation

function StrToGID(const S: string): TAria2GID;
begin
  Result := StrToInt64('$' + S);
end;

function GIDToStr(GID: TAria2GID): string;
var
  G: packed record Lo, Hi: Integer; end absolute GID;
begin
  Result := LowerCase(IntToHex(G.Hi, 8) + IntToHex(G.Lo, 8));
end;

function StrToEnum(const S: string; const Values: array of string): Integer;
begin
  for Result := Low(Values) to High(Values) do
    if Values[Result] = S then
      Exit;
  Result := 0;
end;

function MakeDword(Lo, Hi: Word): Cardinal;
begin
  Result := (Hi shl 16) or Lo;
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

function Quote(const S: string): string;
begin
  if S = '' then
    Result := S
  else
    Result := '"' + S + '"';
end;

function ArrayToJson(const Items: array of string): string;
var
  i: Integer;
begin
  if Length(Items) < 1 then
  begin
    Result := '';
    Exit;
  end;
  Result := '[';
  for i := 0 to High(Items) do
    Result := Result + Quote(Items[i]) + ',';
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

constructor TAria2.Create(OnRequest: TOnRPCRequest; const RPCSecret: string);
begin
  inherited Create;
  FOnRequest := OnRequest;
  FRPCSecret := RPCSecret;
end;

function TAria2.AddUri(const Uris: array of string; const Options: string; Position: Integer): TAria2GID;
begin
  Result := StrToGID(SendRequestStr('aria2.addUri', MakeParams(['', '{}', ''],
    [ArrayToJson(Uris), Options, Check(Position >= 0, IntToStr(Position))])));
end;

function TAria2.AddTorrent(const Torrent: string; const Uris: array of string; const Options: string; Position: Integer): TAria2GID;
begin
  Result := StrToGID(SendRequestStr('aria2.addTorrent', MakeParams(['', '[]', '{}', ''],
    [Quote(Base64Encode(Torrent)), ArrayToJson(Uris), Options, Check(Position >= 0, IntToStr(Position))])));
end;

function TAria2.AddMetalink(const Metalink: string; const Options: string; Position: Integer): TAria2GIDArray;
var
  i: Integer;
  Res: PJsonValue;
begin
  Result := nil;
  Res := SendRequest('aria2.addMetalink', MakeParams(['', '{}', ''],
    [Quote(Base64Encode(Metalink)), Options, Check(Position >= 0, IntToStr(Position))]));
  try
    if Res.VType <> jtArray then Exit;
    SetLength(Result, Res.Arr.Length);
    for i := 0 to Res.Arr.Length - 1 do
      Result[i] := StrToGID(JsonStr(JsonItem(Res, i)));
  finally
    JsonFree(Res);
  end;
end;

function TAria2.Remove(GID: TAria2GID; Force: Boolean): TAria2GID;
const
  Method: array[Boolean] of string = ('aria2.remove', 'aria2.forceRemove');
begin
  Result := StrToGID(SendRequestStr(Method[Force], Quote(GIDToStr(GID))));
end;

function TAria2.Pause(GID: TAria2GID; Force: Boolean): TAria2GID;
const
  Method: array[Boolean] of string = ('aria2.pause', 'aria2.forcePause');
begin
  Result := StrToGID(SendRequestStr(Method[Force], Quote(GIDToStr(GID))));
end;

function TAria2.PauseAll(Force: Boolean): Boolean;
const
  Method: array[Boolean] of string = ('aria2.pauseAll', 'aria2.forcePauseAll');
begin
  Result := SendRequestStr(Method[Force], '') = 'OK';
end;

function TAria2.Unpause(GID: TAria2GID): TAria2GID;
begin
  Result := StrToGID(SendRequestStr('aria2.unpause', Quote(GIDToStr(GID))));
end;

function TAria2.UnpauseAll: Boolean;
begin
  Result := SendRequestStr('aria2.unpauseAll', '') = 'OK';
end;

function TAria2.TellStatus(GID: TAria2GID; const Keys: array of string): PJsonValue;
begin
  Result := SendRequest('aria2.tellStatus', MakeParams(['""', ''], [Quote(GIDToStr(GID)), ArrayToJson(Keys)]));
end;

function TAria2.GetUris(GID: TAria2GID): PJsonValue;
begin
  Result := SendRequest('aria2.getUris', Quote(GIDToStr(GID)));
end;

function TAria2.GetFiles(GID: TAria2GID): PJsonValue;
begin
  Result := SendRequest('aria2.getFiles', Quote(GIDToStr(GID)));
end;

function TAria2.GetPeers(GID: TAria2GID): PJsonValue;
begin
  Result := SendRequest('aria2.getPeers', Quote(GIDToStr(GID)));
end;

function TAria2.GetServers(GID: TAria2GID): PJsonValue;
begin
  Result := SendRequest('aria2.getServers', Quote(GIDToStr(GID)));
end;

function TAria2.TellActive(const Keys: array of string): PJsonValue;
begin
  Result := SendRequest('aria2.tellActive', ArrayToJson(Keys));
end;

function TAria2.TellWaiting(Offset, Num: Integer; const Keys: array of string): PJsonValue;
begin
  Result := SendRequest('aria2.tellWaiting', MakeParams(['', '', ''], [IntToStr(Offset), IntToStr(Num), ArrayToJson(Keys)]));
end;

function TAria2.TellStopped(Offset, Num: Integer; const Keys: array of string): PJsonValue;
begin
  Result := SendRequest('aria2.tellStopped', MakeParams(['', '', ''], [IntToStr(Offset), IntToStr(Num), ArrayToJson(Keys)]));
end;

function TAria2.ChangePosition(GID: TAria2GID; Pos: Integer; Origin: TAria2PosOrigin): Integer;
const
  OriginValues: array[TAria2PosOrigin] of string = ('"POS_SET"', '"POS_CUR"', '"POS_END"');
var
  Res: PJsonValue;
begin
  Res := SendRequest('aria2.changePosition', MakeParams(['""', '', ''], [Quote(GIDToStr(GID)), IntToStr(Pos), OriginValues[Origin]]));
  try
    Result := JsonInt(Res);
  finally
    JsonFree(Res);
  end;
end;

function TAria2.ChangeUri(GID: TAria2GID; FileIndex: Integer; const DelUris, AddUris: string; Position: Integer): Cardinal;
var
  Res: PJsonValue;
begin
  Res := SendRequest('aria2.changeUri', MakeParams(['""', '', '[]', '[]', ''],
    [Quote(GIDToStr(GID)), IntToStr(FileIndex), ArrayToJson(DelUris), ArrayToJson(AddUris), Check(Position >= 0, IntToStr(Position))]));
  try
    Result := MakeDword(JsonInt(JsonItem(Res, 1)), JsonInt(JsonItem(Res, 0)));
  finally
    JsonFree(Res);
  end;
end;

function TAria2.GetOptions(GID: TAria2GID): PJsonValue;
begin
  Result := SendRequest('aria2.getOption', Quote(GIDToStr(GID)));
end;

function TAria2.ChangeOptions(GID: TAria2GID; const Options: string): Boolean;
begin
  Result := SendRequestStr('aria2.changeOption', MakeParams(['""', '""'], [Quote(GIDToStr(GID)), Options])) = 'OK';
end;

function TAria2.GetGlobalOptions: PJsonValue;
begin
  Result := SendRequest('aria2.getGlobalOption', '');
end;

function TAria2.ChangeGlobalOptions(const Options: string): Boolean;
begin
  Result := SendRequestStr('aria2.changeGlobalOption', Options) = 'OK';
end;

function TAria2.GetGlobalStats: PJsonValue;
begin
  Result := SendRequest('aria2.getGlobalStat', '');
end;

function TAria2.PurgeDownloadResult: Boolean;
begin
  Result := SendRequestStr('aria2.purgeDownloadResult', '') = 'OK';
end;

function TAria2.RemoveDownloadResult(GID: TAria2GID): Boolean;
begin
  Result := SendRequestStr('aria2.removeDownloadResult', Quote(GIDToStr(GID))) = 'OK';
end;

function TAria2.GetVersion(Features: Boolean): string;
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
      Result := JsonStr(JsonItem(Res, 'version'));
      if Features then
      begin
        Result := Result + ' (features: ';
        with JsonItem(Res, 'enabledFeatures')^ do
          for i := 0 to Arr.Length - 1 do
            Result := Result + JsonStr(Arr.Values[i]) + Term[i = Integer(Arr.Length) - 1];
      end;
    end;
  finally
    JsonFree(Res);
  end;
end;

function TAria2.GetSessionInfo: PJsonValue;
begin
  Result := SendRequest('aria2.getSessionInfo', '');
end;

function TAria2.Shutdown(Force: Boolean): Boolean;
const
  Method: array[Boolean] of string = ('aria2.shutdown', 'aria2.forceShutdown');
begin
  Result := SendRequestStr(Method[Force], '') = 'OK';
end;

function TAria2.SaveSession: Boolean;
begin
  Result := SendRequestStr('aria2.saveSession', '') = 'OK';
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
