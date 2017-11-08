unit Aria2;

interface

uses
  AvL, avlUtils, avlJSON, Base64;

type
  TOnRPCRequest = function(Sender: TObject; const Request: string): string of object;
  TAria2GID = string;
  TAria2GIDArray = array of TAria2GID;
  TStringArray = array of string;
  TAria2PosOrigin = (poFromBeginning, poFromCurrent, poFromEnd);
  TAria2Status = (asActive, asWaiting, asPaused, asError, asComplete, asRemoved);
  TAria2TorrentMode = (atmSingle, atmMulti);
  TAria2UriStatus = (ausUsed, ausWaiting);
  TAria2Option = record
    Key, Value: string;
  end;
  TAria2OptionArray = array of TAria2Option;
  TAria2Struct = class
  private
    FIndex: Integer;
    FRoot: string;
    FJson: PJsonValue;
    function Find(Name: string): PJsonValue;
    function GetLength(const Name: string): Integer;
    function GetStr(const Name: string): string;
    function GetBool(const Name: string): Boolean;
    function GetInt(const Name: string): Integer;
    function GetInt64(const Name: string): Int64;
    function Exists(const Name: string): Boolean;
  public
    constructor Create(Json: PJsonValue);
    destructor Destroy; override;
    property Raw: PJsonValue read FJson;
    property Index: Integer read FIndex write FIndex;
    property Root: string read FRoot write FRoot;
    property Has[const Name: string]: Boolean read Exists;
    property Length[const Name: string]: Integer read GetLength;
    property Str[const Name: string]: string read GetStr; default;
    property Bool[const Name: string]: Boolean read GetBool;
    property Int[const Name: string]: Integer read GetInt;
    property Int64[const Name: string]: Int64 read GetInt64;
  end;
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
    function AddUri(const Uris: array of string; const Options: array of TAria2Option; Position: Integer = -1): TAria2GID;
    function AddTorrent(const Torrent: string; const Uris: array of string; const Options: array of TAria2Option; Position: Integer = -1): TAria2GID;
    function AddMetalink(const Metalink: string; const Options: array of TAria2Option; Position: Integer = -1): TAria2GIDArray;
    function Remove(GID: TAria2GID; Force: Boolean = false): TAria2GID;
    function Pause(GID: TAria2GID; Force: Boolean = false): TAria2GID;
    function PauseAll(Force: Boolean = false): Boolean;
    function Unpause(GID: TAria2GID): TAria2GID;
    function UnpauseAll: Boolean;
    function TellStatus(GID: TAria2GID; const Keys: array of string): TAria2Struct;
    function GetUris(GID: TAria2GID): TAria2Struct;
    function GetFiles(GID: TAria2GID): TAria2Struct;
    function GetPeers(GID: TAria2GID): TAria2Struct;
    function GetServers(GID: TAria2GID): TAria2Struct;
    function TellActive(const Keys: array of string): TAria2Struct;
    function TellWaiting(Offset, Num: Integer; const Keys: array of string): TAria2Struct;
    function TellStopped(Offset, Num: Integer; const Keys: array of string): TAria2Struct;
    function ChangePosition(GID: TAria2GID; Pos: Integer; Origin: TAria2PosOrigin): Integer;
    function ChangeUri(GID: TAria2GID; FileIndex: Integer; const DelUris, AddUris: string; Position: Integer = -1): Cardinal;
    function GetOptions(GID: TAria2GID): TAria2Struct;
    function ChangeOptions(GID: TAria2GID; const Options: array of TAria2Option): Boolean;
    function GetGlobalOptions: TAria2Struct;
    function ChangeGlobalOptions(const Options: array of TAria2Option): Boolean;
    function GetGlobalStats: TAria2Struct;
    function PurgeDownloadResult: Boolean;
    function RemoveDownloadResult(GID: TAria2GID): Boolean;
    function GetVersion(Features: Boolean = false): string;
    function GetSessionInfo: TAria2Struct;
    function Shutdown(Force: Boolean = false): Boolean;
    function SaveSession: Boolean;
    property OnRequest: TOnRPCRequest read FOnRequest write FOnRequest;
    property RPCSecret: string read FRPCSecret write FRPCSecret;
  end;

function StrToEnum(const S: string; const Values: array of string): Integer;
procedure SetArray(var Dest: TStringArray; const Src: array of string);

const
  // Status keys
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
  sfErrorMessage = 'errorMessage';
  sfFollowedBy = 'followedBy';
  sfFollowing = 'following';
  sfBelongsTo = 'belongsTo';
  sfDir = 'dir';
  sfFiles = 'files';
  sfBittorrent = 'bittorrent';
  sfAnnounceList = 'announceList';
  sfBTAnnounceList = 'bittorrent.announceList';
  sfComment = 'comment';
  sfBTComment = 'bittorrent.comment';
  sCreationDate = 'creationDate';
  sfBTCreationDate = 'bittorrent.creationDate';
  sfMode = 'mode';
  sfBTMode = 'bittorrent.mode';
  sfInfo = 'info';
  sfBTInfo = 'bittorrent.info';
  sfName = 'name';
  sfBTName = 'bittorrent.info.name';
  sfVerifiedLength = 'verifiedLength';
  sfVerifyPending = 'verifyIntegrityPending';
  //Uris keys (+sfStatus)
  sfUri = 'uri';
  //Files keys (+sfCompletedLength)
  sfIndex = 'index';
  sfPath = 'path';
  sfLength = 'length';
  sfSelected = 'selected';
  sfUris = 'uris';
  //Peers keys (+sfBitfield, sfDownloadSpeed, sfUploadSpeed, sfSeeder)
  sfPeerId = 'peerId';
  sfIP = 'ip';
  sfPorts = 'port';
  sfAmChoking = 'amChoking';
  sfPeerChoking = 'peerChoking';
  //Servers keys (+sfIndex, sfUri, sfDownloadSpeed)
  sfServers = 'servers';
  sfCurrentUri = 'currentUri';
  //GlobalStat keys (+sfDownloadSpeed, sfUploadSpeed)
  sfNumActive = 'numActive';
  sfNumWaiting = 'numWaiting';
  sfNumStopped = 'numStopped';
  sfNumStoppedTotal = 'numStoppedTotal';
  //SessionInfo keys
  sfSessionId = 'sessionId';
  //Enums
  sfBoolValues: array[Boolean] of string = ('false', 'true');
  sfStatusValues: array[TAria2Status] of string = ('active', 'waiting', 'paused', 'error', 'complete', 'removed');
  sfbtModeValues: array[TAria2TorrentMode] of string = ('single', 'multi');
  sfUriStatusValues: array[TAria2UriStatus] of string = ('used', 'waiting');
  //Input file options
  soAllProxy = 'all-proxy';
  soAllProxyPasswd = 'all-proxy-passwd';
  soAllProxyUser = 'all-proxy-user';
  soAllowOverwrite = 'allow-overwrite';
  soAllowPieceLengthChange = 'allow-piece-length-change';
  soAlwaysResume = 'always-resume';
  soAsyncDNS = 'async-dns';
  soAutoFileRenaming = 'auto-file-renaming';
  soBTEnableHookAfterHashCheck = 'bt-enable-hook-after-hash-check';
  soBTEnableLPD = 'bt-enable-lpd';
  soBTExcludeTracker = 'bt-exclude-tracker';
  soBTExternalIP = 'bt-external-ip';
  soBTForceEncryption = 'bt-force-encryption';
  soBTHashCheckSeed = 'bt-hash-check-seed';
  soBTMaxPeers = 'bt-max-peers';
  soBTMetadataOnly = 'bt-metadata-only';
  soBTMinCryptoLevel = 'bt-min-crypto-level';
  soBTPrioritizePiece = 'bt-prioritize-piece';
  soBTRemoveUnselectedFile = 'bt-remove-unselected-file';
  soBTRequestPeerSpeedLimit = 'bt-request-peer-speed-limit';
  soBTRequireCrypto = 'bt-require-crypto';
  soBTSaveMetadata = 'bt-save-metadata';
  soBTSeedUnverified = 'bt-seed-unverified';
  soBTStopTimeout = 'bt-stop-timeout';
  soBTTracker = 'bt-tracker';
  soBTTrackerConnectTimeout = 'bt-tracker-connect-timeout';
  soBTTrackerInterval = 'bt-tracker-interval';
  soBTTrackerTimeout = 'bt-tracker-timeout';
  soCheckIntegrity = 'check-integrity';
  soChecksum = 'checksum';
  soConditionalGet = 'conditional-get';
  soConnectTimeout = 'connect-timeout';
  soContentDispositionDefaultUTF8 = 'content-disposition-default-utf8';
  soContinue = 'continue';
  soDir = 'dir';
  soDryRun = 'dry-run';
  soEnableHttpKeepAlive = 'enable-http-keep-alive';
  soEnableHttpPipelining = 'enable-http-pipelining';
  soEnableMmap = 'enable-mmap';
  soEnablePeerExchange = 'enable-peer-exchange';
  soFileAllocation = 'file-allocation';
  soFollowMetalink = 'follow-metalink';
  soFollowTorrent = 'follow-torrent';
  soForceSave = 'force-save';
  soFtpPasswd = 'ftp-passwd';
  soFtpPasv = 'ftp-pasv';
  soFtpProxy = 'ftp-proxy';
  soFtpProxyPasswd = 'ftp-proxy-passwd';
  soFtpProxyUser = 'ftp-proxy-user';
  soFtpReuseConnection = 'ftp-reuse-connection';
  soFtpType = 'ftp-type';
  soFtpUser = 'ftp-user';
  soGID = 'gid';
  soHashCheckOnly = 'hash-check-only';
  soHeader = 'header';
  soHttpAcceptGzip = 'http-accept-gzip';
  soHttpAuthChallenge = 'http-auth-challenge';
  soHttpNoCache = 'http-no-cache';
  soHttpPasswd = 'http-passwd';
  soHttpProxy = 'http-proxy';
  soHttpProxyPasswd = 'http-proxy-passwd';
  soHttpProxyUser = 'http-proxy-user';
  soHttpUser = 'http-user';
  soHttpsProxy = 'https-proxy';
  soHttpsProxyPasswd = 'https-proxy-passwd';
  soHttpsProxyUser = 'https-proxy-user';
  soIndexOut = 'index-out';
  soLowestSpeedLimit = 'lowest-speed-limit';
  soMaxConnectionPerServer = 'max-connection-per-server';
  soMaxDownloadLimit = 'max-download-limit';
  soMaxFileNotFound = 'max-file-not-found';
  soMaxMmapLimit = 'max-mmap-limit';
  soMexResumeFailureTries = 'max-resume-failure-tries';
  soMaxTries = 'max-tries';
  soMaxUploadLimit = 'max-upload-limit';
  soMetalinkBaseUri = 'metalink-base-uri';
  soMetalinkEnableUniqueProtocol = 'metalink-enable-unique-protocol';
  soMetalinkLanguage = 'metalink-language';
  soMetalinkLocation = 'metalink-location';
  soMetalinkOS = 'metalink-os';
  soMetalinkPreferredProtocol = 'metalink-preferred-protocol';
  soMetalinkVersion = 'metalink-version';
  soMinSplitSize = 'min-split-size';
  soNoFileAllocationLimit = 'no-file-allocation-limit';
  soNoNetrc = 'no-netrc';
  soNoProxy = 'no-proxy';
  soOut = 'out';
  soParameterizedUri = 'parameterized-uri';
  soPause = 'pause';
  soPauseMetadata = 'pause-metadata';
  soPieceLength = 'piece-length';
  soProxyMethod = 'proxy-method';
  soRealtimeChunkChecksum = 'realtime-chunk-checksum';
  soReferer = 'referer';
  soRemoteTime = 'remote-time';
  soRemoveControlFile = 'remove-control-file';
  soRetryWait = 'retry-wait';
  soReuseUri = 'reuse-uri';
  soRPCSaveUploadMetadata = 'rpc-save-upload-metadata';
  soSeedRatio = 'seed-ratio';
  soSeedTime = 'seed-time';
  soSelectFile = 'select-file';
  soSplit = 'split';
  soSshHostKeyMD = 'ssh-host-key-md';
  soStreamPieceSelector = 'stream-piece-selector';
  soTimeout = 'timeout';
  soUriSelector = 'uri-selector';
  soUseHead = 'use-head';
  soUserAgent = 'user-agent';
  //Global options
  soBTMaxOpenFiles = 'bt-max-open-files';
  soDownloadResult = 'download-result';
  soKeepUnfinishedDownloadResult = 'keep-unfinished-download-result';
  soLog = 'log';
  soLogLevel = 'log-level';
  soMaxConcurrentDownloads = 'max-concurrent-downloads';
  soMaxDownloadResult = 'max-download-result';
  soMaxOverallDownloadLimit = 'max-overall-download-limit';
  soMaxOverallUploadLimit = 'max-overall-upload-limit';
  soOptimizeConcurrentDownloads = 'optimize-concurrent-downloads';
  soSaveCookies = 'save-cookies';
  soSaveSession = 'save-session';
  soServerStatOf = 'server-stat-of';
  //Options values
  //TODO
  svFalse = 'false';
  svTrue = 'true';
  //Error/exit codes
  aeSuccessful = 0;
  aeUnknownError = 1;
  aeTimeout = 2;
  aeResourceNotFound = 3;
  aeResourceNotFoundMax = 4;
  aeTooLowSpeed = 5;
  aeNetworkProblem = 6;
  aeUnfinishedDownloads = 7;
  aeResumeNotSupperted = 8;
  aeNoEnoughDiskSpace = 9;
  aePieceLengthDifferent = 10;
  aeAlreadyDownloading = 11;
  aeSameInfoHash = 12;
  aeAlreadyExists = 13;
  aeRenameFailed = 14;
  aeCouldntOpen = 15;
  aeCouldntCreate = 16;
  aeIOError = 17;
  aeCounldntCreateDir = 18;
  aeNameResolutionFailed = 19;
  aeCouldntParseMetalink = 20;
  aeFtpCommandFailed = 21;
  aeHttpBadResponse = 22;
  aeTooManyRedirects = 23;
  aeHttpAuthFailed = 24;
  aeCouldntParseBencode = 25;
  aeTorrentCorrupted = 26;
  aeBadMagnet = 27;
  aeBadOption = 28;
  aeRemoteFail = 29;
  aeBadJsonRequest = 30;
  aeReserved = 31;
  aeChecksumFailed = 32;

implementation

function StrToEnum(const S: string; const Values: array of string): Integer;
begin
  for Result := Low(Values) to High(Values) do
    if Values[Result] = S then
      Exit;
  Result := 0;
end;

procedure SetArray(var Dest: TStringArray; const Src: array of string);
var
  i: Integer;
begin
  SetLength(Dest, Length(Src));
  for i := 0 to High(Src) do
    Dest[i] := Src[i];
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

function Escape(C: Char): string;
const
  Esc: array[0..7] of record C: Char; R: string; end = (
    (C: '"'; R: '\"'), (C: '\'; R: '\\'),
    (C: '/'; R: '\/'), (C: #$08; R: '\b'),
    (C: #$09; R: '\t'), (C: #$0A; R: '\n'),
    (C: #$0C; R: '\f'), (C: #$0D; R: '\r'));
var
  i: Integer;
begin
  for i := Low(Esc) to High(Esc) do
    if C = Esc[i].C then
    begin
      Result := Esc[i].R;
      Exit;
    end;
  Result := C;
end;

function MakeStr(const S: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + Escape(S[i]);
  Result := '"' + Result + '"';
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
    Result := Result + MakeStr(Items[i]) + ',';
  Result[Length(Result)] := ']';
end;

function OptionsToJson(const Items: array of TAria2Option): string;
var
  i: Integer;
begin
  if Length(Items) < 1 then
  begin
    Result := '';
    Exit;
  end;
  Result := '{';
  for i := 0 to High(Items) do
    Result := Result + MakeStr(Items[i].Key) + ':' + MakeStr(Items[i].Value) + ',';
  Result[Length(Result)] := '}';
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

function TAria2.AddUri(const Uris: array of string; const Options: array of TAria2Option; Position: Integer = -1): TAria2GID;
begin
  Result := SendRequestStr('aria2.addUri', MakeParams(['', '{}', ''],
    [ArrayToJson(Uris), OptionsToJson(Options), Check(Position >= 0, IntToStr(Position))]));
end;

function TAria2.AddTorrent(const Torrent: string; const Uris: array of string; const Options: array of TAria2Option; Position: Integer = -1): TAria2GID;
begin
  Result := SendRequestStr('aria2.addTorrent', MakeParams(['', '[]', '{}', ''],
    ['"' + Base64Encode(Torrent) + '"', ArrayToJson(Uris), OptionsToJson(Options), Check(Position >= 0, IntToStr(Position))]));
end;

function TAria2.AddMetalink(const Metalink: string; const Options: array of TAria2Option; Position: Integer = -1): TAria2GIDArray;
var
  i: Integer;
  Res: PJsonValue;
begin
  Result := nil;
  Res := SendRequest('aria2.addMetalink', MakeParams(['', '{}', ''],
    ['"' + Base64Encode(Metalink) + '"', OptionsToJson(Options), Check(Position >= 0, IntToStr(Position))]));
  try
    if Res.VType <> jtArray then Exit;
    SetLength(Result, Res.Arr.Length);
    for i := 0 to Res.Arr.Length - 1 do
      Result[i] := JsonStr(JsonItem(Res, i));
  finally
    JsonFree(Res);
  end;
end;

function TAria2.Remove(GID: TAria2GID; Force: Boolean): TAria2GID;
const
  Method: array[Boolean] of string = ('aria2.remove', 'aria2.forceRemove');
begin
  Result := SendRequestStr(Method[Force], MakeStr(GID));
end;

function TAria2.Pause(GID: TAria2GID; Force: Boolean): TAria2GID;
const
  Method: array[Boolean] of string = ('aria2.pause', 'aria2.forcePause');
begin
  Result := SendRequestStr(Method[Force], MakeStr(GID));
end;

function TAria2.PauseAll(Force: Boolean): Boolean;
const
  Method: array[Boolean] of string = ('aria2.pauseAll', 'aria2.forcePauseAll');
begin
  Result := SendRequestStr(Method[Force], '') = 'OK';
end;

function TAria2.Unpause(GID: TAria2GID): TAria2GID;
begin
  Result := SendRequestStr('aria2.unpause', MakeStr(GID));
end;

function TAria2.UnpauseAll: Boolean;
begin
  Result := SendRequestStr('aria2.unpauseAll', '') = 'OK';
end;

function TAria2.TellStatus(GID: TAria2GID; const Keys: array of string): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.tellStatus', MakeParams(['""', ''], [MakeStr(GID), ArrayToJson(Keys)])));
end;

function TAria2.GetUris(GID: TAria2GID): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.getUris', MakeStr(GID)));
end;

function TAria2.GetFiles(GID: TAria2GID): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.getFiles', MakeStr(GID)));
end;

function TAria2.GetPeers(GID: TAria2GID): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.getPeers', MakeStr(GID)));
end;

function TAria2.GetServers(GID: TAria2GID): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.getServers', MakeStr(GID)));
end;

function TAria2.TellActive(const Keys: array of string): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.tellActive', ArrayToJson(Keys)));
end;

function TAria2.TellWaiting(Offset, Num: Integer; const Keys: array of string): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.tellWaiting', MakeParams(['', '', ''], [IntToStr(Offset), IntToStr(Num), ArrayToJson(Keys)])));
end;

function TAria2.TellStopped(Offset, Num: Integer; const Keys: array of string): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.tellStopped', MakeParams(['', '', ''], [IntToStr(Offset), IntToStr(Num), ArrayToJson(Keys)])));
end;

function TAria2.ChangePosition(GID: TAria2GID; Pos: Integer; Origin: TAria2PosOrigin): Integer;
const
  OriginValues: array[TAria2PosOrigin] of string = ('"POS_SET"', '"POS_CUR"', '"POS_END"');
var
  Res: PJsonValue;
begin
  Res := SendRequest('aria2.changePosition', MakeParams(['""', '', ''], [MakeStr(GID), IntToStr(Pos), OriginValues[Origin]]));
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
    [MakeStr(GID), IntToStr(FileIndex), ArrayToJson(DelUris), ArrayToJson(AddUris), Check(Position >= 0, IntToStr(Position))]));
  try
    Result := MakeDword(JsonInt(JsonItem(Res, 1)), JsonInt(JsonItem(Res, 0)));
  finally
    JsonFree(Res);
  end;
end;

function TAria2.GetOptions(GID: TAria2GID): TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.getOption', MakeStr(GID)));
end;

function TAria2.ChangeOptions(GID: TAria2GID; const Options: array of TAria2Option): Boolean;
begin
  Result := SendRequestStr('aria2.changeOption', MakeParams(['""', '""'], [MakeStr(GID), OptionsToJson(Options)])) = 'OK';
end;

function TAria2.GetGlobalOptions: TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.getGlobalOption', ''));
end;

function TAria2.ChangeGlobalOptions(const Options: array of TAria2Option):
  Boolean;
begin
  Result := SendRequestStr('aria2.changeGlobalOption', OptionsToJson(Options)) = 'OK';
end;

function TAria2.GetGlobalStats: TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.getGlobalStat', ''));
end;

function TAria2.PurgeDownloadResult: Boolean;
begin
  Result := SendRequestStr('aria2.purgeDownloadResult', '') = 'OK';
end;

function TAria2.RemoveDownloadResult(GID: TAria2GID): Boolean;
begin
  Result := SendRequestStr('aria2.removeDownloadResult', MakeStr(GID)) = 'OK';
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

function TAria2.GetSessionInfo: TAria2Struct;
begin
  Result := TAria2Struct.Create(SendRequest('aria2.getSessionInfo', ''));
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
  Reply, ReplyId: PJsonValue;
begin
  Result := nil;
  if not Assigned(FOnRequest) then
    raise Exception.Create('Aria2: no transport provided');
  Id := NewId;
  Reply := JsonParse(FOnRequest(Self, '{"jsonrpc":"2.0","id":"' + Id + '","method":"' + Method + '","params":[' + AddToken(Params) + ']}'));
  if not Assigned(Reply) then
    raise Exception.Create('Aria2: invalid reply');
  try
    ReplyId := JsonItem(Reply, 'id');
    if (JsonStr(ReplyId) <> Id) and (Assigned(ReplyId) and not (ReplyId.VType in [jtNone, jtNull])) then
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

{ TAria2Struct }

constructor TAria2Struct.Create(Json: PJsonValue);
begin
  inherited Create;
  FIndex := -1;
  FRoot := '';
  FJson := Json;
end;

destructor TAria2Struct.Destroy;
begin
  JsonFree(FJson);
  inherited;
end;

function TAria2Struct.Exists(const Name: string): Boolean;
begin
  Result := Assigned(Find(Name));
end;

function TAria2Struct.Find(Name: string): PJsonValue;
begin
  if (FRoot = '') or (Name = FRoot) then
    Result := FJson
  else
    Result := Find(FRoot);
  if (FIndex >= 0) and (Name <> FRoot) then 
    Result := JsonItem(Result, FIndex);
  while Assigned(Result) and (Name <> '') do
    if Result.VType = jtArray then
      Result := JsonItem(Result, StrToInt(Tok('.', Name)))
    else if Result.VType = jtObject then
      Result := JsonItem(Result, Tok('.', Name))
    else
      Result := nil;
end;

function TAria2Struct.GetBool(const Name: string): Boolean;
begin
  Result := Boolean(StrToEnum(Str[Name], sfBoolValues));
end;

function TAria2Struct.GetInt(const Name: string): Integer;
begin
  Result := StrToint(Str[Name]);
end;

function TAria2Struct.GetInt64(const Name: string): Int64;
begin
  Result := StrToInt64(Str[Name]);
end;

function TAria2Struct.GetLength(const Name: string): Integer;
var
  Item: PJsonValue;
begin
  Result := -1;
  Item := Find(Name);
  if Assigned(Item) and (Item.VType = jtArray) then
    Result := Item.Arr.Length;
end;

function TAria2Struct.GetStr(const Name: string): string;
begin
  Result := JsonStr(Find(Name));
end;

end.
