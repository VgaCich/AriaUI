unit Aria2;

interface

uses
  AvL, avlSyncObjs, avlUtils, avlJSON, Base64;
  
//TODO: Time-based results purging & type cheking (ID: time:16|type:3-4?|ctr:12-13)

type
  TOnRPCRequest = function(Sender: TObject; const Request: string): string of object;
  TRequestID = Cardinal;
  TAria2GID = string;
  TAria2GIDArray = array of TAria2GID;
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
    function GetNamesCount: Integer;
    function GetName(Index: Integer): string;
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
    property NamesCount: Integer read GetNamesCount;
    property Names[Index: Integer]: string read GetName;
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
    FCurID: TRequestID;
    FBatchRequest: string;
    FResults: array of PJsonValue;
    FBatchLock, FRequestLock: TCriticalSection;
    function AddToken(const Params: string): string;
    procedure AddResult(Reply: PJsonValue);
  protected
    function GetResult(RequestID: TRequestID): PJsonValue;
    function SendRequest(const Method, Params: string): TRequestID;
  public
    constructor Create(OnRequest: TOnRPCRequest; const RPCSecret: string = '');
    destructor Destroy; override;
    function AddUri(const Uris: array of string; const Options: array of TAria2Option; Position: Integer = -1): TRequestID; //GID
    function AddTorrent(const Torrent: string; const Uris: array of string; const Options: array of TAria2Option; Position: Integer = -1): TRequestID; //GID
    function AddMetalink(const Metalink: string; const Options: array of TAria2Option; Position: Integer = -1): TRequestID; //Struct
    function Remove(GID: TAria2GID; Force: Boolean = false): TRequestID; //GID
    function Pause(GID: TAria2GID; Force: Boolean = false): TRequestID; //GID
    function PauseAll(Force: Boolean = false): TRequestID; //Bool
    function Unpause(GID: TAria2GID): TRequestID; //GID
    function UnpauseAll: TRequestID; //Bool
    function TellStatus(GID: TAria2GID; const Keys: array of string): TRequestID; //Struct
    function GetUris(GID: TAria2GID): TRequestID; //Struct
    function GetFiles(GID: TAria2GID): TRequestID; //Struct
    function GetPeers(GID: TAria2GID): TRequestID; //Struct
    function GetServers(GID: TAria2GID): TRequestID; //Struct
    function TellActive(const Keys: array of string): TRequestID; //Struct
    function TellWaiting(Offset, Num: Integer; const Keys: array of string): TRequestID; //Struct
    function TellStopped(Offset, Num: Integer; const Keys: array of string): TRequestID; //Struct
    function ChangePosition(GID: TAria2GID; Pos: Integer; Origin: TAria2PosOrigin): TRequestID; //Int
    function ChangeUri(GID: TAria2GID; FileIndex: Integer; const DelUris, AddUris: string; Position: Integer = -1): TRequestID; //Struct
    function GetOptions(GID: TAria2GID): TRequestID; //Struct
    function ChangeOptions(GID: TAria2GID; const Options: array of TAria2Option): TRequestID; //Bool
    function GetGlobalOptions: TRequestID; //Struct
    function ChangeGlobalOptions(const Options: array of TAria2Option): TRequestID; //Bool
    function GetGlobalStats: TRequestID; //Struct
    function PurgeDownloadResult: TRequestID; //Bool
    function RemoveDownloadResult(GID: TAria2GID): TRequestID; //Bool
    function GetVersion: TRequestID;
    function GetSessionInfo: TRequestID; //Struct
    function Shutdown(Force: Boolean = false): TRequestID; //Bool
    function SaveSession: TRequestID; //Bool
    procedure CheckResult(RequestID: TRequestID);
    function GetBool(RequestID: TRequestID): Boolean;
    function GetInt(RequestID: TRequestID): Integer;
    function GetString(RequestID: TRequestID): string;
    function GetGID(RequestID: TRequestID): TAria2GID;
    function GetStruct(RequestID: TRequestID): TAria2Struct;
    procedure BeginBatch;
    procedure EndBatch;
    procedure FreeResults;
    property OnRequest: TOnRPCRequest read FOnRequest write FOnRequest;
    property RPCSecret: string read FRPCSecret write FRPCSecret;
  end;

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
  //Version keys
  sfVersion = 'version';
  sfEnabledFeatures = 'enabledFeatures';
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
  soMaxResumeFailureTries = 'max-resume-failure-tries';
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
  soSSHHostKeyMD = 'ssh-host-key-md';
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
  //CLI-only options
  soBTDetachSeedOnly = 'detach-seed-only';
  soBTLoadSavedMetadata = 'bt-load-saved-metadata';
  soBTLPDInterface = 'bt-lpd-interface';
  soCACertificate = 'ca-certificate';
  soCertificate = 'certificate';
  soCheckCertificate = 'check-certificate';
  soDHTEntryPoint = 'dht-entry-point';
  soDHTEntryPoint6 = 'dht-entry-point6';
  soDHTFilePath = 'dht-file-path';
  soDHTFilePath6 = 'dht-file-path6';
  soDHTListenAddr6 = 'dht-listen-addr6';
  soDHTListenPort = 'dht-listen-port';
  soDHTMessageTimeout = 'dht-message-timeout';
  soEnableDHT = 'enable-dht';
  soEnableDHT6 = 'enable-dht6';
  soHelp = 'help';
  soInputFile = 'input-file';
  soListenPort = 'listen-port';
  soLoadCookies = 'load-cookies';
  soMetalinkFile = 'metalink-file';
  soNetrcPath = 'netrc-path';
  soPeerAgent = 'peer-agent';
  soPeerIDPrefix = 'peer-id-prefix';
  soPrivateKey = 'private-key';
  soServerStatIf = 'server-stat-if';
  soServerStatTimeout = 'server-stat-timeout';
  soShowFiles = 'show-files';
  soTorrentFile = 'torrent-file';
  //Options values
  svAdaptive = 'adaptive';
  svArc4 = 'arc4';
  svAscii = 'ascii';
  svBinary = 'binary';
  svDefault = 'default';
  svFalse = 'false';
  svFeedback = 'feedback';
  svFtp = 'ftp';
  svGeom = 'geom';
  svGet = 'get';
  svHead = 'head';
  svHttp = 'http';
  svHttps = 'https';
  svInOrder = 'inorder';
  svMem = 'mem';
  svNone = 'none';
  svPlain = 'plain';
  svRandom = 'random';
  svTail = 'tail';
  svTrue = 'true';
  svTunnel = 'tunnel';
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
  //Options info
  OSep = '|';
  ovAddr = '<addr>';
  ovBoolean = svFalse + OSep + svTrue;
  ovChecksum = '<type>=<digest>';
  ovFile = '<file>';
  ovHostPort = '<host>:<port>';
  ovInterface = '<interface>';
  ovIPAddress = '<ip address>';
  ovNum = '<num>';
  ovPath = '<path>';
  ovPort = '<port>';
  ovPorts = '<port>,[port],[<port>-<port>]';
  ovProxy = '[http://][username:password@]<host>[:port]';
  ovPasswd = '<password>';
  ovSec = '<sec>';
  ovSize = '<size>';
  ovSpeed = '<speed>';
  ovUriList = '<uri>[,uri]...';
  ovUser = '<username>';
  Aria2Options: array[0..149] of TAria2Option = ( //TODO
    (Key: soAllProxy; Value: ovProxy),
    (Key: soAllProxyPasswd; Value: ovPasswd),
    (Key: soAllProxyUser; Value: ovUser),
    (Key: soAllowOverwrite; Value: ovBoolean),
    (Key: soAllowPieceLengthChange; Value: ovBoolean),
    (Key: soAlwaysResume; Value: ovBoolean),
    (Key: soAsyncDNS; Value: ovBoolean),
    (Key: soAutoFileRenaming; Value: ovBoolean),
    (Key: soBTEnableHookAfterHashCheck; Value: ovBoolean),
    (Key: soBTEnableLPD; Value: ovBoolean),
    (Key: soBTExcludeTracker; Value: ovUriList),
    (Key: soBTExternalIP; Value: ovIPAddress),
    (Key: soBTForceEncryption; Value: ovBoolean),
    (Key: soBTHashCheckSeed; Value: ovBoolean),
    (Key: soBTMaxPeers; Value: ovNum),
    (Key: soBTMetadataOnly; Value: ovBoolean),
    (Key: soBTMinCryptoLevel; Value: svArc4 + OSep + svPlain),
    (Key: soBTPrioritizePiece; Value: svHead + OSep + svTail + OSep + svHead + ',' + svTail + OSep + '[head[=<size>]],[tail[=<size>]]'),
    (Key: soBTRemoveUnselectedFile; Value: ovBoolean),
    (Key: soBTRequestPeerSpeedLimit; Value: ovSpeed),
    (Key: soBTRequireCrypto; Value: ovBoolean),
    (Key: soBTSaveMetadata; Value: ovBoolean),
    (Key: soBTSeedUnverified; Value: ovBoolean),
    (Key: soBTStopTimeout; Value: ovSec),
    (Key: soBTTracker; Value: ovUriList),
    (Key: soBTTrackerConnectTimeout; Value: ovSec),
    (Key: soBTTrackerInterval; Value: ovSec),
    (Key: soBTTrackerTimeout; Value: ovSec),
    (Key: soCheckIntegrity; Value: ovBoolean),
    (Key: soChecksum; Value: ovChecksum),
    (Key: soConditionalGet; Value: ovBoolean),
    (Key: soConnectTimeout; Value: ovSec),
    (Key: soContentDispositionDefaultUTF8; Value: ovBoolean),
    (Key: soContinue; Value: ovBoolean),
    (Key: soDir; Value: '<dir>'),
    (Key: soDryRun; Value: ovBoolean),
    (Key: soEnableHttpKeepAlive; Value: ovBoolean),
    (Key: soEnableHttpPipelining; Value: ovBoolean),
    (Key: soEnableMmap; Value: ovBoolean),
    (Key: soEnablePeerExchange; Value: ovBoolean),
    (Key: soFileAllocation; Value: ''),
    (Key: soFollowMetalink; Value: ovBoolean + OSep + svMem),
    (Key: soFollowTorrent; Value: ovBoolean + OSep + svMem),
    (Key: soForceSave; Value: ovBoolean),
    (Key: soFtpPasswd; Value: ovPasswd),
    (Key: soFtpPasv; Value: ovBoolean),
    (Key: soFtpProxy; Value: ovProxy),
    (Key: soFtpProxyPasswd; Value: ovPasswd),
    (Key: soFtpProxyUser; Value: ovUser),
    (Key: soFtpReuseConnection; Value: ovBoolean),
    (Key: soFtpType; Value: svAscii + OSep + svBinary),
    (Key: soFtpUser; Value: ovUser),
    (Key: soGID; Value: ''),
    (Key: soHashCheckOnly; Value: ovBoolean),
    (Key: soHeader; Value: '<header>'),
    (Key: soHttpAcceptGzip; Value: ovBoolean),
    (Key: soHttpAuthChallenge; Value: ovBoolean),
    (Key: soHttpNoCache; Value: ovBoolean),
    (Key: soHttpPasswd; Value: ovPasswd),
    (Key: soHttpProxy; Value: ovProxy),
    (Key: soHttpProxyPasswd; Value: ovPasswd),
    (Key: soHttpProxyUser; Value: ovUser),
    (Key: soHttpUser; Value: ovUser),
    (Key: soHttpsProxy; Value: ovProxy),
    (Key: soHttpsProxyPasswd; Value: ovPasswd),
    (Key: soHttpsProxyUser; Value: ovUser),
    (Key: soIndexOut; Value: '<index>=<path>'),
    (Key: soLowestSpeedLimit; Value: ovSpeed),
    (Key: soMaxConnectionPerServer; Value: ovNum),
    (Key: soMaxDownloadLimit; Value: ''),
    (Key: soMaxFileNotFound; Value: ovNum),
    (Key: soMaxMmapLimit; Value: ''),
    (Key: soMaxResumeFailureTries; Value: ''),
    (Key: soMaxTries; Value: ovNum),
    (Key: soMaxUploadLimit; Value: ovSpeed),
    (Key: soMetalinkBaseUri; Value: '<uri>'),
    (Key: soMetalinkEnableUniqueProtocol; Value: ovBoolean),
    (Key: soMetalinkLanguage; Value: '<language>'),
    (Key: soMetalinkLocation; Value: '<location>[,location]...'),
    (Key: soMetalinkOS; Value: '<os>'),
    (Key: soMetalinkPreferredProtocol; Value: svFtp + OSep + svHttp + OSep + svHttps + OSep + svNone),
    (Key: soMetalinkVersion; Value: '<version>'),
    (Key: soMinSplitSize; Value: ovSize),
    (Key: soNoFileAllocationLimit; Value: ''),
    (Key: soNoNetrc; Value: ovBoolean),
    (Key: soNoProxy; Value: '<domains>'),
    (Key: soOut; Value: ovFile),
    (Key: soParameterizedUri; Value: ovBoolean),
    (Key: soPause; Value: ovBoolean),
    (Key: soPauseMetadata; Value: ovBoolean),
    (Key: soPieceLength; Value: ''),
    (Key: soProxyMethod; Value: svGet + OSep + svTunnel),
    (Key: soRealtimeChunkChecksum; Value: ovBoolean),
    (Key: soReferer; Value: '<referer>'),
    (Key: soRemoteTime; Value: ovBoolean),
    (Key: soRemoveControlFile; Value: ovBoolean),
    (Key: soRetryWait; Value: ovSec),
    (Key: soReuseUri; Value: ovBoolean),
    (Key: soRPCSaveUploadMetadata; Value: ovBoolean),
    (Key: soSeedRatio; Value: '<ratio>'),
    (Key: soSeedTime; Value: '<min>'),
    (Key: soSelectFile; Value: '<index>,[index],[<index>-<index>]'),
    (Key: soSplit; Value: ovNum),
    (Key: soSSHHostKeyMD; Value: ovChecksum),
    (Key: soStreamPieceSelector; Value: svDefault + OSep + svGeom + OSep + svInOrder + OSep + svRandom),
    (Key: soTimeout; Value: ovSec),
    (Key: soUriSelector; Value: svAdaptive + OSep + svFeedback + OSep + svInOrder),
    (Key: soUseHead; Value: ovBoolean),
    (Key: soUserAgent; Value: '<user agent>'),
    //Global options
    (Key: soBTMaxOpenFiles; Value: ovNum),
    (Key: soDownloadResult; Value: ''),
    (Key: soKeepUnfinishedDownloadResult; Value: ovBoolean),
    (Key: soLog; Value: '<log>'),
    (Key: soLogLevel; Value: ''),
    (Key: soMaxConcurrentDownloads; Value: ovNum),
    (Key: soMaxDownloadResult; Value: ''),
    (Key: soMaxOverallDownloadLimit; Value: ovSpeed),
    (Key: soMaxOverallUploadLimit; Value: ovSpeed),
    (Key: soOptimizeConcurrentDownloads; Value: ovBoolean + OSep + '<A>:<B>'),
    (Key: soSaveCookies; Value: ovFile),
    (Key: soSaveSession; Value: ''),
    (Key: soServerStatOf; Value: ovFile),
    //CLI-only options
    (Key: soBTDetachSeedOnly; Value: ovBoolean),
    (Key: soBTLoadSavedMetadata; Value: ovBoolean),
    (Key: soBTLPDInterface; Value: ovInterface),
    (Key: soCACertificate; Value: ovFile),
    (Key: soCertificate; Value: ovFile),
    (Key: soCheckCertificate; Value: ovBoolean),
    (Key: soDHTEntryPoint; Value: ovHostPort),
    (Key: soDHTEntryPoint6; Value: ovHostPort),
    (Key: soDHTFilePath; Value: ovPath),
    (Key: soDHTFilePath6; Value: ovPath),
    (Key: soDHTListenAddr6; Value: ovAddr),
    (Key: soDHTListenPort; Value: ovPorts),
    (Key: soDHTMessageTimeout; Value: ovSec),
    (Key: soEnableDHT; Value: ovBoolean),
    (Key: soEnableDHT6; Value: ovBoolean),
    (Key: soHelp; Value: '<tag>|<keyword>'),
    (Key: soInputFile; Value: ovFile),
    (Key: soListenPort; Value: ovPorts),
    (Key: soLoadCookies; Value: ovFile),
    (Key: soMetalinkFile; Value: ovFile),
    (Key: soNetrcPath; Value: ovPath),
    (Key: soPeerAgent; Value: '<peer agent'),
    (Key: soPeerIDPrefix; Value: '<prefix>'),
    (Key: soPrivateKey; Value: ovFile),
    (Key: soServerStatIf; Value: ovFile),
    (Key: soServerStatTimeout; Value: ovSec),
    (Key: soShowFiles; Value: ovBoolean),
    (Key: soTorrentFile; Value: ovFile));

implementation

uses
  Utils;

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
  FBatchLock := TCriticalSection.Create;
  FRequestLock := TCriticalSection.Create;
end;

destructor TAria2.Destroy;
begin
  FreeResults;
  FreeAndNil(FRequestLock);
  FreeAndNil(FBatchLock);
  inherited;
end;

function TAria2.AddUri(const Uris: array of string; const Options: array of TAria2Option; Position: Integer = -1): TRequestID;
begin
  Result := SendRequest('aria2.addUri', MakeParams(['', '{}', ''],
    [ArrayToJson(Uris), OptionsToJson(Options), Check(Position >= 0, IntToStr(Position))]));
end;

function TAria2.AddTorrent(const Torrent: string; const Uris: array of string; const Options: array of TAria2Option; Position: Integer = -1): TRequestID;
begin
  Result := SendRequest('aria2.addTorrent', MakeParams(['', '[]', '{}', ''],
    ['"' + Base64Encode(Torrent) + '"', ArrayToJson(Uris), OptionsToJson(Options), Check(Position >= 0, IntToStr(Position))]));
end;

function TAria2.AddMetalink(const Metalink: string; const Options: array of TAria2Option; Position: Integer = -1): TRequestID;
begin
  Result := SendRequest('aria2.addMetalink', MakeParams(['', '{}', ''],
    ['"' + Base64Encode(Metalink) + '"', OptionsToJson(Options), Check(Position >= 0, IntToStr(Position))]));
end;

function TAria2.Remove(GID: TAria2GID; Force: Boolean): TRequestID;
const
  Method: array[Boolean] of string = ('aria2.remove', 'aria2.forceRemove');
begin
  Result := SendRequest(Method[Force], MakeStr(GID));
end;

function TAria2.Pause(GID: TAria2GID; Force: Boolean): TRequestID;
const
  Method: array[Boolean] of string = ('aria2.pause', 'aria2.forcePause');
begin
  Result := SendRequest(Method[Force], MakeStr(GID));
end;

function TAria2.PauseAll(Force: Boolean): TRequestID;
const
  Method: array[Boolean] of string = ('aria2.pauseAll', 'aria2.forcePauseAll');
begin
  Result := SendRequest(Method[Force], '');
end;

function TAria2.Unpause(GID: TAria2GID): TRequestID;
begin
  Result := SendRequest('aria2.unpause', MakeStr(GID));
end;

function TAria2.UnpauseAll: TRequestID;
begin
  Result := SendRequest('aria2.unpauseAll', '');
end;

function TAria2.TellStatus(GID: TAria2GID; const Keys: array of string): TRequestID;
begin
  Result := SendRequest('aria2.tellStatus', MakeParams(['""', ''],
    [MakeStr(GID), ArrayToJson(Keys)]));
end;

function TAria2.GetUris(GID: TAria2GID): TRequestID;
begin
  Result := SendRequest('aria2.getUris', MakeStr(GID));
end;

function TAria2.GetFiles(GID: TAria2GID): TRequestID;
begin
  Result := SendRequest('aria2.getFiles', MakeStr(GID));
end;

function TAria2.GetPeers(GID: TAria2GID): TRequestID;
begin
  Result := SendRequest('aria2.getPeers', MakeStr(GID));
end;

function TAria2.GetServers(GID: TAria2GID): TRequestID;
begin
  Result := SendRequest('aria2.getServers', MakeStr(GID));
end;

function TAria2.TellActive(const Keys: array of string): TRequestID;
begin
  Result := SendRequest('aria2.tellActive', ArrayToJson(Keys));
end;

function TAria2.TellWaiting(Offset, Num: Integer; const Keys: array of string): TRequestID;
begin
  Result := SendRequest('aria2.tellWaiting', MakeParams(['', '', ''],
    [IntToStr(Offset), IntToStr(Num), ArrayToJson(Keys)]));
end;

function TAria2.TellStopped(Offset, Num: Integer; const Keys: array of string): TRequestID;
begin
  Result := SendRequest('aria2.tellStopped', MakeParams(['', '', ''],
    [IntToStr(Offset), IntToStr(Num), ArrayToJson(Keys)]));
end;

function TAria2.ChangePosition(GID: TAria2GID; Pos: Integer; Origin: TAria2PosOrigin): TRequestID;
const
  OriginValues: array[TAria2PosOrigin] of string = ('"POS_SET"', '"POS_CUR"', '"POS_END"');
begin
  Result := SendRequest('aria2.changePosition', MakeParams(['""', '', ''],
    [MakeStr(GID), IntToStr(Pos), OriginValues[Origin]]));
end;

function TAria2.ChangeUri(GID: TAria2GID; FileIndex: Integer; const DelUris, AddUris: string; Position: Integer): TRequestID;
begin
  Result := SendRequest('aria2.changeUri', MakeParams(['""', '', '[]', '[]', ''],
    [MakeStr(GID), IntToStr(FileIndex), ArrayToJson(DelUris), ArrayToJson(AddUris), Check(Position >= 0, IntToStr(Position))]));
end;

function TAria2.GetOptions(GID: TAria2GID): TRequestID;
begin
  Result := SendRequest('aria2.getOption', MakeStr(GID));
end;

function TAria2.ChangeOptions(GID: TAria2GID; const Options: array of TAria2Option): TRequestID;
begin
  Result := SendRequest('aria2.changeOption', MakeParams(['""', '""'],
    [MakeStr(GID), OptionsToJson(Options)]));
end;

function TAria2.GetGlobalOptions: TRequestID;
begin
  Result := SendRequest('aria2.getGlobalOption', '');
end;

function TAria2.ChangeGlobalOptions(const Options: array of TAria2Option):TRequestID;
begin
  Result := SendRequest('aria2.changeGlobalOption', OptionsToJson(Options));
end;

function TAria2.GetGlobalStats: TRequestID;
begin
  Result := SendRequest('aria2.getGlobalStat', '');
end;

function TAria2.PurgeDownloadResult: TRequestID;
begin
  Result := SendRequest('aria2.purgeDownloadResult', '');
end;

function TAria2.RemoveDownloadResult(GID: TAria2GID): TRequestID;
begin
  Result := SendRequest('aria2.removeDownloadResult', MakeStr(GID));
end;

function TAria2.GetVersion: TRequestID;
begin
  Result := SendRequest('aria2.getVersion', '');
end;

function TAria2.GetSessionInfo: TRequestID;
begin
  Result := SendRequest('aria2.getSessionInfo', '');
end;

function TAria2.Shutdown(Force: Boolean): TRequestID;
const
  Method: array[Boolean] of string = ('aria2.shutdown', 'aria2.forceShutdown');
begin
  Result := SendRequest(Method[Force], '');
end;

function TAria2.SaveSession: TRequestID;
begin
  Result := SendRequest('aria2.saveSession', '');
end;

procedure TAria2.CheckResult(RequestID: TRequestID);
begin
  JsonFree(GetResult(RequestID));
end;

function TAria2.GetBool(RequestID: TRequestID): Boolean;
begin
  Result := GetString(RequestID) = 'OK';
end;

function TAria2.GetGID(RequestID: TRequestID): TAria2GID;
begin
  Result := GetString(RequestID);
end;

function TAria2.GetInt(RequestID: TRequestID): Integer;
var
  Res: PJsonValue;
begin
  Res := GetResult(RequestID);
  try
    Result := JsonInt(Res);
  finally
    JsonFree(Res);
  end;
end;

function TAria2.GetString(RequestID: TRequestID): string;
var
  Res: PJsonValue;
begin
  Res := GetResult(RequestID);
  try
    Result := JsonStr(Res);
  finally
    JsonFree(Res);
  end;
end;

function TAria2.GetStruct(RequestID: TRequestID): TAria2Struct;
begin
  Result := TAria2Struct.Create(GetResult(RequestID));
end;

procedure TAria2.BeginBatch;
begin
  if FBatchRequest <> '' then Exit;
  FBatchLock.Acquire;
  FRequestLock.Acquire;
  try
    FBatchRequest := '[';
  finally
    FRequestLock.Release;
  end;
end;

procedure TAria2.EndBatch;
var
  i: Integer;
  Res: PJsonValue;
begin
  if FBatchRequest = '' then Exit;
  Res := nil;
  FRequestLock.Acquire;
  try
    FBatchRequest[Length(FBatchRequest)] := ']';
    if Length(FBatchRequest) > 2 then
      Res := JsonParse(FOnRequest(Self, FBatchRequest));
    if not Assigned(Res) or (Res.VType <> jtArray) then Exit;
    for i := 0 to Res.Arr.Length - 1 do
    try
      AddResult(JsonExtractItem(Res, i));
    except
    end;
  finally
    FBatchRequest := '';
    FRequestLock.Release;
    FBatchLock.Release;
  end;
end;

procedure TAria2.FreeResults;
var
  i: Integer;
begin
  FBatchLock.Acquire;
  FRequestLock.Acquire;
  try
    for i := 0 to High(FResults) do
      JsonFree(FResults[i]);
    Finalize(FResults);
  finally
    FRequestLock.Release;
    FBatchLock.Release;
  end;
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

procedure TAria2.AddResult(Reply: PJsonValue);
var
  i: Integer;
  ReplyID: PJsonValue;
begin
  ReplyID := JsonItem(Reply, 'id');
  if not Assigned(ReplyID) or (ReplyID.VType <> jtInteger) then
    if Assigned(JsonItem(Reply, 'error')) then
      raise Exception.CreateFmt('Aria2: invalid reply (error %d: %s)',
        [JsonInt(JsonItem(JsonItem(Reply, 'error'), 'code')),
         JsonStr(JsonItem(JsonItem(Reply, 'error'), 'message'))])
    else
      raise Exception.Create('Aria2: invalid reply');
  for i := 0 to High(FResults) do
    if FResults[i] = nil then
    begin
      FResults[i] := Reply;
      Exit;
    end;
  SetLength(FResults, Length(FResults) + 1);
  FResults[High(FResults)] := Reply;
end;

function TAria2.GetResult(RequestID: TRequestID): PJsonValue;
var
  i: Integer;
  Res: PJsonValue;
begin
  Res := nil;
  Result := nil;
  FBatchLock.Acquire;
  try
    FRequestLock.Acquire;
    try
      for i := 0 to High(FResults) do
        if JsonInt(JsonItem(FResults[i], 'id')) = RequestID then
        begin
          Res := FResults[i];
          FResults[i] := nil;
          Break;
        end;
    finally
      FRequestLock.Release;
    end;
    if not Assigned(Res) then
      raise Exception.Create('Aria2: no reply');
    try
      if Assigned(JsonItem(Res, 'error')) then
        raise Exception.CreateFmt('Aria2: request error %d: %s',
          [JsonInt(JsonItem(JsonItem(Res, 'error'), 'code')),
           JsonStr(JsonItem(JsonItem(Res, 'error'), 'message'))]);
      Result := JsonExtractItem(Res, 'result');
      if not Assigned(Result) then
        raise Exception.Create('Aria2: invalid reply');
    finally
      JsonFree(Res);
    end;
  finally
    FBatchLock.Release;
  end;
end;

function TAria2.SendRequest(const Method, Params: string): TRequestID;
const
  RequestTemplate = '{"jsonrpc":"2.0","id":%d,"method":"%s","params":[';
var
  Request: string;
begin
  Result := 0;
  if not Assigned(FOnRequest) then
    raise Exception.Create('Aria2: no transport provided');
  FRequestLock.Acquire;
  try
    FCurID := Max(FCurId, 1) + 1;
    Result := FCurID;
    Request := Format(RequestTemplate, [Result, Method]) + AddToken(Params) + ']}';
    if FBatchRequest = '' then
      AddResult(JsonParse(FOnRequest(Self, Request)))
    else
      FBatchRequest := FBatchRequest + Request + ',';
  finally
    FRequestLock.Release;
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

function TAria2Struct.GetName(Index: Integer): string;
begin
  Result := '';
  if (Index < 0) or (Index >= NamesCount) then Exit;
  Result := Find('').Obj.Values[Index].Name;
end;

function TAria2Struct.GetNamesCount: Integer;
var
  Item: PJsonValue;
begin
  Item := Find('');
  if Assigned(Item) and (Item.VType = jtObject) then
    Result := Item.Obj.Length
  else
    Result := 0;
end;

function TAria2Struct.GetStr(const Name: string): string;
begin
  Result := JsonStr(Find(Name));
end;

end.
