unit Utils;

interface

uses
  Windows, AvL, avlUtils, avlListViewEx, avlSettings, Aria2;

type
  TStringArray = array of string;
  TFieldType = (ftNone, ftString, ftName, ftStatus, ftSize, ftSpeed, ftPercent, ftETA, ftPath, ftLongStatus);
  TListColumn = record
    Caption: string;
    Width: Integer;
    FType: TFieldType;
    Field: string;
  end;
  TListColumns = array of TListColumn;
  TListColumnCallback = procedure(const Column: TListColumn) of object;
  TServerInfo = class
  private
    FName, FSection, FHost, FUsername, FPassword, FToken: string;
    FPort: Word;
    FSSL: Boolean;
    FTemporary: TStringList;
    function GetPersistent(const Name: string): string;
    function GetTemporary(const Name: string): TObject;
    procedure PutPersistent(const Name, Value: string);
    procedure PutTemporary(const Name: string; const Value: TObject);
  public
    constructor Create(ServerIndex: Integer);
    destructor Destroy; override;
    property Name: string read FName;
    property Host: string read FHost;
    property Port: Word read FPort;
    property Username: string read FUsername;
    property Password: string read FPassword;
    property Token: string read FToken;
    property SSL: Boolean read FSSL;
    property Persistent[const Name: string]: string read GetPersistent write PutPersistent;
    property Temporary[const Name: string]: TObject read GetTemporary write PutTemporary; default;
  end;

procedure InsertMenu(Menu, SubMenu: TMenu; const Name: string; ID: Cardinal);
procedure FreeMenu(Menu: TMenu);
procedure SetArray(var Dest: TStringArray; const Src: array of string);
function First(const Pair: string; const Sep: Char = ':'): string;
function Second(const Pair: string; const Sep: Char = ':'): string;
function GetFieldValue(List: TAria2Struct; Names: TStringList; FType: TFieldType; const Field: string): string;
procedure AddStatusKey(var Keys: TStringArray; Field: string);
procedure LoadListColumns(List: TListViewEx; const Section: string; var Columns: TListColumns; const DefColumns: array of TListColumn; Callback: TListColumnCallback);
procedure SaveListColumns(List: TListViewEx; const Section: string; var Columns: TListColumns; const DefColumns: array of TListColumn);
procedure ShowException;
function StrToEnum(const S: string; const Values: array of string): Integer;
function MakeDword(Lo, Hi: Word): Cardinal;
function Select(Exp: Boolean; const STrue, SFalse: string): string;
function Check(Exp: Boolean; const STrue: string): string;
function DecodeURL(const URL: string): string;
function LoadImageList(const Name: PChar): TImageList;

const
  EvServerChanged = 'Global.ServerChanged'; //[PrevServer, CurServer: TServerInfo]
  EvUpdate = 'Global.Update'; //[UpdateThread: TUpdateThread]
  EvLoadSettings = 'Global.LoadSettings'; //No params
  EvSaveSettings = 'Global.SaveSettings'; //No params
  AppName = 'AriaUI';
  SGeneral = 'General';
  SCount = 'Count';
  SFieldCaption = 'Caption.';
  SFieldWidth = 'Width.';
  SFieldType = 'Type.';
  SFieldField = 'Field.';
  SFieldFlags = 'Flags.';
  SServers = 'Servers';
  SServer = 'Server.';
  SHost = 'Host';
  SPort = 'Port';
  SUsername = 'Username';
  SPassword = 'Password';
  SToken = 'Token';
  SUseSSL = 'UseSSL';
  BasicTransferKeys: array[0..6] of string = (sfGID, sfBittorrent, sfStatus, sfErrorMessage, sfSeeder, sfVerifyPending, sfVerifiedLength);

var
  Settings: TSettings;

implementation

procedure InsertMenu(Menu, SubMenu: TMenu; const Name: string; ID: Cardinal);
begin
  SubMenu.Tag := Menu.Tag;
  Menu.Tag := Integer(SubMenu);
  Windows.InsertMenu(Menu.Handle, ID, MF_BYCOMMAND or MF_POPUP, SubMenu.Handle, PChar(Name));
end;

procedure FreeMenu(Menu: TMenu);
begin
  if Assigned(Pointer(Menu.Tag)) then
    FreeMenu(TMenu(Menu.Tag));
  Menu.Free;
end;

procedure SetArray(var Dest: TStringArray; const Src: array of string);
var
  i: Integer;
begin
  SetLength(Dest, Length(Src));
  for i := 0 to High(Src) do
    Dest[i] := Src[i];
end;

function First(const Pair: string; const Sep: Char = ':'): string;
begin
  Result := Copy(Pair, 1, FirstDelimiter(Sep, Pair) - 1);
end;

function Second(const Pair: string; const Sep: Char = ':'): string;
begin
  Result := Copy(Pair, FirstDelimiter(Sep, Pair) + 1, MaxInt);
end;

function GetFieldValue(List: TAria2Struct; Names: TStringList; FType: TFieldType; const Field: string): string;

  function GetPercent(N, Q: Int64): string;
  begin
    if Q = 0 then
      Result := '-'
    else
      Result := FloatToStr2(100 * N / Q, 1, 2) + '%';
  end;

const
  StatusSeeding: array[Boolean] of string = (' [S]', '; seeding');
  StatusVerify: array[Boolean] of string = (' [V]', '; verify pending');
  StatusVerifying: array[Boolean] of string = (' [V: ', '; verifying [');
begin
  case FType of
    ftNone: Result := '';
    ftString: Result := List[Field];
    ftName: if List.Has[sfBittorrent] and (List[sfBTName] <> '') then
               Result := List[sfBTName]
             else
               Result := Names.Values[List[sfGID]];
    ftStatus, ftLongStatus: begin
                 Result := List[sfStatus];
                 if Boolean(StrToEnum(List[sfSeeder], sfBoolValues)) then
                   Result := Result + StatusSeeding[FType = ftLongStatus];
                 if Boolean(StrToEnum(List[sfVerifyPending], sfBoolValues)) then
                   Result := Result + StatusVerify[FType = ftLongStatus];
                 if List.Has[sfVerifiedLength] then
                   Result := Result + StatusVerifying[FType = ftLongStatus] + SizeToStr(List.Int64[sfVerifiedLength]) + ']';
                 if List[sfErrorMessage] <> '' then
                   Result := Result + ' (' + List[sfErrorMessage] + ')';
               end;
    ftSize: Result := SizeToStr(List.Int64[Field]);
    ftSpeed: Result := SizeToStr(List.Int64[Field]) + '/s';
    ftPercent: Result := GetPercent(List.Int64[First(Field)], List.Int64[Second(Field)]);
    ftETA: Result := EtaToStr(List.Int64[First(Second(Field))] - List.Int64[Second(Second(Field))], List.Int64[First(Field)]);
    ftPath: Result := StringReplace(List[Field], '/', '\', [rfReplaceAll]); 
  end;
end;

procedure AddStatusKey(var Keys: TStringArray; Field: string);
var
  i: Integer;
  Key: string;
begin
  while Field <> '' do
  begin
    Key := Tok(':', Field);
    Key := Tok('.', Key);
    for i := 0 to High(Keys) do
      if Keys[i] = Key then Continue;
    SetLength(Keys, Length(Keys) + 1);
    Keys[High(Keys)] := Key;
  end;
end;

procedure LoadListColumns(List: TListViewEx; const Section: string; var Columns: TListColumns; const DefColumns: array of TListColumn; Callback: TListColumnCallback);
var
  i: Integer;
begin
  if Settings.ReadInteger(Section, SCount, 0) <> 0 then
  begin
    SetLength(Columns, Settings.ReadInteger(Section, SCount, 0));
    for i := 0 to High(Columns) do
      with Columns[i] do
      begin
        Caption := Settings.ReadString(Section, SFieldCaption + IntToStr(i), '');
        Width := Settings.ReadInteger(Section, SFieldWidth + IntToStr(i), 0);
        FType := TFieldType(Settings.ReadInteger(Section, SFieldType + IntToStr(i), 0));
        Field := Settings.ReadString(Section, SFieldField + IntToStr(i), '');
      end;
  end
  else begin
    SetLength(Columns, Length(DefColumns));
    for i := 0 to High(DefColumns) do
      Columns[i] := DefColumns[i];
  end;
  while List.ColumnCount > 0 do
    List.ColumnDelete(0);
  for i := 0 to High(Columns) do
    with Columns[i] do
    begin
      List.ColumnAdd(Caption, Width);
      if Assigned(Callback) then
        Callback(Columns[i]);
    end;
end;

procedure SaveListColumns(List: TListViewEx; const Section: string; var Columns: TListColumns; const DefColumns: array of TListColumn);
var
  i: Integer;
  Changed: Boolean;
begin
  for i := 0 to High(Columns) do
    Columns[i].Width := List.ColumnWidth[i];
  Changed := Length(Columns) <> Length(DefColumns);
  if not Changed then
    for i := 0 to High(Columns) do
      if (Columns[i].Caption <> DefColumns[i].Caption) or
         (Columns[i].Width <> DefColumns[i].Width) or
         (Columns[i].FType <> DefColumns[i].FType) or
         (Columns[i].Field <> DefColumns[i].Field) then
        Changed := true;
  if Changed then
  begin
    Settings.WriteInteger(Section, SCount, Length(Columns));
    for i := 0 to High(Columns) do
      with Columns[i] do
      begin
        Settings.WriteString(Section, SFieldCaption + IntToStr(i), Caption);
        Settings.WriteInteger(Section, SFieldWidth + IntToStr(i), Width);
        Settings.WriteInteger(Section, SFieldType + IntToStr(i), Ord(FType));
        Settings.WriteString(Section, SFieldField + IntToStr(i), Field);
      end;
  end;
end;

procedure ShowException;
begin
  MessageDlg(Exception(ExceptObject).Message, 'Error', MB_ICONERROR);
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

function DecodeURL(const URL: string): string;
const
  HexChars = ['0' .. '9', 'a' .. 'f', 'A' .. 'F'];
var
  i: Integer;
begin
  Result := URL;
  i := 1;
  while i <= Length(Result) do
  begin
    if (Result[i] = '%') and (i < Length(Result) - 1) and (Result[i + 1] in HexChars) and (Result[i + 2] in HexChars) then
    begin
      Result[i] := Chr(HexToInt(Copy(Result, i + 1, 2)));
      Delete(Result, i + 1, 2);
    end;
    Inc(i);
  end;
end;

function LoadImageList(const Name: PChar): TImageList;
begin
  Result := TImageList.Create;
  Result.AddMasked(LoadImage(hInstance, Name, IMAGE_BITMAP, 0, 0, 0), clFuchsia);
end;

{ TServerInfo }

constructor TServerInfo.Create(ServerIndex: Integer);
begin
  inherited Create;
  FName := Settings.ReadString(SServers, IntToStr(ServerIndex), '###');
  FSection := SServer + IntToStr(ServerIndex);
  FHost := Settings.ReadString(FSection, SHost, 'localhost');
  FPort := Settings.ReadInteger(FSection, SPort, 6800);
  FUsername := Settings.ReadString(FSection, SUsername, '');
  FPassword := Settings.ReadString(FSection, SPassword, '');
  FToken := Settings.ReadString(FSection, SToken, '');
  FSSL := Settings.ReadBool(FSection, SUseSSL, false);
  FTemporary := TStringList.Create;
end;

destructor TServerInfo.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTemporary.Count - 1 do
    FTemporary.Objects[i].Free;
  FreeAndNil(FTemporary);
  inherited;
end;

function TServerInfo.GetPersistent(const Name: string): string;
begin
  if Assigned(Self) then
    Result := Settings.ReadString(FSection, Name, '')
  else
    Result := '';
end;

function TServerInfo.GetTemporary(const Name: string): TObject;
begin
  if Assigned(Self) then
    Result := FTemporary.Objects[FTemporary.IndexOf(Name)]
  else
    Result := nil;
end;

procedure TServerInfo.PutPersistent(const Name, Value: string);
begin
  if Assigned(Self) then
    Settings.WriteString(FSection, Name, Value);
end;

procedure TServerInfo.PutTemporary(const Name: string; const Value: TObject);
begin
  if not Assigned(Self) then
  begin
    Value.Free;
    Exit;
  end;
  if FTemporary.IndexOf(Name) >= 0 then
    FTemporary.Objects[FTemporary.IndexOf(Name)].Free
  else
    FTemporary.Add(Name);
  FTemporary.Objects[FTemporary.IndexOf(Name)] := Value;
end;

initialization

  Settings := TSettings.Create(AppName);
  Settings.Source := ssIni;

finalization

  FreeAndNil(Settings);

end.