unit ServersList;

interface

uses
  Windows, Messages, AvL, avlUtils, avlEventBus, Utils;

type
  TServerInfo = class
  private
    FSection, FHost, FUsername, FPassword, FToken: string;
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
    property Host: string read FHost;
    property Port: Word read FPort;
    property Username: string read FUsername;
    property Password: string read FPassword;
    property Token: string read FToken;
    property SSL: Boolean read FSSL;
    property Persistent[const Name: string]: string read GetPersistent write PutPersistent;
    property Temporary[const Name: string]: TObject read GetTemporary write PutTemporary; default;
  end;
  TOnServerChange = procedure(Sender: TObject; PrevServer: TServerInfo) of object;
  TServersList = class(TComboBoxEx)
  private
    FOnChange: TOnServerChange;
    FServer: TServerInfo;
    procedure Changed(Sender: TObject);
    procedure Cleanup(Sender: TObject);
    procedure LoadSettings(Sender: TObject; const Args: array of const);
    procedure SaveSettings(Sender: TObject; const Args: array of const);
  public
    constructor Create(Parent: TWinControl);
    destructor Destroy; override;
    procedure Clear;
    property OnChange: TOnServerChange read FOnChange write FOnChange;
    property Server: TServerInfo read FServer;
  end;

const
  SServers = 'Servers';
  SServer = 'Server.';
  SCurrent = 'Current';
  SHost = 'Host';
  SPort = 'Port';
  SUsername = 'Username';
  SPassword = 'Password';
  SToken = 'Token';
  SUseSSL = 'UseSSL';

implementation

{ TServerInfo }

constructor TServerInfo.Create(ServerIndex: Integer);
begin
  inherited Create;
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

{ TServersList }

constructor TServersList.Create(Parent: TWinControl);
begin
  inherited Create(Parent, csDropDownList);
  inherited OnChange := Changed;
  EventBus.AddListener(EvLoadSettings, LoadSettings);
  EventBus.AddListener(EvSaveSettings, SaveSettings);
  OnDestroy := Cleanup;
end;

destructor TServersList.Destroy;
begin
  EventBus.RemoveListeners([LoadSettings, SaveSettings]);
  inherited;
end;

procedure TServersList.Clear;
var
  i: Integer;
begin
  for i := 0 to ItemCount - 1 do
    Objects[i].Free;
  FServer := nil;
  inherited Clear;
end;

procedure TServersList.Changed(Sender: TObject);
var
  PrevServer: TServerInfo;
begin
  if FServer = Objects[ItemIndex] then Exit; //TODO: OnChange is called twice for some reason
  PrevServer := FServer;
  FServer := Objects[ItemIndex] as TServerInfo;
  if Assigned(FOnChange) then
    FOnChange(Self, PrevServer);
  Settings.WriteInteger(SServers, SCurrent, ItemIndex);
end;

procedure TServersList.Cleanup(Sender: TObject);
begin
  Clear;
end;

procedure TServersList.LoadSettings(Sender: TObject; const Args: array of const);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Settings.ReadInteger(SServers, SCount, 0) - 1 do
    Objects[ItemAdd(Settings.ReadString(SServers, IntToStr(i), '###'))] := TServerInfo.Create(i);
  if ItemCount = 0 then
    Objects[ItemAdd('###')] := TServerInfo.Create(0);
  ItemIndex := Settings.ReadInteger(SServers, SCurrent, 0);
  Changed(Self);
end;

procedure TServersList.SaveSettings(Sender: TObject; const Args: array of const);
begin

end;

end.
