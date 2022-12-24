unit ServersList;

interface

uses
  Windows, Messages, AvL, avlUtils, avlEventBus, Utils;

type
  TOnServerChange = procedure(Sender: TObject; PrevServer: TServerInfo) of object;
  TServersList = class(TTabControl)
  private
    FOnChange: TOnServerChange;
    FServer: TServerInfo;
    FServers: TList;
    procedure Changed(Sender: TObject);
    procedure Cleanup(Sender: TObject);
    procedure LoadSettings(Sender: TObject; const Args: array of const);
    procedure SaveSettings(Sender: TObject; const Args: array of const);
    procedure WMContextMenu(var Msg: TMessage); message WM_CONTEXTMENU;
  public
    constructor Create(Parent: TWinControl);
    destructor Destroy; override;
    procedure Clear;
    property OnChange: TOnServerChange read FOnChange write FOnChange;
    property Server: TServerInfo read FServer;
  end;

const
  SCurrent = 'Current';

implementation

{ TServersList }

constructor TServersList.Create(Parent: TWinControl);
begin
  inherited Create(Parent);
  Style := tsTabs;
  inherited OnChange := Changed;
  FServers := TList.Create;
  EventBus.AddListener(EvLoadSettings, LoadSettings);
  EventBus.AddListener(EvSaveSettings, SaveSettings);
  OnDestroy := Cleanup;
end;

destructor TServersList.Destroy;
begin
  EventBus.RemoveListeners([LoadSettings, SaveSettings]);
  FreeAndNil(FServers);
  inherited;
end;

procedure TServersList.Clear;
var
  i: Integer;
begin
  FServer := nil;
  for i := 0 to FServers.Count - 1 do
    TObject(FServers[i]).Free;
  FServers.Clear;
  while TabCount > 0 do
    TabDelete(0);
end;

procedure TServersList.Changed(Sender: TObject);
var
  PrevServer: TServerInfo;
begin
  if FServer = FServers[TabIndex] then Exit; //TODO: OnChange is called twice for some reason (left from ComboBox, recheck)
  PrevServer := FServer;
  FServer := TObject(FServers[TabIndex]) as TServerInfo;
  if Assigned(FOnChange) then
    FOnChange(Self, PrevServer);
  Settings.WriteInteger(SServers, SCurrent, TabIndex);
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
  for i := 0 to Max(0, Settings.ReadInteger(SServers, SCount, 0) - 1) do
    TabAdd((TObject(FServers[FServers.Add(TServerInfo.Create(i))]) as TServerInfo).Name);
  TabIndex := Settings.ReadInteger(SServers, SCurrent, 0);
  if Assigned(OnResize) then OnResize(Self);
  Changed(Self);
end;

procedure TServersList.SaveSettings(Sender: TObject; const Args: array of const);
begin

end;

procedure TServersList.WMContextMenu(var Msg: TMessage);
begin
  with Msg do
    Result := Parent.Perform(Msg, WParam, LParam);
end;

end.
