unit ServersList;

interface

uses
  Windows, Messages, AvL, avlUtils, avlEventBus, Utils;

type
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
  SCurrent = 'Current';

implementation

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
