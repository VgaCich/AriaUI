unit TransfersList;

interface

uses
  Windows, Messages, CommCtrl, AvL, avlUtils, avlListViewEx, avlEventBus,
  avlMasks, Aria2, Utils;

type
  TTransfersList = class(TListViewEx)
  private
    FUpdateState: record
      Item: Integer;
      Selected: TAria2GID;
    end;
    FColumns: TListColumns;
    FUpdateKeys: TStringArray;
    FSearchString: string;
    procedure Cleanup(Sender: TObject);
    procedure AddTransferKey(const Column: TListColumn);
    function GetGID(Index: Integer): TAria2GID;
    procedure LoadSettings(Sender: TObject; const Args: array of const);
    procedure SaveSettings(Sender: TObject; const Args: array of const);
  public
    constructor Create(Parent: TWinControl);
    destructor Destroy; override;
    procedure Clear;
    procedure Find(const S: string);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Update(List: TAria2Struct; Names: TStringList);
    property GID[Index: Integer]: TAria2GID read GetGID;
    property UpdateKeys: TStringArray read FUpdateKeys;
  end;

const
  STransferColumns = 'TransferListColumns';

implementation

const
  DefTransferColumns: array[0..10] of TListColumn = (
    (Caption: 'Name'; Width: 200; FType: ftName; Field: ''),
    (Caption: 'Size'; Width: 80; FType: ftSize; Field: sfTotalLength),
    (Caption: 'Progress'; Width: 60; FType: ftPercent; Field: sfCompletedLength + ':' + sfTotalLength),
    (Caption: 'ETA'; Width: 60; FType: ftETA; Field: sfDownloadSpeed + ':' + sfTotalLength + ':' + sfCompletedLength),
    (Caption: 'Status'; Width: 100; FType: ftStatus; Field: ''),
    (Caption: 'Uploaded'; Width: 80; FType: ftSize; Field: sfUploadLength),
    (Caption: 'DL speed'; Width: 80; FType: ftSpeed; Field: sfDownloadSpeed),
    (Caption: 'UL speed'; Width: 80; FType: ftSpeed; Field: sfUploadSpeed),
    (Caption: 'Ratio'; Width: 50; FType: ftPercent; Field: sfUploadLength + ':' + sfCompletedLength),
    (Caption: 'Conns.'; Width: 50; FType: ftString; Field: sfConnections),
    (Caption: 'Seeds'; Width: 50; FType: ftString; Field: sfNumSeeders));

{ TTransfersList }

constructor TTransfersList.Create(Parent: TWinControl);
begin
  inherited;
  Style := Style and not LVS_SINGLESEL or LVS_SHOWSELALWAYS {or LVS_EDITLABELS} or LVS_NOSORTHEADER; //TODO: switches for sorting & etc
  ViewStyle := LVS_REPORT;
  OptionsEx := OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  SmallImages := LoadImageList('TLICONS');
  EventBus.AddListener(EvLoadSettings, LoadSettings);
  EventBus.AddListener(EvSaveSettings, SaveSettings);
  OnDestroy := Cleanup;
end;

destructor TTransfersList.Destroy;
begin
  EventBus.RemoveListeners([LoadSettings, SaveSettings]);
  Finalize(FColumns);
  Finalize(FUpdateKeys);
  SmallImages.Free;
  inherited;
end;

procedure TTransfersList.Clear;
var
  i: Integer;
begin
  for i := 0 to ItemCount - 1 do
    FreeMem(PChar(ItemObject[i]));
  inherited Clear;
end;

procedure TTransfersList.Find(const S: string);
var
  i, From: Integer;
  Mask: TMask;
begin
  if S <> '' then
  begin
    FSearchString := S;
    if (Pos('*', FSearchString) = 0) and (Pos('?', FSearchString) = 0) then
      FSearchString := '*' + FSearchString + '*';
    From := -1;
  end
    else From := SelectedIndex;
  Mask := TMask.Create(FSearchString);
  try
    for i := Max(0, From + 1) to ItemCount - 1 do
      if Mask.Matches(Items[i, 0]) then
      begin
        ClearSelection;
        SelectedIndex := i;
        Perform(LVM_ENSUREVISIBLE, i, 0);
        SetFocus;
        Exit;
      end;
    MessageDlg('Not found', Caption, MB_ICONINFORMATION);
  finally
    Mask.Free;
  end;
end;

procedure TTransfersList.BeginUpdate;
begin
  inherited;
  with FUpdateState do
  begin
    Item := 0;
    Selected := GID[SelectedIndex];
  end;
end;

procedure TTransfersList.EndUpdate;
var
  i: Integer;
begin
  while ItemCount > FUpdateState.Item do
  begin
    FreeMem(PChar(ItemObject[ItemCount - 1]));
    ItemDelete(ItemCount - 1);
  end;
  if (FUpdateState.Selected <> '') and (SelCount <= 1) and (GID[SelectedIndex] <> FUpdateState.Selected) then
  begin
    ClearSelection;
    for i := 0 to ItemCount - 1 do
      if GID[i] = FUpdateState.Selected then
      begin
        SelectedIndex := i;
        Break;
      end;
    if SelCount = 0 then
      SelectedIndex := ItemCount - 1; //TODO: Setting 'where to scroll'
  end;
  inherited;
end;

procedure TTransfersList.Update(List: TAria2Struct; Names: TStringList);
var
  i, j, Image, TopItem, BottomItem: Integer;
  P: PChar;
  S: string;
  Pt: TPoint;
begin
  TopItem := Perform(LVM_GETTOPINDEX, 0, 0);
  BottomItem := TopItem + Perform(LVM_GETCOUNTPERPAGE, 0, 0);
  for i := 0 to List.Length[''] - 1 do
  begin
    List.Index := i;
    if FUpdateState.Item >= ItemCount then
    begin
      FUpdateState.Item := ItemAdd('');
      GetMem(P, 32);
      ZeroMemory(P, 32);
      ItemObject[FUpdateState.Item] := TObject(P);
    end;
    Perform(LVM_GETORIGIN, 0, Integer(@Pt));
    if GID[FUpdateState.Item] <> List[sfGID] then
      LStrCpy(PChar(ItemObject[FUpdateState.Item]), PChar(List[sfGID]))
    else if (FUpdateState.Item < TopItem) or (FUpdateState.Item > BottomItem) then
    begin
      Inc(FUpdateState.Item); //TODO: refresh items on scrolling
      Continue;
    end;
    for j := Low(FColumns) to High(FColumns) do
    begin
      S := GetFieldValue(List, Names, FColumns[j].FType, FColumns[j].Field);
      if Items[FUpdateState.Item, j] <> S then
        Items[FUpdateState.Item, j] := S;
    end;
    Image := StrToEnum(List[sfStatus], sfStatusValues);
    if (TAria2Status(Image) in [asActive, asWaiting]) and Boolean(StrToEnum(List[sfSeeder], sfBoolValues)) then
      Inc(Image, 6);
    if Boolean(StrToEnum(List[sfVerifyPending], sfBoolValues)) or List.Has[sfVerifiedLength] then
      Image := 8;
    if ItemImageIndex[FUpdateState.Item] <> Image then
      ItemImageIndex[FUpdateState.Item] := Image;
    Inc(FUpdateState.Item);
  end;
end;

procedure TTransfersList.Cleanup(Sender: TObject);
begin
  Clear;
end;

procedure TTransfersList.AddTransferKey(const Column: TListColumn);
begin
  AddStatusKey(FUpdateKeys, Column.Field);
end;

function TTransfersList.GetGID(Index: Integer): TAria2GID;
begin
  if (Index < 0) or (Index >= ItemCount) then
    Result := ''
  else
    Result := PChar(ItemObject[Index]);
end;

procedure TTransfersList.LoadSettings(Sender: TObject; const Args: array of const);
begin
  SetArray(FUpdateKeys, BasicTransferKeys);
  LoadListColumns(Self, STransferColumns, FColumns, DefTransferColumns, AddTransferKey);
end;

procedure TTransfersList.SaveSettings(Sender: TObject; const Args: array of const);
begin
  SaveListColumns(Self, STransferColumns, FColumns, DefTransferColumns);
end;

end.
