unit OptionsList;

//TODO: Options description

interface

uses
  Windows, Messages, CommCtrl, AvL, avlListViewEx, Aria2;

type
  TOptionStates = (osChanged, osUnknown);
  TOptionState = set of TOptionStates;
  TOptionsList = class(TSimplePanel)
  private
    function GetOptionState(Item: Integer): TOptionState;
    procedure SetOptionState(Item: Integer; const Value: TOptionState);
  private
    List: TListViewEx;
    Value: TComboBox;
    BtnSet: TButton;
    procedure AdjustColumns;
    procedure Resize(Sender: TObject);
    procedure SetValue(Sender: TObject);
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    property State[Item: Integer]: TOptionState read GetOptionState write SetOptionState;
  public
    constructor Create(AParent: TWinControl);
    procedure SetOptions(Options: TAria2Struct);
    function GetChangedOptions: TAria2OptionArray;
  end;

implementation

{ TOptionsList }

constructor TOptionsList.Create(AParent: TWinControl);
begin
  inherited Create(AParent, '');
  Border := 2;
  ExStyle := 0;
  OnResize := Resize;
  List := TListViewEx.Create(Self);
  List.Style := List.Style or LVS_SINGLESEL or LVS_SHOWSELALWAYS or LVS_NOSORTHEADER or LVS_SORTASCENDING;
  List.ViewStyle := LVS_REPORT;
  List.OptionsEx := List.OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  List.ColumnAdd('Option', 50);
  List.ColumnAdd('Value', 50);
  List.SetPosition(0, 0);
  Value := TComboBox.Create(Self, csDropDown);
  BtnSet := TButton.Create(Self, 'Set');
  BtnSet.SetSize(50, Value.Height);
  BtnSet.OnClick := SetValue;
end;

procedure TOptionsList.AdjustColumns;
begin
  List.ColumnWidth[0] := LVSCW_AUTOSIZE;
  List.ColumnWidth[0] := List.ColumnWidth[0] + 10;
  List.ColumnWidth[1] := List.ClientWidth - List.ColumnWidth[0];
end;

procedure TOptionsList.Resize(Sender: TObject);
begin
  List.SetSize(ClientWidth, ClientHeight - 30);
  Value.SetBounds(0, ClientHeight - Value.Height, ClientWidth - 55, Value.Height);
  BtnSet.SetPosition(ClientWidth - 50, Value.Top);
  AdjustColumns;
end;

procedure TOptionsList.SetOptions(Options: TAria2Struct);
var
  i, Item: Integer;
begin
  List.Clear;
  Value.Clear;
  for i := 0 to Options.NamesCount - 1 do
  begin
    Item := List.ItemAdd(Options.Names[i]);
    List.Items[Item, 1] := Options[Options.Names[i]];
    State[Item] := [];
  end;
  AdjustColumns;
end;

procedure TOptionsList.WMNotify(var Msg: TWMNotify);
begin
  inherited;
  if Assigned(List) and (PNMHdr(Msg.NMHdr).hwndFrom = List.Handle) then
    if (Msg.NMHdr.code = LVN_ITEMCHANGED) and (PNMListView(Msg.NMHdr).uChanged and LVIF_STATE <> 0) and (PNMListView(Msg.NMHdr).uNewState and LVIS_SELECTED <> 0) then
      with PNMListView(Msg.NMHdr)^ do
      begin
        Value.Text := List.Items[iItem, 1];
        //TODO: Fill combobox
      end;
end;

function TOptionsList.GetChangedOptions: TAria2OptionArray;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to List.ItemCount - 1 do
    if osChanged in State[i] then
    begin
      SetLength(Result, Length(Result) + 1);
      with Result[High(Result)] do
      begin
        Key := List.Items[i, 0];
        if (Key <> '') and (Key[1] = '*') then
          Delete(Key, 1, 1);
        Value := List.Items[i, 1];
      end;
    end;
end;

procedure TOptionsList.SetValue(Sender: TObject);
var
  Item: Integer;
begin
  Item := List.SelectedIndex;
  if Item >= 0 then
  begin
    if (List.Items[Item, 0] <> '') and (List.Items[Item, 0][1] <> '*') then
      List.Items[Item, 0] := '*' + List.Items[Item, 0];
    List.Items[Item, 1] := Value.Text;
    State[Item] := State[Item] + [osChanged];
  end;
end;

function TOptionsList.GetOptionState(Item: Integer): TOptionState;
begin
  Result := TOptionState(Byte(List.ItemObject[Item]));
end;

procedure TOptionsList.SetOptionState(Item: Integer; const Value: TOptionState);
begin
  List.ItemObject[Item] := TObject(Byte(Value));
end;

end.
