unit OptionsList;

//TODO: Options description
//TODO: Go to webdocs on F1 in options list (or embedded help?)
//TODO: Save values history?
//TODO: Better handling of overlay combobox
//TODO: Do not select in combo by scroll, show editor by clicking on value, not name

interface

uses
  Windows, Messages, CommCtrl, AvL, avlUtils, avlListViewEx, Aria2;

type
  TOptionStates = (osChanged, osUnknown);
  TOptionState = set of TOptionStates;
  TOptionsList = class(TListViewEx)
  private
    function GetOptionState(Item: Integer): TOptionState;
    procedure SetOptionState(Item: Integer; const Value: TOptionState);
  private
    Value: TComboBox;
    procedure AdjustColumns;
    function GetOptionName(Item: Integer): string;
    procedure ValueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueChange(Sender: TObject);
    procedure AdjustValue;
    procedure SetValue(const Value: string);
    procedure OptionChange(Sender: TObject);
    procedure WMUser(var Msg: TMessage); message WM_USER;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    property State[Item: Integer]: TOptionState read GetOptionState write SetOptionState;
  public
    constructor Create(AParent: TWinControl);
    procedure SetOptions(Options: TAria2Struct);
    function GetChangedOptions: TAria2OptionArray;
  end;

implementation

function MsgHookProc(hWnd: THandle; Msg: UINT; wParam, lParam: Longint): Longint; stdcall;
begin
  Result := CallWindowProc(Pointer(GetWindowLong(hWnd, GWL_USERDATA)), hWnd, Msg, wParam, lParam);
  if (Msg = WM_VSCROLL) or (Msg = WM_VSCROLL) or (Msg = LVM_SCROLL) or (Msg = WM_MOUSEWHEEL) then
    SendMessage(hWnd, WM_USER, 0, 0);
end;

function EditHookProc(hWnd: THandle; Msg: UINT; wParam, lParam: Longint): Longint; stdcall;
begin
  if (Msg = WM_KEYUP) and (wParam in [VK_RETURN, VK_ESCAPE]) then
    SendMessage(GetWindowLong(hWnd, GWL_HWNDPARENT), Msg, wParam, lParam);
  Result := CallWindowProc(Pointer(GetWindowLong(hWnd, GWL_USERDATA)), hWnd, Msg, wParam, lParam);
end;

{ TOptionsList }

constructor TOptionsList.Create(AParent: TWinControl);
var
  CBInfo: TComboBoxInfo;
begin
  inherited Create(AParent);
  Style := Style or LVS_SINGLESEL or LVS_SHOWSELALWAYS or LVS_NOSORTHEADER or LVS_SORTASCENDING;
  ViewStyle := LVS_REPORT;
  OptionsEx := OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  ColumnAdd('Option', 50);
  ColumnAdd('Value', 50);
  //OnResize := Resize;
  OnChange := OptionChange;
  SetWindowLong(Handle, GWL_USERDATA, SetWindowLong(Handle, GWL_WNDPROC, Longint(@MsgHookProc)));
  Value := TComboBox.Create(Self, csDropDown);
  CBInfo.cbSize := SizeOf(CBInfo);
  GetComboBoxInfo(Value.Handle, CBInfo);
  Value.Tag := Integer(CBInfo.hwndItem);
  Value.Visible := false;
  Value.OnKeyUp := ValueKeyUp;
  Value.OnChange := ValueChange;
  SetWindowLong(GetWindow(Value.Handle, GW_CHILD), GWL_USERDATA,
    SetWindowLong(GetWindow(Value.Handle, GW_CHILD), GWL_WNDPROC, Longint(@EditHookProc)));
end;

procedure TOptionsList.AdjustColumns;
begin
  ColumnWidth[0] := LVSCW_AUTOSIZE;
  ColumnWidth[0] := ColumnWidth[0] + 10;
  ColumnWidth[1] := ClientWidth - ColumnWidth[0];
  AdjustValue;
end;

procedure TOptionsList.AdjustValue;
var
  IR, VR: TRect;
begin
  GetClientRect(Handle, VR);
  IR.Left := LVIR_BOUNDS;
  Perform(LVM_GETITEMRECT, SelectedIndex, Longint(@IR));
  IR.Left := IR.Left + ColumnWidth[0];
  with IR do
    Value.SetBounds(Left, Top, Right - Left, Bottom - Top);
  Value.Visible := (IR.Left < VR.Right) and (IR.Top < VR.Bottom) and
                   (IR.Right > VR.Left) and (IR.Bottom > VR.Top);
end;

{procedure TOptionsList.Resize(Sender: TObject);
begin
  AdjustColumns;
end;}

procedure TOptionsList.SetOptions(Options: TAria2Struct);
var
  i, Item: Integer;
begin
  Clear;
  Value.Clear;
  Value.Visible := false;
  for i := 0 to Options.NamesCount - 1 do
  begin
    Item := ItemAdd(Options.Names[i]);
    Items[Item, 1] := Options[Options.Names[i]];
    State[Item] := [];
  end;
  AdjustColumns;
end;

procedure TOptionsList.OptionChange(Sender: TObject);
var
  i: Integer;
  OptInfo: string;
begin
  OptInfo := '';
  for i := 0 to High(Aria2Options) do
    if Aria2Options[i].Key = GetOptionName(SelectedIndex) then
    begin
      OptInfo := Aria2Options[i].Value;
      Break;
    end;
  Value.Clear;
  while OptInfo <> '' do
    Value.ItemAdd(Tok(OSep, OptInfo));
  Value.Text := Items[SelectedIndex, 1];
  AdjustValue;
  Value.SetFocus;
end;

procedure TOptionsList.WMUser(var Msg: TMessage);
begin
  AdjustValue;
end;

procedure TOptionsList.WMSetFocus(var Msg: TWMSetFocus);
begin
  if ((Msg.FocusedWnd = Value.Handle) or (Msg.FocusedWnd = THandle(Value.Tag))) and Value.Visible then
    Value.SetFocus;
end;

function TOptionsList.GetChangedOptions: TAria2OptionArray;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ItemCount - 1 do
    if osChanged in State[i] then
    begin
      SetLength(Result, Length(Result) + 1);
      with Result[High(Result)] do
      begin
        Key := GetOptionName(i);
        Value := Items[i, 1];
      end;
    end;
end;

function TOptionsList.GetOptionName(Item: Integer): string;
begin
  Result := Items[Item, 0];
  if (Result <> '') and (Result[1] = '*') then
    Delete(Result, 1, 1);
end;

procedure TOptionsList.ValueKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    SetValue(Value.Text)
  else if Key = VK_ESCAPE then
    Value.Text := Items[SelectedIndex, 1];
end;

procedure TOptionsList.ValueChange(Sender: TObject);
begin
  SetValue(Value.Items[Value.ItemIndex]);
end;

function TOptionsList.GetOptionState(Item: Integer): TOptionState;
begin
  Result := TOptionState(Byte(ItemObject[Item]));
end;

procedure TOptionsList.SetOptionState(Item: Integer; const Value: TOptionState);
begin
  ItemObject[Item] := TObject(Byte(Value));
end;

procedure TOptionsList.SetValue(const Value: string);
var
  Item: Integer;
begin
  Item := SelectedIndex;
  if Item >= 0 then
  begin
    Items[Item, 0] := '*' + GetOptionName(Item);
    Items[Item, 1] := Value;
    State[Item] := State[Item] + [osChanged];
  end;
end;

end.
