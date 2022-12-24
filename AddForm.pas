unit AddForm;

//TODO: Configurable "pinned options", path history & pinned paths
//TODO: "Option presets"

interface

uses
  Windows, Messages, AvL, avlUtils, avlEventBus, Base64, Aria2;

type
  TAddForm = class(TForm)
    Options: array of TAria2Option;
    URLs: array of string;
    FileName: string;
    SeparateURLs: Boolean;
  private
    LName, LPath, LOptions: TLabel;
    EFileName: TEdit;
    CBPath: TComboBox;
    MURLs, MOptions: TMemo;
    CSeparateURLs, CPause, CMetaPause: TCheckBox;
    BtnOK, BtnCancel, BtnBrowse, BtnPath: TButton;
    POptions: TSimplePanel;
    FHandler: TOnEvent;
    FMaxHistory: Integer;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BrowseClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure LoadSettings(Sender: TObject; const Args: array of const);
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    procedure Show(const Caption, Filter: string; AddFile: Boolean; Handler: TOnEvent);
  end;

var
  FormAdd: TAddForm;

implementation

uses
  Utils, MainForm;

const
  SMaxPathHistory = 'MaxPathHistory';
  SPersOptions = 'AddForm.Options';
  SPersPathHistory = 'AddForm.PathHistory';
  SPersSeparateURLs = 'AddForm.SeparateURLs';
  SPersPause = 'AddForm.Pause';
  SPersMetaPause = 'AddForm.MetaPause';
  LNameCaption: array[Boolean] of string = ('Enter URLs (one per line):', 'Enter file name:');

{ TAddForm }

constructor TAddForm.Create(AParent: TWinControl);
var
  Ctrl: TWinControl;
begin
  inherited Create(AParent, '');
  BorderStyle := bsDialog;
  SetSize(400 + Width - ClientWidth, 350 + Height - ClientHeight);
  Position := poScreenCenter;
  OnShow := FormShow;
  LName := TLabel.Create(Self, '');
  LName.SetBounds(5, 5, ClientWidth div 2, 15);
  CSeparateURLs := TCheckBox.Create(Self, 'Separate transfers');
  CSeparateURLs.SetBounds(ClientWidth div 2 + 5, 5, ClientWidth div 2 - 10, 15);
  EFileName := TEdit.Create(Self, '');
  EFileName.SetBounds(5, 20, ClientWidth - 90, 24);
  BtnBrowse := TButton.Create(Self, 'Browse');
  BtnBrowse.SetBounds(ClientWidth - 80, 20, 75, 24);
  BtnBrowse.OnClick := BrowseClick;
  MURLs := TMemo.Create(Self, '');
  MURLs.SetBounds(5, 20, ClientWidth - 10, 100);
  MURLs.Style := MURLs.Style or WS_VSCROLL or WS_HSCROLL or ES_AUTOHSCROLL or ES_AUTOVSCROLL;
  POptions := TSimplePanel.Create(Self, '');
  POptions.Border := 2;
  POptions.SetBounds(5, 125, ClientWidth - 10, ClientHeight - 180);
  LPath := TLabel.Create(POptions, 'Download path:');
  LPath.SetBounds(0, 0, POptions.Width, 15);
  CBPath := TComboBox.Create(POptions, csDropDown);
  CBPath.SetBounds(0, 15, POptions.Width - 80, 24);
  BtnPath := TButton.Create(POptions, 'Browse');
  BtnPath.SetBounds(POptions.Width - 75, 15, 75, 24);
  BtnPath.OnClick := PathClick;
  CPause := TCheckBox.Create(POptions, 'Pause');
  CPause.SetBounds(0, 45, POptions.Width div 2, 15);
  CMetaPause := TCheckBox.Create(POptions, 'Pause following transfers');
  CMetaPause.SetBounds(CPause.Width + 5, 45, POptions.Width - CPause.Width - 5, 15);
  LOptions := TLabel.Create(POptions, 'Options (one per line, key=value):');
  LOptions.SetBounds(0, 65, POptions.Width, 15);
  MOptions := TMemo.Create(POptions, '');
  MOptions.SetBounds(0, 80, POptions.Width, POptions.Height - 80);
  MOptions.Style := MOptions.Style or WS_VSCROLL or WS_HSCROLL or ES_AUTOHSCROLL or ES_AUTOVSCROLL;
  BtnOK := TButton.Create(Self, 'OK');
  BtnOK.SetBounds(ClientWidth - 160, ClientHeight - 30, 75, 25);
  BtnOK.OnClick := OKClick;
  BtnCancel := TButton.Create(Self, 'Cancel');
  BtnCancel.SetBounds(ClientWidth - 80, ClientHeight - 30, 75, 25);
  BtnCancel.OnClick := CancelClick;
  Ctrl := Self;
  repeat
    Ctrl.OnKeyUp := FormKeyUp;
    Ctrl := Ctrl.NextControl;
  until (Ctrl.Parent <> Self) and (Ctrl.Parent <> POptions);
  EventBus.AddListener(EvLoadSettings, LoadSettings);
end;

destructor TAddForm.Destroy;
begin
  EventBus.RemoveListener(LoadSettings);
  Finalize(Options);
  Finalize(URLs);
  inherited;
end;

procedure TAddForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and ((ssCtrl in Shift) or not (Sender is TMemo)) then
    OKClick(BtnOK);
  if Key = VK_ESCAPE then
    CancelClick(BtnCancel);
end;

procedure TAddForm.FormShow(Sender: TObject);
var
  i: Integer;
  SL: TStringList;
begin
  if EFileName.Enabled then
  begin
    EFileName.SetFocus;
    EFileName.SelectAll;
    POptions.Top := EFileName.Top + EFileName.Height + 5;
  end;
  if MURLs.Enabled then //TODO: Auto-paste URLs
  begin
    MURLs.SetFocus;
    MURLs.Perform(EM_SETSEL, 0, -1);
    POptions.Top := MURLs.Top + MURLs.Height + 5;
  end;
  Height := Height - ClientHeight + POptions.Top + POptions.Height + 35;
  BtnOK.Top := ClientHeight - 30;
  BtnCancel.Top := ClientHeight - 30;
  MOptions.Text := Base64Decode(FormMain.Server.Persistent[SPersOptions]);
  SL := TStringList.Create;
  try
    SL.Text := Base64Decode(FormMain.Server.Persistent[SPersPathHistory]);
    CBPath.Clear;
    for i := 0 to SL.Count - 1 do
      CBPath.ItemAdd(SL[i]);
    CBPath.ItemIndex := 0;
  finally
    FreeAndNil(SL);
  end;
  CSeparateURLs.Checked := Boolean(StrToInt(FormMain.Server.Persistent[SPersSeparateURLs]));
  CPause.Checked := Boolean(StrToInt(FormMain.Server.Persistent[SPersPause]));
  CMetaPause.Checked := Boolean(StrToInt(FormMain.Server.Persistent[SPersMetaPause]));
end;

procedure TAddForm.BrowseClick(Sender: TObject);
var
  FN: string;
begin
  FN := EFileName.Text;
  if OpenSaveDialog(Handle, true, '', '', BtnBrowse.TagEx, '', 0, OFN_FILEMUSTEXIST, FN) then
    EFileName.Text := FN;
end;

procedure TAddForm.PathClick(Sender: TObject);
var
  Path: string;
begin
  Path := CBPath.Text;
  if OpenDirDialog(Handle, '', true, Path) then
    CBPath.Text := Path;
end;

procedure TAddForm.OKClick(Sender: TObject);

  procedure AddOption(const K, V: string);
  begin
    if Trim(K) = '' then Exit;
    SetLength(Options, Length(Options) + 1);
    with Options[High(Options)] do
    begin
      Key := Trim(K);
      Value := Trim(V);
    end;
  end;

var
  i: Integer;
  S: string;
begin
  if EFileName.Visible then
    FileName := EFileName.Text
  else begin
    SeparateURLs := CSeparateURLs.Checked;
    SetLength(URLs, 0);
    for i := 0 to MURLs.LineCount - 1 do
      if MURLs.LineStrings[i] <> '' then
      begin
        SetLength(URLs, Length(URLs) + 1);
        URLs[High(URLs)] := MURLs.LineStrings[i];
      end;
  end;
  SetLength(Options, 0);
  AddOption(soDir, CBPath.Text);
  if CPause.Checked then
    AddOption(soPause, svTrue);
  if CMetaPause.Checked then
    AddOption(soPauseMetadata, svTrue);
  for i := 0 to MOptions.LineCount - 1 do
    if MOptions.LineStrings[i] <> '' then
      AddOption(First(MOptions.LineStrings[i], '='), Second(MOptions.LineStrings[i], '='));
  FormMain.Server.Persistent[SPersOptions] := Base64Encode(MOptions.Text);
  S := CBPath.Text;
  CBPath.ItemInsert(S, 0);
  for i := CBPath.ItemCount - 1 downto 1 do
    if SameText(CBPath.Items[i], S) then
      CBPath.ItemDelete(i);
  while CBPath.ItemCount > FMaxHistory do
    CBPath.ItemDelete(CBPath.ItemCount - 1);
  S := '';
  for i := 0 to CBPath.ItemCount - 1 do
    S := S + CBPath.Items[i] + CRLF;
  FormMain.Server.Persistent[SPersPathHistory] := Base64Encode(Trim(S));
  FormMain.Server.Persistent[SPersSeparateURLs] := IntToStr(Byte(CSeparateURLs.Checked));
  FormMain.Server.Persistent[SPersPause] := IntToStr(Byte(CPause.Checked));
  FormMain.Server.Persistent[SPersMetaPause] := IntToStr(Byte(CMetaPause.Checked));
  if Assigned(FHandler) then
    FHandler(Self);
  Close;
end;

procedure TAddForm.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TAddForm.Show(const Caption, Filter: string; AddFile: Boolean; Handler: TOnEvent);

  procedure SetActive(Ctrls: array of TWinControl; Active: Boolean);
  var
    i: Integer;
  begin
    for i := 0 to High(Ctrls) do
      with Ctrls[i] do
      begin
        Visible := Active;
        Enabled := Active;
      end;
  end;

begin
  Self.Caption := Caption;
  BtnBrowse.TagEx := Filter;
  FHandler := Handler;
  LName.Caption := LNameCaption[AddFile];
  SetActive([EFileName, BtnBrowse], AddFile);
  SetActive([MURLs, CSeparateURLs], not AddFile);
  ShowModal;
end;

procedure TAddForm.LoadSettings(Sender: TObject; const Args: array of const);
begin
  FMaxHistory := Settings.ReadInteger(SGeneral, SMaxPathHistory, 10);
end;

end.
