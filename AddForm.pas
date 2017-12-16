unit AddForm;

interface

uses
  Windows, Messages, AvL, avlUtils, Base64, Aria2;

type
  TAddForm = class(TForm)
    LName, LOptions: TLabel;
    FileName: TEdit;
    MURLs, MOptions: TMemo;
    BtnOK, BtnCancel, BtnBrowse: TButton;
    Options: array of TAria2Option;
    URLs: array of string;
  private
    FHandler: TOnEvent;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BrowseClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
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
  SPersOptions = 'AddForm.Options';
  LNameCaption: array[Boolean] of string = ('Enter URLs (one per line):', 'Enter file name:');

{ TAddForm }

constructor TAddForm.Create(AParent: TWinControl);
begin
  inherited Create(AParent, '');
  BorderStyle := bsDialog;
  SetSize(400 + Width - ClientWidth, 300 + Height - ClientHeight);
  Position := poScreenCenter;
  OnKeyUp := FormKeyUp;
  OnShow := FormShow;
  LName := TLabel.Create(Self, '');
  LName.SetBounds(5, 5, ClientWidth - 10, 15);
  LOptions := TLabel.Create(Self, 'Options (one per line, key=value):');
  LOptions.SetBounds(5, 155, ClientWidth - 10, 15);
  FileName := TEdit.Create(Self, '');
  FileName.SetBounds(5, 25, ClientWidth - 90, 24);
  MURLs := TMemo.Create(Self, '');
  MURLs.SetBounds(5, 25, ClientWidth - 10, 125);
  MOptions := TMemo.Create(Self, '');
  MOptions.SetBounds(5, 175, ClientWidth - 10, 90);
  BtnBrowse := TButton.Create(Self, 'Browse');
  BtnBrowse.SetBounds(ClientWidth - 80, 25, 75, 24);
  BtnBrowse.OnClick := BrowseClick;
  BtnOK := TButton.Create(Self, 'OK');
  BtnOK.SetBounds(ClientWidth - 160, ClientHeight - 30, 75, 25);
  BtnOK.OnClick := OKClick;
  BtnCancel := TButton.Create(Self, 'Cancel');
  BtnCancel.SetBounds(ClientWidth - 80, ClientHeight - 30, 75, 25);
  BtnCancel.OnClick := CancelClick;
end;

destructor TAddForm.Destroy;
begin
  Finalize(Options);
  Finalize(URLs);
  inherited;
end;

procedure TAddForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then OKClick(BtnOK);
  if Key = VK_ESCAPE then CancelClick(BtnCancel);
end;

procedure TAddForm.FormShow(Sender: TObject);
begin
  if FileName.Enabled then
  begin
    FileName.SetFocus;
    FileName.SelectAll;
  end;
  if MURLs.Enabled then
  begin
    MURLs.SetFocus;
    MURLs.Perform(EM_SETSEL, 0, -1);
  end;
  MOptions.Text := Base64Decode(FormMain.CurServerStorage.Persistent[SPersOptions]);
end;

procedure TAddForm.BrowseClick(Sender: TObject);
var
  FN: string;
begin
  FN := FileName.Text;
  if not OpenSaveDialog(Handle, true, '', '', BtnBrowse.TagEx, '', 0, OFN_FILEMUSTEXIST, FN) then Exit;
  FileName.Text := FN;
end;

procedure TAddForm.OKClick(Sender: TObject);
var
  i: Integer;
begin
  if MURLs.Visible then
  begin
    SetLength(URLs, 0);
    for i := 0 to MURLs.LineCount - 1 do
      if MURLs.LineStrings[i] <> '' then
      begin
        SetLength(URLs, Length(URLs) + 1);
        URLs[High(URLs)] := MURLs.LineStrings[i];
      end;
  end;
  SetLength(Options, 0);
  for i := 0 to MOptions.LineCount - 1 do
    if MOptions.LineStrings[i] <> '' then
    begin
      SetLength(Options, Length(Options) + 1);
      with Options[High(Options)] do
      begin
        Key := Trim(First(MOptions.LineStrings[i], '='));
        Value := Trim(Second(MOptions.LineStrings[i], '='));
      end;
    end;
  FormMain.CurServerStorage.Persistent[SPersOptions] := Base64Encode(MOptions.Text);
  if Assigned(FHandler) then
    FHandler(Self);
  Close;
end;

procedure TAddForm.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TAddForm.Show(const Caption, Filter: string; AddFile: Boolean; Handler: TOnEvent);
begin
  Self.Caption := Caption;
  BtnBrowse.TagEx := Filter;
  FHandler := Handler;
  LName.Caption := LNameCaption[AddFile];
  FileName.Visible := AddFile;
  FileName.Enabled := AddFile;
  BtnBrowse.Visible := AddFile;
  BtnBrowse.Enabled := AddFile;
  MURLs.Visible := not AddFile;
  MURLs.Enabled := not AddFile;
  ShowModal;
end;

end.
