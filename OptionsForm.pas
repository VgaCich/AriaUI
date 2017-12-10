unit OptionsForm;

interface

uses
  Windows, Messages, AvL, avlUtils, avlSettings, Aria2, OptionsList;

type
  TOptionsPage = class(TSimplePanel)
  protected
    function GetName: string; virtual; abstract;
    function Settings: TSettings;
  public
    constructor Create(Parent: TWinControl); virtual;
    procedure Load; virtual; abstract;
    procedure Apply; virtual; abstract;
    property Name: string read GetName;
  end;
  TOptionsForm = class(TForm)
    Tabs: TTabControl;
    BtnOK, BtnCancel: TButton;
    Pages: array of TOptionsPage;
  private
    FCurPage: TOptionsPage;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Resize(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    procedure Show;
  end;
  TPageGeneral = class(TOptionsPage)
  protected
    function GetName: string; override;
  public
    constructor Create(Parent: TWinControl); override;
    procedure Load; override;
    procedure Apply; override;
  end;
  TPageServers = class(TOptionsPage)
  protected
    function GetName: string; override;
  public
    constructor Create(Parent: TWinControl); override;
    procedure Load; override;
    procedure Apply; override;
  end;
  TPageInterface = class(TOptionsPage)
  protected
    function GetName: string; override;
  public
    constructor Create(Parent: TWinControl); override;
    procedure Load; override;
    procedure Apply; override;
  end;

var
  FormOptions: TOptionsForm;

implementation

uses
  MainForm;

type
  TOptionsPageClass = class of TOptionsPage;

var
  OptionsPages: array[0..2] of TOptionsPageClass = (TPageGeneral, TPageServers, TPageInterface);

{ TOptionsForm }

constructor TOptionsForm.Create(AParent: TWinControl);
var
  Page: Integer;
begin
  inherited Create(AParent, 'Options');
  BorderStyle := bsDialog; //TODO: resizing?
  SetSize(400 + Width - ClientWidth, 500 + Height - ClientHeight);
  Position := poScreenCenter;
  OnResize := Resize;
  OnKeyUp := FormKeyUp;
  Tabs := TTabControl.Create(Self);
  Tabs.Style := tsTabs;
  Tabs.SetBounds(0, 0, ClientWidth, ClientHeight - 35);
  SetLength(Pages, Length(OptionsPages));
  for Page := Low(OptionsPages) to High(OptionsPages) do
  begin
    Pages[Page] := OptionsPages[Page].Create(Self);
    Pages[Page].BringToFront;
    Pages[Page].Visible := false;
    Tabs.TabAdd(Pages[Page].Name);
  end;
  FCurPage := Pages[0];
  FCurPage.Visible := true;
  Tabs.TabIndex := 0;
  BtnOK := TButton.Create(Self, 'OK');
  BtnOK.SetBounds(ClientWidth - 160, ClientHeight - 30, 75, 25);
  BtnOK.OnClick := OKClick;
  BtnCancel := TButton.Create(Self, 'Cancel');
  BtnCancel.SetBounds(ClientWidth - 80, ClientHeight - 30, 75, 25);
  BtnCancel.OnClick := CancelClick;
end;

procedure TOptionsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then OKClick(BtnOK);
  if Key = VK_ESCAPE then CancelClick(BtnCancel);
end;

procedure TOptionsForm.OKClick(Sender: TObject);
var
  Page: Integer;
begin
  for Page := 0 to High(Pages) do
    Pages[Page].Apply;
  FormMain.LoadSettings;
  Close;
end;

procedure TOptionsForm.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TOptionsForm.Show;
var
  Page: Integer;
begin
  for Page := 0 to High(Pages) do
    Pages[Page].Load;
  ShowModal;
end;

destructor TOptionsForm.Destroy;
begin
  Finalize(Pages);
  inherited;
end;

procedure TOptionsForm.Resize(Sender: TObject);
var
  Rect: TRect;
  Page: Integer;
begin
  Tabs.SetSize(ClientWidth, ClientHeight - 35);
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := Tabs.ClientWidth;
  Rect.Bottom := Tabs.ClientHeight;
  Tabs.Perform(TCM_ADJUSTRECT, 0, Integer(@Rect));
  for Page := 0 to High(Pages) do
    Pages[Page].SetBounds(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
  BtnOK.SetPosition(ClientWidth - 160, ClientHeight - 30);
  BtnCancel.SetPosition(ClientWidth - 80, ClientHeight - 30);
end;

procedure TOptionsForm.WMNotify(var Msg: TWMNotify);
begin
  if Assigned(Tabs) and (Msg.NMHdr.hwndFrom = Tabs.Handle) and (Msg.NMHdr.code = TCN_SELCHANGE) then
  begin
    FCurPage.Visible := false;
    FCurPage := Pages[Tabs.TabIndex];
    FCurPage.Visible := true;
  end;
end;

{ TOptionsPage }

constructor TOptionsPage.Create(Parent: TWinControl);
begin
  inherited Create(Parent, '');
  Border := 2;
  ExStyle := 0;
end;

function TOptionsPage.Settings: TSettings;
begin
  Result := FormMain.Settings;
end;

{ TPageGeneral }

constructor TPageGeneral.Create(Parent: TWinControl);
begin
  inherited;
  Caption := Name;
end;

function TPageGeneral.GetName: string;
begin
  Result := 'General';
end;

procedure TPageGeneral.Apply;
begin
  inherited;

end;

procedure TPageGeneral.Load;
begin
  inherited;

end;

{ TPageServers }

constructor TPageServers.Create(Parent: TWinControl);
begin
  inherited;
  Caption := Name;
end;

function TPageServers.GetName: string;
begin
  Result := 'Servers'; 
end;

procedure TPageServers.Apply;
begin
  inherited;

end;

procedure TPageServers.Load;
begin
  inherited;

end;

{ TPageInterface }

constructor TPageInterface.Create(Parent: TWinControl);
begin
  inherited;
  Caption := Name;
end;

function TPageInterface.GetName: string;
begin
  Result := 'Interface';
end;

procedure TPageInterface.Apply;
begin
  inherited;

end;

procedure TPageInterface.Load;
begin
  inherited;

end;

end.
