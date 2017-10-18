unit InfoPane;

interface

uses
  Windows, Messages, AvL;

type
  TInfoPane = class;
  TInfoPage = class(TSimplePanel)
  protected
    //FNode: TDataNode;
    FParent: TInfoPane;
    function GetName: string; virtual; abstract;
    //procedure SetNode(Value: TDataNode); virtual;
  public
    constructor Create(Parent: TInfoPane); virtual;
    property Name: string read GetName;
    //property Node: TDataNode read FNode write SetNode;
  end;
  TInfoPane = class(TSimplePanel)
    Tabs: TTabControl;
    Pages: array of TInfoPage;
  private
    //FNode: TDataNode;
    FCurPage: TInfoPage;
    procedure Resize(Sender: TObject);
    //procedure SetNode(const Value: TDataNode);
    procedure TabChange(Sender: TObject);
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
  public
    constructor Create(AParent: TWinControl);
    //property Node: TDataNode read FNode write SetNode;
  end;

implementation

//uses
//  TextEditor, ImagesEditor, StreamsEditor;

var
  DummyNum: Integer = 0;

const
  DummyPages: array[0..1] of string = ('Info', 'Files');

type
  TDummyPage = class(TInfoPage)
  private
    FName: string;
    function GetName: string; override;
  public
    constructor Create(Parent: TInfoPane); override;
  end;

constructor TDummyPage.Create(Parent: TInfoPane);
begin
  FName := DummyPages[DummyNum];
  Inc(DummyNum);
end;

function TDummyPage.GetName: string;
begin
  Result := FName;
end;

type
  TInfoPageClass = class of TInfoPage;

const
  InfoPages: array[0..1] of TInfoPageClass = (TDummyPage, TDummyPage);

{ TInfoPane }

constructor TInfoPane.Create(AParent: TWinControl);
var
  Page: Integer;
begin
  inherited Create(AParent, '');
  Border := 2;
  ExStyle := 0;
  Tabs := TTabControl.Create(Self);
  Tabs.Style := tsTabs;
  Tabs.SetPosition(0, 0);
  SetLength(Pages, Length(InfoPages));
  for Page := Low(InfoPages) to High(InfoPages) do
  begin
    Pages[Page] := InfoPages[Page].Create(Self);
    Pages[Page].BringToFront;
    Pages[Page].Visible := false;
    Tabs.TabAdd(Pages[Page].Name);
  end;
  FCurPage := Pages[0];
  FCurPage.Visible := true;
  Tabs.TabIndex := 0;
  OnResize := Resize;
end;

procedure TInfoPane.Resize(Sender: TObject);
var
  Rect: TRect;
  Page: Integer;
begin
  Tabs.SetSize(ClientWidth, ClientHeight);
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := Tabs.ClientWidth;
  Rect.Bottom := Tabs.ClientHeight;
  Tabs.Perform(TCM_ADJUSTRECT, 0, Integer(@Rect));
  for Page := 0 to High(Pages) do
    Pages[Page].SetBounds(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
end;

{procedure TInfoPane.SetNode(const Value: TDataNode);
begin
  FNode := Value;
  FCurPage.Node := FNode;
end;}

procedure TInfoPane.TabChange(Sender: TObject);
begin
  FCurPage.Visible := false;
  FCurPage := Pages[Tabs.TabIndex];
  //FCurPage.Node := FNode;
  FCurPage.Visible := true;
end;

procedure TInfoPane.WMNotify(var Msg: TWMNotify);
begin
  if Assigned(Tabs) and (Msg.NMHdr.hwndFrom = Tabs.Handle) and (Msg.NMHdr.code = TCN_SELCHANGE) then
    TabChange(Tabs);
end;

{ TInfoPage }

constructor TInfoPage.Create(Parent: TInfoPane);
begin
  inherited Create(Parent, '');
  FParent := Parent;
  Border := 2;
  ExStyle := 0;
end;

{procedure TInfoPage.SetNode(Value: TDataNode);
begin
  FNode := Value;
end;}

end.
