unit PageSpeed;

interface

uses
  Windows, Messages, AvL, avlUtils, avlEventBus, avlSettings, MainForm, InfoPane,
  Aria2, UpdateThread;

type
  TGraph = class(TGraphicControl)
  private
    FBitmap: TBitmap;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
  end;
  TPageSpeed = class(TInfoPage)
  private
    Graph: TGraph;
    procedure UpdateGraph(Sender: TObject; const Args: array of const);
    procedure Resize(Sender: TObject);
  protected
    function GetName: string; override;
  public
    constructor Create(Parent: TInfoPane); override;
    destructor Destroy; override;
    procedure Update(UpdateThread: TUpdateThread); override;
  end;

implementation

{ TGraph }

constructor TGraph.Create(AOwner: TWinControl);
begin
  inherited;
  //ExStyle := ExStyle or WS_EX_STATICEDGE;
  CanvasInit;
  FBitmap := TBitmap.CreateNew(1, 1);
end;

destructor TGraph.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TGraph.Paint;
begin
  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  FBitmap.DrawStretch(Canvas.Handle, Rect(0, 0, ClientWidth, ClientHeight));
end;

{ TPageInfo }

constructor TPageSpeed.Create(Parent: TInfoPane);
begin
  inherited;
  SetArray(FUpdateKeys, [sfGID]);
  Graph := TGraph.Create(Self);
  OnResize := Resize;
  EventBus.AddListener(EvUpdate, UpdateGraph);
end;

destructor TPageSpeed.Destroy;
begin
  EventBus.RemoveListener(UpdateGraph);
  inherited;
end;

function TPageSpeed.GetName: string;
begin
  Result := 'Speed';
end;

procedure TPageSpeed.Resize(Sender: TObject);
begin
  Graph.SetBounds(0, 0, ClientWidth, ClientHeight);
end;

procedure TPageSpeed.UpdateGraph(Sender: TObject; const Args: array of const);
begin

end;

procedure TPageSpeed.Update(UpdateThread: TUpdateThread);
begin

end;

end.
