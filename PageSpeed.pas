unit PageSpeed;

//TODO: Customization (GridQuant, Colors, etc)

interface

uses
  Windows, Messages, AvL, avlUtils, avlEventBus, avlSettings, MainForm, InfoPane,
  Aria2, UpdateThread;

const
  MaxGraphs = 8;

type
  TGraphPoint = record
    Time: Cardinal;
    Values: array[0 .. MaxGraphs - 1] of Integer;
  end;
  TGraphPoints = array of TGraphPoint;
  TGraphParam = record
    Name: string;
    Color: TColor;
    Style: TPenStyle;
  end;
  TGraphParams = array[0 .. MaxGraphs - 1] of TGraphParam;
  TOnGridLabel = function(Sender: TObject; Value: Integer): string of object;
  TGraph = class(TGraphicControl)
  private
    FBitmap: TBitmap;
    FCount: Integer;
    FPoints: TGraphPoints;
    FGridSpan, FTimeSpan: Cardinal;
    FGridQuant: Integer;
    FOnGridLabel: TOnGridLabel;
    procedure CopyPoints(const Src: TGraphPoints; var Dst: TGraphPoints);
    function PurgeOldPoints: Cardinal;
    procedure Resize(Sender: TObject);
    procedure SetTimeSpan(const Value: Cardinal);
    function GetBackup: TGraphPoints;
    procedure Restore(Backup: TGraphPoints);
  protected
    procedure Paint; override;
  public
    Params: TGraphParams;
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure AddPoint(const Vals: array of Integer);
    procedure Clear;
    procedure Update;
    property Backup: TGraphPoints read GetBackup write Restore;
    property Count: Integer read FCount write FCount;
    property TimeSpan: Cardinal read FTimeSpan write SetTimeSpan;
    property GridSpan: Cardinal read FGridSpan write FGridSpan;
    property GridQuant: Integer read FGridQuant write FGridQuant;
    property OnGridLabel: TOnGridLabel read FOnGridLabel write FOnGridLabel;
  end;
  TGraphData = class
  public
    Backup: TGraphPoints;
    constructor Create(Data: TGraphPoints);
    destructor Destroy; override;
  end;
  TPageSpeed = class(TInfoPage)
  private
    Graph: TGraph;
    FTimer: TTimer;
    procedure ServerChanged(Sender: TObject; const Args: array of const);
    procedure UpdateGraph(Sender: TObject; const Args: array of const);
    function GridLabel(Sender: TObject; Value: Integer): string;
    procedure Resize(Sender: TObject);
    procedure TimerTick(Sender: TObject);
  protected
    function GetName: string; override;
  public
    constructor Create(Parent: TInfoPane); override;
    destructor Destroy; override;
    procedure Update(UpdateThread: TUpdateThread); override;
  end;

implementation

uses
  Utils;

const
  SGraphData = 'Speed.GraphData';

{ TGraph }

constructor TGraph.Create(AOwner: TWinControl);
begin
  inherited;
  ExStyle := ExStyle or WS_EX_STATICEDGE;
  CanvasInit;
  FBitmap := TBitmap.CreateNew(1, 1);
  FBitmap.Canvas.Font := TFont.Create(FBitmap.Canvas);
  OnResize := Resize;
end;

destructor TGraph.Destroy;
begin
  Finalize(FPoints);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TGraph.AddPoint(const Vals: array of Integer);
var
  i: Integer;
begin
  SetLength(FPoints, Length(FPoints) + 1);
  with FPoints[High(FPoints)] do
  begin
    Time := GetTickCount;
    for i := 0 to Min(Length(Vals), MaxGraphs) - 1 do
      Values[i] := Vals[i];
  end;
  Update;
end;

procedure TGraph.Clear;
begin
  SetLength(FPoints, 0);
  Update;
end;

procedure TGraph.CopyPoints(const Src: TGraphPoints; var Dst: TGraphPoints);
var
  i: Integer;
begin
  SetLength(Dst, Length(Src) + 1);
  for i := 0 to High(Src) do
    Dst[i] := Src[i];
  with Dst[High(Dst)] do
  begin
    Time := GetTickCount;
    for i := 0 to MaxGraphs - 1 do
      Values[i] := -1;
  end;
end;

procedure TGraph.Paint;
var
  CurTime, GridPos: Cardinal;
  i, j, MaxVal, GridLines, MaxWidth, X, Y: Integer;

  function GetX(Time: Cardinal): Integer;
  begin
    Result := Round((1 - (CurTime - Time) / FTimeSpan) * (FBitmap.Width - 1));
  end;

  function GetY(Value: Integer): Integer;
  begin
    Result := Round((1 - Value / MaxVal) * (FBitmap.Height - 1));
  end;

  function GetLabel(Value: Integer): string;
  begin
    if Assigned(FOnGridLabel) then
      Result := FOnGridLabel(Self, Value)
    else
      Result := IntToStr(Value);
  end;

begin
  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
  SetBkMode(FBitmap.Canvas.Handle, TRANSPARENT);
  try
    CurTime := PurgeOldPoints;
    FBitmap.Canvas.Pen.Color := clSilver;
    FBitmap.Canvas.Pen.Style := psDash;
    GridPos := 0;
    while GridPos < FTimeSpan do
    begin
      FBitmap.Canvas.MoveTo(GetX(CurTime - GridPos), 0);
      FBitmap.Canvas.LineTo(GetX(CurTime - GridPos), FBitmap.Height);
      Inc(GridPos, FGridSpan);
    end;
    if Length(FPoints) = 0 then Exit;
    MaxVal := 1;
    for i := 0 to High(FPoints) do
      for j := 0 to FCount - 1 do
        MaxVal := Max(MaxVal, FPoints[i].Values[j]);
    MaxVal := ((MaxVal + FGridQuant - 1) div FGridQuant) * FGridQuant;
    GridLines := FBitmap.Height div 50;
    for i := 0 to GridLines do
    begin
      Y := FBitmap.Height - Round((i / GridLines) * (FBitmap.Height - 1));
      FBitmap.Canvas.MoveTo(0, Y);
      FBitmap.Canvas.LineTo(FBitmap.Width, Y);
      if i > 0 then
        FBitmap.Canvas.TextOut(5, Y + 2, GetLabel(Round((i / GridLines) * MaxVal)));
    end;
    FBitmap.Canvas.Pen.Color := clBlack;
    FBitmap.Canvas.Pen.Style := psSolid;
    FBitmap.Canvas.MoveTo(0, FBitmap.Height - 1);
    FBitmap.Canvas.LineTo(FBitmap.Width, FBitmap.Height - 1);
    MaxWidth := 0;
    for i := 0 to FCount - 1 do
      MaxWidth := Max(MaxWidth, FBitmap.Canvas.TextWidth(Params[i].Name));
    X := FBitmap.Width - MaxWidth - 5;
    for i := 0 to FCount - 1 do
    begin
      Y := FBitmap.Height - 20 * FCount + 20 * i - 5;
      FBitmap.Canvas.Pen.Color := Params[i].Color;
      FBitmap.Canvas.Pen.Style := Params[i].Style;
      FBitmap.Canvas.MoveTo(X - 25, Y + 6);
      FBitmap.Canvas.LineTo(X - 5, Y + 6);
      FBitmap.Canvas.TextOut(X, Y, Params[i].Name);
    end;
    for i := 0 to FCount - 1 do
    begin
      FBitmap.Canvas.Pen.Color := Params[i].Color;
      FBitmap.Canvas.Pen.Style := Params[i].Style;
      FBitmap.Canvas.MoveTo(GetX(FPoints[0].Time), GetY(FPoints[0].Values[i]));
      for j := 1 to High(FPoints) do
        if (FPoints[j].Values[i] = -1) or ((j > 0) and (FPoints[j - 1].Values[i] = -1)) then
          FBitmap.Canvas.MoveTo(GetX(FPoints[j].Time), GetY(FPoints[j].Values[i]))
        else
          FBitmap.Canvas.LineTo(GetX(FPoints[j].Time), GetY(FPoints[j].Values[i]));
      FBitmap.Canvas.Pen.Color := clGray;
      FBitmap.Canvas.LineTo(GetX(CurTime), GetY(FPoints[High(FPoints)].Values[i]));
    end;
  finally
    FBitmap.Draw(Canvas.Handle, 0, 0);
  end;
end;

function TGraph.PurgeOldPoints: Cardinal;
var
  i, Purge: Integer;
  T: Single;
begin
  Purge := 0;
  Result := GetTickCount;
  while Purge < Length(FPoints) do
    if Result - FPoints[Purge].Time < FTimeSpan then
      Break
    else
      Inc(Purge);
  if Purge = 0 then
    Exit
  else if Purge = Length(FPoints) then
    Finalize(FPoints)
  else begin
    Dec(Purge);
    for i := 0 to High(FPoints) - Purge do
      FPoints[i] := FPoints[i + Purge];
    T := (Result - FTimeSpan - FPoints[0].Time) / (FPoints[1].Time - FPoints[0].Time);
    FPoints[0].Time := Result - FTimeSpan;
    for i := 0 to FCount - 1 do
      FPoints[0].Values[i] := Round((FPoints[1].Values[i] - FPoints[0].Values[i]) * T) + FPoints[0].Values[i];
  end;
  SetLength(FPoints, Length(FPoints) - Purge);
end;

procedure TGraph.Resize(Sender: TObject);
begin
  FBitmap.Width := ClientWidth;
  FBitmap.Height := ClientHeight;
end;

procedure TGraph.SetTimeSpan(const Value: Cardinal);
begin
  FTimeSpan := Value;
  PurgeOldPoints;
end;

procedure TGraph.Update;
begin
  Invalidate;
  UpdateWindow(Handle);
end;

function TGraph.GetBackup: TGraphPoints;
begin
  CopyPoints(FPoints, Result);
end;

procedure TGraph.Restore(Backup: TGraphPoints);
begin
  CopyPoints(Backup, FPoints);
  Update;
end;

{ TPageSpeed }

constructor TPageSpeed.Create(Parent: TInfoPane);
begin
  inherited;
  SetArray(FUpdateKeys, [sfGID]);
  Graph := TGraph.Create(Self);
  Graph.Count := 2;
  Graph.TimeSpan := 10 * 60 * 1000;
  Graph.GridSpan := 60 * 1000;
  Graph.GridQuant := 1048576;
  Graph.OnGridLabel := GridLabel;
  Graph.Params[0].Name := 'DL Speed';
  Graph.Params[0].Color := clBlue;
  Graph.Params[0].Style := psSolid;
  Graph.Params[1].Name := 'UL Speed';
  Graph.Params[1].Color := clGreen;
  Graph.Params[1].Style := psSolid;
  FTimer := TTimer.CreateEx(1000, true);
  FTimer.OnTimer := TimerTick;
  OnResize := Resize;
  EventBus.AddListener(EvServerChanged, ServerChanged);
  EventBus.AddListener(EvUpdate, UpdateGraph);
end;

destructor TPageSpeed.Destroy;
begin
  FreeAndNil(FTimer);
  EventBus.RemoveListeners([UpdateGraph, ServerChanged]);
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

procedure TPageSpeed.ServerChanged(Sender: TObject; const Args: array of const);
var
  Data: TGraphData;
begin
  (Args[0].VObject as TPerServerStorage)[SGraphData] := TGraphData.Create(Graph.Backup);
  Data := (Args[1].VObject as TPerServerStorage)[SGraphData] as TGraphData;
  if Assigned(Data) then
    Graph.Backup := Data.Backup
  else
    Graph.Clear;
  (Args[1].VObject as TPerServerStorage)[SGraphData] := nil;
end;

procedure TPageSpeed.UpdateGraph(Sender: TObject; const Args: array of const);
begin
  with Args[0].VObject as TUpdateThread do
    if Assigned(Stats) then
      Graph.AddPoint([Stats.Int[sfDownloadSpeed], Stats.Int[sfUploadSpeed]])
    else
      Graph.AddPoint([-1, -1]);
end;

procedure TPageSpeed.Update(UpdateThread: TUpdateThread);
begin

end;

function TPageSpeed.GridLabel(Sender: TObject; Value: Integer): string;
begin
  Result := SizeToStr(Value) + '/s';
end;

procedure TPageSpeed.TimerTick(Sender: TObject);
begin
  if Visible then
    Graph.Update;
end;

{ TGraphData }

constructor TGraphData.Create(Data: TGraphPoints);
begin
  inherited Create;
  Backup := Data;
end;

destructor TGraphData.Destroy;
begin
  Finalize(Backup);
  inherited;
end;

end.
