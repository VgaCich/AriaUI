unit PageInfo;

interface

uses
  Windows, Messages, AvL, avlJSON, InfoPane, Aria2, UpdateThread;

type
  TPieces = array of Boolean;
  TPieceBar = class(TGraphicControl)
  private
    FPiecesCount: Integer;
    FPieces: TPieces;
    FBitmap: TBitmap;
    procedure SetPiecesCount(const Value: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure Clear;
    property PiecesCount: Integer read FPiecesCount write SetPiecesCount;
    property Pieces: TPieces read FPieces;
  end;
  TPageInfo = class(TInfoPage)
  private
    MInfo: TMemo;
    Pieces: TPieceBar;
    FBitfield: string;
    procedure Resize(Sender: TObject);
  protected
    function GetName: string; override;
    procedure SetGID(Value: TAria2GID); override;
  public
    constructor Create(Parent: TInfoPane); override;
    procedure Update(UpdateThread: TUpdateThread); override;
  end;

implementation

{ TPieceBar }

procedure TPieceBar.Clear;
begin
  PiecesCount := 0;
  Invalidate;
end;

constructor TPieceBar.Create(AOwner: TWinControl);
begin
  inherited;
  ExStyle := ExStyle or WS_EX_STATICEDGE;
  CanvasInit;
  FBitmap := TBitmap.CreateNew(1, 1);
end;

destructor TPieceBar.Destroy;
begin
  Finalize(FPieces);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TPieceBar.Paint;
var
  i: Integer;
begin
  FBitmap.Canvas.FillRect(Rect(0, 0, FPiecesCount, 1));
  for i := 0 to High(FPieces) do
    if FPieces[i] then
      FBitmap.Canvas.Pixels[i, 0] := clBlue;
  FBitmap.DrawStretch(Canvas.Handle, Rect(0, 0, ClientWidth, ClientHeight));
end;

procedure TPieceBar.SetPiecesCount(const Value: Integer);
begin
  if Value <> FPiecesCount then
  begin
    FPiecesCount := Value;
    SetLength(FPieces, Value);
    FBitmap.Width := Max(FPiecesCount, 1);
    Invalidate;
  end;
end;

{ TPageInfo }

constructor TPageInfo.Create(Parent: TInfoPane);
begin
  inherited;
  SetArray(FUpdateKeys, [sfGID, sfStatus, sfBitfield, sfInfoHash, sfPieceLength, sfNumPieces, sfErrorMessage, sfFollowedBy, sfFollowing, sfBelongsTo, sfDir, sfBittorrent]);
  Pieces := TPieceBar.Create(Self);
  Pieces.SetPosition(5, 5);
  MInfo := TMemo.Create(Self, '');
  MInfo.Style := MInfo.Style or WS_VSCROLL;
  MInfo.SetPosition(5, 25);
  OnResize := Resize;
end;

function TPageInfo.GetName: string;
begin
  Result := 'Info';
end;

procedure TPageInfo.Resize(Sender: TObject);
begin
  Pieces.SetSize(ClientWidth - 10, 15);
  MInfo.SetSize(ClientWidth - 10, ClientHeight - 30);
end;

procedure TPageInfo.SetGID(Value: TAria2GID);
begin
  inherited;
  
end;

procedure TPageInfo.Update(UpdateThread: TUpdateThread);
label SkipUpdate;
var
  i, Bits: Integer;
begin
  with UpdateThread do
  begin
    if not Assigned(Info) then Exit;
    MInfo.Text := JsonToStr(Info.Raw);
    if Info.Has[sfNumPieces] and Info.Has[sfBitfield] then
    begin
      if FBitfield = Info[sfBitfield] then goto SkipUpdate;
      Pieces.PiecesCount := Info.Int[sfNumPieces];
      FBitfield := LowerCase(Info[sfBitfield]);
      for i := 0 to Pieces.PiecesCount - 1 do
      begin
        if i mod 4 = 0 then
        begin
          Bits := Ord(FBitfield[i div 4 + 1]) - $30;
          if Bits > 9 then Dec(Bits, $27);
        end;
        Pieces.Pieces[i] := Bits and $08 <> 0;
        Bits := Bits shl 1;
      end;
      Pieces.Invalidate;
    end
    else if FBitfield <> '' then
    begin
      Pieces.Clear;
      FBitfield := '';
    end;
    SkipUpdate:
  end;
end;

end.
