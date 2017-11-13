unit PageInfo;

interface

uses
  Windows, Messages, AvL, InfoPane, Aria2, UpdateThread;

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
    LPieces: TLabel;
    Pieces: TPieceBar;
    Labels: array of TLabel;
    FBitfield: string;
    procedure LabelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Resize(Sender: TObject);
  protected
    function GetName: string; override;
    procedure SetGID(Value: TAria2GID); override;
  public
    constructor Create(Parent: TInfoPane); override;
    procedure Update(UpdateThread: TUpdateThread); override;
  end;

implementation

uses
  MainForm;

type
  TLabelFlag = (lfFullRow, lfBold, lfHighlight);
  TLabelFlags = set of TLabelFlag;
  
const
  ColCount = 3;
  AdditionalKeys = sfNumPieces + ':' + sfBitfield;
  //TODO: configure from settings?
  InfoFields: array[0..18] of record Caption: string; Flags: TLabelFlags; FType: TFieldType; Field: string; end = (
    (Caption: 'General%s'; Flags: [lfBold, lfFullRow, lfHighlight]; FType: ftNone; Field: ''),
    (Caption: 'Name: %s'; Flags: [lfFullRow]; FType: ftName; Field: ''),
    (Caption: 'Status: %s'; Flags: [lfFullRow]; FType: ftLongStatus; Field: ''),
    (Caption: 'Save to: %s'; Flags: [lfFullRow]; FType: ftPath; Field: sfDir),
    (Caption: 'Comment: %s'; Flags: [lfFullRow]; FType: ftString; Field: sfBTComment),
    (Caption: 'Total size: %s'; Flags: []; FType: ftSize; Field: sfTotalLength),
    (Caption: 'GID: %s'; Flags: []; FType: ftString; Field: sfGID),
    (Caption: 'InfoHash: %s'; Flags: []; FType: ftString; Field: sfInfoHash),
    (Caption: 'Pieces: %s'; Flags: []; FType: ftString; Field: sfNumPieces),
    (Caption: 'Piece size: %s'; Flags: []; FType: ftSize; Field: sfPieceLength),
    (Caption: 'Following: %s'; Flags: []; FType: ftString; Field: sfFollowing),
    (Caption: '%s'; Flags: []; FType: ftNone; Field: ''),
    (Caption: 'Transfer%s'; Flags: [lfBold, lfFullRow, lfHighlight]; FType: ftNone; Field: ''),
    (Caption: 'Downloaded: %s'; Flags: []; FType: ftSize; Field: sfCompletedLength),
    (Caption: 'Uploaded: %s'; Flags: []; FType: ftSize; Field: sfUploadLength),
    (Caption: 'Ratio: %s'; Flags: []; FType: ftPercent; Field: sfUploadLength + ':' + sfCompletedLength),
    (Caption: 'Download speed: %s'; Flags: []; FType: ftSpeed; Field: sfDownloadSpeed),
    (Caption: 'Upload speed: %s'; Flags: []; FType: ftSpeed; Field: sfUploadSpeed),
    (Caption: 'Seeders: %s'; Flags: []; FType: ftString; Field: sfNumSeeders));

{ TPieceBar }

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

procedure TPieceBar.Clear;
begin
  PiecesCount := 0;
  Invalidate;
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
var
  i: Integer;
begin
  inherited;
  SetArray(FUpdateKeys, BasicTransferKeys);
  AddStatusKey(FUpdateKeys, AdditionalKeys);
  LPieces := TLabel.Create(Self, 'Completed');
  LPieces.SetPosition(5, 5);
  LPieces.Font.Style := LPieces.Font.Style + [fsBold];
  LPieces.Color := clSilver;
  Pieces := TPieceBar.Create(Self);
  Pieces.SetPosition(5, 25);
  SetLength(Labels, Length(InfoFields));
  for i := 0 to High(Labels) do
  begin
    Labels[i] := TLabel.Create(Self, Format(InfoFields[i].Caption, ['']));
    if lfBold in InfoFields[i].Flags then
      Labels[i].Font.Style := Labels[i].Font.Style + [fsBold];
    if lfHighlight in InfoFields[i].Flags then
      Labels[i].Color := clSilver
    else begin
      Labels[i].Hint := 'Right click for copy';
      Labels[i].OnMouseUp := LabelMouseUp;
    end;
    AddStatusKey(FUpdateKeys, InfoFields[i].Field);
  end;
  OnResize := Resize;
end;

function TPageInfo.GetName: string;
begin
  Result := 'Info';
end;

procedure TPageInfo.LabelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    SetClipboardText((Sender as TWinControl).TagEx);
end;

procedure TPageInfo.Resize(Sender: TObject);
var
  i, Row, Column: Integer;
  Columns: array[0 .. ColCount] of Integer;
begin
  LPieces.SetSize(ClientWidth - 10, 15);
  Pieces.SetSize(ClientWidth - 10, 25);
  for i := 0 to ColCount do
    Columns[i] := 5 + Round((ClientWidth - 5) * i / ColCount);
  Row := Pieces.Top + Pieces.Height + 10;
  Column := 0;
  for i := 0 to High(Labels) do
  begin
    if (Column = ColCount) or ((lfFullRow in InfoFields[i].Flags) and (Column <> 0)) then
    begin
      Inc(Row, 15);
      Column := 0;
    end;
    if lfFullRow in InfoFields[i].Flags then
    begin
      Labels[i].SetBounds(Columns[Column], Row, ClientWidth - 10, 15);
      Column := ColCount - 1;
      if lfHighlight in InfoFields[i].Flags then
        Inc(Row, 5);
    end
    else
      Labels[i].SetBounds(Columns[Column], Row, Columns[Column + 1] - Columns[Column] - 5, 15);
    Inc(Column);
  end;
end;

procedure TPageInfo.SetGID(Value: TAria2GID);
var
  i: Integer;
begin
  if Value = FGID then Exit;
  inherited;
  Pieces.Clear;
  for i := 0 to High(Labels) do
    Labels[i].Caption := Format(InfoFields[i].Caption, ['']);
end;

procedure TPageInfo.Update(UpdateThread: TUpdateThread);
label SkipUpdate;
var
  i, Bits: Integer;
begin
  with UpdateThread do
  begin
    if not Assigned(Info) then Exit;
    if Info.Has[sfNumPieces] and Info.Has[sfBitfield] then
    begin
      if FBitfield = Info[sfBitfield] then goto SkipUpdate;
      Pieces.PiecesCount := Info.Int[sfNumPieces];
      FBitfield := LowerCase(Info[sfBitfield]);
      Bits := 0;
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
    for i := 0 to High(Labels) do
      with InfoFields[i] do
      begin
        Labels[i].TagEx := GetFieldValue(Info, Names, FType, Field);
        Labels[i].Caption := Format(Caption, [Labels[i].TagEx]);
      end;
  end;
end;

end.
