unit BTUtils;

interface

uses
  Windows, AvL, avlUtils;

type
  TBEElement = class
  protected
    function WriteStr(Stream: TStream; const Str: string): Integer;
  public
    procedure Write(Stream: TStream); virtual; abstract;
  end;
  TBEString = class(TBEElement)
  private
    FValue: string;
  public
    constructor Create(const Value: string = '');
    procedure Write(Stream: TStream); override;
    property Value: string read FValue write FValue;
  end;
  TBEInt = class(TBEElement)
  private
    FValue: Int64;
  public
    constructor Create(Value: Int64 = 0);
    procedure Write(Stream: TStream); override;
    property Value: Int64 read FValue write FValue;
  end;
  TBEList = class(TBEElement)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TBEElement;
    procedure SetItems(Index: Integer; Value: TBEElement);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(Stream: TStream); override;
    procedure Add(Item: TBEElement);
    procedure Delete(Idx: Integer);
    procedure Insert(At: Integer; Item: TBEElement);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TBEElement read GetItems write SetItems; default;
  end;
  TBEMap = class(TBEElement)
  private
    FMap: TStringList;
    function GetCount: Integer;
    function GetItems(Index: string): TBEElement;
    function GetNames(Index: Integer): string;
    procedure SetItems(Index: string; Value: TBEElement);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(Stream: TStream); override;
    procedure Delete(const Name: string);
    property Count: Integer read GetCount;
    property Items[Index: string]: TBEElement read GetItems write SetItems; default;
    property Names[Index: Integer]: string read GetNames;
  end;

function BEString(Elem: TBEElement): string;
function BEInt(Elem: TBEElement): Int64;
function BEItem(Elem: TBEElement; Path: string): TBEElement;
function BELoadElement(Stream: TStream): TBEElement;
function HexString(const S: string): string;
function SHA1(const Data; Size: Integer): string;
function TorrentInfoHash(Torrent: TBEElement): string;

implementation

function CompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: string;
  i: Integer;
begin
  S1 := List[Index1];
  S2 := List[Index2];
  for i := 1 to Min(Length(S1), Length(S2)) do
    if S1[i] <> S2[i] then
    begin
      Result := Integer(Ord(S1[i]) - Ord(S2[i]));
      Exit;
    end;
  Result := Length(S1) - Length(S2);
end;

{TBEElement}

function TBEElement.WriteStr(Stream: TStream; const Str: string): Integer;
begin
  Result := Stream.Write(Str[1], Length(Str));
end;

{TBEString}

constructor TBEString.Create(const Value: string = '');
begin
  inherited Create;
  FValue := Value;
end;

procedure TBEString.Write(Stream: TStream);
begin
  WriteStr(Stream, IntToStr(Length(FValue)) + ':' + FValue);
end;

{TBEInt}

constructor TBEInt.Create(Value: Int64 = 0);
begin
  inherited Create;
  FValue := Value;
end;

procedure TBEInt.Write(Stream: TStream);
begin
  WriteStr(Stream, 'i' + Int64ToStr(FValue) + 'e');
end;

{TBEList}

constructor TBEList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TBEList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count do
    TBEElement(FList[i]).Free;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TBEList.Write(Stream: TStream);
var
  i: Integer;
begin
  WriteStr(Stream, 'l');
  for i := 0 to FList.Count - 1 do
    TBEElement(FList[i]).Write(Stream);
  WriteStr(Stream, 'e');
end;

procedure TBEList.Add(Item: TBEElement);
begin
  FList.Add(Item);
end;

procedure TBEList.Delete(Idx: Integer);
begin
  FList.Delete(Idx);
end;

function TBEList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBEList.GetItems(Index: Integer): TBEElement;
begin
  Result := TBEElement(FList[Index]);
end;

procedure TBEList.Insert(At: Integer; Item: TBEElement);
begin
  FList.Insert(at, Item);
end;

procedure TBEList.SetItems(Index: Integer; Value: TBEElement);
begin
  FList[Index] := Value;
end;

{TBEMap}

constructor TBEMap.Create;
begin
  inherited;
  FMap := TStringList.Create;
end;

destructor TBEMap.Destroy;
var
  i: Integer;
begin
  for i := 0 to FMap.Count - 1 do
    FMap.Objects[i].Free;
  FreeAndNil(FMap);
  inherited Destroy;
end;

procedure TBEMap.Delete(const Name: string);
var
  i: Integer;
begin
  i := FMap.IndexOf(Name);
  if i >= 0 then
  begin
    FMap.Objects[i].Free;
    FMap.Delete(i);
  end;
end;

function TBEMap.GetCount: Integer;
begin
  Result := FMap.Count;
end;

function TBEMap.GetItems(Index: string): TBEElement;
var
  i: Integer;
begin
  i := FMap.IndexOf(Index);
  if i < 0 then
    Result := nil
  else
    Result := TBEElement(FMap.Objects[i]);
end;

function TBEMap.GetNames(Index: Integer): string;
begin
  Result := FMap[Index];
end;

procedure TBEMap.SetItems(Index: string; Value: TBEElement);
var
  i: Integer;
begin
  i := FMap.IndexOf(Index);
  if i < 0 then
    FMap.AddObject(Index, Value)
  else
    FMap.Objects[i] := Value;
end;

procedure TBEMap.Write(Stream: TStream);
var
  i: Integer;
  Key: TBEString;
begin
  FMap.CustomSort(CompareStrings);
  Key := TBEString.Create;
  try
    WriteStr(Stream, 'd');
    for i := 0 to FMap.Count - 1 do
    begin
      Key.Value := FMap[i];
      Key.Write(Stream);
      TBEElement(FMap.Objects[i]).Write(Stream);
    end;
  finally
    WriteStr(Stream, 'e');
    Key.Free;
  end;
end;

{Functions}

function BEString(Elem: TBEElement): string;
begin
  Result := '';
  if not Assigned(Elem) then Exit;
  if Elem is TBEString then
    Result := UTF8Decode(TBEString(Elem).Value)
  else if Elem is TBEInt then
    Result := Int64ToStr(TBEInt(Elem).Value);
end;

function BEInt(Elem: TBEElement): Int64;
begin
  if Assigned(Elem) and (Elem is TBEInt) then
    Result := TBEInt(Elem).Value
  else
    Result := 0;
end;

function BEItem(Elem: TBEElement; Path: string): TBEElement;
begin
  Result := Elem;
  while Assigned(Result) and (Path <> '') do
    if Result is TBEMap then
      Result := TBEMap(Result)[Tok('.', Path)]
    else if Result is TBEList then
      Result := TBEList(Result)[StrToInt(Tok('.', Path))]
    else Result := nil;
end;

function BELoadElement(Stream: TStream): TBEElement;

  function ReadUntil(Mark: Char): string;
  var
    C: Char;
  begin
    Result := '';
    Stream.Read(C, 1);
    while C <> Mark do
    begin
      Result := Result + C;
      Stream.Read(C, 1);
    end;
  end;

var
  C: Char;
  S: string;
  Len: Integer;
  Element: TBEElement;
  Key: TBEString;
begin
  Result := nil;
  Stream.Read(C, 1);
  case C of
    '1'..'9':
      begin
        Len := StrToInt(C + ReadUntil(':'));
        if Len > Stream.Size - Stream.Position then Exit;
        SetLength(S, Len);
        Stream.Read(S[1], Len);
        Result := TBEString.Create(S);
      end;
    'i': Result := TBEInt.Create(StrToInt64(ReadUntil('e')));
    'l':
      begin
        Result := TBEList.Create;
        Element := BELoadElement(Stream);
        while Assigned(Element) do
        begin
          (Result as TBEList).Add(Element);
          Element := BELoadElement(Stream);
        end;
      end;
    'd':
      begin
        Result := TBEMap.Create;
        while true do
        begin
          TBEElement(Key) := BELoadElement(Stream);
          if not (Assigned(Key) and (Key is TBEString)) then Break;
          Element := BELoadElement(Stream);
          if not Assigned(Element) then Break;
          (Result as TBEMap).Items[Key.Value] := Element;
        end;
      end;
    'e': ;
  else
    Stream.Seek(-1, soFromCurrent);
  end;
end;

function HexString(const S: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + IntToHex(Ord(S[i]), 2);
end;

function SHA1(const Data; Size: Integer): string;
const
  hlen = 20;
  iv: packed array[0..4] of Cardinal = ($67452301, $EFCDAB89, $98BADCFE, $10325476, $C3D2E1F0);
var
  Tail: packed array[0..127] of Byte;
  i, TailSize: Integer;
  TempSize: Int64;
  P: Pointer;
  f, k, t: Cardinal;
  h, s: packed array[0..4] of Cardinal;
  w: packed array[0..79] of Cardinal;

  procedure AddTail(B: Byte);
  begin
    Tail[TailSize] := B;
    Inc(TailSize);
  end;

  function GetBlock: Boolean;
  begin
    Result := Size + TailSize > 0;
    if not Result then Exit;
    if Size = 0 then
    begin
      P := @Tail;
      Size := TailSize;
      TailSize := 0;
    end;
    Move(P^, w, 64);
    Inc(Cardinal(P), 64);
    Dec(Size, 64);
  end;

  function Swap(D: Cardinal): Cardinal;
  asm
    BSWAP EAX
  end;

  function ROL(D: Cardinal; S: Integer): Cardinal;
  asm
    PUSH ECX
    MOV ECX, EDX
    ROL EAX, CL
    POP ECX
  end;

begin
  TailSize := Size mod 64;
  TempSize := 8 * Size;
  Size := Size - TailSize;
  P := @Data;
  Move(Pointer(Integer(P) + Size)^, Tail, TailSize);
  AddTail($80);
  while TailSize mod 64 <> 56 do
    AddTail(0);
  for i := 7 downto 0 do
    AddTail((TempSize shr (8 * i)) and $FF);
  Move(iv, h, hlen);
  while GetBlock do
  begin
    for i := 0 to 15 do
      w[i] := Swap(w[i]);
    for i := 16 to 79 do
      w[i] := ROL(w[i - 3] xor w[i - 8] xor w[i - 14] xor w[i - 16], 1);
    Move(h, s, hlen);
    for i := 0 to 79 do
    begin
      if i < 20 then
      begin
        f := (s[1] and s[2]) or ((not s[1]) and s[3]);
        k := $5A827999;
      end
      else if i < 40 then
      begin
        f := s[1] xor s[2] xor s[3];
        k := $6ED9EBA1;
      end
      else if i < 60 then
      begin
        f := (s[1] and s[2]) or (s[1] and s[3]) or (s[2] and s[3]);
        k := $8F1BBCDC;
      end
      else begin
        f := s[1] xor s[2] xor s[3];
        k := $CA62C1D6;
      end;
      t := ROL(s[0], 5) + f + s[4] + k + w[i];
      s[4] := s[3];
      s[3] := s[2];
      s[2] := ROL(s[1], 30);
      s[1] := s[0];
      s[0] := t;
    end;
    for i := 0 to 4 do
      h[i] := h[i] + s[i];
  end;
  for i := 0 to 4 do
    h[i] := Swap(h[i]);
  SetLength(Result, hlen);
  Move(h, Result[1], hlen);
end;

function TorrentInfoHash(Torrent: TBEElement): string;
var
  Temp: TMemoryStream;
  Info: TBEElement;
begin
  Result := '';
  if not (Assigned(Torrent) and (Torrent is TBEMap)) then Exit;
  Info := (Torrent as TBEMap)['info'];
  if not Assigned(Info) then Exit;
  Temp := TMemoryStream.Create;
  try
    Info.Write(Temp);
    Result := SHA1(Temp.Memory^, Temp.Size);
  finally
    FreeAndNil(Temp);
  end;
end;

end.
