program TorrentInfo;

{$APPTYPE CONSOLE}

uses
  Windows, AvL, avlUtils, BTUtils;

type
  TFile = record
    Name: string;
    Pos: LARGE_INTEGER;
    Len: Integer;
  end;
  TFiles = array of TFile;

function ToOEM(const S: string): string;
begin
  SetLength(Result, Length(S));
  CharToOem(PChar(S), PChar(Result));
end;

function GetInfo(Torrent: TBEMap): TBEMap;
begin
  Result := Torrent['info'] as TBEMap;
  if not Assigned(Result) then
    WriteLn('Invalid torrent');
end;

function GetPath(Path: TBEList): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Path.Count - 1 do
    Result := AddTrailingBackslash(Result) + BEString(Path[i]);
end;

function GetPieceFiles(Info: TBEMap; Piece: Integer): TFiles;

  procedure AddFile(F: TFile);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := F;
  end;

var
  i, PieceSize: Integer;
  Pos, PiecePos: Int64;
  F: TFile;
  Files: TBEList;
begin
  PieceSize := BEInt(Info['piece length']);
  PiecePos := Int64(Piece) * PieceSize;
  if Assigned(Info['files']) then
  begin
    Pos := 0;
    i := 0;
    Files := Info['files'] as TBEList;
    while Pos + BEInt((Files[i] as TBEMap)['length']) < PiecePos do
    begin
      Pos := Pos + BEInt((Files[i] as TBEMap)['length']);
      Inc(i);
    end;
    while (PieceSize > 0) and (i < Files.Count) do
    begin
      with Files[i] as TBEMap do
      begin
        F.Name := GetPath(Items['path'] as TBEList);
        F.Pos.QuadPart := PiecePos - Pos;
        F.Len := Min(PieceSize, BEInt(Items['length']) - F.Pos.QuadPart);
      end;
      AddFile(F);
      Pos := PiecePos;
      Dec(PieceSize, F.Len);
      Inc(i);
    end;
  end
  else begin
    F.Name := BEString(Info['name']);
    F.Pos.QuadPart := PiecePos;
    F.Len := Min(PieceSize, BEInt(Info['length']) - PiecePos);
    AddFile(F);
  end;
end;

function GetPiece(const Dir: string; const Files: array of TFile; Piece: Integer): TMemoryStream;
var
  i: Integer;
  F: TFileStream;
begin
  Result := TMemoryStream.Create;
  for i := 0 to High(Files) do
  begin
    F := TFileStream.Create(AddTrailingBackslash(Dir) + Files[i].Name, fmOpenRead);
    try
      SetFilePointer(F.Handle, Files[i].Pos.LowPart, @Files[i].Pos.HighPart, FILE_BEGIN);
      Result.CopyFrom(F, Files[i].Len);
    finally
      FreeAndNil(F);
    end;
  end;
end;

procedure PrintInfo(Torrent: TBEMap);
var
  Info: TBEMap;
begin
  Info := GetInfo(Torrent);
  if not Assigned(Info) then Exit;
  WriteLn('Name: ', ToOEM(BEString(Info['name'])));
  WriteLn('Info hash: ', HexString(TorrentInfoHash(Torrent)));
  WriteLn('Comment: ', ToOEM(BEString(Torrent['comment'])));
  WriteLn('Created by: ', ToOEM(BEString(Torrent['created by'])));
  WriteLn('Creation date: ', ToOEM(DateTimeToStr(UnixToDateTime(BEInt(Torrent['creation date']))) + ' UTC'));
  WriteLn('Pieces: ', IntToStr(Length((Info['pieces'] as TBEString).Value) div 20) + ' x ' + SizeToStr(BEInt(Info['piece length'])));
  if Assigned(Info['files']) then
    WriteLn('Multi-file torrent (', IntToStr((Info['files'] as TBEList).Count), ' files)')
  else
    WriteLn('Single-file torrent');
  WriteLn;
end;

procedure DumpElement(Elem: TBEElement; Level: Integer = 0; const Prefix: string = '');

  function Indent(Level: Integer): string;
  begin
    Result := '';
    while Level > 0 do
    begin
      Result := Result + '  ';
      Dec(Level);
    end;
  end;

var
  i: Integer;
begin
  if Elem is TBEString then
    WriteLn(Indent(Level) + Prefix + (Elem as TBEString).Value)
  else if Elem is TBEInt then
    WriteLn(Indent(Level) + Prefix + Int64ToStr((Elem as TBEInt).Value))
  else if Elem is TBeList then
  begin
    WriteLn(Indent(Level) + Prefix + 'List:');
    with Elem as TBEList do
      for i := 0 to Count - 1 do
        DumpElement(Items[i], Level + 1);
  end
  else if Elem is TBEMap then
  begin
    WriteLn(Indent(Level) + Prefix + 'Map:');
    with Elem as TBEMap do
      for i := 0 to Count - 1 do
        DumpElement(Items[Names[i]], Level + 1, Names[i] + ': ');
  end
  else
    WriteLn(Indent(Level) + Prefix + 'Invalid element');
  if Level = 0 then
    WriteLn;
end;

procedure ListFiles(Torrent: TBEMap);
var
  Info: TBEMap;
  Files: TBEList;
  i: Integer;
begin
  Info := GetInfo(Torrent);
  if not Assigned(Info) then Exit;
  Files := Info['files'] as TBEList;
  if Assigned(Files) then
    for i := 0 to Files.Count - 1 do
      with Files[i] as TBEMap do
        WriteLn(SizeToStr(BEInt(Items['length'])), ' '#9, ToOEM(GetPath(Items['path'] as TBEList)))
  else
    WriteLn(SizeToStr(BEInt(Info['length'])), ' '#9, ToOEM(BEString(Info['name'])));
  WriteLn;
end;

procedure Verify(Torrent: TBEMap; const Dir: string);
var
  Info: TBEMap;
  Files: TFiles;
  Pieces: string;
  Piece: TMemoryStream;
  FaultyFiles: TStringList;
  i, j: Integer;
begin
  Info := GetInfo(Torrent);
  if not Assigned(Info) then Exit;
  Pieces := (Info['pieces'] as TBEString).Value;
  FaultyFiles := TStringList.Create;
  try
    WriteLn('Verification...');
    for i := 0 to Length(Pieces) div 20 - 1 do
    begin
      Write(#13, Round(100 * (i / (Length(Pieces) div 20))), '%');
      Files := GetPieceFiles(Info, i);
      try
        Piece := GetPiece(Dir, Files, i);
        try
          if SHA1(Piece.Memory^, Piece.Size) <> Copy(Pieces, 20 * i + 1, 20) then
          begin
            WriteLn(#13'Piece #', i + 1, ' hash failed');
            for j := 0 to High(Files) do
              if FaultyFiles.IndexOf(Files[j].Name) < 0 then
                FaultyFiles.Add(Files[j].Name);
          end;
        finally
          FreeAndNil(Piece);
        end;
      finally
        Finalize(Files);
      end;
    end;
    WriteLn(#13'Verification completed');
    WriteLn;
  finally
    if FaultyFiles.Count > 0 then
    begin
      WriteLn('Verification failed. Affected files:');
      for i := 0 to FaultyFiles.Count - 1 do
        WriteLn(ToOEM(FaultyFiles[i]));
      WriteLn;
    end;
    FreeAndNil(FaultyFiles);
  end;
end;

var
  Torrent: TBEElement;
  F: TFileStream;
  i: Integer;
  Arg: string;

begin
  WriteLn('TorrentInfo 1.0 (c)Vga, 2017');
  WriteLn;
  if ParamCount = 0 then
  begin
    WriteLn('Usage: TorrentInfo <torrent file> [switches]');
    WriteLn('  -l: list files');
    WriteLn('  -d: dump torrent structure');
    WriteLn('  -v<dir>: verify torrent');
    WriteLn('  -w: wait');
    Exit;
  end;
  if not FileExists(ParamStr(1)) then
  begin
    WriteLn('File "' + ParamStr(1) + '" not found');
    Exit;
  end;
  F := TFileStream.Create(ParamStr(1), fmOpenRead);
  try
    Torrent := BELoadElement(F);
    try
      PrintInfo(Torrent as TBEMap);
      for i := 2 to ParamCount do
      begin
        Arg := ParamStr(i);
        if (Length(Arg) < 2) or (Arg[1] <> '-') then
        begin
          WriteLn('Unknown argument: ' + Arg);
          Continue;
        end;
        case Arg[2] of
          'd': DumpElement(Torrent);
          'l': ListFiles(Torrent as TBEMap);
          'v': Verify(Torrent as TBEMap, Copy(Arg, 3, MaxInt));
          'w': ReadLn;
          else WriteLn('Unknown switch ' + Arg[2]);
        end;
      end;
    finally
      FreeAndNil(Torrent);
    end;
  finally
    FreeAndNil(F);
  end;
end.