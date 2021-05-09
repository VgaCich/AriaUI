program TorrentInfo;

{$APPTYPE CONSOLE}

uses
  SysSfIni, Windows, AvL, avlUtils, BTUtils;

const
  AppName = 'TorrentInfo 2.3';

type
  TExitCodes = (ecUnknownArgument, ecUnknownCommand, ecException, ecCreateTorrent,
    ecAddTracker, ecSetComment, ecSetName, ecSetPrivate, ecDumpElement, ecListFiles,
    ecFilesLayout, ecVerify, ecSaveTorrent, ecNone = 31);
  TExitCode = set of TExitCodes;

function ToOEM(const S: string): string;
begin
  Result := '';
  if S <> '' then
  begin
    SetLength(Result, Length(S));
    CharToOem(PChar(S), PChar(Result));
  end;
end;

function FileSize64(const FileName: WideString): Int64;
var
  F: THandle;
begin
  F := FileOpenW(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := FileSeek64(F, 0, soFromEnd);
  finally
    FileClose(F);
  end;
end;

function AddTrailingBackslashW(const Path: WideString): WideString;
begin
  Result := Path;
  if (Length(Result) > 0) and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;

function FirstDelimiterW(const S: WideString; D: WideChar): Integer;
begin
  for Result := 1 to Length(S) do
    if S[Result] = D then Exit;
  Result := 0;
end;

function Now: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  SystemTimeToDateTime(SystemTime, Result);
end;

function GetInfo(Torrent: TBEMap): TBEMap;
begin
  if Assigned(Torrent) then
  begin
    Result := Torrent[btfInfo] as TBEMap;
    if not Assigned(Result) then
      WriteLn('Invalid torrent');
  end
  else
    Result := nil;
end;

procedure MakePath(Path: WideString; List: TBEList);
begin
  while FirstDelimiterW(Path, '\') > 0 do
  begin
    List.Add(TBEString.Create(UTF8Encode(Copy(Path, 1, FirstDelimiterW(Path, '\') - 1))));
    Delete(Path, 1, FirstDelimiterW(Path, '\'));
  end;
  if Length(Path) > 0 then
    List.Add(TBEString.Create(UTF8Encode(Path)));
end;

function GetPieceLength(Len: Int64): Integer;
const
  MinPiece = 16384;
  MaxPieces: array[0..10] of Integer = (250, 500, 500, 1000, 1000, 2000, 2000, 2000, 4000, 4000, MaxInt);
var
  i: Integer;
begin
  Result := MinPiece;
  for i := 0 to High(MaxPieces) do
    if Len div Result > MaxPieces[i] then
      Result := 2 * Result;
end;

function FindFiles(const BasePath, Path: WideString; Files: TBEList): Int64;
var
  F: THandle;
  FD: TWin32FindDataW;
  FI: TBEMap;
  LI: LARGE_INTEGER;
begin
  Result := 0;
  F := FindFirstFileW(PWideChar(AddTrailingBackslashW(BasePath) + AddTrailingBackslashW(Path) + '*.*'), FD);
  if F = INVALID_HANDLE_VALUE then Exit;
  try
    repeat
      if FD.dwFileAttributes and faDirectory = 0 then
      begin
        FI := TBEMap.Create;
        LI.LowPart := FD.nFileSizeLow;
        LI.HighPart := FD.nFileSizeHigh;
        Result := Result + LI.QuadPart;
        FI[btfLength] := TBEInt.Create(LI.QuadPart);
        FI[btfPath] := TBEList.Create;
        MakePath(AddTrailingBackslashW(Path) + WideString(FD.cFileName), FI[btfPath] as TBEList);
        Files.Add(FI);
      end
      else if (WideString(FD.cFileName) <> '.') and (WideString(FD.cFileName) <> '..') then
        Result := Result + FindFiles(BasePath, AddTrailingBackslashW(Path) + WideString(FD.cFileName), Files);
    until not FindNextFileW(F, FD);
  finally
    Windows.FindClose(F);
  end;
end;

procedure PrintInfo(Torrent: TBEMap);
var
  Info: TBEMap;
begin
  Info := GetInfo(Torrent);
  if not Assigned(Info) then Exit;
  try
    WriteLn('Name: ', ToOEM(BEString(Info[btfName])));
    WriteLn('Info hash: ', HexString(TorrentInfoHash(Torrent)));
    WriteLn('Private: ', BEInt(Info[btfPrivate]));
    WriteLn('Comment: ', ToOEM(BEString(Torrent[btfComment])));
    WriteLn('Created by: ', ToOEM(BEString(Torrent[btfCreatedBy])));
    WriteLn('Creation date: ', ToOEM(DateTimeToStr(UnixToDateTime(BEInt(Torrent[btfCreationDate]))) + ' UTC'));
    WriteLn('Pieces: ', IntToStr(Length((Info[btfPieces] as TBEString).Value) div 20) + ' x ' + SizeToStr(BEInt(Info[btfPieceLength])));
    if Assigned(Info[btfFiles]) then
      WriteLn('Multi-file torrent (', IntToStr((Info[btfFiles] as TBEList).Count), ' files)')
    else
      WriteLn('Single-file torrent');
    WriteLn;
  except
    on E:Exception do
      WriteLn(ErrOutput, 'Exception: ', E.Message);
  end;
end;

function CreateTorrent(const Path: string): TBEMap;

  procedure ReportFile(Self: Pointer; Sender: TObject; const FileName: WideString);
  begin
    WriteLn('Error reading file "' + ToOEM(FileName) + '"');
  end;

var
  Info: TBEMap;
  PieceReader: TPieceReader;
  Pieces, Piece, Hash: string;
  Len: Int64;
  i, PieceLen: Integer;
begin
  Result := nil;
  if not (FileExists(Path) or DirectoryExists(Path)) then
  begin
    WriteLn('Path "' + Path + '" not found');
    Exit;
  end;
  WriteLn('Creating torrent...');
  Result := TBEMap.Create;
  Info := TBEMap.Create;
  Result[btfInfo] := Info;
  Result[btfCreatedBy] := TBEString.Create(AppName);
  Result[btfCreationDate] := TBEInt.Create(DateTimeToUnix(Now));
  Info[btfName] := TBEString.Create(UTF8Encode(ExtractFileName(Path)));
  if FileExists(Path) then
  begin
    Len := FileSize64(Path);
    Info[btfLength] := TBEInt.Create(Len);
  end
  else begin
    Info[btfFiles] := TBEList.Create;
    Len := FindFiles(Path, '', Info[btfFiles] as TBEList);
  end;
  PieceLen := GetPieceLength(Len);
  Info[btfPieceLength] := TBEInt.Create(PieceLen);
  SetLength(Pieces, 20 * ((Len + PieceLen - 1) div PieceLen));
  ZeroMemory(PChar(Pieces), Length(Pieces));
  Info[btfPieces] := TBEString.Create(Pieces);
  UniqueString(Pieces);
  PieceReader := TPieceReader.Create(Info, ExtractFilePath(Path));
  try
    PieceReader.OnFileError := TOnFileError(MakeMethod(@ReportFile));
    for i := 0 to PieceReader.PieceCount - 1 do
    begin
      Write(#13, Round(100 * (i / PieceReader.PieceCount)), '%');
      Piece := PieceReader[i];
      Hash := SHA1(Piece[1], Length(Piece));
      Move(Hash[1], Pieces[20 * i + 1], 20);
    end;
    (Info[btfPieces] as TBEString).Value := Pieces;
  finally
    PieceReader.Free;
  end;
  WriteLn(#13'Completed');
  WriteLn;
end;

function AddTracker(Torrent: TBEMap; const URL: string): TExitCode;
var
  L: TBEList;
begin
  Result := [ecAddTracker];
  if not Assigned(Torrent) then Exit;
  if URL = '' then
  begin
    Torrent.Delete(btfAnnounce);
    Torrent.Delete(btfAnnounceList);
  end
  else begin
    if not Assigned(Torrent[btfAnnounce]) then
      Torrent[btfAnnounce] := TBEString.Create(UTF8Encode(URL));
    if not Assigned(Torrent[btfAnnounceList]) then
      Torrent[btfAnnounceList] := TBEList.Create;
    L := TBEList.Create;
    L.Add(TBEString.Create(UTF8Encode(URL)));
    (Torrent[btfAnnounceList] as TBEList).Add(L);
  end;
  Result := [];
end;

function SetComment(Torrent: TBEMap; const Comment: string): TExitCode;
begin
  Result := [ecSetComment];
  if not Assigned(Torrent) then Exit;
  if Comment = '' then
    Torrent.Delete(btfComment)
  else if Assigned(Torrent[btfComment]) then
    (Torrent[btfComment] as TBEString).Value := UTF8Encode(Comment)
  else
    Torrent[btfComment] := TBEString.Create(UTF8Encode(Comment));
  Result := [];
end;

function SetName(Torrent: TBEMap; const Name: string): TExitCode;
var
  Info: TBEMap;
begin
  Result := [ecSetName];
  Info := GetInfo(Torrent);
  if not Assigned(Info) or not Assigned(Info[btfName]) or (Name = '') then
  begin
    WriteLn('Invalid torrent or no name given');
    Exit;
  end;
  (Info[btfName] as TBEString).Value := UTF8Encode(Name);
  Result := [];
end;

function SetPrivate(Torrent: TBEMap; const Flag: string): TExitCode;
var
  Info: TBEMap;
begin
  Result := [ecSetPrivate];
  Info := GetInfo(Torrent);
  if not Assigned(Info) then Exit;
  if (Flag = '') or (StrToInt(Flag) = 0) then
    Info.Delete(btfPrivate)
  else if Assigned(Info[btfPrivate]) then
    (Info[btfPrivate] as TBEInt).Value := StrToInt(Flag)
  else
    Info[btfPrivate] := TBEInt.Create(StrToInt(Flag));
  Result := [];
end;

function DumpElement(Elem: TBEElement; Level: Integer = 0; const Prefix: string = ''): TExitCode;

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
  Result := [ecDumpElement];
  if not Assigned(Elem) then Exit;
  Result := [];
  if Elem is TBEString then
    WriteLn(Indent(Level) + Prefix + (Elem as TBEString).Value)
  else if Elem is TBEInt then
    WriteLn(Indent(Level) + Prefix + Int64ToStr((Elem as TBEInt).Value))
  else if Elem is TBeList then
  begin
    WriteLn(Indent(Level) + Prefix + 'List:');
    with Elem as TBEList do
      for i := 0 to Count - 1 do
        Result := Result + DumpElement(Items[i], Level + 1);
  end
  else if Elem is TBEMap then
  begin
    WriteLn(Indent(Level) + Prefix + 'Map:');
    with Elem as TBEMap do
      for i := 0 to Count - 1 do
        Result := Result + DumpElement(Items[Names[i]], Level + 1, Names[i] + ': ');
  end
  else begin
    WriteLn(Indent(Level) + Prefix + 'Invalid element');
    Result := [ecDumpElement];
  end;
  if Level = 0 then
    WriteLn;
end;

function ListFiles(Torrent: TBEMap): TExitCode;
var
  Info: TBEMap;
  Files: TBEList;
  i: Integer;
begin
  Result := [ecListFiles];
  Info := GetInfo(Torrent);
  if not Assigned(Info) then Exit;
  Files := Info[btfFiles] as TBEList;
  if Assigned(Files) then
    for i := 0 to Files.Count - 1 do
      with Files[i] as TBEMap do
        WriteLn(SizeToStr(BEInt(Items[btfLength])), ' '#9, ToOEM(TorrentGetPath(Items[btfPath] as TBEList)))
  else
    WriteLn(SizeToStr(BEInt(Info[btfLength])), ' '#9, ToOEM(BEString(Info[btfName])));
  WriteLn;
  Result := [];
end;

function FilesLayout(Torrent: TBEMap): TExitCode;
var
  Info: TBEMap;
  Files: TBEList;
  i: Integer;
  Offset: Int64;
begin
  Result := [ecFilesLayout];
  Info := GetInfo(Torrent);
  if not Assigned(Info) then Exit;
  Files := Info[btfFiles] as TBEList;
  Offset := 0;
  WriteLn('Offset'#9'Size'#9'Name');
  if Assigned(Files) then
    for i := 0 to Files.Count - 1 do
      with Files[i] as TBEMap do
      begin
        WriteLn(Offset, ' '#9, BEInt(Items[btfLength]), ' '#9, ToOEM(TorrentGetPath(Items[btfPath] as TBEList)));
        Offset := Offset + BEInt(Items[btfLength]);
      end
  else
    WriteLn(Offset, ' '#9, BEInt(Info[btfLength]), ' '#9, ToOEM(BEString(Info[btfName])));
  WriteLn;
  Result := [];
end;

function Verify(Torrent: TBEMap; const Dir: string): TExitCode;
var
  Info: TBEMap;
  Piece: string;
  Pieces: TPieceReader;
  Files, FaultyFiles: TStringList;
  i, j, Idx: Integer;

  function FilePieces(const Name: string): Integer;
  var
    i: Integer;
  begin
    for i := 0 to Pieces.FilesCount - 1 do
      if string(Pieces.Files[i].Name) = Name then
      begin
        with Pieces.Files[i] do
          Result := (Pos + Len) div Pieces.PieceSize - Pos div Pieces.PieceSize + 1; 
        Exit;
      end;
    Result := 0;
  end;

begin
  Result := [ecVerify];
  Info := GetInfo(Torrent);
  if not Assigned(Info) then Exit;
  i := GetFileAttributesW(PWideChar(WideString(AddTrailingBackslash(Dir)) + BEString(Info[btfName])));
  if (i = -1) or ((i and FILE_ATTRIBUTE_DIRECTORY <> 0) <> Assigned(Info[btfFiles])) then
  begin
    WriteLn('"' + ToOEM(AddTrailingBackslash(Dir) + BEString(Info[btfName])) + '" not found');
    Exit;
  end;
  FaultyFiles := TStringList.Create;
  try
    WriteLn('Verification...');
    Pieces := TPieceReader.Create(Info, Dir);
    for i := 0 to Pieces.PieceCount - 1 do
    begin
      Write(#13, Round(100 * (i / Pieces.PieceCount)), '%');
      Piece := Pieces[i];
      if SHA1(Piece[1], Length(Piece)) <> Pieces.Hash[i] then
      begin
        WriteLn(#13'Piece #', i + 1, ' hash failed');
        Files := TStringList.Create;
        try
          Pieces.GetPieceFiles(i, Files);
          for j := 0 to Files.Count - 1 do
          begin
            Idx := FaultyFiles.IndexOf(Files[j]);
            if (Idx < 0) or (Files.Count > 1) then
               WriteLn('  ', Files[j]);
            if Idx < 0 then
              FaultyFiles.AddObject(Files[j], TObject(0))
            else
              FaultyFiles.Objects[Idx] := TObject(Integer(FaultyFiles.Objects[Idx]) + 1);
          end
        finally
          FreeAndNil(Files);
        end;
      end;
    end;
    WriteLn(#13'Verification completed');
    WriteLn;
  finally
    try
      if FaultyFiles.Count = 0 then
        Result := []
      else begin
        WriteLn('Verification failed. Affected files:');
        for i := 0 to FaultyFiles.Count - 1 do
          WriteLn(Format('[%d/%d] '#9'%s', [Integer(FaultyFiles.Objects[i]) + 1, FilePieces(FaultyFiles[i]), ToOEM(FaultyFiles[i])]));
        WriteLn;
      end;
    finally
      FreeAndNil(FaultyFiles);
      FreeAndNil(Pieces);
    end;
  end;
end;

function LoadTorrent(const FileName: string): TBEElement;
var
  F: TFileStream;
begin
  Result := nil;
  if not FileExists(FileName) then
  begin
    WriteLn('File "' + FileName + '" not found');
    Exit;
  end;
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    try
      Result := BELoadElement(F);
    except
      on E:Exception do
      begin
        WriteLn('Can''t load torrent file');
        WriteLn('Exception: ', E.Message);
        FreeAndNil(Result);
      end;
    end;
  finally
    FreeAndNil(F);
  end;
end;

function SaveTorrent(const FileName: string; Torrent: TBEElement): TExitCode;
var
  F: TFileStream;
begin
  Result := [ecSaveTorrent];
  if not Assigned(Torrent) then
  begin
    WriteLn('No torrent loaded');
    Exit;
  end;
  F := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  try
    Torrent.Write(F);
    WriteLn('Saved');
    Result := [];
  finally
    F.Free;
  end;
end;

var
  Torrent: TBEElement;
  i: Integer;
  Arg: string;
  ExitCode: TExitCode absolute System.ExitCode;

begin
  ExitCode := [];
  WriteLn(AppName + ' (c)Vga, 2017-2021');
  WriteLn;
  if ParamCount = 0 then
  begin
    WriteLn('Usage: TorrentInfo <torrent file> [commands]');
    WriteLn('Commands are processed in order of appearance:');
    WriteLn('  -a[url]: add tracker (clear list if no [url] given)');
    WriteLn('  -c<path>: create torrent');
    WriteLn('  -d: dump torrent structure');
    WriteLn('  -f: dump files layout');
    WriteLn('  -l: list files');
    WriteLn('  -m[text]: set torrent comment');
    WriteLn('  -n<name>: set torrent name');
    WriteLn('  -p[0|1]: set private flag');
    WriteLn('  -s: save torrent');
    WriteLn('  -v<dir>: verify torrent');
    WriteLn('  -w: wait');
    Exit;
  end;
  if FileExists(ParamStr(1)) then
    Torrent := LoadTorrent(ParamStr(1));
  try
    PrintInfo(Torrent as TBEMap);
    for i := 2 to ParamCount do
    begin
      Arg := ParamStr(i);
      if (Length(Arg) < 2) or (Arg[1] <> '-') then
      begin
        WriteLn('Unknown argument: ' + Arg);
        ExitCode := ExitCode + [ecUnknownArgument];
        Continue;
      end;
      try
        case Arg[2] of
          'a': ExitCode := ExitCode + AddTracker(Torrent as TBEMap, Copy(Arg, 3, MaxInt));
          'c': begin
            Torrent := CreateTorrent(Copy(Arg, 3, MaxInt));
            if not Assigned(Torrent) then
              ExitCode := ExitCode + [ecCreateTorrent];
          end;
          'd': ExitCode := ExitCode + DumpElement(Torrent);
          'f': ExitCode := ExitCode + FilesLayout(Torrent as TBEMap);
          'l': ExitCode := ExitCode + ListFiles(Torrent as TBEMap);
          'm': ExitCode := ExitCode + SetComment(Torrent as TBEMap, Copy(Arg, 3, MaxInt));
          'n': ExitCode := ExitCode + SetName(Torrent as TBEMap, Copy(Arg, 3, MaxInt));
          'p': ExitCode := ExitCode + SetPrivate(Torrent as TBEMap, Copy(Arg, 3, MaxInt));
          's': ExitCode := ExitCode + SaveTorrent(ParamStr(1), Torrent);
          'v': ExitCode := ExitCode + Verify(Torrent as TBEMap, Copy(Arg, 3, MaxInt));
          'w': ReadLn;
          else begin
            WriteLn('Unknown command ' + Arg[2]);
            ExitCode := ExitCode + [ecUnknownCommand];
          end;
        end;
      except
        on E:Exception do
        begin
          WriteLn(ErrOutput, 'Exception: ', E.Message);
          ExitCode := ExitCode + [ecException];
        end;
      end;
    end;
  finally
    FreeAndNil(Torrent);
  end;
end.
