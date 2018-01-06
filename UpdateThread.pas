unit UpdateThread;

interface

uses
  Windows, AvL, Utils, Aria2;

type
  TUpdateThread = class(TThread)
  private
    FAria2: TAria2;
    FBeforeUpdate, FOnUpdate: TThreadMethod;
  protected
    procedure Execute; override;
  public
    Stats, Active, Waiting, Stopped, Info: TAria2Struct;
    Names: TStringList;
    StatsOnly: Boolean;
    InfoGID: TAria2GID;
    TransferKeys, InfoKeys: TStringArray;
    constructor Create(Aria2: TAria2);
    destructor Destroy; override;
    property BeforeUpdate: TThreadMethod read FBeforeUpdate write FBeforeUpdate;
    property OnUpdate: TThreadMethod read FOnUpdate write FOnUpdate;
  end;

implementation

{ TUpdateThread }

constructor TUpdateThread.Create(Aria2: TAria2);
begin
  FAria2 := Aria2;
  Names := TStringList.Create;
  inherited Create(true);
end;

destructor TUpdateThread.Destroy;
begin
  FreeAndNil(Names);
  Finalize(TransferKeys);
  inherited;
end;

procedure TUpdateThread.Execute;
var
  NameIDs: TStringList;

  procedure FetchNames(List: TAria2Struct);
  var
    i: Integer;
  begin
    try
      for i := 0 to List.Length[''] - 1 do
      begin
        List.Index := i;
        if not List.Has[sfBittorrent] or (List[sfBTName] = '') then
        try
          NameIDs.AddObject(List[sfGID], TObject(FAria2.GetFiles(List[sfGID])));
        except
        end;
      end;
    finally
      List.Index := -1;
    end;
  end;

var
  i: Integer;
  ActiveID, WaitingID, StoppedID, InfoID: TRequestID;
  Files: TAria2Struct;
begin
  Stats := nil;
  Active := nil;
  Waiting := nil;
  Stopped := nil;
  Info := nil;
  NameIDs := TStringList.Create;
  while not Terminated do
  begin
    try
      if Assigned(FBeforeUpdate) then
        Synchronize(FBeforeUpdate);
      with FAria2 do
        Stats := GetStruct(GetGlobalStats);
      try
        if not StatsOnly then
        begin
          with FAria2 do
          begin
            BeginBatch;
            try
              ActiveID := TellActive(TransferKeys);
              WaitingID := TellWaiting(0, Stats.Int[sfNumWaiting], TransferKeys);
              StoppedID := TellStopped(0, Stats.Int[sfNumStopped], TransferKeys);
              if InfoGID <> '' then
                InfoID := TellStatus(InfoGID, InfoKeys);
            finally
              EndBatch;
            end;
            Active := GetStruct(ActiveID);
            Waiting := GetStruct(WaitingID);
            Stopped := GetStruct(StoppedID);
            if InfoGID <> '' then
            try
              Info := GetStruct(InfoID);
            except
              Info := nil;
            end;
            BeginBatch;
            try
              NameIDs.Clear;
              Names.Clear;
              FetchNames(Active);
              FetchNames(Waiting);
              FetchNames(Stopped);
            finally
              EndBatch;
              for i := 0 to NameIDs.Count - 1 do
              try
                Files := GetStruct(TRequestID(NameIDs.Objects[i]));
                Files.Index := 0;
                try
                  if Files[sfPath] <> '' then
                    Names.Values[NameIDs[i]] := ExtractFileName(Files[sfPath])
                  else
                    Names.Values[NameIDs[i]] := DecodeURL(ExtractFileName(Files[sfUris + '.0.' + sfUri]));
                finally
                  FreeAndNil(Files);
                end;
              except
              end;
            end;
          end;
        end;
        if Assigned(FOnUpdate) then
          Synchronize(FOnUpdate);
      finally
        FreeAndNil(Stats);
        FreeAndNil(Active);
        FreeAndNil(Waiting);
        FreeAndNil(Stopped);
        FreeAndNil(Info);
      end;
      Sleep(1000);
    except
      if Assigned(FOnUpdate) then
        Synchronize(FOnUpdate);
      Sleep(5000);
    end;
  end;
  NameIDs.Free;
end;

end.