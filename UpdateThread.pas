unit UpdateThread;

interface

uses
  Windows, AvL, Aria2;

type
  TUpdateThread = class(TThread)
  private
    FAria2: TAria2;
    FOnBeforeUpdate, FOnUpdate: TThreadMethod;
  protected
    procedure Execute; override;
  public
    Stats, Active, Waiting, Stopped, Info: TAria2Struct;
    Names: TStringList;
    InfoGID: TAria2GID;
    TransfersKeys, InfoKeys: TStringArray;
    constructor Create(Aria2: TAria2);
    destructor Destroy; override;
    property OnBeforeUpdate: TThreadMethod read FOnBeforeUpdate write FOnBeforeUpdate;
    property OnUpdate: TThreadMethod read FOnUpdate write FOnUpdate;
  end;

implementation

{ TUpdateThread }

constructor TUpdateThread.Create(Aria2: TAria2);
var
  i: Integer;
begin
  FAria2 := Aria2;
  Names := TStringList.Create;
  inherited Create(true);
end;

destructor TUpdateThread.Destroy;
begin
  FreeAndNil(Names);
  Finalize(TransfersKeys);
  inherited;
end;

procedure TUpdateThread.Execute;

  procedure FetchNames(List: TAria2Struct);
  var
    i: Integer;
    Files: TAria2Struct;
  begin
    try
      for i := 0 to List.Length[''] - 1 do
      begin
        List.Index := i;
        if not List.Has[sfBittorrent] or (List[sfBTName] = '') then
        try
          Files := FAria2.GetFiles(List[sfGID]);
          Files.Index := 0;
          try
            if Files[sfPath] <> '' then
              Names.Values[List[sfGID]] := ExtractFileName(Files[sfPath])
            else
              Names.Values[List[sfGID]] := ExtractFileName(Files[sfUris + '.0.' + sfUri]);
          finally
            FreeAndNil(Files);
          end;
        except
        end;
      end;
    finally
      List.Index := -1;
    end;
  end;

begin
  Stats := nil;
  Active := nil;
  Waiting := nil;
  Stopped := nil;
  Info := nil;
  while not Terminated do
  begin
    try
      if Assigned(FOnBeforeUpdate) then
        Synchronize(FOnBeforeUpdate);
      Stats := FAria2.GetGlobalStats;
      try
        Active := FAria2.TellActive(TransfersKeys);
        Waiting := FAria2.TellWaiting(0, Stats.Int[sfNumWaiting], TransfersKeys);
        Stopped := FAria2.TellStopped(0, Stats.Int[sfNumStopped], TransfersKeys);
        if InfoGID <> '' then
        try
          Info := FAria2.TellStatus(InfoGID, InfoKeys);
        except
          Info := nil;
        end;
        if Names.Count > 100 then
          Names.Clear;
        FetchNames(Active);
        FetchNames(Waiting);
        FetchNames(Stopped);
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
end;

end.