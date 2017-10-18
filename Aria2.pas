unit Aria2;

interface

uses
  AvL, avlJSON;

type
  TOnRPCRequest = function(Sender: TObject; const Request: string): string of object;
  {TAria2Transfer = class
  private

  public

  end;}
  TAria2 = class
  private
    FOnRequest: TOnRPCRequest;
    FRPCSecret: string;
    function MakeParams(const Params: string): string;
    function NewId: string;
    function SendRequest(const Method, Params: string): PJsonValue;
  public
    constructor Create(OnRequest: TOnRPCRequest; const RPCSecret: string = '');
    function GetVersion: string;
    function Shutdown(Force: Boolean = false): Boolean;
    property OnRequest: TOnRPCRequest read FOnRequest write FOnRequest;
    property RPCSecret: string read FRPCSecret write FRPCSecret;
  end;

implementation

{ TAria2 }

constructor TAria2.Create(OnRequest: TOnRPCRequest; const RPCSecret: string = '');
begin
  inherited Create;
  FOnRequest := OnRequest;
  FRPCSecret := RPCSecret;
end;

function TAria2.GetVersion: string;
const
  Term: array[Boolean] of string = (', ', ')');
var
  i: Integer;
  Res: PJsonValue;
begin
  Result := '';
  Res := SendRequest('aria2.getVersion', '');
  try
    if Assigned(Res) then
    begin
      Result := JsonStr(JsonItem(Res, 'version')) + ' (features: ';
      with JsonItem(Res, 'enabledFeatures')^ do
        for i := 0 to Arr.Length - 1 do
          Result := Result + JsonStr(Arr.Values[i]) + Term[i = Arr.Length - 1];
    end;
  finally
    JsonFree(Res);
  end;
end;

function TAria2.MakeParams(const Params: string): string;
begin
  Result := '';
  if FRPCSecret <> '' then
  begin
    Result := '"token:' + FRPCSecret + '"';
    if Params <> '' then
      Result := Result + ','
  end;
  Result := Result + Params;
end;

function TAria2.NewId: string;
begin
  Result := IntToHex(Random(MaxInt), 8);
end;

function TAria2.SendRequest(const Method, Params: string): PJsonValue;
const
  RequestTemplate = '{"jsonrpc":"2.0","id":"%s","method":"%s","params":[%s]}';
var
  Id: string;
  Reply: PJsonValue;
begin
  Result := nil;
  if not Assigned(FOnRequest) then
    raise Exception.Create('Aria2: no transport provided');
  Id := NewId;
  Reply := JsonParse(FOnRequest(Self, Format(RequestTemplate, [Id, Method, MakeParams(Params)])));
  if not Assigned(Reply) then
    raise Exception.Create('Aria2: invalid reply');
  try
    if JsonStr(JsonItem(Reply, 'id')) <> Id then
      raise Exception.Create('Aria2: reply id mismatch');
    if Assigned(JsonItem(Reply, 'error')) then
      raise Exception.CreateFmt('Aria2: request error %d: %s',
        [JsonInt(JsonItem(JsonItem(Reply, 'error'), 'code')),
         JsonStr(JsonItem(JsonItem(Reply, 'error'), 'message'))]);
    Result := JsonExtractItem(Reply, 'result');
    if not Assigned(Result) then
      raise Exception.Create('Aria2: invalid reply');
  finally
    JsonFree(Reply);
  end;
end;

function TAria2.Shutdown(Force: Boolean): Boolean;
const
  Method: array[Boolean] of string = ('aria2.shutdown', 'aria2.forceShutdown');
var
  Res: PJsonValue;
begin
  Res := SendRequest(Method[Force], '');
  try
    Result := JsonStr(Res) = 'OK'; 
  finally
    JsonFree(Res);
  end;
end;

end.
