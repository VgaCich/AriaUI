unit RPCRequestForm;

interface

uses
  Windows, AvL, avlUtils, avlJSON, Aria2;

type
  TRPCRequestForm = class(TForm)
    LMethod, LParams, LResult: TLabel;
    Method: TComboBox;
    Params, Result: TMemo;
    BtnSend: TButton;
  private
    procedure Send(Sender: TObject);
  public
    constructor Create(AParent: TWinControl);
  end;

var
  FormRPCRequest: TRPCRequestForm;

implementation

uses
  Utils, MainForm;

const
  RPCMethods: array[0..35] of string = (
    'aria2.addUri (uris [options] [position])',
    'aria2.addTorrent (torrent [uris] [options] position])',
    'aria2.addMetalink (metalink [options] [position])',
    'aria2.remove (gid)',
    'aria2.forceRemove (gid)',
    'aria2.pause (gid)',
    'aria2.pauseAll',
    'aria2.forcePause (gid)',
    'aria2.forcePauseAll',
    'aria2.unpause (gid)',
    'aria2.unpauseAll',
    'aria2.tellStatus (gid [keys])',
    'aria2.getUris (gid)',
    'aria2.getFiles (gid)',
    'aria2.getPeers (gid)',
    'aria2.getServers (gid)',
    'aria2.tellActive ([keys])',
    'aria2.tellWaiting (offset num [keys])',
    'aria2.tellStopped (offset num [keys])',
    'aria2.changePosition (gid pos how)',
    'aria2.changeUri (gid fileIndex delUris addUris [position])',
    'aria2.getOption (gid)',
    'aria2.changeOption (gid options)',
    'aria2.getGlobalOption',
    'aria2.changeGlobalOption (options)',
    'aria2.getGlobalStat',
    'aria2.purgeDownloadResult',
    'aria2.removeDownloadResult (gid)',
    'aria2.getVersion',
    'aria2.getSessionInfo',
    'aria2.shutdown',
    'aria2.forceShutdown',
    'aria2.saveSession',
    'system.multicall (methods)',
    'system.listMethods',
    'system.listNotifications'
  );

type
  TAria2G = class(TAria2)
  public
    function SendRequest(const Method, Params: string): PJsonValue;
  end;

{ TAria2G }

function TAria2G.SendRequest(const Method, Params: string): PJsonValue;
begin
  Result := GetResult(inherited SendRequest(Method, Params));
end;

{ TRPCRequestForm }

constructor TRPCRequestForm.Create(AParent: TWinControl);
var
  i: Integer;
begin
  inherited Create(AParent, 'RPC Request');
  BorderStyle := bsDialog; //TODO: resizing
  SetSize(400 + Width - ClientWidth, 500 + Height - ClientHeight);
  Position := poScreenCenter;
  LMethod := TLabel.Create(Self, 'Method:');
  LMethod.SetBounds(5, 5, ClientWidth - 90, 15);
  LParams := TLabel.Create(Self, 'Parameters:');
  LParams.SetBounds(5, 50, ClientWidth - 10, 15);
  LResult := TLabel.Create(Self, 'Result:');
  LResult.SetBounds(5, 170, ClientWidth - 10, 15);
  Method := TComboBox.Create(Self, csDropDown);
  Method.SetBounds(5, 20, ClientWidth - 90, 24);
  for i := 0 to High(RPCMethods) do
    Method.ItemAdd(RPCMethods[i]);
  Params := TMemo.Create(Self, '');
  Params.SetBounds(5, 65, ClientWidth - 10, 100);
  Result := TMemo.Create(Self, '');
  Result.SetBounds(5, 185, ClientWidth - 10, ClientHeight - 190);
  Result.Style := Result.Style or WS_VSCROLL or WS_HSCROLL;
  Result.ReadOnly := true;
  BtnSend := TButton.Create(Self, 'Send');
  BtnSend.SetBounds(ClientWidth - 80, 20, 75, 25);
  BtnSend.OnClick := Send;
end;

procedure TRPCRequestForm.Send(Sender: TObject);
var
  S: string;
  Res: PJsonValue;
begin
  try
    S := Method.Text;
    Res := TAria2G(FormMain.Aria2).SendRequest(Tok(' ', S), Params.Text);
    try
      Result.Text := JsonToStr(Res);
    finally
      JsonFree(Res);
    end;
  except
    ShowException;
  end;
end;

end.
