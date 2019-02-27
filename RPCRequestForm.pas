unit RPCRequestForm;

interface

uses
  Windows, Messages, AvL, avlUtils, avlJSON, Aria2;

type
  TRPCRequestForm = class(TForm)
    LMethod, LParams, LResult: TLabel;
    Method: TComboBox;
    Params, Result: TMemo;
    BtnSend, BtnEscape: TButton;
  private
    procedure Escape(Sender: TObject);
    procedure Send(Sender: TObject);
  public
    constructor Create(AParent: TWinControl);
  end;

var
  FormRPCRequest: TRPCRequestForm;

implementation

uses
  Utils, MainForm;

function EscapeParams(const S: string): string;
const
  Esc: array[0..7] of record C: Char; R: string; end = (
    (C: '"'; R: '\"'), (C: '\'; R: '\\'),
    (C: '/'; R: '\/'), (C: #$08; R: '\b'),
    (C: #$09; R: '\t'), (C: #$0A; R: '\n'),
    (C: #$0C; R: '\f'), (C: #$0D; R: '\r'));
var
  i, j: Integer;
label
  Next;
begin
  for i := 1 to Length(S) do
  begin
    for j := Low(Esc) to High(Esc) do
      if S[i] = Esc[j].C then
      begin
        Result := Result + Esc[j].R;
        goto Next;
      end;
    Result := Result + S[i];
    Next:
  end;
end;

type
  TAria2G = class(TAria2)
  public
    function SendRequest(const Method, Params: string): PJsonValue;
  end;

{ TAria2G }

function TAria2G.SendRequest(const Method, Params: string): PJsonValue;
begin
  Result := GetResult(inherited SendRequest(Method, Params, rvtUnknown), rvtUnknown);
end;

{ TRPCRequestForm }

constructor TRPCRequestForm.Create(AParent: TWinControl);
var
  i: Integer;
begin
  inherited Create(AParent, 'RPC Request');
  BorderStyle := bsToolWindow; //TODO: resizing
  SetSize(400 + Width - ClientWidth, 500 + Height - ClientHeight);
  Position := poScreenCenter;
  LMethod := TLabel.Create(Self, 'Method:');
  LMethod.SetBounds(5, 5, ClientWidth - 90, 15);
  LParams := TLabel.Create(Self, 'Parameters:');
  LParams.SetBounds(5, 50, ClientWidth - 90, 15);
  LResult := TLabel.Create(Self, 'Result:');
  LResult.SetBounds(5, 170, ClientWidth - 10, 15);
  Method := TComboBox.Create(Self, csDropDown);
  Method.SetBounds(5, 20, ClientWidth - 90, 24);
  for i := 0 to High(Aria2Methods) do
    Method.ItemAdd(Aria2Methods[i]);
  Params := TMemo.Create(Self, '');
  Params.SetBounds(5, 65, ClientWidth - 10, 100);
  Result := TMemo.Create(Self, '');
  Result.SetBounds(5, 185, ClientWidth - 10, ClientHeight - 190);
  Result.Style := Result.Style or WS_VSCROLL or WS_HSCROLL;
  Result.ReadOnly := true;
  BtnSend := TButton.Create(Self, 'Send');
  BtnSend.SetBounds(ClientWidth - 80, 5, 75, 25);
  BtnSend.OnClick := Send;
  BtnEscape := TButton.Create(Self, 'Escape');
  BtnEscape.SetBounds(ClientWidth - 80, 35, 75, 25);
  BtnEscape.OnClick := Escape;
end;

procedure TRPCRequestForm.Escape(Sender: TObject);
begin
  if Params.SelLength = 0 then
    Params.Perform(EM_SETSEL, 0, -1);
  Params.SelText := EscapeParams(Params.SelText);
end;

procedure TRPCRequestForm.Send(Sender: TObject);
var
  S: string;
  Res: PJsonValue;
begin
  Result.Text := '';
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
