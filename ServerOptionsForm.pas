unit ServerOptionsForm;

interface

uses
  Windows, Messages, AvL, avlUtils, avlJSON, Aria2, OptionsList;

type
  TServerOptionsForm = class(TForm)
    Options: TOptionsList;
    BtnOK, BtnCancel: TButton;
  private
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
  public
    constructor Create(AParent: TWinControl);
    procedure Show;
  end;

var
  FormServerOptions: TServerOptionsForm;

implementation

uses
  MainForm;

{ TServerOptionsForm }

constructor TServerOptionsForm.Create(AParent: TWinControl);
begin
  inherited Create(AParent, 'Server options');
  BorderStyle := bsDialog;
  SetSize(400 + Width - ClientWidth, 500 + Height - ClientHeight);
  Position := poScreenCenter;
  OnKeyUp := FormKeyUp;
  Options := TOptionsList.Create(Self);
  Options.SetBounds(5, 5, ClientWidth - 10, ClientHeight - 40);
  BtnOK := TButton.Create(Self, 'OK');
  BtnOK.SetBounds(ClientWidth - 160, ClientHeight - 30, 75, 25);
  BtnOK.OnClick := OKClick;
  BtnCancel := TButton.Create(Self, 'Cancel');
  BtnCancel.SetBounds(ClientWidth - 80, ClientHeight - 30, 75, 25);
  BtnCancel.OnClick := CancelClick;
end;

procedure TServerOptionsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then OKClick(BtnOK);
  if Key = VK_ESCAPE then CancelClick(BtnCancel);
end;

procedure TServerOptionsForm.OKClick(Sender: TObject);
var
  Opts: TAria2OptionArray;
begin
  Opts := Options.GetChangedOptions;
  try
    if Length(Opts) > 0 then
    try
      FormMain.Aria2.ChangeGlobalOptions(Opts);
    except
      on E: Exception do ShowException;
    end;
  Finally
    Finalize(Opts);
  end;
  Close;
end;

procedure TServerOptionsForm.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TServerOptionsForm.Show;
var
  Opts: TAria2Struct;
begin
  Opts := FormMain.Aria2.GetGlobalOptions;
  try
    Options.SetOptions(Opts);
  finally
    FreeAndNil(Opts);
  end;
  ShowModal;
end;

end.