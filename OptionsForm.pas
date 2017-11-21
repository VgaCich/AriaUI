unit OptionsForm;

interface

uses
  Windows, Messages, AvL, avlUtils, Aria2, OptionsList;

type
  TOptionsForm = class(TForm)
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
  FormOptions: TOptionsForm;

implementation

uses
  MainForm;

{ TOptionsForm }

constructor TOptionsForm.Create(AParent: TWinControl);
begin
  inherited Create(AParent, 'Options');
  BorderStyle := bsDialog; //TODO: resizing?
  SetSize(400 + Width - ClientWidth, 500 + Height - ClientHeight);
  Position := poScreenCenter;
  OnKeyUp := FormKeyUp;
  BtnOK := TButton.Create(Self, 'OK');
  BtnOK.SetBounds(ClientWidth - 160, ClientHeight - 30, 75, 25);
  BtnOK.OnClick := OKClick;
  BtnCancel := TButton.Create(Self, 'Cancel');
  BtnCancel.SetBounds(ClientWidth - 80, ClientHeight - 30, 75, 25);
  BtnCancel.OnClick := CancelClick;
end;

procedure TOptionsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then OKClick(BtnOK);
  if Key = VK_ESCAPE then CancelClick(BtnCancel);
end;

procedure TOptionsForm.OKClick(Sender: TObject);
begin
  //TODO: apply options
  Close;
end;

procedure TOptionsForm.CancelClick(Sender: TObject);
begin
  Close;
end;

procedure TOptionsForm.Show;
begin
  //TODO: load options
  ShowModal;
end;

end.