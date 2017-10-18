unit TransfersList;

interface

uses
  Windows, Messages, CommCtrl, AvL, avlUtils, avlListViewEx;

type
  TTransfersList = class(TListViewEx)
  private
    FIcons: TImageList;
    procedure Resize(Sender: TObject);
  public
    constructor Create(Parent: TWinControl);
    destructor Destroy; override;
  end;

implementation

//uses
//  MainForm;

const
  ListColumns: array[0..3] of record Caption: string; Width: Integer end = ( //TODO: User-customizable columns
    (Caption: 'Name'; Width: 300),
    (Caption: 'Size'; Width: 80),
    (Caption: 'Status'; Width: 120),
    (Caption: 'ETA'; Width: 60));

{ TTransfersList }

constructor TTransfersList.Create(Parent: TWinControl);
var
  i: Integer;
begin
  inherited;
  FIcons := TImageList.Create;
  FIcons.AddMasked(LoadImage(hInstance, 'TLICONS', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  Style := Style and not LVS_SINGLESEL or LVS_SHOWSELALWAYS or LVS_EDITLABELS or LVS_NOSORTHEADER or LVS_SORTASCENDING; //TODO: switches for sorting & etc
  //ExStyle := ExStyle or WS_EX_STATICEDGE and not WS_EX_CLIENTEDGE;
  ViewStyle := LVS_REPORT;
  OptionsEx := OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  SmallImages := FIcons;
  for i := 0 to High(ListColumns) do
    with ListColumns[i] do
      ColumnAdd(Caption, Width);
  OnResize := Resize;
end;

destructor TTransfersList.Destroy;
begin
  FreeAndNil(FIcons);
  inherited;
end;

procedure TTransfersList.Resize(Sender: TObject);
begin
  //TODO: Column autosizing
end;

end.
