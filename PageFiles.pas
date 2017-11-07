unit PageFiles;

interface

uses
  Windows, Messages, AvL, avlListViewEx, InfoPane, Aria2, UpdateThread, MainForm;

type
  TPageFiles = class(TInfoPage)
  private
    FFilesColumns: TListColumns;
    FilesList: TListViewEx;
    procedure Resize(Sender: TObject);
  protected
    function GetName: string; override;
    procedure SetGID(Value: TAria2GID); override;
  public
    constructor Create(Parent: TInfoPane); override;
    destructor Destroy; override;
    procedure Update(UpdateThread: TUpdateThread); override;
    procedure SaveSettings; override;
  end;

const
  SFilesColumns = 'FilesListColumns';

implementation

const
  DefFilesColumns: array[0..3] of TListColumn = (
    (Caption: 'Name'; Width: 500; FType: ftPath; Field: sfPath),
    (Caption: 'Size'; Width: 80; FType: ftSize; Field: sfLength),
    (Caption: 'Progress'; Width: 60; FType: ftPercent; Field: sfCompletedLength + ':' + sfLength),
    (Caption: 'Selected'; Width: 60; FType: ftString; Field: sfSelected));

{ TPageFiles }

constructor TPageFiles.Create(Parent: TInfoPane);
begin
  inherited;
  SetArray(FUpdateKeys, [sfGID]);
  FilesList := TListViewEx.Create(Self);
  FilesList.SetPosition(0, 0);
  FilesList.Style := FilesList.Style and not LVS_SINGLESEL or LVS_SHOWSELALWAYS or LVS_SORTASCENDING {or LVS_EDITLABELS} or LVS_NOSORTHEADER; //TODO: switches for sorting & etc
  FilesList.ViewStyle := LVS_REPORT;
  FilesList.OptionsEx := FilesList.OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  //FilesList.SmallImages := FTransferIcons;
  (Parent.Parent as TMainForm).LoadListColumns(FilesList, SFilesColumns, FFilesColumns, DefFilesColumns, nil);
  OnResize := Resize;
end;

destructor TPageFiles.Destroy;
begin
  Finalize(FFilesColumns);
  inherited;
end;

function TPageFiles.GetName: string;
begin
  Result := 'Files';
end;

procedure TPageFiles.Resize(Sender: TObject);
begin
  FilesList.SetSize(ClientWidth, ClientHeight);
end;

procedure TPageFiles.SaveSettings;
begin
  inherited;
  FormMain.SaveListColumns(FilesList, SFilesColumns, FFilesColumns, DefFilesColumns);
end;

procedure TPageFiles.SetGID(Value: TAria2GID);
begin
  inherited;
  SetArray(FUpdateKeys, [sfDir, sfFiles]);
end;

procedure TPageFiles.Update(UpdateThread: TUpdateThread);
var
  i: Integer;
begin
  with UpdateThread do
  begin
    if not Assigned(Info) or not Info.Has[sfFiles] then Exit;
    SetArray(FUpdateKeys, [sfGID]);
    FilesList.BeginUpdate;
    try
      FilesList.Clear;
      for i := 0 to Info.Length[sfFiles] - 1 do
        FilesList.ItemAdd(Info[sfFiles + '.' + IntToStr(i) + '.' + sfPath]);
    finally
      FilesList.EndUpdate;
    end;
  end;
end;

end.
