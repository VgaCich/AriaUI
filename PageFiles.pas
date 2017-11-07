unit PageFiles;

//TODO: context menu (+refresh) and hotkeys, file selection, file renaming, sorting

interface

uses
  Windows, Messages, AvL, avlUtils, avlListViewEx, InfoPane, Aria2, UpdateThread, MainForm;

type
  TPageFiles = class(TInfoPage)
  private
    FFilesColumns: TListColumns;
    FFilesIcons: TImageList;
    FDir: string;
    FilesList: TListViewEx;
    procedure Refresh;
    procedure Resize(Sender: TObject);
    procedure FilesDblClick(Sender: TObject);
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
  DefFilesColumns: array[0..4] of TListColumn = (
    (Caption: 'Name'; Width: 500; FType: ftPath; Field: sfPath),
    (Caption: 'Index'; Width: 50; FType: ftString; Field: sfIndex),
    (Caption: 'Size'; Width: 80; FType: ftSize; Field: sfLength),
    (Caption: 'Progress'; Width: 60; FType: ftPercent; Field: sfCompletedLength + ':' + sfLength),
    (Caption: 'Selected'; Width: 60; FType: ftString; Field: sfSelected));

{ TPageFiles }

constructor TPageFiles.Create(Parent: TInfoPane);
begin
  inherited;
  SetArray(FUpdateKeys, [sfGID]);
  FFilesIcons := TImageList.Create;
  FFilesIcons.LoadSystemIcons(true);
  FilesList := TListViewEx.Create(Self);
  FilesList.SetPosition(0, 0);
  FilesList.Style := FilesList.Style and not LVS_SINGLESEL or LVS_SHOWSELALWAYS or LVS_SORTASCENDING {or LVS_EDITLABELS} or LVS_NOSORTHEADER; //TODO: switches for sorting & etc
  FilesList.ViewStyle := LVS_REPORT;
  FilesList.OptionsEx := FilesList.OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  FilesList.SmallImages := FFilesIcons;
  FilesList.OnDblClick := FilesDblClick;
  (Parent.Parent as TMainForm).LoadListColumns(FilesList, SFilesColumns, FFilesColumns, DefFilesColumns, nil);
  OnResize := Resize;
end;

destructor TPageFiles.Destroy;
begin
  Finalize(FFilesColumns);
  FFilesIcons.Handle := 0;
  FreeAndNil(FFilesIcons);
  inherited;
end;

function TPageFiles.GetName: string;
begin
  Result := 'Files';
end;

procedure TPageFiles.Refresh;
begin
  SetArray(FUpdateKeys, [sfDir, sfFiles]);
end;

procedure TPageFiles.Resize(Sender: TObject);
begin
  FilesList.SetSize(ClientWidth, ClientHeight);
end;

procedure TPageFiles.FilesDblClick(Sender: TObject);
begin
  if FileExists(AddTrailingBackslash(FDir) + FilesList.SelectedCaption) then
    Execute(AddTrailingBackslash(FDir) + FilesList.SelectedCaption)
  else
    MessageDlg('File "' + FilesList.SelectedCaption + '" not found', 'Error', MB_ICONERROR);
end;

procedure TPageFiles.SaveSettings;
begin
  inherited;
  FormMain.SaveListColumns(FilesList, SFilesColumns, FFilesColumns, DefFilesColumns);
end;

procedure TPageFiles.SetGID(Value: TAria2GID);
begin
  inherited;
  Refresh;
end;

procedure TPageFiles.Update(UpdateThread: TUpdateThread);

  function GetValue(Column: Integer): string;
  begin
    with FFilesColumns[Column] do
      Result := GetFieldValue(UpdateThread.Info, nil, FType, Field);
    if SameText(FDir, Copy(Result, 1, Length(FDir))) then
    begin
      Delete(Result, 1, Length(FDir));
      if (Result <> '') and (Result[1] = '\') then
        Delete(Result, 1, 1);
    end;
  end;

var
  i, j, Item: Integer;
begin
  with UpdateThread do
  begin
    if not Assigned(Info) or not Info.Has[sfFiles] then Exit;
    SetArray(FUpdateKeys, [sfGID]);
    FilesList.BeginUpdate;
    try
      FilesList.Clear;
      FDir := StringReplace(Info[sfDir], '/', '\', [rfReplaceAll]);
      Info.Root := sfFiles;
      for i := 0 to Info.Length[sfFiles] - 1 do
      begin
        Info.Index := i;
        Item := FilesList.ItemAdd(GetValue(0));
        FilesList.ItemImageIndex[Item] := FileIconIndex(ExtractFileName(Info[sfPath]), false);
        for j := 1 to High(FFilesColumns) do
          FilesList.Items[Item, j] := GetValue(j);
      end;
    finally
      FilesList.EndUpdate;
    end;
  end;
end;

end.
