unit PageFiles;

//TODO: hotkeys, file renaming, sorting

interface

uses
  Windows, Messages, AvL, avlUtils, avlListViewEx, InfoPane, Aria2, UpdateThread, MainForm;

type
  TChangeSelection = (csAdd, csRemove, csInvert);
  TPageFiles = class(TInfoPage)
  private
    FFilesColumns: TListColumns;
    FFilesIcons: TImageList;
    FDir: string;
    FilesList: TListViewEx;
    FilesMenu: TMenu;
    procedure ChangeSelection(Action: TChangeSelection);
    procedure Refresh;
    procedure Resize(Sender: TObject);
    procedure FilesDblClick(Sender: TObject);
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
  protected
    function GetName: string; override;
    procedure SetGID(Value: TAria2GID); override;
  public
    constructor Create(Parent: TInfoPane); override;
    destructor Destroy; override;
    procedure Update(UpdateThread: TUpdateThread); override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
  end;

const
  SFilesColumns = 'FilesListColumns';

implementation

type
  TMenuID = (IDMenuFiles = 20000, IDRefresh, IDSelectAll, IDFilesSep0, IDOpen, IDOpenFolder, IDFilesSep1, IDSelect, IDDeselect, IDInvertSelection);

const
  DefFilesColumns: array[0..4] of TListColumn = (
    (Caption: 'Name'; Width: 500; FType: ftPath; Field: sfPath),
    (Caption: 'Index'; Width: 50; FType: ftString; Field: sfIndex),
    (Caption: 'Size'; Width: 80; FType: ftSize; Field: sfLength),
    (Caption: 'Progress'; Width: 60; FType: ftPercent; Field: sfCompletedLength + ':' + sfLength),
    (Caption: 'Selected'; Width: 60; FType: ftString; Field: sfSelected));
  MenuFiles: array[0..9] of PChar = ('20001',
    '&Refresh',
    'Select &all',
    '-',
    '&Open',
    'Open &folder',
    '-',
    '&Select',
    '&Deselect',
    '&Invert selection');

{ TPageFiles }

constructor TPageFiles.Create(Parent: TInfoPane);
begin
  inherited;
  SetArray(FUpdateKeys, [sfGID]);
  FilesMenu := TMenu.Create(Self, false, MenuFiles);
  FFilesIcons := TImageList.Create;
  FFilesIcons.LoadSystemIcons(true);
  FilesList := TListViewEx.Create(Self);
  FilesList.SetPosition(0, 0);
  FilesList.Style := FilesList.Style and not LVS_SINGLESEL or LVS_SHOWSELALWAYS or LVS_SORTASCENDING {or LVS_EDITLABELS} or LVS_NOSORTHEADER; //TODO: switches for sorting & etc
  FilesList.ViewStyle := LVS_REPORT;
  FilesList.OptionsEx := FilesList.OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  FilesList.SmallImages := FFilesIcons;
  FilesList.OnDblClick := FilesDblClick;
  OnResize := Resize;
end;

destructor TPageFiles.Destroy;
begin
  Finalize(FFilesColumns);
  FFilesIcons.Handle := 0;
  FreeAndNil(FFilesIcons);
  inherited;
end;

procedure TPageFiles.ChangeSelection(Action: TChangeSelection);
var
  i, Start: Integer;
  Selection: array of Boolean;
  Files: TAria2Struct;
  Option: TAria2Option;
begin
  Files := (FParent.Parent as TMainForm).Aria2.GetFiles(FGID);
  try
    SetLength(Selection, Files.Length['']);
    try
      for i := 0 to High(Selection) do
      begin
        Files.Index := i;
        Selection[i] := Boolean(StrToEnum(Files[sfSelected], sfBoolValues));
      end;
      if Action in [csAdd, csRemove] then
        for i := 0 to FilesList.SelCount - 1 do
          Selection[Integer(FilesList.ItemObject[FilesList.Selected[i]])] := Action = csAdd
      else
        for i := 0 to High(Selection) do
          Selection[i] := not Selection[i];
      Option.Key := soSelectFile;
      Start := -1;
      for i := 0 to High(Selection) do
        if Selection[i] and (Start < 0)then
          Start := i
        else if not Selection[i] and (Start >= 0) then
        begin
          if i = Start - 1 then
            Option.Value := Option.Value + IntToStr(i) + ','
          else
            Option.Value := Option.Value + IntToStr(Start + 1) + '-' + IntToStr(i) + ',';
          Start := -1;
        end;
      if Start >= 0 then
        Option.Value := Option.Value + IntToStr(Start + 1) + '-' + IntToStr(Length(Selection)) + ',';
      if Option.Value <> '' then
        Delete(Option.Value, Length(Option.Value), 1);
      (FParent.Parent as TMainForm).Aria2.ChangeOptions(FGID, [Option]);
      Refresh;
    finally
      Finalize(Selection);
    end;
  finally
    FreeAndNil(Files);
  end;
end;

procedure TPageFiles.Refresh;
begin
  SetArray(FUpdateKeys, [sfGID, sfDir, sfFiles]);
end;

procedure TPageFiles.Resize(Sender: TObject);
begin
  FilesList.SetSize(ClientWidth, ClientHeight);
end;

procedure TPageFiles.FilesDblClick(Sender: TObject);
begin
  Perform(WM_COMMAND, MakeWParam(Ord(IDOpen), 0), 0);
end;

procedure TPageFiles.WMCommand(var Msg: TWMCommand);
var
  Dir: string;
begin
  if (Msg.Ctl = 0) and (Msg.NotifyCode in [0, 1]) then
    try
      case TMenuID(Msg.ItemID) of
        IDRefresh: Refresh;
        IDSelectAll: FilesList.SelectAll;
        IDOpen: if FileExists(AddTrailingBackslash(FDir) + FilesList.SelectedCaption) then
                  Execute(AddTrailingBackslash(FDir) + FilesList.SelectedCaption)
                else
                  Exception.Create('File "' + FilesList.SelectedCaption + '" not found');
        IDOpenFolder:
        begin
          Dir := ExtractFilePath(AddTrailingBackslash(FDir) + FilesList.SelectedCaption);
          if DirectoryExists(Dir) then
            Execute(Dir)
          else
            Exception.Create('Directory "' + Dir + '" not found');
        end;
        IDSelect: ChangeSelection(csAdd);
        IDDeselect: ChangeSelection(csRemove);
        IDInvertSelection: ChangeSelection(csInvert);
      end;
    except
      on E: Exception do ShowException;
    end;
end;

procedure TPageFiles.WMContextMenu(var Msg: TWMContextMenu);
begin
  if Assigned(FilesList) and (Msg.hWnd = FilesList.Handle) then
    FilesMenu.Popup(Msg.XPos, Msg.YPos);
end;

function TPageFiles.GetName: string;
begin
  Result := 'Files';
end;

procedure TPageFiles.SetGID(Value: TAria2GID);
begin
  inherited;
  Refresh;
end;

procedure TPageFiles.LoadSettings;
begin
  inherited;
  (Parent.Parent as TMainForm).LoadListColumns(FilesList, SFilesColumns, FFilesColumns, DefFilesColumns, nil);
end;

procedure TPageFiles.SaveSettings;
begin
  inherited;
  FormMain.SaveListColumns(FilesList, SFilesColumns, FFilesColumns, DefFilesColumns);
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
    if not Assigned(Info) or not Info.Has[sfFiles] or (Info[sfGID] <> FGID) then Exit;
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
        FilesList.ItemObject[Item] := TObject(StrToInt(Info[sfIndex]) - 1);
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
