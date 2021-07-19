unit NDE.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs
, Menus
, ActnList
, JSONPropStorage
, StdActns
, NDE.Forms.ConnectionManager
, NDE.Data.Connections
, NDE.Data.Connection
;

type
{ TfrmMain }
  TfrmMain = class(TForm)
    actFileConnectionManager: TAction;
    alMain: TActionList;
    actFileExit: TFileExit;
    jsonpsMain: TJSONPropStorage;
    MenuItem1: TMenuItem;
    mnuFileSep1: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mmMain: TMainMenu;
    procedure actFileConnectionManagerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    FConnections: TConnections;
    FFormConnectionManager: TfrmConnectionManager;

    procedure Initialize;
    procedure Finalize;
    procedure InitShortCuts;
    procedure ActivatePropStorage;
    procedure DeActivatePropStorage;

    procedure ConnectionsChanged(Sender: TObject);
  public
  published
  end;

var
  frmMain: TfrmMain;

const
  cVersion = 'v0.1.0.1';

resourcestring
  rsFormCaption = 'Noso Desktop Explorer';

implementation

uses
  LCLType
;

{$R *.lfm}

{ TfrmMain }

function GetApplicationName: String;
begin
  Result:= 'nosodesktopexplorer';
end;

procedure TfrmMain.Initialize;
var
  connection: TConnection = nil;
begin
  OnGetApplicationName:= @GetApplicationName;
  Caption := Format('%s %s', [rsFormCaption,cVersion]);
  FConnections:= TConnections.Create;
  FFormConnectionManager:= nil;
  {### REMOVE ###}
  connection:= TConnection.Create;
  connection.Name:= 'GCarreno Main';
  connection.ConnectionType:= ctFolder;
  connection.Folder:= '/home/gcarreno/Applications/Noso/Wallet/NOSODATA';
  FConnections.Add(connection);
  {### REMOVE ###}
end;

procedure TfrmMain.Finalize;
begin
  FConnections.Free;
end;

procedure TfrmMain.InitShortCuts;
begin
{$IFDEF LINUX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.ActivatePropStorage;
begin
{$IFDEF WINDOWS}
  if not DirectoryExists(GetAppConfigDir(False)) then
  begin
    ForceDirectories(GetAppConfigDir(False));
  end;
{$ENDIF}
  jsonpsMain.JSONFileName:= GetAppConfigFile(False);
  jsonpsMain.Active:= True;
end;

procedure TfrmMain.DeActivatePropStorage;
begin
  jsonpsMain.Save;
  jsonpsMain.Active:= False;
end;

procedure TfrmMain.ConnectionsChanged(Sender: TObject);
begin
  //
end;

procedure TfrmMain.actFileConnectionManagerExecute(Sender: TObject);
var
  mResult: Integer;
begin
  actFileConnectionManager.Enabled:= False;
  Application.ProcessMessages;
  try
    FFormConnectionManager:= TfrmConnectionManager.Create(Self);
    FFormConnectionManager.Connections:= FConnections;
    FFormConnectionManager.OnConnectionsChange:= @ConnectionsChanged;
    FFormConnectionManager.PopulateConnections;
    mResult:= FFormConnectionManager.ShowModal;
  finally
    Application.ProcessMessages;
    actFileConnectionManager.Enabled:= True;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Initialize;
  InitShortCuts;
  ActivatePropStorage;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DeActivatePropStorage;
  Finalize;
end;

end.

