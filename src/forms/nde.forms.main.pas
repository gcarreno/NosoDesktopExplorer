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
, StdActns, ComCtrls
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
    mnuFileCOnnectionManager: TMenuItem;
    mnuFileSep1: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mmMain: TMainMenu;
    pcConnections: TPageControl;
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

    procedure ConnectToFolder(const AConnectionIndex: Integer);
    procedure ConnectToJSONRPC(const AConnectionIndex: Integer);
    procedure ConnectToWebAPI(const AConnectionIndex: Integer);

    procedure OnConnect(ConnectionIndex: Integer);
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
, NDE.Frames.FolderConnection
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
  {$IFDEF Linux}
  connection.Folder:= '/home/gcarreno/Applications/Noso/Wallet';
  {$ENDIF}
  {$IFDEF Windows}
  connection.Folder:= 'D:\Applications\Noso\Wallet';
  {$ENDIF}
  FConnections.Add(connection);
  {### REMOVE ###}
end;

procedure TfrmMain.Finalize;
var
  connection: TConnection = nil;
begin
  for connection in FConnections do
  begin
    if connection.Connected then
    begin
      if connection.ConnectionType = ctFolder then
      begin
        TfrmFolderConnection(connection.Frame).Finalize;
      end;
    end;
  end;
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

procedure TfrmMain.ConnectToFolder(const AConnectionIndex: Integer);
var
  tsConnection: TTabSheet;
  frameConnection: TFrame;
begin
  if not FConnections[AConnectionIndex].Connected then
  begin
    tsConnection:= pcConnections.AddTabSheet;
    tsConnection.Caption:= Format('%s (Folder)', [FConnections[AConnectionIndex].Name]);

    frameConnection:= TfrmFolderConnection.Create(tsConnection);
    frameConnection.Parent:= tsConnection;

    TfrmFolderConnection(frameConnection).Connection:= FConnections[AConnectionIndex];
    TfrmFolderConnection(frameConnection).Initialize;

    FConnections[0].Frame:= frameConnection;
    FConnections[0].Connected:= True;
  end;
end;

procedure TfrmMain.ConnectToJSONRPC(const AConnectionIndex: Integer);
var
  tsConnection: TTabSheet;
begin
  if not FConnections[AConnectionIndex].Connected then
  begin
    tsConnection:= pcConnections.AddTabSheet;
    tsConnection.Caption:= Format('%s (JSON-RPC)', [FConnections[AConnectionIndex].Name]);
  end;
end;

procedure TfrmMain.ConnectToWebAPI(const AConnectionIndex: Integer);
var
  tsConnection: TTabSheet;
begin
  if not FConnections[AConnectionIndex].Connected then
  begin
    tsConnection:= pcConnections.AddTabSheet;
    tsConnection.Caption:= Format('%s (Web API)', [FConnections[AConnectionIndex].Name]);
  end;
end;

procedure TfrmMain.OnConnect(ConnectionIndex: Integer);
begin
  if FConnections[ConnectionIndex].ConnectionType = ctFolder then
  begin
    ConnectToFolder(ConnectionIndex);
  end;
  if FConnections[ConnectionIndex].ConnectionType = ctJSONRPC then
  begin
    ConnectToJSONRPC(ConnectionIndex);
  end;
  if FConnections[ConnectionIndex].ConnectionType = ctWebAPI then
  begin
    ConnectToWebAPI(ConnectionIndex);
  end;
end;

procedure TfrmMain.actFileConnectionManagerExecute(Sender: TObject);
//var
//  mResult: Integer;
begin
  actFileConnectionManager.Enabled:= False;
  Application.ProcessMessages;
  try
    FFormConnectionManager:= TfrmConnectionManager.Create(Self);
    FFormConnectionManager.Connections:= FConnections;
    FFormConnectionManager.OnConnectionsConnect:= @OnConnect;
    FFormConnectionManager.PopulateConnections;
    //mResult:= FFormConnectionManager.ShowModal;
    FFormConnectionManager.ShowModal;
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

