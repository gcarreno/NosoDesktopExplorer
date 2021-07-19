unit NDE.Forms.ConnectionManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs, ExtCtrls, StdCtrls, PairSplitter, ActnList
, NDE.Data.Connections
, NDE.Data.Connection
;

type
{ TConnectionsChangedNotify }
  TConnectionsChangedNotify = procedure(Sender: TObject) of object;

{ TfrmConnectionManager }
  TfrmConnectionManager = class(TForm)
    actConnectionsNew: TAction;
    actConnectionsDelete: TAction;
    actConnectionsConnect: TAction;
    alConnections: TActionList;
    btnConnect: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    btnConnectionsNew: TButton;
    btnConnectionsDelete: TButton;
    edtConnectionName: TEdit;
    edtConnectionFolder: TEdit;
    gbFolder: TGroupBox;
    gbJSONRPC: TGroupBox;
    gbWebAPI: TGroupBox;
    lblConnectionFolder: TLabel;
    lblConnectionName: TLabel;
    lbConnections: TListBox;
    panConnectionsButtons: TPanel;
    panCOnnectionDetails: TPanel;
    psConnectionsDetails: TPairSplitter;
    pssConnections: TPairSplitterSide;
    pssDetails: TPairSplitterSide;
    panFormButtons: TPanel;
    rgConnectionType: TRadioGroup;

    procedure alConnectionsUpdate(AAction: TBasicAction; var Handled: Boolean);

    procedure actConnectionsNewExecute(Sender: TObject);
    procedure actConnectionsDeleteExecute(Sender: TObject);
    procedure actConnectionsConnectExecute(Sender: TObject);

    procedure lbConnectionsSelectionChange(Sender: TObject; User: boolean);

    procedure rgConnectionTypeSelectionChanged(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConnections: TConnections;

    FOnConnectionsChange: TConnectionsChangedNotify;

  public
    property Connections: TConnections
      read FConnections
      write FConnections;
    property OnConnectionsChange: TConnectionsChangedNotify
      read FOnConnectionsChange
      write FOnConnectionsChange;

    procedure PopulateConnections;
  end;

var
  frmConnectionManager: TfrmConnectionManager;

resourcestring
  rsFormConnectionManagerCaption = 'Connection Manager';

implementation

{$R *.lfm}

{ TfrmConnectionManager }

procedure TfrmConnectionManager.PopulateConnections;
var
  connection: TConnection;
begin
  for connection in FConnections do
  begin
    lbConnections.Items.Add(connection.Name);
  end;
end;

procedure TfrmConnectionManager.alConnectionsUpdate(
  AAction: TBasicAction;
  var Handled: Boolean
);
begin
  panCOnnectionDetails.Visible:= lbConnections.ItemIndex > -1;
  if AAction = actConnectionsDelete then
  begin
    actConnectionsDelete.Enabled:= lbConnections.ItemIndex > -1;
    Handled:= True;
  end;
  if AAction = actConnectionsConnect then
  begin
    actConnectionsConnect.Enabled:= lbConnections.ItemIndex > -1;
    Handled:= True;
  end;
end;

procedure TfrmConnectionManager.actConnectionsNewExecute(Sender: TObject);
var
  connection: TConnection;
begin
  connection:= TConnection.Create;
  connection.Name:= 'New Connection';
  connection.ConnectionType:= ctFolder;
  connection.Folder:= '';
  FConnections.Add(connection);
  lbConnections.Items.Add(connection.Name);
end;

procedure TfrmConnectionManager.actConnectionsDeleteExecute(Sender: TObject);
var
  deleteIndex: Integer;
begin
  deleteIndex:= lbConnections.ItemIndex;
  if deleteIndex > -1 then
  begin
    lbConnections.Items.Delete(deleteIndex);
    FConnections.Delete(deleteIndex);
  end;
end;

procedure TfrmConnectionManager.actConnectionsConnectExecute(Sender: TObject);
begin
  { #todo 100 -ogcarreno : Implement the connection code on the Connection manager }
end;

procedure TfrmConnectionManager.lbConnectionsSelectionChange(
  Sender: TObject;
  User: boolean
);
begin
  if lbConnections.ItemIndex > -1 then
  begin
    edtConnectionName.Text:= FConnections[lbConnections.ItemIndex].Name;
    rgConnectionType.ItemIndex:=
      Ord(FConnections[lbConnections.ItemIndex].ConnectionType)-1;
    if FConnections[lbConnections.ItemIndex].ConnectionType = ctFolder then
    begin
      edtConnectionFolder.Text:= FConnections[lbConnections.ItemIndex].Folder;
    end;
  end;
end;

procedure TfrmConnectionManager.rgConnectionTypeSelectionChanged(
  Sender: TObject
);
begin
  if rgConnectionType.ItemIndex > -1 then
  begin
    case rgConnectionType.ItemIndex of
      0:begin
        gbFolder.Visible:= True;
        gbFolder.BringToFront;
        gbJSONRPC.Visible:= False;
        gbWebAPI.Visible:= False;
      end;
      1:begin
        gbJSONRPC.Visible:= True;
        gbJSONRPC.BringToFront;
        gbFolder.Visible:= False;
        gbWebAPI.Visible:= False;
      end;
      2:begin
        gbWebAPI.Visible:= True;
        gbWebAPI.BringToFront;
        gbFolder.Visible:= False;
        gbJSONRPC.Visible:= False;
      end;
    end;
  end;
end;

procedure TfrmConnectionManager.FormCreate(Sender: TObject);
begin
  FConnections:= nil;
  FOnConnectionsChange:= nil;
  Caption:= rsFormConnectionManagerCaption;
end;

procedure TfrmConnectionManager.FormCloseQuery(
  Sender: TObject;
  var CanClose: Boolean
);
begin
  CanClose:= True;
end;

procedure TfrmConnectionManager.FormClose(
  Sender: TObject;
  var CloseAction: TCloseAction
);
begin
  CloseAction:= caFree;
end;

procedure TfrmConnectionManager.FormDestroy(Sender: TObject);
begin
  FOnConnectionsChange:= nil;
  FConnections:= nil;
end;

end.

