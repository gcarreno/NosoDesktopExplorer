unit NDE.Forms.ConnectionManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs, ExtCtrls, StdCtrls
, NDE.Data.Connections
, NDE.Data.Connection
;

type
{ TConnectionsChangedNotify }
  TConnectionsChangedNotify = procedure(Sender: TObject) of object;

{ TfrmConnectionManager }
  TfrmConnectionManager = class(TForm)
    btnConnect: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    panButtons: TPanel;
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
  end;

var
  frmConnectionManager: TfrmConnectionManager;

resourcestring
  rsFormConnectionManagerCaption = 'Connection Manager';

implementation

{$R *.lfm}

{ TfrmConnectionManager }

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

