unit NDE.Frames.FolderConnection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls, ExtCtrls, StdCtrls
, NDE.Data.Connection
;

type
{ TfrmFolderConnection }
  TfrmFolderConnection = class(TFrame)
    imgNoso: TImage;
    lblConnectionName: TLabel;
    lblFolderPath: TLabel;
    panFolderDetails: TPanel;
    panFolderDetailsContainer: TPanel;
  private
    FConnection: TConnection;
  public
    procedure Init;

    property Connection: TConnection
      read FConnection
      write FConnection;
  end;

resourcestring
  rsConnectionCaption = 'Connection: %s';
  rsFolderCaption = 'Folder: %s';

implementation

{$R *.lfm}

{ TfrmFolderConnection }

procedure TfrmFolderConnection.Init;
begin
  if Assigned(FConnection) then
  begin
    lblConnectionName.Caption:= Format(rsConnectionCaption, [FConnection.Name]);
    lblFolderPath.Caption:= Format(rsFolderCaption, [FConnection.Folder]);
    // Initialize the blocks container
  end;
end;

end.

