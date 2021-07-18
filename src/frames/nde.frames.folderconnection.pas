unit NDE.Frames.FolderConnection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, NDE.Data.Connection
;

type
{ TfrmFolderConnection }
  TfrmFolderConnection = class(TFrame)
  private
    FConnection: TConnection;
  public
    procedure Init;

    property Connection: TConnection
      read FConnection
      write FConnection;
  end;

implementation

{$R *.lfm}

{ TfrmFolderConnection }

procedure TfrmFolderConnection.Init;
begin
  //
end;

end.

