unit NDE.Frames.FolderConnection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, ExtCtrls
, StdCtrls
, NDE.Data.Connection
, Noso.Data.Legacy.Blocks
;

type
{ TfrmFolderConnection }
  TfrmFolderConnection = class(TFrame)
    imgNoso: TImage;
    lblBlockHeight: TLabel;
    lblConnectionName: TLabel;
    lblFolderPath: TLabel;
    panFolderDetails: TPanel;
    panFolderDetailsContainer: TPanel;
  private
    FConnection: TConnection;
    FBlocks: TLegacyBlocks;
  public
    procedure Initialize;
    procedure Finalize;

    property Connection: TConnection
      read FConnection
      write FConnection;
  end;

resourcestring
  rsConnectionCaption = 'Connection: %s';
  rsFolderCaption = 'Folder: %s';
  rsBlockHeightCaption = 'Block Height: %d (0..%d)';

implementation

{$R *.lfm}

{ TfrmFolderConnection }

procedure TfrmFolderConnection.Initialize;
begin
  FBlocks:= nil;
  if Assigned(FConnection) then
  begin
    lblConnectionName.Caption:= Format(rsConnectionCaption, [FConnection.Name]);
    lblFolderPath.Caption:= Format(rsFolderCaption, [FConnection.Folder]);
    FBlocks:= TLegacyBlocks.Create;
    FBlocks.Folder:= FConnection.Folder;
    FBlocks.Refresh;
    if FBlocks.Count > 0 then
    begin
      lblBlockHeight.Caption:= Format(rsBlockHeightCaption, [FBlocks.Count, Pred(FBlocks.Count)]);
    end;
  end;
end;

procedure TfrmFolderConnection.Finalize;
begin
  FBlocks.Free;
end;

end.

