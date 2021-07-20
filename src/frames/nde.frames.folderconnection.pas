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
, ComCtrls
, Spin
, PairSplitter
, NDE.Data.Connection
, Noso.Data.Legacy.Blocks
, Noso.Data.Legacy.Block
;

type
{ TfrmFolderConnection }
  TfrmFolderConnection = class(TFrame)
    btnFirst: TButton;
    btnLast: TButton;
    edtBlockTimeEnd: TEdit;
    edtBlockHash: TEdit;
    edtBlockTimeStart: TEdit;
    edtBlockTimeTotal: TEdit;
    imgNoso: TImage;
    lblBlockTimeEnd: TLabel;
    lblBlockNumber: TLabel;
    lblBlockHash: TLabel;
    lblBlock: TLabel;
    lblBlockHeight: TLabel;
    lblBlockTimeTotal: TLabel;
    lblConnectionName: TLabel;
    lblFolderPath: TLabel;
    lblBlockTimeStart: TLabel;
    psBlockTransactions: TPairSplitter;
    pssBlock: TPairSplitterSide;
    pssTransactions: TPairSplitterSide;
    pcFolderConnectionDetails: TPageControl;
    panBlockDetails: TPanel;
    panFolderDetails: TPanel;
    panFolderDetailsContainer: TPanel;
    edtBlock: TSpinEdit;
    sbBlock: TScrollBox;
    tsBlocks: TTabSheet;
    procedure btnFirstClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure edtBlockChange(Sender: TObject);
  private
    FConnection: TConnection;
    FBlocks: TLegacyBlocks;
    FBlock: TLegacyBlock;

    procedure PopulateBlockDetails;
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
  rsBlockNumber = 'Number: %d';

implementation

uses
  DateUtils
;

{$R *.lfm}

{ TfrmFolderConnection }

procedure TfrmFolderConnection.edtBlockChange(Sender: TObject);
begin
  if Assigned(FBlock) then
  begin
    FreeAndNil(FBlock);
  end;
  FBlock:= FBlocks[edtBlock.Value];
  if Assigned(FBlock) then
  begin
    PopulateBlockDetails;
  end;
end;

procedure TfrmFolderConnection.btnFirstClick(Sender: TObject);
begin
  edtBlock.Value:= 0;
end;

procedure TfrmFolderConnection.btnLastClick(Sender: TObject);
begin
  edtBlock.Value:= FBlocks.Count - 1;
end;

procedure TfrmFolderConnection.PopulateBlockDetails;
begin
  if Assigned(FBlock) then
  begin
    lblBlockNumber.Caption:= Format(rsBlockNumber, [FBlock.Number]);
    edtBlockHash.Text:= FBlock.Hash;
    edtBlockTimeEnd.Text:= DateTimeToStr(UnixToDateTime(FBlock.TimeEnd));
    edtBlockTimeStart.Text:= DateTimeToStr(UnixToDateTime(FBlock.TimeStart));
    edtBlockTimeTotal.Text:= FormatDateTime('hh:nn:ss', FBlock.TimeTotal / SecsPerDay);
  end;
end;

procedure TfrmFolderConnection.Initialize;
begin
  FBlocks:= nil;
  FBlock:= nil;
  if Assigned(FConnection) then
  begin
    lblConnectionName.Caption:= Format(rsConnectionCaption, [FConnection.Name]);
    lblFolderPath.Caption:= Format(rsFolderCaption, [FConnection.Folder]);
    FBlocks:= TLegacyBlocks.Create(FConnection.Folder);
    if FBlocks.Count > 0 then
    begin
      lblBlockHeight.Caption:= Format(rsBlockHeightCaption, [FBlocks.Count, Pred(FBlocks.Count)]);
    end;
    edtBlock.MinValue:= 0;
    edtBlock.MaxValue:= FBlocks.Count - 1;
    edtBlock.Value:= edtBlock.MaxValue;
  end;
end;

procedure TfrmFolderConnection.Finalize;
begin
  FBlock.Free;
  FBlocks.Free;
end;

end.

