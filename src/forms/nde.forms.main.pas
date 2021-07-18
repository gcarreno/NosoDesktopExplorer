unit NDE.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  JSONPropStorage, StdActns;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    alMain: TActionList;
    actFileExit: TFileExit;
    jsonpsMain: TJSONPropStorage;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mmMain: TMainMenu;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure Init;
    procedure InitShortCuts;
    procedure ActivatePropStorage;
    procedure DeActivatePropStorage;
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

procedure TfrmMain.Init;
begin
  Caption := Format('%s %s', [rsFormCaption,cVersion]);
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

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= True;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Init;
  InitShortCuts;
  ActivatePropStorage;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DeActivatePropStorage;
end;

initialization
  //Application.on;
end.

