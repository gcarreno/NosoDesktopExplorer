unit TestNosoDesktopExplorerDataConnections;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
//, testutils
, testregistry
, NDE.Data.Connections
;

type
{ TTestNosoDesktopExplorerConnections }
  TTestNosoDesktopExplorerConnections= class(TTestCase)
  private
    FConnections: TConnections;

    procedure CheckFieldsCreate;
    procedure CheckFieldsOneFolderConnection;
  protected
  public
  published
    procedure TestNosoDesktopExplorerConnectionsCreate;

    procedure TestNosoDesktopExplorerConnectionsFromJSON;
    procedure TestNosoDesktopExplorerConnectionsFromJSONData;
    procedure TestNosoDesktopExplorerConnectionsFromJSONArray;
    procedure TestNosoDesktopExplorerConnectionsFromStream;

    procedure TestNosoDesktopExplorerConnectionsAsJSON;
    procedure TestNosoDesktopExplorerConnectionsAsJSONData;
    procedure TestNosoDesktopExplorerConnectionsAsJSONArray;
    procedure TestNosoDesktopExplorerConnectionsAsStream;

  end;

implementation

uses
  fpjson
, jsonparser
, NDE.Data.Connection
;

const
  cjConnectionsOneFolderConnection =
    '[{'+
      '"'+cjConnectionType+'":1,'+
      '"'+cjName+'":"Folder Connection",'+
      '"'+cjFolder+'":"/home/user/NosoWallet/NOSODATA",'+
      '"'+cjHost+'":"",'+
      '"'+cjPort+'":-1,'+
      '"'+cjAddress+'":"",'+
      '"'+cjBasePath+'":""'+
    '}]';

procedure TTestNosoDesktopExplorerConnections.CheckFieldsCreate;
begin
  AssertEquals('Noso Data Explorer Connections Count is 0', 0, FConnections.Count);
end;

procedure TTestNosoDesktopExplorerConnections.CheckFieldsOneFolderConnection;
begin
  AssertEquals('Noso Data Explorer Connections Count is 1', 1, FConnections.Count);
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsCreate;
begin
  FConnections:= TConnections.Create;
  CheckFieldsCreate;
  FConnections.Free;
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsFromJSON;
begin
  FConnections:= TConnections.Create(cjConnectionsOneFolderConnection);
  CheckFieldsOneFolderConnection;
  FConnections.Free;
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsFromJSONData;
var
  jData: TJSONData = nil;
begin
  jData:= GetJSON(cjConnectionsOneFolderConnection);
  FConnections:= TConnections.Create(jData);
  jData.Free;
  CheckFieldsOneFolderConnection;
  FConnections.Free;
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsFromJSONArray;
var
  jData: TJSONData = nil;
begin
  jData:= GetJSON(cjConnectionsOneFolderConnection);
  FConnections:= TConnections.Create(jData as TJSONArray);
  jData.Free;
  CheckFieldsOneFolderConnection;
  FConnections.Free;
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsFromStream;
var
  ssConnectionsArray: TJSONData = nil;
begin
  ssConnectionsArray:= GetJSON(cjConnectionsOneFolderConnection);
  FConnections:= TConnections.Create(ssConnectionsArray);
  ssConnectionsArray.Free;
  CheckFieldsOneFolderConnection;
  FConnections.Free;
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsAsJSON;
begin
  FConnections:= TConnections.Create(cjConnectionsOneFolderConnection);
  AssertEquals('Noso Desktop Explorer Connections AsJSON matches',
    cjConnectionsOneFolderConnection,
    FConnections.AsJSON
  );
  FConnections.Free;
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsAsJSONData;
var
  jData: TJSONData = nil;
begin
  FConnections:= TConnections.Create(cjConnectionsOneFolderConnection);
  jData:= FConnections.AsJSONData;
  AssertEquals('Noso Desktop Explorer Connections AsJSONData matches',
    cjConnectionsOneFolderConnection,
    jData.AsJSON
  );
  jData.Free;
  FConnections.Free;
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsAsJSONArray;
var
  jArray: TJSONArray = nil;
begin
  FConnections:= TConnections.Create(cjConnectionsOneFolderConnection);
  jArray:= FConnections.AsJSONArray;
  AssertEquals('Noso Desktop Explorer Connections AsJSONArray matches',
    cjConnectionsOneFolderConnection,
    jArray.AsJSON
  );
  jArray.Free;
  FConnections.Free;
end;

procedure TTestNosoDesktopExplorerConnections.TestNosoDesktopExplorerConnectionsAsStream;
var
  ssConnections: TStringStream = nil;
  sConnections: TStream = nil;
begin
  FConnections:= TConnections.Create(cjConnectionsOneFolderConnection);
  ssConnections:= TStringStream.Create('', TEncoding.UTF8);
  sConnections:= FConnections.AsStream;
  ssConnections.LoadFromStream(sConnections);
  sConnections.Free;
  AssertEquals('Noso Desktop Explorer Connections AsJSONArray matches',
    cjConnectionsOneFolderConnection,
    ssConnections.DataString
  );
  ssConnections.Free;
  FConnections.Free;
end;

initialization
  RegisterTest(TTestNosoDesktopExplorerConnections);
end.

