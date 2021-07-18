unit TestNosoDesktopExplorerDataConnection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
//, testutils
, testregistry
, NDE.Data.Connection
;

type
{ TTestNosoDesktopExplorerConnection }
  TTestNosoDesktopExplorerConnection= class(TTestCase)
  private
    FConnection: TConnection;

    procedure CheckFieldsCreate;
    procedure CheckFieldsFromConnectionFolder;
    procedure CheckFieldsFromConnectionJSONRPC;
    procedure CheckFieldsFromConnectionWebAPI;
  protected
  public
  published
    procedure TestNosoDesktopExplorerConnectionCreate;
    procedure TestNosoDesktopExplorerConnectionChangeType;
    procedure TestNosoDesktopExplorerConnectionFromConnectionFolder;
    procedure TestNosoDesktopExplorerConnectionFromConnectionJSONRPC;
    procedure TestNosoDesktopExplorerConnectionFromConnectionWebAPI;

    procedure TestNosoDesktopExplorerConnectionAsJSON;
    procedure TestNosoDesktopExplorerConnectionAsJSONData;
    procedure TestNosoDesktopExplorerConnectionAsJSONObject;
    procedure TestNosoDesktopExplorerConnectionAsStream;
  end;

implementation

uses
  fpjson
, jsonparser
;

const
  cjConnectionFolder =
    '{'+
      '"'+cjConnectionType+'":1,'+
      '"'+cjName+'":"Folder Connection",'+
      '"'+cjFolder+'":"/home/user/NosoWallet/NOSODATA",'+
      '"'+cjHost+'":"",'+
      '"'+cjPort+'":-1,'+
      '"'+cjAddress+'":"",'+
      '"'+cjBasePath+'":""'+
    '}';

  cjConnectionJSONRPC =
    '{'+
      '"'+cjConnectionType+'":2,'+
      '"'+cjName+'":"JSON-RPC Connection",'+
      '"'+cjFolder+'":"",'+
      '"'+cjHost+'":"localhost",'+
      '"'+cjPort+'":8078,'+
      '"'+cjAddress+'":"",'+
      '"'+cjBasePath+'":""'+
    '}';

  cjConnectionWebAPI =
    '{'+
      '"'+cjConnectionType+'":3,'+
      '"'+cjName+'":"Web API Connection",'+
      '"'+cjFolder+'":"",'+
      '"'+cjHost+'":"",'+
      '"'+cjPort+'":-1,'+
      '"'+cjAddress+'":"https://explorer.example.com",'+
      '"'+cjBasePath+'":"/api/v1/"'+
    '}';

{ TTestNosoDesktopExplorerConnection }

procedure TTestNosoDesktopExplorerConnection.CheckFieldsCreate;
begin
  AssertEquals('Noso Data Explorer Connection type is ctUnknown', Ord(ctUnknown), Ord(FConnection.ConnectionType));
  AssertEquals('Noso Data Explorer Connection Name is empty', EmptyStr, FConnection.Name);
  AssertEquals('Noso Data Explorer Connection Folder is empty', EmptyStr, FConnection.Folder);
  AssertEquals('Noso Data Explorer Connection Host is empty', EmptyStr, FConnection.Host);
  AssertEquals('Noso Data Explorer Connection Port is -1', -1, FConnection.Port);
  AssertEquals('Noso Data Explorer Connection Address is empty', EmptyStr, FConnection.Address);
  AssertEquals('Noso Data Explorer Connection BasePath is empty', EmptyStr, FConnection.BasePath);
end;

procedure TTestNosoDesktopExplorerConnection.CheckFieldsFromConnectionFolder;
begin
  AssertEquals('Noso Data Explorer Connection type is ctFolder', Ord(ctFolder), Ord(FConnection.ConnectionType));
  AssertEquals('Noso Data Explorer Connection Name is Folder Connection', 'Folder Connection', FConnection.Name);
  AssertEquals('Noso Data Explorer Connection Folder is /home/user/NosoWallet/NOSODATA',
    '/home/user/NosoWallet/NOSODATA',
    FConnection.Folder
  );
  AssertEquals('Noso Data Explorer Connection Host is empty', EmptyStr, FConnection.Host);
  AssertEquals('Noso Data Explorer Connection Port is -1', -1, FConnection.Port);
  AssertEquals('Noso Data Explorer Connection Address is empty', EmptyStr, FConnection.Address);
  AssertEquals('Noso Data Explorer Connection BasePath is empty', EmptyStr, FConnection.BasePath);
end;

procedure TTestNosoDesktopExplorerConnection.CheckFieldsFromConnectionJSONRPC;
begin
  AssertEquals('Noso Data Explorer Connection type is ctJSONRPC', Ord(ctJSONRPC), Ord(FConnection.ConnectionType));
  AssertEquals('Noso Data Explorer Connection Name is JSON-RPC Connection', 'JSON-RPC Connection', FConnection.Name);
  AssertEquals('Noso Data Explorer Connection Folder is empty', EmptyStr, FConnection.Folder);
  AssertEquals('Noso Data Explorer Connection Host is localhost', 'localhost', FConnection.Host);
  AssertEquals('Noso Data Explorer Connection Port is 8078', 8078, FConnection.Port);
  AssertEquals('Noso Data Explorer Connection Address is empty', EmptyStr, FConnection.Address);
  AssertEquals('Noso Data Explorer Connection BasePath is empty', EmptyStr, FConnection.BasePath);
end;

procedure TTestNosoDesktopExplorerConnection.CheckFieldsFromConnectionWebAPI;
begin
  AssertEquals('Noso Data Explorer Connection type is ctWebAPI', Ord(ctWebAPI), Ord(FConnection.ConnectionType));
  AssertEquals('Noso Data Explorer Connection Name is Web API Connection', 'Web API Connection', FConnection.Name);
  AssertEquals('Noso Data Explorer Connection Folder is empty', EmptyStr, FConnection.Folder);
  AssertEquals('Noso Data Explorer Connection Host is empty', EmptyStr, FConnection.Host);
  AssertEquals('Noso Data Explorer Connection Port is -1', -1, FConnection.Port);
  AssertEquals('Noso Data Explorer Connection Address is ', 'https://explorer.example.com', FConnection.Address);
  AssertEquals('Noso Data Explorer Connection BasePath is ', '/api/v1/', FConnection.BasePath);
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionCreate;
begin
  FConnection:= TConnection.Create;
  CheckFieldsCreate;
  FConnection.Free;
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionChangeType;
begin
  FConnection:= TConnection.Create;

  FConnection.Name:= 'Folder Connection';
  FConnection.Folder:= '/home/user/NosoWallet/NOSODATA';
  FConnection.Host:= 'localhost';
  FConnection.Port:= 8078;
  FConnection.Address:= 'https://explorer.example.com';
  FConnection.BasePath:= '/api/v1/';
  FConnection.ConnectionType:= ctFolder;
  CheckFieldsFromConnectionFolder;

  FConnection.Name:= 'JSON-RPC Connection';
  FConnection.Folder:= '/home/user/NosoWallet/NOSODATA';
  FConnection.Host:= 'localhost';
  FConnection.Port:= 8078;
  FConnection.Address:= 'https://explorer.example.com';
  FConnection.BasePath:= '/api/v1/';
  FConnection.ConnectionType:= ctJSONRPC;
  CheckFieldsFromConnectionJSONRPC;

  FConnection.Name:= 'Web API Connection';
  FConnection.Folder:= '/home/user/NosoWallet/NOSODATA';
  FConnection.Host:= 'localhost';
  FConnection.Port:= 8078;
  FConnection.Address:= 'https://explorer.example.com';
  FConnection.BasePath:= '/api/v1/';
  FConnection.ConnectionType:= ctWebAPI;
  CheckFieldsFromConnectionWebAPI;

  FConnection.Name:= EmptyStr;
  FConnection.ConnectionType:= ctUnknown;
  CheckFieldsCreate;

  FConnection.Free;
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionFromConnectionFolder;
begin
  FConnection:= TConnection.Create(cjConnectionFolder);
  CheckFieldsFromConnectionFolder;
  FConnection.Free;
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionFromConnectionJSONRPC;
begin
  FConnection:= TConnection.Create(cjConnectionJSONRPC);
  CheckFieldsFromConnectionJSONRPC;
  FConnection.Free;
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionFromConnectionWebAPI;
begin
  FConnection:= TConnection.Create(cjConnectionWebAPI);
  CheckFieldsFromConnectionWebAPI;
  FConnection.Free;
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionAsJSON;
begin
  FConnection:= TConnection.Create(cjConnectionFolder);
  AssertEquals('Noso Desktop Explorer Connection AsJSON matches', cjConnectionFolder, FConnection.AsJSON);
  FConnection.Free;
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionAsJSONData;
var
  jData: TJSONData = nil;
begin
  FConnection:= TConnection.Create(cjConnectionFolder);
  jData:= FConnection.AsJSONData;
  AssertEquals('Noso Desktop Explorer Connection AsJSONData matches', cjConnectionFolder, jData.AsJSON);
  jData.Free;
  FConnection.Free;
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionAsJSONObject;
var
  jObject: TJSONObject = nil;
begin
  FConnection:= TConnection.Create(cjConnectionFolder);
  jObject:= FConnection.AsJSONObject;
  AssertEquals('Noso Desktop Explorer Connection AsJSONObject matches', cjConnectionFolder, jObject.AsJSON);
  jObject.Free;
  FConnection.Free;
end;

procedure TTestNosoDesktopExplorerConnection.TestNosoDesktopExplorerConnectionAsStream;
var
  ssConnection: TStringStream = nil;
  sConnection: TStream = nil;
begin
  FConnection:= TConnection.Create(cjConnectionFolder);
  ssConnection:= TStringStream.Create('', TEncoding.UTF8);
  sConnection:= FConnection.AsStream;
  ssConnection.LoadFromStream(sConnection);
  sConnection.Free;
  AssertEquals('Noso Desktop Explorer Connection AsStream matches', cjConnectionFolder, ssConnection.DataString);
  ssConnection.Free;
  FConnection.Free;
end;

initialization
  RegisterTest(TTestNosoDesktopExplorerConnection);
end.

