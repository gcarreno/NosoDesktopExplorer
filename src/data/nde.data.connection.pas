unit NDE.Data.Connection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, fpjson
, jsonparser
;

const
  cjConnectionType = 'type';
  cjName = 'name';
  cjFolder = 'folder';
  cjHost = 'host';
  cjPort = 'port';
  cjAddress = 'address';
  cjBasePath = 'base-path';

resourcestring
  rsEConnectionWrongJSONObject = 'JSON data is not an object';

type
{ TConnectionType }
  TConnectionType = (ctUnknown, ctFolder, ctJSONRPC, ctWebAPI);

{ EConnectionWrongJSONObject }
  EConnectionWrongJSONObject = class(Exception);

{ TConnection }
  TConnection = class(TObject)
  private
    FConnectionType: TConnectionType;
    FName: String;
    // ctFolder
    FFolder: String;
    // ctJSONRPC
    FHost: String;
    FPort: Integer;
    // ctWebAPI
    FAddress: String;
    FBasePath: String;

    FCompressedJSON: Boolean;

    procedure SetConnectionType(AValue: TConnectionType);

    procedure setFromJSON(const AJSON: TJSONStringType);
    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONObject(const AJSONObject: TJSONObject);
    procedure setFromStream(const AStream: TStream);

    function getAsJSON: TJSONStringType;
    function getAsJSONData: TJSONData;
    function getAsJSONObject: TJSONObject;
    function getAsStream: TStream;

  protected
  public
    constructor Create; overload;
    constructor Create(const AJSON: TJSONStringType); overload;
    constructor Create(const AJSONData: TJSONData); overload;
    constructor Create(const AJSONObject: TJSONObject); overload;
    constructor Create(const AStream: TStream); overload;

    function FormatJSON(
      AOptions : TFormatOptions = DefaultFormat;
      AIndentsize : Integer = DefaultIndentSize
    ): TJSONStringType;

    property ConnectionType: TConnectionType
      read FConnectionType
      write SetConnectionType;
    property Name: String
      read FName
      write FName;
    property Folder: String
      read FFolder
      write FFolder;
    property Host: String
      read FHost
      write FHost;
    property Port: Integer
      read FPort
      write FPort;
    property Address: String
      read FAddress
      write FAddress;
    property BasePath: String
      read FBasePath
      write FBasePath;

    property AsJSON: TJSONStringType
      read getAsJSON;
    property AsJSONData: TJSONData
      read getAsJSONData;
    property AsJSONObject: TJSONObject
      read getAsJSONObject;
    property AsStream: TStream
      read getAsStream;

    property CompressedJSON: Boolean
      read FCompressedJSON
      write FCompressedJSON;
  published
  end;

implementation

{ TConnection }

procedure TConnection.SetConnectionType(AValue: TConnectionType);
begin
  if FConnectionType = AValue then Exit;
  FConnectionType:= AValue;
  case FConnectionType of
    ctUnknown:begin
      FFolder:= EmptyStr;
      FHost:= EmptyStr;
      FPort:= -1;
      FAddress:= EmptyStr;
      FBasePath:= EmptyStr;
    end;
    ctFolder:begin
      FHost:= EmptyStr;
      FPort:= -1;
      FAddress:= EmptyStr;
      FBasePath:= EmptyStr;
    end;
    ctJSONRPC:begin
      FFolder:= EmptyStr;
      FAddress:= EmptyStr;
      FBasePath:= EmptyStr;
    end;
    ctWebAPI:begin
      FFolder:= EmptyStr;
      FHost:= EmptyStr;
      FPort:= -1;
    end;
  end;
end;

procedure TConnection.setFromJSON(const AJSON: TJSONStringType);
var
  jData: TJSONData = nil;
begin
  jData:= GetJSON(AJSON);
  try
    setFromJSONData(jData);
  finally
    jData.Free;
  end;
end;

procedure TConnection.setFromJSONData(const AJSONData: TJSONData);
begin
  if aJSONData.JSONType <> jtObject then
  begin
    raise EConnectionWrongJSONObject.Create(rsEConnectionWrongJSONObject);
  end;
  setFromJSONObject(aJSONData as TJSONObject);
end;

procedure TConnection.setFromJSONObject(const AJSONObject: TJSONObject);
begin
  FConnectionType:=
    TConnectionType(AJSONObject.Get(cjConnectionType, Ord(ConnectionType)));
  FName:= AJSONObject.Get(cjName, FName);
  FFolder:= AJSONObject.Get(cjFolder, FFolder);
  FHost:= AJSONObject.Get(cjHost, FHost);
  FPort:= AJSONObject.Get(cjPort, FPort);
  FAddress:= AJSONObject.Get(cjAddress, FAddress);
  FBasePath:= AJSONObject.Get(cjBasePath, FBasePath);
end;

procedure TConnection.setFromStream(const AStream: TStream);
var
  jData: TJSONData = nil;
begin
  jData:= GetJSON(AStream);
  try
    setFromJSONData(jData);
  finally
    jData.Free;
  end;
end;

function TConnection.getAsJSON: TJSONStringType;
var
  jObject: TJSONObject = nil;
begin
  Result:= '';
  jObject:= getAsJSONObject;
  jObject.CompressedJSON:= FCompressedJSON;
  Result:= jObject.AsJSON;
  jObject.Free;
end;

function TConnection.getAsJSONData: TJSONData;
begin
  Result:= getAsJSONObject as TJSONData;
end;

function TConnection.getAsJSONObject: TJSONObject;
begin
  Result:= TJSONObject.Create;
  Result.Add(cjConnectionType, Ord(FConnectionType));
  Result.Add(cjName, FName);
  Result.Add(cjFolder, FFolder);
  Result.Add(cjHost, FHost);
  Result.Add(cjPort, FPort);
  Result.Add(cjAddress, FAddress);
  Result.Add(cjBasePath, FBasePath);
end;

function TConnection.getAsStream: TStream;
begin
  Result:= TStringStream.Create(getAsJSON, TEncoding.UTF8);
end;

constructor TConnection.Create;
begin
  FConnectionType:= ctUnknown;
  FName:= EmptyStr;
  FFolder:= EmptyStr;
  FHost:= EmptyStr;
  FPort:= -1;
  FAddress:= EmptyStr;
  FBasePath:= EmptyStr;
  FCompressedJSON:= True;
end;

constructor TConnection.Create(const AJSON: TJSONStringType);
begin
  Create;
  setFromJSON(AJSON);
end;

constructor TConnection.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

constructor TConnection.Create(const AJSONObject: TJSONObject);
begin
  Create;
  setFromJSONObject(AJSONObject);
end;

constructor TConnection.Create(const AStream: TStream);
begin
  Create;
  setFromStream(AStream);
end;

function TConnection.FormatJSON(
  AOptions: TFormatOptions;
  AIndentsize: Integer
): TJSONStringType;
begin
  Result:= getAsJSONObject.FormatJSON(AOptions, AIndentsize);
end;

end.

