unit NDE.Data.Connections;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, Contnrs
, fpjson
, jsonparser
, NDE.Data.Connection
;

resourcestring
  rsEConnectionsWrongJSONObject = 'JSON data is not an array';

type
{ EConnectionsWrongJSONObject }
  EConnectionsWrongJSONObject = class(Exception);

  TConnectionsEnumerator =  class; // Forward
{ TConnections }
  TConnections = class(TObject)
  private
    FConnections: TFPObjectList;
    // Compressed JSON field to mimic TJSON one
    FCompressedJSON: Boolean;

    function GetConnection(Index: Integer): TConnection;
    function GetCount: Integer;
    procedure SetConnection(Index: Integer; AValue: TConnection);

    procedure setFromJSON(const AJSON: TJSONStringType);
    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONArray(const AJSONArray: TJSONArray);
    procedure setFromStream(const AStream: TStream);

    function getAsJSON: TJSONStringType;
    function getAsJSONData: TJSONData;
    function getAsJSONArray: TJSONArray;
    function getAsStream: TStream;

  protected
  public
    constructor Create; overload;
    constructor Create(const AJSON: TJSONStringType); overload;
    constructor Create(const AJSONData: TJSONData); overload;
    constructor Create(const AJSONArray: TJSONArray); overload;
    constructor Create(const AStream: TStream); overload;
    destructor Destroy; override;

    procedure Clear;
    function Add(const AConnection: TConnection): Integer;
    procedure Delete(const AIndex: Integer); overload;
    procedure Delete(const AConnection: TConnection); overload;

    function FormatJSON(
      AOptions : TFormatOptions = DefaultFormat;
      AIndentsize : Integer = DefaultIndentSize
    ): TJSONStringType;

    function GetEnumerator: TConnectionsEnumerator;

    property Count: Integer
      read GetCount;
    property Items[Index: Integer]: TConnection
      read GetConnection
      write SetConnection; default;

    property AsJSON: TJSONStringType
      read getAsJSON;
    property AsJSONData: TJSONData
      read getAsJSONData;
    property AsJSONArray: TJSONArray
      read getAsJSONArray;
    property AsStream: TStream
      read getAsStream;

    property CompressedJSON: Boolean
      read FCompressedJSON
      write FCompressedJSON;

  published
  end;

{ TConnectionsEnumerator }
  TConnectionsEnumerator = class(TObject)
  private
    FConnections: TConnections;
    FPosition: Integer;
  protected
  public
    constructor Create(const AConnections: TConnections);
    function GetCurrent: TConnection;
    function MoveNext: Boolean;

    property Current: TConnection
      read GetCurrent;
  published
  end;

implementation

{ TConnectionsEnumerator }

constructor TConnectionsEnumerator.Create(const AConnections: TConnections);
begin
  FConnections := AConnections;
  FPosition := -1;
end;

function TConnectionsEnumerator.GetCurrent: TConnection;
begin
  Result:= FConnections.Items[FPosition] as TConnection;
end;

function TConnectionsEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FConnections.Count;
end;

{ TConnections }

function TConnections.GetCount: Integer;
begin
  Result:= FConnections.Count;
end;

function TConnections.GetConnection(Index: Integer): TConnection;
begin
  Result:= FConnections.Items[Index] as TConnection;
end;

procedure TConnections.SetConnection(Index: Integer; AValue: TConnection);
begin
  if FConnections.Items[Index] = AValue then exit;
  FConnections.Items[Index]:= AValue;
end;

procedure TConnections.setFromJSON(const AJSON: TJSONStringType);
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

procedure TConnections.setFromJSONData(const AJSONData: TJSONData);
begin
  if AJSONData.JSONType <> jtArray then
  begin
    raise EConnectionsWrongJSONObject.Create(rsEConnectionsWrongJSONObject);
  end;
  setFromJSONArray(AJSONData as TJSONArray);
end;

procedure TConnections.setFromJSONArray(const AJSONArray: TJSONArray);
var
  index: Integer;
  connection: TConnection = nil;
begin
  for index:= 0 to Pred(AJSONArray.Count) do
  begin
    connection:= TConnection.Create(AJSONArray[index]);
    FConnections.Add(connection);
  end;
end;

procedure TConnections.setFromStream(const AStream: TStream);
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

function TConnections.getAsJSON: TJSONStringType;
var
  jArray: TJSONArray = nil;
begin
  Result:= EmptyStr;
  jArray:= getAsJSONArray;
  jArray.CompressedJSON:= FCompressedJSON;
  Result:= jArray.AsJSON;
  jArray.Free;
end;

function TConnections.getAsJSONData: TJSONData;
begin
  Result:= getAsJSONArray as TJSONData;
end;

function TConnections.getAsJSONArray: TJSONArray;
var
  index: Integer;
  jData: TJSONData = nil;
begin
  Result:= TJSONArray.Create;
  for index := 0 to Pred(FConnections.Count) do
  begin
    jData:= TConnection(FConnections.Items[index]).AsJSONData;
    Result.Add(jData);
  end;
end;

function TConnections.getAsStream: TStream;
begin
  Result:= TStringStream.Create(getAsJSON, TEncoding.UTF8);
end;

procedure TConnections.Clear;
begin
  FConnections.Clear;
end;

function TConnections.Add(const AConnection: TConnection): Integer;
begin
  Result:= FConnections.Add(AConnection);
end;

procedure TConnections.Delete(const AIndex: Integer);
begin
  FConnections.Delete(AIndex);
end;

procedure TConnections.Delete(const AConnection: TConnection);
begin
  FConnections.Delete(FConnections.IndexOf(AConnection));
end;

function TConnections.FormatJSON(
  AOptions: TFormatOptions;
  AIndentsize: Integer
): TJSONStringType;
begin
  Result:= getAsJSONArray.FormatJSON(AOptions, AIndentsize);
end;

function TConnections.GetEnumerator: TConnectionsEnumerator;
begin
  Result:= TConnectionsEnumerator.Create(Self);
end;

constructor TConnections.Create;
begin
  FCompressedJSON:= True;
  FConnections:= TFPObjectList.Create(True);
end;

constructor TConnections.Create(const AJSON: TJSONStringType);
begin
  Create;
  setFromJSON(AJSON);
end;

constructor TConnections.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

constructor TConnections.Create(const AJSONArray: TJSONArray);
begin
  Create;
  setFromJSONArray(AJSONArray);
end;

constructor TConnections.Create(const AStream: TStream);
begin
  Create;
  setFromStream(AStream);
end;

destructor TConnections.Destroy;
begin
  FConnections.Free;
  inherited Destroy;
end;

end.

