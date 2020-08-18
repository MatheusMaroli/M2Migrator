unit USQLDialect;

interface
  uses SysUtils, Classes, SQLExpr, UMigrationType, UColumnOptions, UUniqueKeyOptions,
       Generics.Collections, UForeginKeyOptions, UPrimaryKeyOptions;
  type

    TSQLDialect = class abstract
    private
       FSQLConnection: TSQLConnection;
    protected


      function WrapperDataBaseType(column : TColumnOptions) : string; virtual;
      function WrapperDataBaseVarchar(column : TColumnOptions) : string; virtual;
      function WrapperNotNullValue(column : TColumnOptions) : string; virtual;
      function WrapperDefaultValue(column : TColumnOptions) : string; virtual;
      function WrapperColumn(column : TColumnOptions): string;virtual;
      function WrapperDataBaseNumber(column : TColumnOptions): string;virtual;
      function WrapperDataBaseTimestamp(column : TColumnOptions): string;virtual;

      function WrapperColumnForConstraint(columns : TStringList) : string;virtual;

    public
      property SQLConnection : TSQLConnection read FSQLConnection write FSQLConnection;
      class function Factory(aType :TSQLDialectType) : TSQLDialect;

      procedure ExecuteSQLScript(const aScript : string);
      function ExecuteSQLScriptWithCallback(const aScript : string) : TSQLQuery;

      procedure CreateTable(tableName : string; columns  : TObjectList<TColumnOptions>  );virtual;abstract;
      procedure DropTable(tableName : string);virtual;abstract;
      procedure CreateColumn(tableName : string; column:  TCOlumnOptions );virtual;abstract;
      procedure DropColumn(tableName, columnName : string); virtual;abstract;
      procedure CreateForeginKey(foreginKeyOptions : TForeginKeyOptions); virtual;abstract;
      procedure CreateUniqueKey(uniqueKeyOptions : TUniqueKeyOptions);virtual;abstract;
      procedure DropConstraint(tableName : string; foreginKeyName : string); virtual;abstract;
      procedure CreatePrimaryKey(primaryKeyOptions : TPrimaryKeyOptions);virtual;abstract;
      procedure CustomScript(aScript : string);virtual;
    end;

    TSQLDialectOracle = class(TSQLDialect)
    public
      procedure CreateTable(tableName : string; columns  : TObjectList<TColumnOptions> );override;
      procedure DropTable(tableName : string);override;
      procedure CreateColumn(tableName : string; column:  TCOlumnOptions );override;
      procedure DropColumn(tableName : string; columnName : string );override;
      procedure CreateForeginKey(foreginKeyOptions : TForeginKeyOptions); override;
      procedure CreatePrimaryKey(primaryKeyOptions : TPrimaryKeyOptions);override;
      procedure CreateUniqueKey(uniqueKeyOptions : TUniqueKeyOptions);override;
      procedure DropConstraint(tableName : string; foreginKeyName : string);override;
    end;


implementation

{ TSQLDialect }

procedure TSQLDialect.CustomScript(aScript: string);
begin
  try
    ExecuteSQLScript(aScript);
  except
    on E : Exception do
       raise Exception.Create('Fail to execute method CreatePrimaryKey on Abstract Implemention. Fail Message ===> ' + e.Message);
  end;
end;

procedure TSQLDialect.ExecuteSQLScript(const aScript: string);
var ASql : TSQLQuery;
begin
  try
    try
      ASql := TSQLQuery.Create(nil);
      ASql.SQLConnection := SQLConnection;
      ASql.SQL.Text := aScript;
      ASql.ExecSQL();
    except
      on E: Exception do
      begin
         raise Exception.Create('Fail to execute SQL script. Fail ===> ' + e.Message);
      end;
    end;
  finally
    FreeAndNil(Asql);
  end;

end;

function TSQLDialect.ExecuteSQLScriptWithCallback(const aScript: string): TSQLQuery;
var ASql : TSQLQuery;
begin
  try
    ASql := TSQLQuery.Create(nil);
    ASql.SQLConnection := SQLConnection;
    ASql.Close();
    ASql.SQL.Text := aScript;
    ASql.Open();
    Result := ASql;
  except
    on E: Exception do
    begin
      raise Exception.Create('Fail to execute SQL script. Fail ===> ' + e.Message);
    end;
  end;
end;

class function TSQLDialect.Factory(aType: TSQLDialectType): TSQLDialect;
begin
  case aType of
    Oracle : Result := TSQLDialectOracle.Create();
  else
   raise Exception.Create('Factory not implement for information type');
  end;
end;

function TSQLDialect.WrapperDataBaseNumber(column: TColumnOptions): string;
begin
  Result := ' NUMBER( '+ FloatToStr(column.Precision) + ')';
end;

function TSQLDialect.WrapperDataBaseTimestamp(column: TColumnOptions): string;
begin
  Result := ' TIMESTAMP( '+ FloatToStr(column.Precision) + ')';
end;

function TSQLDialect.WrapperDataBaseType(column: TColumnOptions): string;
begin
  case column.ColumnType of
    Varchar: Result :=  WrapperDataBaseVarchar(column);
    Number: Result := WrapperDataBaseNumber(column);
    Long: raise Exception.Create('Type long not implement.');
    Timestamp: Result := WrapperDataBaseTimestamp(column);
    Blob: raise Exception.Create('Type long not implement.');
  end;
end;

function TSQLDialect.WrapperColumn(column: TColumnOptions): string;
begin
  Result := column.ColumnName + ' ' + WrapperDataBaseType(column) + ' ' + WrapperNotNullValue(column) + ' ' + WrapperDefaultValue(column);
end;

function TSQLDialect.WrapperColumnForConstraint(columns: TStringList): string;
var column  : string;
var isFirstColumn : Boolean;
begin
  Result := '';
  isFirstColumn := true;
  for column in columns do
  begin
    if isFirstColumn then
    begin
      Result := column;
      isFirstColumn := false;
    end
    else  Result :=  Result + ',' + column;
  end;
end;

function TSQLDialect.WrapperDataBaseVarchar(column: TColumnOptions): string;
begin
  Result := ' VARCHAR(' + IntToStr(column.Length) + ')';
end;

function TSQLDialect.WrapperDefaultValue(column: TColumnOptions): string;
begin
  Result := '';
  if column.DefaultValue <> '' then
  begin
    if column.ColumnType = Number then
       Result := stringReplace(column.DefaultValue, ',', '.', [rfReplaceAll])
    else if column.ColumnType = Timestamp then
       Result := column.DefaultValue
    else  if column.ColumnType = Varchar then
      Result := QuotedStr(column.DefaultValue)
    else raise Exception.Create('Default value implement for Number, TimeStamp, Varchar types.');
    Result := ' DEFAULT ' + Result;
  end;
end;

function TSQLDialect.WrapperNotNullValue(column: TColumnOptions): string;
begin
  Result := '';
  if column.NotNull then
    Result := ' NOT NULL ';
end;

{ TSQLDialectOracle }

procedure TSQLDialectOracle.CreateColumn(tableName: string; column: TCOlumnOptions);
var script : string;
begin
  try
    script := ' ALTER TABLE ' + tableName +' ADD ' + WrapperColumn(column) + ' ';
    ExecuteSQLScript(script);
  except
    on E: Exception do
      raise Exception.Create('Fail to execute method CreateColumn on Oracle Implemention. Fail Message ===> ' + e.Message);
  end;
end;

procedure TSQLDialectOracle.CreateForeginKey(foreginKeyOptions: TForeginKeyOptions);
var script : string;
begin
  try
    script := ' ALTER TABLE ' +foreginKeyOptions.TableOrigin + ' ADD ( '+
    ' CONSTRAINT ' + foreginKeyOptions.Name + ' FOREIGN KEY ( '+  WrapperColumnForConstraint(foreginKeyOptions.ColumnTableOrigin) + ')' +
    ' REFERENCES ' + foreginKeyOptions.TableDestiny + ' ( '+ WrapperColumnForConstraint(foreginKeyOptions.ColumnTableDestiny) +'))';
    ExecuteSQLScript(script);
  except
    on E: Exception do
      raise Exception.Create('Fail to execute method CreateForeginKey on Oracle Implemention. Fail Message ===> ' + e.Message);
  end;
end;

procedure TSQLDialectOracle.CreatePrimaryKey(primaryKeyOptions: TPrimaryKeyOptions);
var script : string;
begin
  try
    script := ' ALTER TABLE ' + primaryKeyOptions.TableName +' ADD CONSTRAINT ' +primaryKeyOptions.Name + ' PRIMARY KEY ('
     + WrapperColumnForConstraint(primaryKeyOptions.Columns) + ') ';
    ExecuteSQLScript(script);
  except
    on E: Exception do
      raise Exception.Create('Fail to execute method CreatePrimaryKey on Oracle Implemention. Fail Message ===> ' + e.Message);
  end;
end;

procedure TSQLDialectOracle.CreateTable(tableName: string; columns : TObjectList<TCOlumnOptions> );
var script, columnScript  : string;
var column : TColumnOptions;
var isFirstColumn : Boolean;
begin
  try
    isFirstColumn := True;
    script := ' CREATE TABLE ' + tableName +' (' ;
    for column in columns do
    begin
      columnScript := WrapperColumn(column);// column.ColumnName + ' ' + WrapperDataBaseType(column) + ' ' + WrapperNotNullValue(column) + ' ' + WrapperDefaultValue(column);
      if isFirstColumn then
      begin
        isFirstColumn := false;
        script := script + columnScript;
      end
      else script := script +','+ columnScript;
    end;
    script := script + ')';
    ExecuteSQLScript(script);
  except
    on E: Exception do
      raise Exception.Create('Fail to execute method CreateTable on Oracle Implemention. Fail Message ===> ' + e.Message);
  end;
end;

procedure TSQLDialectOracle.CreateUniqueKey(uniqueKeyOptions: TUniqueKeyOptions);
var script : string;
begin
  try
    script := ' ALTER TABLE ' + uniqueKeyOptions.TableName +' ADD CONSTRAINT ' +uniqueKeyOptions.Name + ' UNIQUE ('
     + WrapperColumnForConstraint(uniqueKeyOptions.Columns) + ') ';
    ExecuteSQLScript(script);
  except
    on E: Exception do
      raise Exception.Create('Fail to execute method CreateUniqueKey on Oracle Implemention. Fail Message ===> ' + e.Message);
  end;
end;
                    //ERRADO
procedure TSQLDialectOracle.DropColumn(tableName, columnName : string);
var script : string;
begin
  try
    script := ' ALTER TABLE ' + tableName + ' DROP COLUMN ' + columnName + ' ';
    ExecuteSQLScript(script);
   // ExecutaSql(script, SQLConnection);
  except
    on E: Exception do
      raise Exception.Create('Fail to execute method DropColumn on Oracle Implemention. Fail Message ===> ' + e.Message);
  end;
end;

procedure TSQLDialectOracle.DropConstraint(tableName, foreginKeyName: string);
begin
  try
    ExecuteSQLScript(' ALTER TABLE '+ tableName +' DROP CONSTRAINT ' + foreginKeyName);
  except
    on E: Exception do
        raise Exception.Create('Fail to execute method DropConstraint on Oracle Implemention. Fail Message ===> ' + e.Message);
  end;
end;

procedure TSQLDialectOracle.DropTable(tableName: string);
begin
  try
    ExecuteSQLScript('DROP TABLE '+ tableName + ' CASCADE CONSTRAINTS');
  except
    on E: Exception do
        raise Exception.Create('Fail to execute method DropTable on Oracle Implemention. Fail Message ===> ' + e.Message);
  end;
end;

end.
