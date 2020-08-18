unit UMigrator;

interface
  uses SysUtils, SQLExpr, USQLDialect, UMigrationType,  UMigrationScript, UMigrationCreateTableHistory, UMigrationRigisterScript,
       Classes, Dialogs, Generics.Collections, Rtti, UMigrationAuthorAndName, Controls;

    type

     TMigrator = class abstract(TComponent)
     private
      FSQLConnection: TSQLConnection;
      FDialectType: TSQLDialectType;
      FSQLDialect: TSQLDialect;
      FSystemVersion: string;
      FMigrationsDir: string;
      FMigrationsClasses: TStringList;
      FMigrationClassRegister: TMigrationRigisterScript;


      procedure ExecuteHistoryMigration();


      procedure ExecuteMigration(aMigration : TClass);
     protected
      property SQLDialect : TSQLDialect read FSQLDialect;
      function StartedMigrations(): Boolean; virtual;abstract;
      function MigrationExecuted(aMigrationName : string) : Boolean; virtual;abstract;
      procedure InserLogMigration(aMigrationName, aMigrationAuthor : string);virtual;abstract;
      procedure DeleteLogMigration(aMigrationName : string);virtual;abstract;
     public


      class function Factory(aDialect : TSQLDialectType) : TMigrator;
      constructor Create(AOwner : TComponent);overload;override;
      constructor Create();overload;

      procedure ExecuteRegistredMigrations();
      //procedure ExecuteMigrationsScript();
      procedure ExecuteRegistredSingleMigration(aName : string);
      procedure DropRegistredSingleMigration(aName : string);
      function GetAllMigrationAuthorAndName() : TList<TMigrationAuthorAndName>;
     published
      property DialectType : TSQLDialectType read FDialectType write FDialectType;
      property SQLConnection : TSQLConnection read FSQLConnection write FSQLConnection;
      property SystemVersion : string read FSystemVersion write FSystemVersion;
      property MigrationClassRegister : TMigrationRigisterScript read FMigrationClassRegister write FMigrationClassRegister;



    end;

    TMigratorOracle = class(TMigrator)
    protected
      function StartedMigrations(): Boolean; override;
      function MigrationExecuted(aMigrationName : string) : Boolean;override;
      procedure InserLogMigration(aMigrationName, aMigrationAuthor : string);override;
      procedure DeleteLogMigration(aMigrationName : string);override;

    end;
    procedure Register;
implementation

procedure Register;
begin
 RegisterComponents('M2 Migrator', [TMigratorOracle]);
end;

constructor TMigrator.Create(AOwner: TComponent);
begin
  inherited;
  //MigrationsClasses := TStringList.Create();
end;

constructor TMigrator.Create;
begin
end;



procedure TMigrator.DropRegistredSingleMigration(aName: string);
var migrations : TList<TClass>;
var migration : TClass;
var rttiContext : TRttiContext;
var rttiType : TRttiInstanceType;
var rttiMethodName, rttiCreate, rttiMethodDrop : TRttiMethod;
var Instance, returnMethodName : TValue;
begin
  try
    ExecuteHistoryMigration();
    rttiContext := TRttiContext.Create.Create();
    migrations := MigrationClassRegister.GetAllMigrations();
    for migration in migrations do
    begin
      rttiType := rttiContext.GetType(migration.ClassInfo).AsInstance;

      rttiCreate := rttiType.GetMethod('Create');
      rttiMethodName := rttiType.GetMethod('MigrationName');
      rttiMethodDrop := rttiType.GetMethod('Drop');
      Instance :=  rttiCreate.Invoke(rttiType.MetaclassType,[]);
      returnMethodName := rttiMethodName.Invoke(Instance, []);
      if returnMethodName.AsString = aName then
      begin
        try
          rttiMethodDrop.Invoke(Instance, [SQLDialect])
        except
          on E:Exception do
            raise Exception.Create('Fail to Drop Single Migration with name ' +aName+ '. Fail ===> ' + E.Message);
        end;
        DeleteLogMigration(returnMethodName.AsString);
        Break;
      end;
    end;
  finally
    FreeAndNil(migrations);
  end;
end;

procedure TMigrator.ExecuteHistoryMigration;
var historyMigration  : TMigrationCreateTableHistory;
begin
  FSQLDialect := TSQLDialect.Factory(DialectType);
  FSQLDialect.SQLConnection :=  SQLConnection;
  if not StartedMigrations()  then
  begin
    try
      historyMigration  := TMigrationCreateTableHistory.Create();
      historyMigration.Up(FSQLDialect);
      InserLogMigration(historyMigration.MigrationName(), historyMigration.MigrationAuthor() );
    except
      on E:Exception do
      begin
        historyMigration.Down(FSQLDialect);
        raise Exception.Create('Fail to execute migration name ' + historyMigration.MigrationName() + '. Fail ===> ' + e.message);
      end;
    end;
  end;
end;

procedure TMigrator.ExecuteMigration(aMigration: TClass);
var rttiContext : TRttiContext;
var rttiType : TRttiInstanceType;
var rttiMethodUp, rttiMethodDown, rttiMethodName, rttiMethodAuthor, rttiCreate : TRttiMethod;
var Instance, returnMethodName, returnMethodAuthor : TValue;
begin
  try
    rttiContext := TRttiContext.Create.Create();
    rttiType := rttiContext.GetType(aMigration.ClassInfo).AsInstance;

    rttiCreate := rttiType.GetMethod('Create');
    rttiMethodUp := rttiType.GetMethod('Up');
    rttiMethodDown := rttiType.GetMethod('Down');
    rttiMethodName := rttiType.GetMethod('MigrationName');
    rttiMethodAuthor := rttiType.GetMethod('MigrationAuthor');

    Instance :=  rttiCreate.Invoke(rttiType.MetaclassType,[]);
    returnMethodName := rttiMethodName.Invoke(Instance, []);
    returnMethodAuthor := rttiMethodAuthor.Invoke(Instance, []);


    if not MigrationExecuted(returnMethodName.AsString) then
    begin
      try
        rttiMethodUp.Invoke(Instance, [SQLDialect]);
      except
        on E:Exception do
        begin
          if MessageDlg('Execute Reverter Script ?',mtCustom,[mbOk,mbCancel ], 0) = mrOK then
              rttiMethodDown.Invoke(Instance, [SQLDialect]);
          ShowMessage('Fail to execute Up method for migration name ' + returnMethodName.AsString + '. Fail ===> ' + e.Message);
        end;
      end;


      try
        InserLogMigration(returnMethodName.AsString, returnMethodAuthor.AsString);
      except
        on E:Exception do
        begin
          rttiMethodDown.Invoke(Instance, [SQLDialect]);
            raise Exception.Create('Fail to execute method InsertLogMigration for migration name ' + returnMethodName.AsString +
                                   '. Migration Drop Method Executed. Fail ===> ' + e.Message);
          //DeleteLogMigration(returnMethodName.AsString);
        end;
      end;
    end;
  except
    on E:Exception do
      raise Exception.Create(E.Message);
  end;
end;
     (*
procedure TMigrator.ExecuteMigrationsScript;
var historyMigration  : TMigrationCreateTableHistory;
begin
  FSQLDialect := TSQLDialect.Factory(DialectType);
  FSQLDialect.SQLConnection :=  SQLConnection;
  if not StartedMigrations()  then
  begin
    try
      historyMigration  := TMigrationCreateTableHistory.Create();
      historyMigration.Up(FSQLDialect);
      InserLogMigration(historyMigration.MigrationName(), historyMigration.MigrationAuthor() );
    except
      on E:Exception do
      begin
        historyMigration.Down(FSQLDialect);
        raise Exception.Create('Fail to execute migration name ' + historyMigration.MigrationName() + '. Fail ===> ' + e.message);
      end;
    end;
  end;
  ExecuteRegistredMigrations();

end;

              *)

procedure TMigrator.ExecuteRegistredMigrations;
var migrations : TList<TClass>;
var migration : TClass;
begin
  try
    ExecuteHistoryMigration();
    migrations := MigrationClassRegister.GetAllMigrations();
    for migration in migrations do
      ExecuteMigration(migration);
   (* begin

      rttiType := rttiContext.GetType(migration.ClassInfo).AsInstance;


      rttiCreate := rttiType.GetMethod('Create');
      rttiMethodUp := rttiType.GetMethod('Up');
      rttiMethodDown := rttiType.GetMethod('Down');
      rttiMethodName := rttiType.GetMethod('MigrationName');
      rttiMethodAuthor := rttiType.GetMethod('MigrationAuthor');

      Instance :=  rttiCreate.Invoke(rttiType.MetaclassType,[]);
      returnMethodName := rttiMethodName.Invoke(Instance, []);
      returnMethodAuthor := rttiMethodAuthor.Invoke(Instance, []);


      if not MigrationExecuted(returnMethodName.AsString) then
      begin
        try
          rttiMethodUp.Invoke(Instance, [SQLDialect]);
        except
          raise Exception.Create('Fail to execute Up method for migration name ' + returnMethodName.AsString + .'. Fail ===> ' + e.Message);
        end;


        try
          InserLogMigration(returnMethodName.AsString, returnMethodAuthor.AsString);
        except
          on E:Exception do
          begin
            rttiMethodDown.Invoke(Instance, [SQLDialect]);
              raise Exception.Create('Fail to execute method InsertLogMigration for migration name ' + returnMethodName.AsString +
                                     '. Migration Drop Method Executed. Fail ===> ' + e.Message);
            //DeleteLogMigration(returnMethodName.AsString);
          end;
        end;
      end;
    end;   *)
  finally
    FreeAndNil(migrations);
  end;
end;



procedure TMigrator.ExecuteRegistredSingleMigration(aName: string);
var migrations : TList<TClass>;
var migration : TClass;
var rttiContext : TRttiContext;
var rttiType : TRttiInstanceType;
var rttiMethodName, rttiCreate : TRttiMethod;
var Instance, returnMethodName : TValue;
begin
  try
    ExecuteHistoryMigration();
    rttiContext := TRttiContext.Create.Create();
    migrations := MigrationClassRegister.GetAllMigrations();
    for migration in migrations do
    begin
      rttiType := rttiContext.GetType(migration.ClassInfo).AsInstance;

      rttiCreate := rttiType.GetMethod('Create');
      rttiMethodName := rttiType.GetMethod('MigrationName');
      Instance :=  rttiCreate.Invoke(rttiType.MetaclassType,[]);
      returnMethodName := rttiMethodName.Invoke(Instance, []);
      if returnMethodName.AsString = aName then
      begin
        ExecuteMigration(migration);
        Break;
      end;
    end;
  finally
    FreeAndNil(migrations);
  end;

end;

class function TMigrator.Factory(aDialect: TSQLDialectType): TMigrator;
begin
  case aDialect of
    Oracle : Result := TMigratorOracle.Create();
  else
    raise Exception.Create('Migrator not implement for SQLDialect');
  end;
  Result.DialectType := aDialect;
end;


function TMigrator.GetAllMigrationAuthorAndName: TList<TMigrationAuthorAndName>;
var migrations : TList<TClass>;
var migration : TClass;
var rttiContext : TRttiContext;
var rttiType : TRttiInstanceType;
var rttiMethodName, rttiMethodAuthor, rttiCreate : TRttiMethod;
var Instance, returnMethodName, returnMethodAuthor : TValue;
var _record : TMigrationAuthorAndName;
begin
  try
    Result := TList<TMigrationAuthorAndName>.Create();
    rttiContext := TRttiContext.Create.Create();
    migrations := MigrationClassRegister.GetAllMigrations();
    for migration in migrations do
    begin

      rttiType := rttiContext.GetType(migration.ClassInfo).AsInstance;


      rttiCreate := rttiType.GetMethod('Create');
      rttiMethodName := rttiType.GetMethod('MigrationName');
      rttiMethodAuthor := rttiType.GetMethod('MigrationAuthor');

      Instance :=  rttiCreate.Invoke(rttiType.MetaclassType,[]);
      returnMethodName := rttiMethodName.Invoke(Instance, []);
      returnMethodAuthor := rttiMethodAuthor.Invoke(Instance, []);

      Result.Add(TMigrationAuthorAndName.Create(returnMethodName.AsString, returnMethodAuthor.AsString));
    end;
  finally
    FreeAndNil(migrations);
  end;

end;

{ TMigratorOracle }

procedure TMigratorOracle.DeleteLogMigration(aMigrationName: string);
begin
   SQLDialect.ExecuteSQLScript(' DELETE FROM  ' + MIGRATION_TABLE + ' WHERE MIGRATION_NAME = ' + QuotedStr(aMigrationName));
end;

procedure TMigratorOracle.InserLogMigration(aMigrationName, aMigrationAuthor: string);
begin
  SQLDialect.ExecuteSQLScript(
  ' INSERT INTO ' + MIGRATION_TABLE + ' ( '+
  ' AUTHOR, MIGRATION_NAME, DATA_EXECUTED, VERSION_SYSTEM ) VALUES (' +
    QuotedStr(aMigrationAuthor) + ', ' +   QuotedStr(aMigrationName) + ', SYSDATE, ' + QuotedStr(SystemVersion) + ')')

end;

function TMigratorOracle.MigrationExecuted(aMigrationName: string): Boolean;
var query : TSQLQuery;
begin
  query := SQLDialect.ExecuteSQLScriptWithCallback(
   ' SELECT COUNT(*) AS RESULT FROM ' + MIGRATION_TABLE +
   ' WHERE MIGRATION_NAME = '+ QuotedStr(aMigrationName)
   );
  Result := query.FieldByName('RESULT').AsInteger > 0;
end;

function TMigratorOracle.StartedMigrations: Boolean;
var query : TSQLQuery;
begin
  query := SQLDialect.ExecuteSQLScriptWithCallback(
   ' SELECT COUNT(TABLE_NAME)AS RESULT FROM ALL_TABLES ' +
   '  WHERE OWNER = ' + QuotedStr(SQLConnection.Params.Values['User_name']) +
   '    AND TABLE_NAME = ' +  QuotedStr(MIGRATION_TABLE));
  Result := query.FieldByName('RESULT').AsInteger > 0;

end;

end.
