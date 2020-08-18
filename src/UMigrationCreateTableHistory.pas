unit UMigrationCreateTableHistory;

interface
    uses SysUtils, UMigrationScript, USQLDialect, UColumnOptions, UMigrationType, UPrimaryKeyOptions, Generics.Collections, Classes;
    type
    TMigrationCreateTableHistory = class(TInterfacedObject, IMigrationScript)

      function MigrationName(): string;
      function MigrationAuthor(): string;
      procedure Up(aDialect : TSQLDialect);
      procedure Down(aDialect : TSQLDialect);

    end;
implementation

{ TMigrationCreateTableHistory }

procedure TMigrationCreateTableHistory.Down(aDialect: TSQLDialect);
begin
  aDialect.DropTable(MIGRATION_TABLE);
end;

function TMigrationCreateTableHistory.MigrationAuthor: string;
begin
  Result := 'MIGRATIOR_SYSTEM';
end;

function TMigrationCreateTableHistory.MigrationName: string;
begin
  Result := 'Migration_History';
end;

procedure TMigrationCreateTableHistory.Up(aDialect: TSQLDialect);
var columns : TObjectList<TColumnOptions>;
var pkColumns : TStringList;
begin
  try

    columns := TObjectList<TColumnOptions>.Create();
    columns.Add(TColumnOptions.Create('MIGRATION_NAME', TDataBaseType.Varchar, 500, 0, True, ''));
    columns.Add(TColumnOptions.Create('AUTHOR', TDataBaseType.Varchar, 500, 0, True, ''));
    columns.Add(TColumnOptions.Create('VERSION_SYSTEM', TDataBaseType.Varchar, 50, 0, False, 'NOT_CONTROLLED'));
    columns.Add(TColumnOptions.Create('DATA_EXECUTED', TDataBaseType.Timestamp, 0, 6, False, ''));

    pkColumns := TStringList.Create();
    pkColumns.Add('MIGRATION_NAME');

    aDialect.CreateTable(MIGRATION_TABLE, columns);
    aDialect.CreatePrimaryKey(TPrimaryKeyOptions.Create('PK_MIGRATIONS_HISTORY',MIGRATION_TABLE, pkColumns));
  finally
    FreeAndNil(pkColumns);
    FreeAndNil(columns);
  end;
end;

end.
