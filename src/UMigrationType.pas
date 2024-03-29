unit UMigrationType;

interface
  type

   TDataBaseType = (Varchar, Number, Long, Timestamp, Blob, Integer);
   TSQLDialectType = (Oracle, Postgre, Firebird);
   const MIGRATION_TABLE  : string  = 'MIGRATIONS_HISTORY';
implementation

end.
