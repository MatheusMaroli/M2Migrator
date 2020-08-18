unit UMigrationScript;

interface
    uses USQLDialect;
    type

    IMigrationScript = interface
      function MigrationName(): string;
      function MigrationAuthor(): string;
      procedure Up(aDialect : TSQLDialect);
      procedure Down(aDialect : TSQLDialect);
    end;
implementation

end.
