unit UMigrationAuthorAndName;

interface
    type
    TMigrationAuthorAndName = class
    private
        FAuthor: string;
        FName: string;
    public
      property Name : string read FName write FName;
      property Author : string read FAuthor write FAuthor;
      constructor Create();overload;
      constructor Create(aName, aAuthor : string);overload;
    end;
implementation

{ TMigrationAuthorAndName }

constructor TMigrationAuthorAndName.Create;
begin

end;

constructor TMigrationAuthorAndName.Create(aName, aAuthor: string);
begin
  Name:= aName;
  Author := aAuthor;
end;

end.
