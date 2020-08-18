unit UPrimaryKeyOptions;

interface
   uses Classes;
   type
   TPrimaryKeyOptions = class
   private
     FName: string;
     FTableName: string;
     FColumns: TStringList;
   public
     property Name      : string      read FName write FName;
     property TableName : string      read FTableName write FTableName;
     property Columns   : TStringList read FColumns write FColumns;

     constructor Create(); overload;
     constructor Create(aName, aTableName :string; aColumns :TStringList);overload;
      constructor Create(aName, aTableName, aColumn :string);overload;
   end;

implementation

{ TPrimaryKeyOptions }

constructor TPrimaryKeyOptions.Create;
begin
  Columns  := TStringList.Create();
end;

constructor TPrimaryKeyOptions.Create(aName, aTableName: string; aColumns: TStringList);
begin
  Name       := aName;
  TableName  := aTableName;
  Columns    := aColumns;
end;

constructor TPrimaryKeyOptions.Create(aName, aTableName, aColumn: string);
begin
  Create();
  Columns.Add(aColumn);
  Create(aName, aTableName,Columns);
end;

end.
