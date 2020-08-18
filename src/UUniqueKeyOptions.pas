unit UUniqueKeyOptions;

interface
   uses Classes;
   type
   TUniqueKeyOptions = class
   private
     FName: string;
     FTableName: string;
     FColumns: TStringList;
   public
     property Name : string read FName write FName;
     property TableName : string read FTableName write FTableName;
     property Columns : TStringList read FColumns write FColumns;

     constructor Create();  overload;
     constructor Create(aName, aTableName: string; aColumns : TStringList);  overload;
   end;
implementation

{ TUniqueKeyOptions }

constructor TUniqueKeyOptions.Create;
begin
  Columns := TStringList.Create();
end;

constructor TUniqueKeyOptions.Create(aName, aTableName : string; aColumns: TStringList);
begin
  Name := aName;
  TableName := aTableName;
  Columns  := aColumns;
end;

end.
