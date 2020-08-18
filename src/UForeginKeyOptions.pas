unit UForeginKeyOptions;

interface
   uses Classes;
   type
   TForeginKeyOptions = class
   private
     FColumnTableDestiny: TStringList;
     FColumnTableOrigin: TStringList;
     FTableDestiny: string;
     FTableOrigin: string;
     FName: string;

   public
     property Name : string read FName write FName;
     property TableOrigin    : string read FTableOrigin write FTableOrigin;
     property TableDestiny   : string read FTableDestiny write FTableDestiny;
     property ColumnTableOrigin : TStringList read FColumnTableOrigin write FColumnTableOrigin;
     property ColumnTableDestiny :  TStringList read FColumnTableDestiny write FColumnTableDestiny;

     constructor Create();  overload;
     constructor Create(aName, aTableOrigin, aTableDestiny : string; aColumnTableOrigin, aColumnTableDestiny : TStringList);  overload;
     constructor Create(aName, aTableOrigin, aTableDestiny, aColumnTableOrigin, aColumnTableDestiny : string);  overload;
   end;
implementation

{ TForeginKeyOptions }

constructor TForeginKeyOptions.Create;
begin
  ColumnTableOrigin := TStringList.Create();
  ColumnTableDestiny := TStringList.Create();
end;

constructor TForeginKeyOptions.Create(aName, aTableOrigin, aTableDestiny: string; aColumnTableOrigin, aColumnTableDestiny: TStringList);
begin
  Create();
  Name := aName;
  TableOrigin  := aTableOrigin;
  TableDestiny := aTableDestiny;
  ColumnTableOrigin := aColumnTableOrigin;
  ColumnTableDestiny := aColumnTableDestiny;
end;

constructor TForeginKeyOptions.Create(aName, aTableOrigin, aTableDestiny, aColumnTableOrigin, aColumnTableDestiny: string);
begin
  Create();
  ColumnTableOrigin.Add(aColumnTableOrigin);
  ColumnTableDestiny.Add(aColumnTableDestiny);
  Create(aName, aTableOrigin, aTableDestiny, ColumnTableOrigin, ColumnTableDestiny);
end;

end.
