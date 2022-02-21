unit UColumnOptions;

interface
  uses SysUtils, UMigrationType;
  type
    TColumnOptions = class
  private
    FLength: Double;
    FDefaultValue: string;
    FPrecision: Double;
    FColumnType: TDataBaseType;
    FNotNull: Boolean;
    FColumnName: string;


    public
      property ColumnName: string    read FColumnName write FColumnName;
      property ColumnType : TDataBaseType  read FColumnType write FColumnType;
      property Length : Double           read FLength write  FLength;
      property Precision : Double         read FPrecision write FPrecision;
      property NotNull : Boolean          read FNotNull write FNotNull;
      property DefaultValue : string      read FDefaultValue write FDefaultValue;

      constructor Create(aColumnName : string;
        aColumnType : TDataBaseType;
        aLength :Double; aPrecision : Double;
        aNotNull : Boolean;
        aDefaultValue : string);

    end;
implementation

{ TColumnOptions }

constructor TColumnOptions.Create(aColumnName: string; aColumnType: TDataBaseType; aLength: Double; aPrecision: Double; aNotNull: Boolean; aDefaultValue: string);
begin
  ColumnName := aColumnName;
  ColumnType := aColumnType;
  Length := aLength;
  Precision := aPrecision;
  NotNull := aNotNull;
  DefaultValue := aDefaultValue;
end;

end.
