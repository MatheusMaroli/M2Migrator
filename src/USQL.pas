unit USQL;

interface
  type
    TSQL = class abstract

    public
      procedure ExecuteSQL(script : string); virtual; abstract;
      function GetIntegerValue(script  : string) : Integer; virtual;abstract;


    end;
implementation

end.
