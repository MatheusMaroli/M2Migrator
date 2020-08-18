unit UMigrationRigisterScript;

interface
   uses Generics.Collections, Classes;
   type
   TMigrationRigisterScript = class;
   TRegisterMigrationEvent = procedure(sender : TMigrationRigisterScript) of object;
   TMigrationRigisterScript = class(TComponent)
   private
      FOnRegister: TRegisterMigrationEvent;
      FMigrations: TList<TClass>;


   published
     property OnRegister : TRegisterMigrationEvent read FOnRegister write FOnRegister;

     function GetAllMigrations() : TList<TClass>;
     procedure AppendMigration(aMigrationClass : TClass);
     constructor Create(aOwner : TComponent);override ;
   end;
   procedure Register();
implementation

procedure Register;
begin
 RegisterComponents('M2 Migrator', [TMigrationRigisterScript]);
end;
{ TMigrationRigisterScript }

procedure TMigrationRigisterScript.AppendMigration(aMigrationClass: TClass);
begin
  FMigrations.Add(aMigrationClass);
end;

constructor TMigrationRigisterScript.Create(aOwner: TComponent);
begin
  inherited;
end;


function TMigrationRigisterScript.GetAllMigrations: TList<TClass>;
begin
  FMigrations := TList<TClass>.Create();
  OnRegister(Self);
  Result := FMigrations
end;

end.
