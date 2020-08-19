Componentes / Classes


TMigrator
Essa classe contem o core do migrador, responsável por criação do controle de histórico das migrações, execução das migrações pendentes e reversões de migrações já executadas.
Sendo uma classe abstraída deve ser implementado para cada banco de dados correspondente devido seus próprio dialeto SQL.

Property
SQLConnection : Conexão do banco de dados
MigrationClassRegister : componente que contem todas as implementações da interface IMigrationScript
SystemVersion : Versão da migração, podendo ser controlada pela versão do sistema que está executando os script.
DialectType: Tipo de dialeto de SQL que sera utilizado, no momento só está implementado Oracle.

TMigratorOracle
Extensão da classe TMigrator, responsável pelas implementação do banco de dados Oracle.

TMigrationRegisterScript
Responsável por registrar todos os script de migrations implementado através da interface "IMigrationScript"

Events
MigrationRigister: evento de registro dos scripts, cada classe que foi implementada a interface "IMigrationScript"
deve ser registrada nesse evento no utilizando o método AppendMigration disponibilizado pelo parametro sender

TSQLDialect
Classe responsável por criação do script e execução dos script SQL, para criação de script pode ser utilizado métodos pré-definido ou criar um script SQL e passar como parâmetro para método "CustonScript"
Métodos  pré-definido

CreateTable(tableName : string; columns : TObjectList<TColumnOptions>)
DropTable(tableName : string)
CreateColumn(tableName : string; column: TCOlumnOptions )
DropColumn(tableName, columnName : string)
CreateForeginKey(foreginKeyOptions : TForeginKeyOptions)
CreateUniqueKey(uniqueKeyOptions : TUniqueKeyOptions)
DropConstraint(tableName : string; foreginKeyName : string)
CreatePrimaryKey(primaryKeyOptions : TPrimaryKeyOptions)

Método de execução de SQL Nativo.
CustomScript(aScript : string)

TCOlumnOptions
Property
ColumnName:Nome da coluna
ColumnType : Tipo de dado da coluna
Length : tamanho do campo caso for varchar // caso o campo for numeric pode deixar zerado
Precision : precisão do campo para numérico (11,2) (11,6) // caso o campo for string pode deixar zerado
NotNull : deve ser True para coluna ser not Null, false para coluna Null
DefaultValue : string  // caso não tiver um valor default deve se atribuir string vazia.

TForeginKeyOptions
Property
Name : Nome da FK para registrar no banco de dados
TableOrigin : tabela de origem
TableDestiny : tabela de destino(referencia)
ColumnTableOrigin : lista de colunas da tabela de origem // obs: também é possível passar string separada por virgula caso tiver N campos
ColumnTableDestiny : lista de colunas da tabela de destino(referencia) // obs: também é possível passar string separada por virgula caso tiver N campos

TUniqueKeyOptions
Property
Name : Nome da UK no banco de dados
TableName : table que sera criado a UK
Columns : lista de campos da UK

TPrimaryKeyOptions
Property
Name : Nome da Pk para banco de dados
TableName : Nome da tabela da Pk
Columns : colunas que fazem parte da chave.
