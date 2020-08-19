Componentes / Classes


TMigrator
Essa classe contem o core do migrador, respons�vel por cria��o do controle de hist�rico das migra��es, execu��o das migra��es pendentes e revers�es de migra��es j� executadas.
Sendo uma classe abstra�da deve ser implementado para cada banco de dados correspondente devido seus pr�prio dialeto SQL.

Property
SQLConnection : Conex�o do banco de dados
MigrationClassRegister : componente que contem todas as implementa��es da interface IMigrationScript
SystemVersion : Vers�o da migra��o, podendo ser controlada pela vers�o do sistema que est� executando os script.
DialectType: Tipo de dialeto de SQL que sera utilizado, no momento s� est� implementado Oracle.

TMigratorOracle
Extens�o da classe TMigrator, respons�vel pelas implementa��o do banco de dados Oracle.

TMigrationRegisterScript
Respons�vel por registrar todos os script de migrations implementado atrav�s da interface "IMigrationScript"

Events
MigrationRigister: evento de registro dos scripts, cada classe que foi implementada a interface "IMigrationScript"
deve ser registrada nesse evento no utilizando o m�todo AppendMigration disponibilizado pelo parametro sender

TSQLDialect
Classe respons�vel por cria��o do script e execu��o dos script SQL, para cria��o de script pode ser utilizado m�todos pr�-definido ou criar um script SQL e passar como par�metro para m�todo "CustonScript"
M�todos  pr�-definido

CreateTable(tableName : string; columns : TObjectList<TColumnOptions>)
DropTable(tableName : string)
CreateColumn(tableName : string; column: TCOlumnOptions )
DropColumn(tableName, columnName : string)
CreateForeginKey(foreginKeyOptions : TForeginKeyOptions)
CreateUniqueKey(uniqueKeyOptions : TUniqueKeyOptions)
DropConstraint(tableName : string; foreginKeyName : string)
CreatePrimaryKey(primaryKeyOptions : TPrimaryKeyOptions)

M�todo de execu��o de SQL Nativo.
CustomScript(aScript : string)

TCOlumnOptions
Property
ColumnName:Nome da coluna
ColumnType : Tipo de dado da coluna
Length : tamanho do campo caso for varchar // caso o campo for numeric pode deixar zerado
Precision : precis�o do campo para num�rico (11,2) (11,6) // caso o campo for string pode deixar zerado
NotNull : deve ser True para coluna ser not Null, false para coluna Null
DefaultValue : string  // caso n�o tiver um valor default deve se atribuir string vazia.

TForeginKeyOptions
Property
Name : Nome da FK para registrar no banco de dados
TableOrigin : tabela de origem
TableDestiny : tabela de destino(referencia)
ColumnTableOrigin : lista de colunas da tabela de origem // obs: tamb�m � poss�vel passar string separada por virgula caso tiver N campos
ColumnTableDestiny : lista de colunas da tabela de destino(referencia) // obs: tamb�m � poss�vel passar string separada por virgula caso tiver N campos

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
