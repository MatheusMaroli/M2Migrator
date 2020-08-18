Instala��o
1� Abrir o projeto pelo delphi com o projeto aberto na aba "Project Manager"  clicar com o bot�o direito no menu que foi aberto selecionar a op��o "Build", 
se n�o ocorrer nenhum erro realizar o mesmo processo e selecionar a op��o "Install" do menu.

2� Adicionar a library path do caminho ondem foi descompactado o projeto. 

Componentes / Clases

TMigrator
   Essa clase contem o core do migrador, responsavel por cria��o do cotrole de hist�rico das migra��es, execu��o das migra��es pendentes e revers�es de migra��es j� executadas.
   Sendo uma classe abstrada deve ser implementado para cada banco de dados correspondente devido seus pr�prio dialeto SQL.
   
   Propertys
	SQLConnection : Conex�o do banco de dados
	MigrationClassRegister : componente que contem todas as implementa��es da interface IMigrationScript
	SystemVersion : Vers�o da migra��o, podendo ser controlada pela vers�o do sistema que est� executando os script.
	DialectType: Tipo de dialeto de SQL que sera utilizado, no momento s� est� implementado oracle. 

TMigratorOracle
	Exten��o da classe TMigrator, responsavel pelas implementa��o do banco de dados oracle.
	
TMigrationRegisterScript
	Responsavel por registrar todos os script de migrations implementado atravez da interface "IMigrationScript"
	
	Events
		MigrationRigister: evento de registro dos scripts, cada classe que foi implementada a interface "IMigrationScript" 
		deve ser registrada nesse evento no utilizando o m�todo AppendMigration disponibilizado pelo parametro sender
		
IMigrationScript
	Interface que deve ser implementada para controlar cada altera��o que foi realizada no banco de dados.
	Metodos
		MigrationName: nome da migration n�o pode ter nomes repetidos devem ser unicos, para n�o gerar confu��o sugiro utilizar o nome da classe criada 
		MigrationAuthor: nome do autor que realiz a cria��o da migra��o, utilizado para gera��o de hist�rico e controle de erros.
		Up(aDialect : TSQLDialect): esse m�todo � responsavel pelo script de cria��o do banco, podendo ser criado pelas fun��o pr� definida ou CustomScript 
					  onde � passado uma string com o script sql e a classe realiza a exeu��o.
		Down(aDialect : TSQLDialect): m�todo responsavel por reverter o script rodado no m�todos Up caso algum erro ocorra, no momento que esse m�todo sera executado a ferramenta realiza uma pergunta para usu�rio
					                  caso n�o queria reverter o script	
