Instalação
1º Abrir o projeto pelo delphi com o projeto aberto na aba "Project Manager"  clicar com o botão direito no menu que foi aberto selecionar a opção "Build", 
se não ocorrer nenhum erro realizar o mesmo processo e selecionar a opção "Install" do menu.

2º Adicionar a library path do caminho ondem foi descompactado o projeto. 

Componentes / Clases

TMigrator
   Essa clase contem o core do migrador, responsavel por criação do cotrole de histórico das migrações, execução das migrações pendentes e reversões de migrações já executadas.
   Sendo uma classe abstrada deve ser implementado para cada banco de dados correspondente devido seus próprio dialeto SQL.
   
   Propertys
	SQLConnection : Conexão do banco de dados
	MigrationClassRegister : componente que contem todas as implementações da interface IMigrationScript
	SystemVersion : Versão da migração, podendo ser controlada pela versão do sistema que está executando os script.
	DialectType: Tipo de dialeto de SQL que sera utilizado, no momento só está implementado oracle. 

TMigratorOracle
	Extenção da classe TMigrator, responsavel pelas implementação do banco de dados oracle.
	
TMigrationRegisterScript
	Responsavel por registrar todos os script de migrations implementado atravez da interface "IMigrationScript"
	
	Events
		MigrationRigister: evento de registro dos scripts, cada classe que foi implementada a interface "IMigrationScript" 
		deve ser registrada nesse evento no utilizando o método AppendMigration disponibilizado pelo parametro sender
		
IMigrationScript
	Interface que deve ser implementada para controlar cada alteração que foi realizada no banco de dados.
	Metodos
		MigrationName: nome da migration não pode ter nomes repetidos devem ser unicos, para não gerar confução sugiro utilizar o nome da classe criada 
		MigrationAuthor: nome do autor que realiz a criação da migração, utilizado para geração de histórico e controle de erros.
		Up(aDialect : TSQLDialect): esse método é responsavel pelo script de criação do banco, podendo ser criado pelas função pré definida ou CustomScript 
					  onde é passado uma string com o script sql e a classe realiza a exeução.
		Down(aDialect : TSQLDialect): método responsavel por reverter o script rodado no métodos Up caso algum erro ocorra, no momento que esse método sera executado a ferramenta realiza uma pergunta para usuário
					                  caso não queria reverter o script	
