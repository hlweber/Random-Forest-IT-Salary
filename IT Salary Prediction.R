##########################################################################################

#________________________________ 0. SETUP ______________________________________#


# 0.1. Importando Bibliotecas
library(fpp3)
library(mice)
library(stringr)
library(readxl)
library(GGally)
library(rpart)
library(corrplot)
library("scales")
require(glmnetUtils)
require(caret)


# 0.2. Definindo a pasta
setwd('C:/Users/hlweb/OneDrive/Documentos/Faculdade/Pós/Materias/Aprendizado de Maquinas/TrabalhoFinal')


# 0.3. Lendo a base de dados
original_df = read.csv('data_tratada.csv',na.strings="")


##########################################################################################

#________________________________ 1. OLHANDO O DATAFRAME ________________________________#

## -> Entender estrutura do dataframe
## -> Observar dados faltantes

#_________________________


# 1.1. Plotando algumas linhas do dataframe
head(original_df)


# 1.2. Plotando estatísticas descritivas
summary(original_df)


# 1.3. Plotando nome das variáveis
colnames(original_df)

# 1.4. Observando algumas colunas individualmente
infos_colunas = function(coluna){
  print(coluna)
  print(length(unique(original_df[,coluna])))
  print(as.data.frame(table(original_df[,coluna]))%>%
          arrange(desc(Freq)))
}
  
  # 1.4.1. DEMOGRÁFICO
    # "Country"
    infos_colunas('Country')
    
  # 1.4.2. PESSOAL
    # "ProgramHobby"
    infos_colunas('ProgramHobby')
    # "YearsProgram"
    infos_colunas('YearsProgram')
    # "YearsCodedJob"
    infos_colunas('YearsCodedJob')
    # "WebDeveloperType"
    infos_colunas('WebDeveloperType')
    # "ProblemSolving"
    infos_colunas('ProblemSolving')
    # "AssessJobIndustry"
    infos_colunas("AssessJobIndustry")
    # "JobProfile"
    infos_colunas('JobProfile')
    # "LearnedHiring"
    infos_colunas('LearnedHiring')
    # "ImportantHiringAlgorithms"
    infos_colunas('ImportantHiringAlgorithms')
    # "TabsSpaces"
    infos_colunas('TabsSpaces')
    # "CousinEducation"
    infos_colunas('CousinEducation')
    # "AuditoryEnvironment"
    infos_colunas('AuditoryEnvironment')
    # "VersionControl"
    infos_colunas('VersionControl')
    # "CheckInCode"
    infos_colunas('CheckInCode')
    # "ShipIt"
    infos_colunas('ShipIt') 
    
  # 1.4.3. EDUCAÇÃO
    # "University"
    infos_colunas('University')
    # "FormalEducation"
    infos_colunas('FormalEducation')
    # "MajorUndergrad"
    infos_colunas('MajorUndergrad')
    # "EducationImportant"
    infos_colunas('EducationImportant')
    # "SelfTaughtTypes"
    infos_colunas("SelfTaughtTypes")
    # "Methodology"
    infos_colunas('Methodology')
    
  # 1.4.4. EMPRESA
    # "CompanySize"
    infos_colunas('CompanySize')
    # "CompanyType"
    infos_colunas('CompanyType')

  # 1.4.5. CONDICOES TRABALHO
    # "EmploymentStatus"
    infos_colunas('EmploymentStatus')
    # "HomeRemote"
    infos_colunas('HomeRemote')
    # "JobSeekingStatus"
    infos_colunas('JobSeekingStatus')
    # "LastNewJob"
    infos_colunas('LastNewJob')
    # "Overpaid"
    infos_colunas('Overpaid')
    # "WorkStart"
    infos_colunas('WorkStart')
  
  
  

  
  
# =====================================> INSIGHTS
  
  # __________DEMOGRÁFICO
    # "Country" e "Currency"
      #   - Agrupar - Agrupamento por continente (?)
      #   - Novo - Média salarial do país - Comparar diferença do salário que a pessoa recebe em relação a média
      #   - Novo - Alguma medida de paridade de pdoer de compra (PPP, Big Mac Index, Litros de Gasolina etc)
    
    
  # __________PESSOAL
    # "ProgramHobby"
      # - Agrupar - sim ou não apenas (?)
    # "YearsProgram" e "YearsCodedJob"
      # - Agrupar - Criar grupos maiores. Atualmente cada grupo é de um em um ano
      # - Transformar - Mudar de categorica para numerico
      # - Novo - Nova variável soma dos dois valores
    # "WebDeveloperType"
      # - Agrupar - É Web developer ou não
    # "CarrerSatisfaction" e "JobSatisfaction"
      # - Novo - Job/ Carrer divisão
    # De "ProblemaSolving" a "ChangeWorld"
      # - Transformar - Categorias em valores de -2 a 2
      # - Novo - Soma dos valores das variáveis (categoria "Personalidade") (atribuir peso a cada categoria na soma??)
    # De "AssessJobIndustry" a "AssessJobFinances"
      # - Transformar - Categorias em valores de 0 a 4
      # - Novo - Soma dos valores das variáveis (categoria "NivelExigenciaTrabalho") (atribuir peso a cada categoria na soma??)
    # "JobProfile"
      # - Criar - Variável quantidade de JobProfiles
    # De "ImportantHiringAlgorithms" a "ImportantHiringGettingThingsDone"
      # - Transformar - Categorias em valores de 0 a 4
      # - Novo - Soma dos valores das variáveis (categoria "NivelExigenciaPessoal") (atribuir peso a cada categoria na soma??)
    # "CousinEducation"
      # - Ecluir - Alta cardinalidade
    # "HaveWorkedFramework" e "WantWorkedFramework"
      # - Criar - Variável quantidade de linguagem
      # - Agrupar - Linguagens por tipo de desenvolvimento
      # - Criar - Variável quantidade de tipos de desenvolvimentos
    # "CheckInCode"
      # - Agrupar - Frequente e Não Frequente (?)
    # "ShipIt"
      # - Transformar - Categorias em valores de -2 a 2 
    
  # __________EDUCAÇÃO
    # "University"
      # - Agrupar - Deixar apenas Sim e Não (?)
    # "FormalEducation"
      # - Agrupar - Ensino básico, graducao, mestrado e doutorado (?)
    # "MajorUndergrad"
      # - Agrupar em relacionados a computação - exatas - outros (?)
    # "EducationImportant"
      # - Transformar - Categorias em valores de 0 a 4
    # "SelfTaughtTypes"
      # - Criar - Variável "QtdaFontes" - quantidade de fontes de estudos
    # "Methodology"
      # - Criar - Variável quantidade de metodologias
      # - Agrupar - Algum agrupamento de metodologia (?)
      # - Criar - Qtda de diferentes agrupamentos
  
  # __________EMPRESA
    # "CompanySize"
      # - Agrupar - em pequenas, médias e grande empresas (?)
    # "CompanyType"
      # - Agrupar - Privada estruturada - Startup - Proprio - Outros (?)
    
    
  # __________CONDICOES TRABALHO
    # "EmploymentStatus"
      # - Excluir - Quase 100% é full-time, talvez a coluna não seja tão importante
    # "HomeRemote"
      # - Agurpar - Sim, não, as vezes (?)
    # "LastNewJob"
      # - Criar - Variável "PrimeiroTrabalho", caso workingcode e lastnewjob tenham msm periodo
    # "Overpaid"
      # - Transformar - de -2 a 2
    # "WorkStart"
      # - Agrupar - Horários em segmentos de horário -> manhã_cedo, manhã, dia, tarde, noite, madrugada
    

##########################################################################################
    
#________________________________ 2. PRIMEIRA TRANSFORMAÇÃO DOS DADOS____________________#
    

#_________________________


# 2.1. Importando dados externos
infos_paises = read_excel("Auxiliares/AuxiliarCompleto.xlsx")
    
# 2.1. Juntando Informação Externa
new_df = original_df
new_df = left_join(new_df,infos_paises,by='Country')


# 2.2. Ajustando variável salário
new_df = new_df %>%
  mutate(newSalary = ifelse(is.na(Currency) | Currency!= "U.S. dollars ($)", Salary/PPP, Salary)) %>%
  mutate(Salary_Mean_Ratio = newSalary/AverageAnualIncome17)

new_df %>%
  select(newSalary,Country, AverageAnualIncome17, Salary_Mean_Ratio)
#==> Observando os dados e comparando com o PIB per Capita com ajuste por PPP de 2017, é possível verificar que em alguns paises os valores de salário foram salários mensaias e até semanais e não anual.
# Plotando valores de salários ajustados
ggplot(new_df, aes(y=newSalary)) +
  geom_boxplot() +
  theme_classic()
#==> Alguns valores não fazem sentido, como salário igual a zero. Vamos plotar o gráfico para cada pais
ggplot(new_df, aes(y=newSalary,x=Country)) +
  geom_boxplot() +
  theme_classic()
#==> Vamos ver mais de perto os valores que estão baixos quando comparado ao PIB per Capita
#    Será plotado tbm a avaliação da pessoa a respeito do salário
verificacao_salarios = new_df %>%
                      select(Country,Overpaid, Salary, newSalary, Salary_Mean_Ratio, AverageAnualIncome17) %>%
                      filter(Salary_Mean_Ratio<0.6)
# Vamos agrupar os dados por pais e tirar a média do Salary_Mean_Ratio, parece que alguns paises apresentam o msm problema
new_df %>%
  select(Country,Salary_Mean_Ratio) %>%
  group_by(Country) %>%
  add_count(Country) %>%
  summarise(media=mean(Salary_Mean_Ratio),obs=mean(n)) %>%
  arrange(media) %>%
  filter(media<20,obs>10) %>%
  ggplot(aes(x=media,y=Country)) +
  geom_col() +
  geom_vline(xintercept = 0.1, linetype=2, color='red') +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA))
# ==> Alguns paises parecem ser intricicos a eles essa defasagem.
# Talvez as pessoas negociam o salário com base de mês e responderam a pesquisa nessa base ao inves de anual

# ==> Multiplicaremos newSalary por 12 quando -> Salary_Mean_Ratio for menor que 0.01 e não se considerar underpaid ou se for menor que 0.01 e pertencer a um dos paises que estão no limite
new_df = new_df %>%
        mutate(newSalary=ifelse((Salary_Mean_Ratio<0.1&!Overpaid%in%c("Somewhat underpaid","Greatly underpaid"))|(Salary_Mean_Ratio<0.1&Country%in%c('Sweden','Russian Federation','India','Czech Republic')),newSalary*12,newSalary)) %>%
        mutate(Salary_Mean_Ratio = newSalary/AverageAnualIncome17)

new_df = new_df[!is.na(new_df$newSalary),]
##########################################################################################

#________________________________ 3. VISUALIZANDO DADOS _________________________________#

## -> Distribuição das variáveis
## -> Relações entre variáveis

#_________________________


# 3.1. Boxplot das variáveis
variaveis_boxplot = c('Salary','newSalary','Salary_Mean_Ratio')
for (coluna in variaveis_boxplot) {
  show(ggplot(new_df, mapping = aes(x=new_df[,coluna])) +
         geom_boxplot(color="black", fill="white") +
         labs(title=str_glue('Distribuição dos Valores - {coluna}'),x=coluna) +
         theme_classic())
  ggsave(str_glue("Plots/BoxPlot_{coluna}.png"), width = 8, height = 3, units = "in", dpi = 300)
}

# 3.2. Relação Variável Categórica e newSalary
  # Melhor Segregação por Localização
  # Pais
  new_df %>%
    ggplot(aes(x=Country,y=newSalary)) +
    geom_boxplot()+
    labs(title=str_glue('Distribuição dos Valores Salario - Overpaid'),y='Salário') +
    theme_classic()
  ggsave(str_glue("Plots/Relacao_Salario_Pais.png"), width = 8, height = 3, units = "in", dpi = 300)
  # Continente
  new_df %>%
    ggplot(aes(x=Continent,y=newSalary)) +
    geom_boxplot()+
    labs(title=str_glue('Distribuição dos Valores Salario - Overpaid'),y='Salário') +
    theme_classic()
  ggsave(str_glue("Plots/Relacao_Salario_Continente.png"), width = 8, height = 3, units = "in", dpi = 300)
  # Região
  new_df %>%
    ggplot(aes(x=Region,y=newSalary)) +
    geom_boxplot()+
    labs(title=str_glue('Distribuição dos Valores Salario - Overpaid'),y='Salário') +
    theme_classic()
  ggsave(str_glue("Plots/Relacao_Salario_Regiao.png"), width = 8, height = 3, units = "in", dpi = 300)
  # Desenvolvimento do Pais
  new_df %>%
    ggplot(aes(x=`Development Status`,y=newSalary)) +
    geom_boxplot()+
    labs(title=str_glue('Distribuição dos Valores Salario - Overpaid'),y='Salário') +
    theme_classic()
  ggsave(str_glue("Plots/Relacao_Salario_Desenvolvimento.png"), width = 8, height = 3, units = "in", dpi = 300)
  # ==> Existe uma relação boa nos agrupamentos de desenvolvimento e continente - vamos usar esses agrupamentos
  # IMPORTANTE VERIFICAR -> Salarios em paises não desenvolvidos maior do que nos em desenvolvimento, pq?
  vetor_comparacao_eixo_x = c('ProgramHobby','WebDeveloperType','ProblemSolving',
                              'CheckInCode','CousinEducation','University','FormalEducation',
                              'HomeRemote','Overpaid','WorkStart','JobSeekingStatus','ResumePrompted',
                              'LearnedHiring','TabsSpaces','StackOverflowDescribes','StackOverflowJobListing',
                              'StackOverflowMakeMoney','HighestEducationParents')
  vetor_comparacao_eixo_y = c('YearsProgram','YearsCodedJob','MajorUndergrad','CompanySize','CompanyType')
  vetor_comparacao_scatter = c('CareerSatisfaction','JobSatisfaction')
  
  for (comp in vetor_comparacao_eixo_x) {
    show(new_df %>%
      ggplot(aes(x=!!sym(comp),y=newSalary)) +
      geom_boxplot()+
      labs(title=str_glue('Distribuição dos Valores Salario - {comp}'),y='Salário') +
      theme_classic())
    ggsave(str_glue("Plots/Relacao_Salario_{comp}.png"), width = 8, height = 3, units = "in", dpi = 300)
  }
  for (comp in vetor_comparacao_eixo_y) {
    show(new_df %>%
           ggplot(aes(y=!!sym(comp),x=newSalary)) +
           geom_boxplot()+
           labs(title=str_glue('Distribuição dos Valores Salario - {comp}'),y='Salário') +
           theme_classic())
    ggsave(str_glue("Plots/Relacao_Salario_{comp}.png"), width = 8, height = 3, units = "in", dpi = 300)
  }
  for (comp in vetor_comparacao_scatter) {
    show(new_df %>%
           ggplot(aes(x=!!sym(comp),y=newSalary)) +
           geom_point(alpha=0.2)+
           labs(title=str_glue('Distribuição dos Valores Salario - {comp}'),y='Salário') +
           theme_classic())
    ggsave(str_glue("Plots/RelacaoScatter_Salario_{comp}.png"), width = 8, height = 3, units = "in", dpi = 300)
  }
  
  
# =====================================> RESUMO
  # EXCEL "CONTROLE VARIAVEIS"
  


##########################################################################################

#________________________________ 4. TRATANDO BASE ______________________________________#

## -> arrumando valores faltantes
## -> transformando variaveis conforme observações
## -> divindo base de treino e teste

#_________________________

clean_df = new_df
  
infos_colunas2 = function(coluna){
    print(coluna)
    print(length(unique(clean_df[,coluna])))
    print(as.data.frame(table(clean_df[,coluna]))%>%
            arrange(desc(Freq)))
}
  
# 4.1. Valores Faltantes
missing = md.pairs(new_df)$mm
# ==> variaveis de escala likert vamos imputar com valor igual a zero
# ==> as demais vamos verificar caso a caso após a limpeza da base

infos_colunas2("StackOverflowMakeMoney")
# 4.2. AGRUPAMENTOS
  # University
  clean_df = clean_df %>%
            mutate(University = ifelse(University=="No",0,1))
  # FormalEducation
  clean_df = clean_df %>%
    mutate(FormalEducation = ifelse(FormalEducation=="Doctoral degree","Doc",ifelse(FormalEducation%in%c("Bachelor's degree","Master's degree","Some college/university study without earning a bachelor's degree"),"High Education","Basic Education")))
  # MajorUndergrad
  clean_df = clean_df %>%
    mutate(ComputerMajor = ifelse(MajorUndergrad%in%c("Computer science or software engineering","Computer engineering or electrical/electronics engineering","Computer programming or Web development","Information technology, networking, or system administration","Management information systems"),1,0))
  clean_df$MajorUndergrad = NULL
  # HomeRemote
  clean_df = clean_df %>%
    mutate(HomeRemote = ifelse(HomeRemote=="All or almost all the time (I'm full-time remote)","Sempre",ifelse(HomeRemote=="Never","Nunca","As Vezes")))
  # CompanySize
  clean_df = clean_df %>%
    mutate(CompanySize = ifelse(CompanySize%in%c("Fewer than 10 employees","10 to 19 employees","20 to 99 employees"),"Pequena",ifelse(CompanySize%in%c("100 to 499 employees","500 to 999 employees"),"Média",ifelse(CompanySize%in%c("1,000 to 4,999 employees","5,000 to 9,999 employees","10,000 or more employees"),"Grande","Outro"))))
  # YearsProgram
  vetor_cat1 = c('Less than a year','1 to 2 years','2 to 3 years')
  vetor_cat2 = c('3 to 4 years','4 to 5 years','5 to 6 years','6 to 7 years','7 to 8 years','8 to 9 years','9 to 10 years')
  vetor_cat3 = c('10 to 11 years','11 to 12 years','12 to 13 years','13 to 14 years','14 to 15 years','15 to 16 years','16 to 17 years','17 to 18 years','18 to 19 years','19 to 20 years')
  clean_df = clean_df %>%
    mutate(YearsProgram = ifelse(YearsProgram%in%vetor_cat1,"Menos de 3 Anos",ifelse(YearsProgram%in%vetor_cat2,"Entre 3 e 10 anos",ifelse(YearsProgram%in%vetor_cat3,"Entre 10 e 20 anos",ifelse(YearsProgram=="20 or more years","Mais de 20 anos","Outro")))))
  # YearsCodedJob
  clean_df = clean_df %>%
    mutate(YearsCodedJob = ifelse(YearsCodedJob%in%vetor_cat1,"Menos de 3 Anos",ifelse(YearsCodedJob%in%vetor_cat2,"Entre 3 e 10 anos",ifelse(YearsCodedJob%in%vetor_cat3,"Entre 10 e 20 anos",ifelse(YearsCodedJob=="20 or more years","Mais de 20 anos","Outro")))))
  # LastNewJob
  clean_df = clean_df %>%
    mutate(MaisQuatroAnosUltimoTrabalho = ifelse(LastNewJob=="More than 4 years ago",1,0))
  clean_df$LastNewJob=NULL
  # CheckInCode
  clean_df = clean_df %>%
    mutate(CheckInCode = ifelse(CheckInCode%in%c("Multiple times a day","A few times a week","Once a day"),1,0))
  # StackOverflowDescribes
  clean_df = clean_df %>%
    mutate(StackOverflowCV = ifelse(StackOverflowDescribes=="I have created a CV or Developer Story on Stack Overflow",1,0))
  clean_df$StackOverflowDescribes=NULL
  # StackOverflowFoundAnswer
  vetor_cat1 = c('At least once each week','Several times','At least once each day')
  clean_df = clean_df %>%
    mutate(StackOverflowFoundAnswer = ifelse(StackOverflowFoundAnswer%in%vetor_cat1,1,0))
  # StackOverflowCopiedCode
  clean_df = clean_df %>%
    mutate(StackOverflowCopiedCode = ifelse(StackOverflowCopiedCode%in%vetor_cat1,1,0))
  # StackOverflowJobListing
  clean_df = clean_df %>%
    mutate(StackOverflowJobListing = ifelse(StackOverflowJobListing%in%vetor_cat1,1,0))
  # StackOverflowCompanyPage
  clean_df = clean_df %>%
    mutate(StackOverflowCompanyPage = ifelse(StackOverflowCompanyPage%in%vetor_cat1,1,0))
  # StackOverflowJobSearch
  clean_df = clean_df %>%
    mutate(StackOverflowJobSearch = ifelse(StackOverflowJobSearch%in%vetor_cat1,1,0))
  # StackOverflowNewQuestion
  clean_df = clean_df %>%
    mutate(StackOverflowNewQuestion = ifelse(StackOverflowNewQuestion%in%vetor_cat1,1,0))
  # StackOverflowAnswer
  clean_df = clean_df %>%
    mutate(StackOverflowAnswer = ifelse(StackOverflowAnswer%in%vetor_cat1,1,0))
  # StackOverflowMetaChat
  clean_df = clean_df %>%
    mutate(StackOverflowMetaChat = ifelse(StackOverflowMetaChat%in%vetor_cat1,1,0))

# 4.3. TRANSFORMANDO ESCALA LIKERT
  # GRUPO 1
  vetor_grupo_1 = c('ProblemSolving','BuildingThings','LearningNewTech',
                     'BoringDetails',
                     'JobSecurity',
                     'DiversityImportant',
                     'AnnoyingUI',
                     'FriendsDevelopers',
                     'RightWrongWay',
                     'UnderstandComputers',
                     'SeriousWork',
                     'InvestTimeTools',
                     'WorkPayCare',
                     'KinshipDevelopers',
                     'ChallengeMyself',
                     'CompetePeers',
                     'ChangeWorld')
  for (coluna in vetor_grupo_1) {
    clean_df = clean_df %>%
      mutate(!!sym(coluna) := ifelse(is.na(!!sym(coluna)),0,ifelse(!!sym(coluna)=="Strongly agree",2,ifelse(!!sym(coluna)=="Agree",1,ifelse(!!sym(coluna)=="Strongly disagree",-2,ifelse(!!sym(coluna)=="Disagree",-1,0))))))
  }
  
  # GRUPO 2
  vetor_grupo_2 = c('AssessJobIndustry',
                    'AssessJobRole',
                    'AssessJobExp',
                    'AssessJobDept',
                    'AssessJobTech',
                    'AssessJobProjects',
                    'AssessJobCompensation',
                    'AssessJobOffice',
                    'AssessJobCommute',
                    'AssessJobRemote',
                    'AssessJobLeaders',
                    'AssessJobProfDevel',
                    'AssessJobDiversity',
                    'AssessJobProduct',
                    'AssessJobFinances')
  for (coluna in vetor_grupo_2) {
    clean_df = clean_df %>%
      mutate(!!sym(coluna) := ifelse(is.na(!!sym(coluna)),0,ifelse(!!sym(coluna)=="Very important",4,ifelse(!!sym(coluna)=="Important",3,ifelse(!!sym(coluna)=="Somewhat important",2,ifelse(!!sym(coluna)=="Not very important",1,0))))))
  }
  
  # GRUPO 3
  vetor_grupo_3 = c('ImportantHiringAlgorithms',
                    'ImportantHiringTechExp',
                    'ImportantHiringCommunication',
                    'ImportantHiringOpenSource',
                    'ImportantHiringPMExp',
                    'ImportantHiringCompanies',
                    'ImportantHiringTitles',
                    'ImportantHiringEducation',
                    'ImportantHiringRep',
                    'ImportantHiringGettingThingsDone')
  for (coluna in vetor_grupo_3) {
    clean_df = clean_df %>%
      mutate(!!sym(coluna) := ifelse(is.na(!!sym(coluna)),0,ifelse(!!sym(coluna)=="Very important",4,ifelse(!!sym(coluna)=="Important",3,ifelse(!!sym(coluna)=="Somewhat important",2,ifelse(!!sym(coluna)=="Not very important",1,0))))))
  }
  
  # GRUPO 4
  vetor_grupo_4 = c('ShipIt',
                    'OtherPeoplesCode',
                    'ProjectManagement',
                    'EnjoyDebugging',
                    'InTheZone',
                    'DifficultCommunication',
                    'CollaborateRemote')
  for (coluna in vetor_grupo_4) {
    clean_df = clean_df %>%
      mutate(!!sym(coluna) := ifelse(is.na(!!sym(coluna)),0,ifelse(!!sym(coluna)=="Strongly agree",2,ifelse(!!sym(coluna)=="Agree",1,ifelse(!!sym(coluna)=="Strongly disagree",-2,ifelse(!!sym(coluna)=="Disagree",-1,0))))))
  }
  
  # GRUPO 5
  vetor_grupo_5 = c('EquipmentSatisfiedMonitors',
                    'EquipmentSatisfiedCPU',
                    'EquipmentSatisfiedRAM',
                    'EquipmentSatisfiedStorage',
                    'EquipmentSatisfiedRW')
  for (coluna in vetor_grupo_5) {
    clean_df = clean_df %>%
      mutate(!!sym(coluna) := ifelse(is.na(!!sym(coluna)),0,ifelse(!!sym(coluna)=="Very satisfied",4,ifelse(!!sym(coluna)=="Satisfied",3,ifelse(!!sym(coluna)=="Somewhat satisfied",2,ifelse(!!sym(coluna)=="Not very satisfied",1,0))))))
  }
  
  # GRUPO 6
  vetor_grupo_6 = c('InfluenceWorkstation',
                    'InfluenceHardware',
                    'InfluenceServers',
                    'InfluenceTechStack',
                    'InfluenceDeptTech',
                    'InfluenceVizTools',
                    'InfluenceDatabase',
                    'InfluenceCloud',
                    'InfluenceConsultants',
                    'InfluenceRecruitment',
                    'InfluenceCommunication')
  for (coluna in vetor_grupo_6) {
    clean_df = clean_df %>%
      mutate(!!sym(coluna) := ifelse(is.na(!!sym(coluna)),0,ifelse(!!sym(coluna)=="I am the final decision maker",4,ifelse(!!sym(coluna)=="A lot of influence",3,ifelse(!!sym(coluna)=="Some influence",2,ifelse(!!sym(coluna)=="Not much influence",1,0))))))
  }
  
  # GRUPO 7
  vetor_grupo_7 = c('StackOverflowAdsRelevant',
                    'StackOverflowAdsDistracting',
                    'StackOverflowModeration',
                    'StackOverflowCommunity',
                    'StackOverflowHelpful',
                    'StackOverflowBetter',
                    'StackOverflowWhatDo',
                    'StackOverflowMakeMoney')
  for (coluna in vetor_grupo_7) {
    clean_df = clean_df %>%
      mutate(!!sym(coluna) := ifelse(is.na(!!sym(coluna)),0,ifelse(!!sym(coluna)=="Strongly agree",2,ifelse(!!sym(coluna)=="Agree",1,ifelse(!!sym(coluna)=="Strongly disagree",-2,ifelse(!!sym(coluna)=="Disagree",-1,0))))))
  }
  
# 4.4. JUNTANDO COLUNAS
############## UM MONTE DE VETOR - Worked with
  ############## 
  languages_worked_with = c('Worked_with_Assembly',
                            'Worked_with_C',
                            'Worked_with_C.',
                            'Worked_with_C..',
                            'Worked_with_Clojure',
                            'Worked_with_CoffeeScript',
                            'Worked_with_Common.Lisp',
                            'Worked_with_Dart',
                            'Worked_with_Elixir',
                            'Worked_with_Erlang',
                            'Worked_with_F.',
                            'Worked_with_Go',
                            'Worked_with_Groovy',
                            'Worked_with_Hack',
                            'Worked_with_Haskell',
                            'Worked_with_Java',
                            'Worked_with_JavaScript',
                            'Worked_with_Julia',
                            'Worked_with_Lua',
                            'Worked_with_Matlab',
                            'Worked_with_Objective.C',
                            'Worked_with_PHP',
                            'Worked_with_Perl',
                            'Worked_with_Python',
                            'Worked_with_R',
                            'Worked_with_Ruby',
                            'Worked_with_Rust',
                            'Worked_with_SQL',
                            'Worked_with_Scala',
                            'Worked_with_Smalltalk',
                            'Worked_with_Swift',
                            'Worked_with_TypeScript',
                            'Worked_with_VB.NET',
                            'Worked_with_VBA',
                            'Worked_with_Visual.Basic.6')
  data_worked = c('Worked_with_Clojure',
                         'Worked_with_Common.Lisp',
                         'Worked_with_F.',
                         'Worked_with_Julia',
                         'Worked_with_R')
  general_worked = c('Worked_with_SQL',
                     'Worked_with_Go',
                     'Worked_with_Groovy',
                     'Worked_with_Java',
                     'Worked_with_Python',
                     'Worked_with_Ruby',
                     'Worked_with_Swift',
                     'Worked_with_VB.NET',
                     'Worked_with_VBA',
                     'Worked_with_Visual.Basic.6')
  other_worked = c('Worked_with_Assembly',
                   'Worked_with_Haskell',
                   'Worked_with_Matlab')
  software_worked = c('Worked_with_C.',
                      'Worked_with_C..',
                      'Worked_with_Smalltalk',
                      'Worked_with_C',
                      'Worked_with_Lua',
                      'Worked_with_Rust')
  web_dev_worked = c('Worked_with_Erlang',
                     'Worked_with_Objective.C',
                     'Worked_with_CoffeeScript',
                     'Worked_with_Dart',
                     'Worked_with_Elixir',
                     'Worked_with_Hack',
                     'Worked_with_JavaScript',
                     'Worked_with_PHP',
                     'Worked_with_Perl',
                     'Worked_with_Scala',
                     'Worked_with_TypeScript')
  
  
  languages_want_work = c('Want_work_Assembly',
                            'Want_work_C',
                            'Want_work_C.',
                            'Want_work_C..',
                            'Want_work_Clojure',
                            'Want_work_CoffeeScript',
                            'Want_work_Common.Lisp',
                            'Want_work_Dart',
                            'Want_work_Elixir',
                            'Want_work_Erlang',
                            'Want_work_F.',
                            'Want_work_Go',
                            'Want_work_Groovy',
                            'Want_work_Hack',
                            'Want_work_Haskell',
                            'Want_work_Java',
                            'Want_work_JavaScript',
                            'Want_work_Julia',
                            'Want_work_Lua',
                            'Want_work_Matlab',
                            'Want_work_Objective.C',
                            'Want_work_PHP',
                            'Want_work_Perl',
                            'Want_work_Python',
                            'Want_work_R',
                            'Want_work_Ruby',
                            'Want_work_Rust',
                            'Want_work_SQL',
                            'Want_work_Scala',
                            'Want_work_Smalltalk',
                            'Want_work_Swift',
                            'Want_work_TypeScript',
                            'Want_work_VB.NET',
                            'Want_work_VBA',
                            'Want_work_Visual.Basic.6')
  data_want= c('Want_work_Clojure',
                  'Want_work_Common.Lisp',
                  'Want_work_F.',
                  'Want_work_Julia',
                  'Want_work_R')
  general_want = c('Want_work_SQL',
                     'Want_work_Go',
                     'Want_work_Groovy',
                     'Want_work_Java',
                     'Want_work_Python',
                     'Want_work_Ruby',
                     'Want_work_Swift',
                     'Want_work_VB.NET',
                     'Want_work_VBA',
                     'Want_work_Visual.Basic.6')
  other_want = c('Want_work_Assembly',
                   'Want_work_Haskell',
                   'Want_work_Matlab')
  software_want = c('Want_work_C.',
                      'Want_work_C..',
                      'Want_work_Smalltalk',
                      'Want_work_C',
                      'Want_work_Lua',
                      'Want_work_Rust')
  web_dev_want = c('Want_work_Erlang',
                     'Want_work_Objective.C',
                     'Want_work_CoffeeScript',
                     'Want_work_Dart',
                     'Want_work_Elixir',
                     'Want_work_Hack',
                     'Want_work_JavaScript',
                     'Want_work_PHP',
                     'Want_work_Perl',
                     'Want_work_Scala',
                     'Want_work_TypeScript')

  
  ##############
  
  #  Quantidade de Linguagens - Worked
  clean_df$qtda_linguages_worked = (rowSums(clean_df[,languages_worked_with]))
  #  Quantidade de Linguagens - Want
  clean_df$qtda_linguages_want = (rowSums(clean_df[,languages_want_work]))
  ##
  # Data Science Lingua - worked
  clean_df$data_science_worked = ifelse((rowSums(clean_df[,data_worked]))>=1,1,0)
  # Data Science Lingua - want
  clean_df$data_science_want = ifelse((rowSums(clean_df[,data_want]))>=1,1,0)
  ##
  # Geral Lingua - worked
  clean_df$gneral_worked = ifelse((rowSums(clean_df[,general_worked]))>=1,1,0)
  # Geral Lingua - want
  clean_df$gneral_want = ifelse((rowSums(clean_df[,general_want]))>=1,1,0)
  ##
  # Outra Lingua - worked
  clean_df$outra_worked = ifelse((rowSums(clean_df[,other_worked]))>=1,1,0)
  # Outra Lingua - want
  clean_df$outra_want = ifelse((rowSums(clean_df[,other_want]))>=1,1,0)
  ##
  # Software Dev Lingua - worked
  clean_df$soft_worked = ifelse((rowSums(clean_df[,software_worked]))>=1,1,0)
  # Software Dev Lingua - want
  clean_df$soft_want = ifelse((rowSums(clean_df[,software_want]))>=1,1,0)
  ##
  # Web Dev Lingua - worked
  clean_df$webdev_worked = ifelse((rowSums(clean_df[,web_dev_worked]))>=1,1,0)
  # Web Dev Lingua - want
  clean_df$webdev_want = ifelse((rowSums(clean_df[,web_dev_want]))>=1,1,0)
  ##
  vetor_categorias_worked = c('data_science_worked','gneral_worked','outra_worked','soft_worked','webdev_worked')
  vetor_categorias_want = c('data_science_want','gneral_want','outra_want','soft_want','webdev_want')
  ##
  #  Quantidade de categorias - Worked
  clean_df$qtda_categorias_worked = (rowSums(clean_df[,vetor_categorias_worked]))
  #  Quantidade de vategorias - Want
  clean_df$qtda_categorias_want = (rowSums(clean_df[,vetor_categorias_want]))
  
  dev_datascience = c('DevType_Data.scientist',
                      'DevType_Developer.with.a.statistics.or.mathematics.background',
                      'DevType_Machine.learning.specialist')
  dev_design = c('DevType_Graphic.designer',
                 'DevType_Graphics.programming')
  dev_development = c('DevType_Desktop.applications.developer',
                      'DevType_DevOps.specialist',
                      'DevType_Embedded.applications.devices.developer',
                      'DevType_Mobile.developer',
                      'DevType_Web.developer')
  dev_infra = c('DevType_Database.administrator',
                'DevType_Quality.assurance.engineer',
                'DevType_Systems.administrator')
  dev_other = c('DevType_Other')
  
  clean_df$dev_data_scientist = ifelse((rowSums(clean_df[,dev_datascience]))>=1,1,0)
  clean_df$dev_desinger = ifelse((rowSums(clean_df[,dev_design]))>=1,1,0)
  clean_df$dev_desenvolvedor = ifelse((rowSums(clean_df[,dev_development]))>=1,1,0)
  clean_df$dev_infraestrutura = ifelse((rowSums(clean_df[,dev_infra]))>=1,1,0)
  clean_df$dev_outro = ifelse((rowSums(clean_df[,dev_other]))>=1,1,0)
  
  vetor_importancia_beneficio = c('IsImportantBenefit_Annual.bonus',
                                  'IsImportantBenefit_Charitable.match',
                                  'IsImportantBenefit_Child.elder.care',
                                  'IsImportantBenefit_Education.sponsorship',
                                  'IsImportantBenefit_Equipment',
                                  'IsImportantBenefit_Expected.work.hours',
                                  'IsImportantBenefit_Health.benefits',
                                  'IsImportantBenefit_Long.term.leave',
                                  'IsImportantBenefit_Meals',
                                  'IsImportantBenefit_None.of.these',
                                  'IsImportantBenefit_Other',
                                  'IsImportantBenefit_Private.office',
                                  'IsImportantBenefit_Professional.development.sponsorship',
                                  'IsImportantBenefit_Remote.options',
                                  'IsImportantBenefit_Retirement',
                                  'IsImportantBenefit_Stock.options',
                                  'IsImportantBenefit_Vacation.days.off')
  clean_df$exigencia_beneficios = (rowSums(clean_df[,vetor_importancia_beneficio]))/length(vetor_importancia_beneficio)
  
  vetor_outros_generos = c('Gender_Gender.non.conforming',
                           'Gender_Other',
                           'Gender_Transgender')
  clean_df$genero_outro = ifelse((rowSums(clean_df[,vetor_outros_generos]))>=1,1,0)
  
  vetor_raca_asiatico = c('Race_East.Asian', 'Race_South.Asian')
  vetor_raca_outro = c('Race_I.donâ..t.know', 'Race_I.prefer.not.to.say')
  clean_df$raca_asiatico = ifelse((rowSums(clean_df[,vetor_raca_asiatico]))>=1,1,0)
  clean_df$raca_outro = ifelse((rowSums(clean_df[,vetor_raca_outro]))>=1,1,0)
  
  vetor_exigencia_habilidades = c('MetricAssess__Benchmarked.product.performance',
                                  'MetricAssess__Bugs.found',
                                  'MetricAssess__Commit.frequency',
                                  'MetricAssess__Customer.satisfaction',
                                  'MetricAssess__Hours.worked',
                                  'MetricAssess__Lines.of.code',
                                  'MetricAssess__Manager.s.rating',
                                  'MetricAssess__On.time.in.budget',
                                  'MetricAssess__Other',
                                  'MetricAssess__Peers..rating',
                                  'MetricAssess__Release.frequency',
                                  'MetricAssess__Revenue.performance',
                                  'MetricAssess__Self.rating')
  clean_df$exigencia_habilidade = (rowSums(clean_df[,vetor_exigencia_habilidades]))/length(vetor_exigencia_habilidades)
  
  vetor_quantidade_educacao = c('Education_Bootcamp',
                                'Education_Coding.competition',
                                'Education_Hackathon',
                                'Education_Industry.certification',
                                'Education_On.the.job.training',
                                'Education_Online.course',
                                'Education_Open.source.contributions',
                                'Education_Part.time.evening.course',
                                'Education_Self.taught')
  clean_df$diferentes_educacao = (rowSums(clean_df[,vetor_quantidade_educacao]))/length(vetor_quantidade_educacao)
  
# 4.5. NOVAS VARIAVEIS
  # UsoStackOverflow
  clean_df$UsoStackOverflow = (clean_df$StackOverflowFoundAnswer + clean_df$StackOverflowCopiedCode + clean_df$StackOverflowJobListing + clean_df$StackOverflowCompanyPage + clean_df$StackOverflowJobSearch + clean_df$StackOverflowNewQuestion + clean_df$StackOverflowAnswer + clean_df$StackOverflowMetaChat)/8
  # Grupo likert 1 - personalidade
  clean_df$personalidade = (rowSums(clean_df[,vetor_grupo_1]))/length(vetor_grupo_1)
  # Grupo likert 2 - exigencia para emprego
  clean_df$exigencia_emprego = (rowSums(clean_df[,vetor_grupo_2]))/length(vetor_grupo_2)
  # Grupo likert 3 - Nivel de exigencia
  clean_df$nivel_exigencia = (rowSums(clean_df[,vetor_grupo_3]))/length(vetor_grupo_3)
  # Grupo likert 4 - Personalidade no trabalho
  clean_df$caracteristicas_trabalho = (rowSums(clean_df[,vetor_grupo_4]))/length(vetor_grupo_4)
  # Grupo likert 5 - Satisfacao com equipmaentos
  clean_df$satisfacao_equipamento = (rowSums(clean_df[,vetor_grupo_5]))/length(vetor_grupo_5)
  # Grupo likert 6 - Influencia
  clean_df$influencia = (rowSums(clean_df[,vetor_grupo_6]))/length(vetor_grupo_6)
  # Grupo likert 7 - Satisfacao Stackoverflow
  clean_df$satisfacaoStackOverflow = (rowSums(clean_df[,vetor_grupo_7]))/length(vetor_grupo_7)

# 4.6. DELETANDO VARIAVEIS QUE NÃO PRECISAMOS
  outras_colunas_deletar = c('EducationImportant',
                      'InfluenceInternet',
                      'StackOverflowSatisfaction',
                      'SurveyLong',
                      'QuestionsInteresting',
                      'QuestionsConfusing',
                      'InterestedAnswers',
                      'ProgramHobby',
                      'Country',
                      'EmploymentStatus',
                      'CompanyType',
                      'WebDeveloperType',
                      'CareerSatisfaction',
                      'JobSatisfaction',
                      'JobSeekingStatus',
                      'HoursPerWeek',
                      'ResumePrompted',
                      'LearnedHiring',
                      'Currency',
                      'Overpaid',
                      'TabsSpaces',
                      'CousinEducation',
                      'WorkStart',
                      'HaveWorkedFramework',
                      'WantWorkFramework',
                      'AuditoryEnvironment',
                      'VersionControl',
                      'HighestEducationParents',
                      'Salary',
                      'Uses_Android.Studio',
                      'Uses_Atom',
                      'Uses_Coda',
                      'Uses_Eclipse',
                      'Uses_Emacs',
                      'Uses_IPython...Jupyter',
                      'Uses_IntelliJ',
                      'Uses_Komodo',
                      'Uses_Light.Table',
                      'Uses_NetBeans',
                      'Uses_Notepad..',
                      'Uses_PHPStorm',
                      'Uses_PyCharm',
                      'Uses_RStudio',
                      'Uses_RubyMine',
                      'Uses_Sublime.Text',
                      'Uses_TextMate',
                      'Uses_Vim',
                      'Uses_Visual.Studio',
                      'Uses_Visual.Studio.Code',
                      'Uses_Xcode',
                      'Uses_Zend',
                      'Uses_StackOverflow_in_Android.app',
                      'Uses_StackOverflow_in_Android.browser',
                      'Uses_StackOverflow_in_Desktop',
                      'Uses_StackOverflow_in_Other.phone.browser',
                      'Uses_StackOverflow_in_iOS.app',
                      'Uses_StackOverflow_in_iOS.browser',
                      'Net migration',
                      'accessibility_to_cities',
                      'Population',
                      'temp_annual_range',
                      'Birthrate',
                      'Service',
                      'LOCATION',
                      'Infant mortality (per 1000 births)',
                      'is_independent',
                      'temp_mean_annual',
                      'ISO4217-currency_name',
                      'GDP ($ per capita)',
                      'Deathrate',
                      'Region',
                      'Pop. Density (per sq. mi.)',
                      'PPP',
                      'AverageAnualIncome17',
                      'Salary_Mean_Ratio',
                      'Phones (per 1000)',
                      'Literacy (%)',
                      'Human Development Index',
                      'Industry',
                      'JobProfile',
                      'SelfTaughtTypes',
                      'Methodology',
                      'StackOverflowFoundAnswer',
                      'StackOverflowCopiedCode',
                      'StackOverflowJobListing',
                      'StackOverflowCompanyPage',
                      'StackOverflowJobSearch',
                      'StackOverflowNewQuestion',
                      'StackOverflowAnswer',
                      'StackOverflowMetaChat')
  df_limpa = clean_df
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_grupo_1)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_grupo_2)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_grupo_3)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_grupo_4)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_grupo_5)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_grupo_6)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_grupo_7)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% languages_worked_with)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% languages_want_work)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_exigencia_habilidades)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_importancia_beneficio)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_outros_generos)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_quantidade_educacao)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_raca_asiatico)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% vetor_raca_outro)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% outras_colunas_deletar)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% dev_other)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% dev_datascience)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% dev_design)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% dev_development)]
  df_limpa = df_limpa[,!(names(df_limpa) %in% dev_infra)]
  

# 4.7. REVISANDO VALORES FALTANTES
  missing = md.pairs(df_limpa)$mm
  md.pattern(df_limpa)
  # ==> 9 valores faltantes de YearCodedJob e 1 de HomeRemote
  
  # Imputando valor faltante de HomeRemote e YearsCodedJob(valor mais frequente)
  df_limpa[is.na(df_limpa$HomeRemote),"HomeRemote"] = "As Vezes"
  df_limpa[is.na(df_limpa$YearsCodedJob),"YearsCodedJob"] = "Entre 3 e 10 anos"
  
# 4.8. REALIZANDO FATORES DAS VARIÁVEIS CATEGORICAS
  df_limpa$FormalEducation = factor(df_limpa$FormalEducation)
  df_limpa$HomeRemote = factor(df_limpa$HomeRemote)
  df_limpa$CompanySize = factor(df_limpa$CompanySize)
  df_limpa$YearsProgram = factor(df_limpa$YearsProgram)
  df_limpa$YearsCodedJob = factor(df_limpa$YearsCodedJob)
  df_limpa$Continent = factor(df_limpa$Continent)
  df_limpa$`Development Status` = factor(df_limpa$`Development Status`)
  
  vetor_novos_fatores = c("University",
                          "CheckInCode",
                          "Gender_Female",
                          "Gender_Male",
                          "Race_Black.or.of.African.descent",
                          "Race_Hispanic.or.Latino.Latina",
                          "Race_Middle.Eastern",
                          "Race_Native.American..Pacific.Islander..or.Indigenous.Australian",
                          "Race_White.or.of.European.descent",
                          "ComputerMajor",
                          "MaisQuatroAnosUltimoTrabalho",
                          "StackOverflowCV",
                          "gneral_worked",
                          "gneral_want",
                          "outra_worked",
                          "outra_want",
                          "soft_worked",
                          "soft_want",
                          "webdev_worked",
                          "webdev_want",
                          "dev_data_scientist",
                          "dev_desinger",
                          "dev_desenvolvedor",
                          "dev_infraestrutura",
                          "genero_outro",
                          "raca_asiatico",
                          "raca_outro")
  for (coluna in vetor_novos_fatores) {
    df_limpa[,coluna] = factor(df_limpa[,coluna])
  }
  
  
  
##########################################################################################
  
#________________________________ 5. VISUALIZANDO DADOS NOVAMENTE _______________________#
  
 
# Plotando matrix de scatter
res = cor(df_limpa %>%
        select_if(is.numeric))
corrplot(res, type = "upper", order = "hclust", 
          tl.col = "black", tl.srt = 45)
# ==> Existem algumas categorias que possuem correlação muito baixa com o salário.

# Plotando gráficos para vermos a relação entre as variaveis e newSalary
vetor_comparacao_eixo_x_novo = c("FormalEducation",
                                 "HomeRemote",
                                 "CompanySize",
                                 "YearsProgram",
                                 "YearsCodedJob",
                                 "Continent",
                                 "Development Status",
                                 "University",
                                 "CheckInCode",
                                 "Gender_Female",
                                 "Gender_Male",
                                 "Race_Black.or.of.African.descent",
                                 "Race_Hispanic.or.Latino.Latina",
                                 "Race_Middle.Eastern",
                                 "Race_Native.American..Pacific.Islander..or.Indigenous.Australian",
                                 "Race_White.or.of.European.descent",
                                 "ComputerMajor",
                                 "MaisQuatroAnosUltimoTrabalho",
                                 "StackOverflowCV",
                                 "data_science_worked",
                                 "data_science_want",
                                 "gneral_worked",
                                 "gneral_want",
                                 "outra_worked",
                                 "outra_want",
                                 "soft_worked",
                                 "soft_want",
                                 "webdev_worked",
                                 "webdev_want",
                                 "dev_data_scientist",
                                 "dev_desinger",
                                 "dev_desenvolvedor",
                                 "dev_infraestrutura",
                                 "genero_outro",
                                 "raca_asiatico",
                                 "raca_outro")
vetor_comparacao_scatter_novo =c("qtda_linguages_worked",
                                 "qtda_linguages_want",
                                 "qtda_categorias_worked",
                                 "qtda_categorias_want",
                                 "exigencia_beneficios",
                                 "exigencia_habilidade",
                                 "diferentes_educacao",
                                 "UsoStackOverflow",
                                 "personalidade",
                                 "exigencia_emprego",
                                 "nivel_exigencia",
                                 "caracteristicas_trabalho",
                                 "satisfacao_equipamento",
                                 "influencia",
                                 "satisfacaoStackOverflow")

comp = "Development Status"
df_limpa %>%
  ggplot(aes(x=!!sym(comp),y=newSalary, fill=!!sym(comp))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(title=str_glue('Distribuição dos Valores Salario - {comp}'),y='Salário') +
  theme_classic()
for (comp in vetor_comparacao_eixo_x_novo) {
  show(df_limpa %>%
         ggplot(aes(x=!!sym(comp),y=newSalary, fill=!!sym(comp))) +
         geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE)+
         labs(title=str_glue('Distribuição dos Valores Salario - {comp}'),y='Salário') +
         theme_classic())
  ggsave(str_glue("Plots/AposAjuste_Relacao_{comp}.png"), width = 8, height = 3, units = "in", dpi = 300)
}
for (comp in vetor_comparacao_scatter_novo) {
  show(df_limpa %>%
         ggplot(aes(x=!!sym(comp),y=newSalary)) +
         geom_point(alpha=0.2)+
         labs(title=str_glue('Distribuição dos Valores Salario - {comp}'),y='Salário') +
         theme_classic())
  ggsave(str_glue("Plots/AposAjuste_RelacaoScatter_Salario_{comp}.png"), width = 8, height = 3, units = "in", dpi = 300)
}

# Removendo Colunas que Não Acrescentam ao Modelo
remocao_ultimos = c('X',
                    "Gender_Female",
                    "Gender_Male",
                    "gneral_want",
                    "outra_want",
                    "soft_want",
                    "webdev_want",
                    "genero_outro",
                    "data_science_want")

df_limpa = df_limpa[,!(names(df_limpa) %in% remocao_ultimos)]

# Normalizar escalas
final_df = df_limpa %>% mutate_at(colnames(df_limpa %>% select_if(is.numeric) %>%select(!newSalary)), ~(rescale(.) %>% as.vector))

# Tirando raiz da variavel Salário
final_df$newSalary = sqrt(final_df$newSalary)

##########################################################################################

#________________________________ 6. TREINANDO MODELOS __________________________________#

# 6.1. DIVIDINDO BASE DE TESTE E TREINO
set.seed(42, kind="Mersenne-Twister", normal.kind="Inversion") # replicabilidade
N = nrow(df_limpa)
ind = sample(N, round(.7*N))
treinamento = final_df[ind, ]
teste = final_df[-ind, ]

# 6.2. MODELOS
  # 6.2.1. Regressão Linear Simples
    mod_linear = lm(newSalary~., data = treinamento)
    yhat_linear = predict(mod_linear, newdata = teste)
    
    data.frame(Observado = teste$newSalary**2, Predito = yhat_linear**2) %>% 
      ggplot(aes(y=Observado, x=Predito))+
      geom_point()+
      geom_abline(slope=1, intercept = 0, color="red") + 
      geom_smooth(method='lm')+
      labs(title = "Salário Observado vs. Predito pelo modelo linear") +
      theme_classic()
    ggsave(str_glue("Plots/ModeloLinear_Ajuste.png"), width = 8, height = 3, units = "in", dpi = 300)
    summary(mod_linear)
    rmse_linear = RMSE(teste$newSalary**2,yhat_linear**2)
  # 6.2.2. Regressão Linear com Linearização
    trainControl <- trainControl(method = "cv",
                                 number = 10)
    #Primeira regularizacao
    grid=expand.grid(
      .alpha = seq(0,1,length.out=5),
      .lambda = exp(seq(-7,1,length.out=10)))
    modelo_regularizado = train(newSalary~.,
                          data = treinamento,
                          method = "glmnet",  
                          trControl = trainControl,
                          metric = "RMSE",
                          tuneGrid = grid,
                          family="gaussian")
    print(modelo_regularizado)
    plot(modelo_regularizado)
    # refinando regularizacao
    grid=expand.grid(
      .alpha = seq(0.5,1,by=0.1),
      .lambda = seq(1,2,by=0.1))
    modelo_regularizado = train(newSalary~.,
                                data = treinamento,
                                method = "glmnet",  
                                trControl = trainControl,
                                metric = "RMSE",
                                tuneGrid = grid,
                                family="gaussian")
    print(modelo_regularizado)
    plot(modelo_regularizado)
    # refinando regularizacao
    grid=expand.grid(
      .alpha = seq(0.7,0.8,by=0.01),
      .lambda = seq(1,1.4,by=0.01))
    modelo_regularizado = train(newSalary~.,
                                data = treinamento,
                                method = "glmnet",  
                                trControl = trainControl,
                                metric = "RMSE",
                                tuneGrid = grid,
                                family="gaussian")
    print(modelo_regularizado)
    plot(modelo_regularizado)
    # refinando regularizacao
    grid=expand.grid(
      .alpha = seq(0.71,0.75,by=0.005),
      .lambda = seq(1,1.3,by=0.001))
    modelo_regularizado = train(newSalary~.,
                                data = treinamento,
                                method = "glmnet",  
                                trControl = trainControl,
                                metric = "RMSE",
                                tuneGrid = grid,
                                family="gaussian")
    print(modelo_regularizado)
    plot(modelo_regularizado)
    # refinando regularizacao
    grid=expand.grid(
      .alpha = seq(0.71,0.75,by=0.005),
      .lambda = seq(1.15,1.25,by=0.001))
    modelo_regularizado = train(newSalary~.,
                                data = treinamento,
                                method = "glmnet",  
                                trControl = trainControl,
                                metric = "RMSE",
                                tuneGrid = grid,
                                family="gaussian")
    print(modelo_regularizado)
    plot(modelo_regularizado)
    
    yhat_regularizado = predict(modelo_regularizado, newdata = teste)
    data.frame(Observado = teste$newSalary**2, Predito = yhat_regularizado**2) %>% 
      ggplot(aes(y=Observado, x=Predito))+
      geom_point()+
      geom_abline(slope=1, intercept = 0, color="red") + 
      geom_smooth(method='lm')+
      labs(title = "Salário Observado vs. Predito pelo modelo linear Regularizado") +
      theme_classic()
    ggsave(str_glue("Plots/ModeloLinearRegularizado_Ajuste.png"), width = 8, height = 3, units = "in", dpi = 300)
    summary(modelo_regularizado)
    rmse_regularizado = RMSE(teste$newSalary**2,yhat_regularizado**2)

  # 6.2.3. Arvore de Decisão
    modelo_arvore = rpart(newSalary~., 
                    data = treinamento)
    yhat_arvore = predict(modelo_arvore, newdata = teste)
    data.frame(Observado = teste$newSalary**2, Predito = yhat_arvore**2) %>% 
      ggplot(aes(y=Observado, x=Predito))+
      geom_point()+
      geom_abline(slope=1, intercept = 0, color="red") + 
      geom_smooth(method='lm')+
      labs(title = "Salário Observado vs. Predito pelo modelo Arvore") +
      theme_classic()
    ggsave(str_glue("Plots/ModeloArvore_Ajuste.png"), width = 8, height = 3, units = "in", dpi = 300)
    summary(modelo_arvore)
    rmse_arvore = RMSE(teste$newSalary**2,yhat_arvore**2)
    plot(modelo_arvore)
    text(modelo_arvore)
    
  # 6.2.4. Arvore Aleatória
    set.seed(42, kind="Mersenne-Twister", normal.kind="Inversion")
    grids <- expand.grid(mtry = c(1,3,5,10, 15), min.node.size = c(1,5,11,31), splitrule = "variance") # hiperparametros 
    modelo_arvore_aleatoria <- train(newSalary ~ . ,
                              data=treinamento,
                              method="ranger",
                              trControl=trainControl(method="oob"),
                              metric="RMSE",
                              tuneGrid=grids,
                              importance = 'impurity') 
    yhat_arvore_aleatoria = predict(modelo_arvore_aleatoria, newdata = teste)
    data.frame(Observado = teste$newSalary**2, Predito = yhat_arvore_aleatoria**2) %>% 
      ggplot(aes(y=Observado, x=Predito))+
      geom_point()+
      geom_abline(slope=1, intercept = 0, color="red") + 
      geom_smooth(method='lm')+
      labs(title = "Salário Observado vs. Predito pelo modelo Arvore Aleatória") +
      theme_classic()
    ggsave(str_glue("Plots/ModeloArvoreAleatória_Ajuste.png"), width = 8, height = 3, units = "in", dpi = 300)
    summary(modelo_arvore_aleatoria)
    rmse_arvore_aleatoria = RMSE(teste$newSalary**2,yhat_arvore_aleatoria**2)
    plot(varImp(modelo_arvore_aleatoria))
    plot(modelo_arvore_aleatoria)
    

    