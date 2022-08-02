#Big Data and Machine Learning for Applied Economics
#MEcA - Uniandes
#Final Project (FP)
#
#Equipo 12

#Jorge E. García
#Ingrid Lorena Molano
#Camilo Villa Moreno

#Agosto 2, 2022

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. PRELIMINARES: PREPARACIÓN ESPACIO DE TRABAJO Y LIBRERÍAS----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##Limpiar el entorno ----
rm(list=ls())


##Carga de librerías ----
install.packages("pacman")


start <- Sys.time()

library(pacman)

p_load(rio,
       igraph,
       doParallel,
       gtsummary,
       GGally,
       stargazer,
       fabricatr,
       tableone,
       arsenal,
       janitor,
       tidyverse,
       #tibble,
       gamlr,
       skimr, 
       caret,
       rvest,
       stargazer,
       smotefamily,
       MASS,
       ROCR,
       pROC,
       rpart,
       rpart.plot,
       glmnet,
       xgboost,
       gtools)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. CARGA DE LAS BASES DE DATOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



setwd("~/GitHub/MECA_BD_Final_project/")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1. Bases de datos de proyectos europeos ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1.1.1. Horizon 2020 (H2020) ----
#++++++++++++++++++++++++++++++++++++


#### Importar todos los archivos de Excel del directorio:

filenames <- list.files("./stores/EU_research_projects/H2020_projects", pattern="*.xlsx", full.names=TRUE)
filenames


#H2020_euroSciVoc <-     import(filenames[1])
#H2020_legalBasis <-     import(filenames[2])
H2020_organization <-   import(filenames[3])
H2020_programme <-      import(filenames[4])
H2020_project <-        import(filenames[5])
H2020_deliverables_1 <- import(filenames[6])
H2020_deliverables_2 <- import(filenames[7])
H2020_Irps <-           import(filenames[8])
H2020_publications_1 <- import(filenames[9])
H2020_publications_2 <- import(filenames[10])
H2020_publications_3 <- import(filenames[11])
H2020_publications_4 <- import(filenames[12])
H2020_publications_5 <- import(filenames[13])
#H2020_reportSummaries<- import(filenames[14])
#H2020_topics <-         import(filenames[15])
#H2020_webItem <-        import(filenames[16])
#H2020_webLink <-        import(filenames[17])



#### Publicaciones:

H2020_publications_list <- lapply(ls(pattern="^H2020_publications_"), function(x) get(x))
H2020_publications <- bind_rows(H2020_publications_list)
nrow(H2020_publications)

rm(list=ls(pattern="^H2020_publications_"))



#### ProjectDeliverables:

H2020_deliverables_list <- lapply(ls(pattern="^H2020_deliverables_"), function(x) get(x))
H2020_deliverables <- bind_rows(H2020_deliverables_list)
nrow(H2020_deliverables)

rm(list=ls(pattern="^H2020_deliverables_"))

rm(filenames)

#Guardar las bases de datos en archivos .rds
# 
# setwd("~/GitHub/MECA_BD_Final_project/stores/EU_research_projects/Final_bases/")
# 
# H2020_vars_list <- lapply(ls(pattern="^H2020_"), function(x) get(x))
# 
# 
# ?lapply
# 
# lapply(H2020_vars_list, function(x) get(x))
# 
# 
# saveRDS(H2020_deliverables,"H2020_deliverables.rds")
# saveRDS(H2020_publications,"H2020_publications.rds")



### 1.1.2. Framework Programme 7 (FP7)  ----
#++++++++++++++++++++++++++++++++++++++++++++


setwd("~/GitHub/MECA_BD_Final_project/")


#### Importar todos los archivos de Excel del directorio:

filenames <- list.files("./stores/EU_research_projects/FP7_projects", pattern="*.xlsx", full.names=TRUE)
filenames


#FP7_euroSciVoc <-     import(filenames[1])
#FP7_publications <-   import(filenames[2])
#FP7_legalBasis <-     import(filenames[3])
FP7_organization <-   import(filenames[4])
FP7_project <-        import(filenames[5])
FP7_Irps <-           import(filenames[6])
#FP7_reportSummaries<- import(filenames[7])
#FP7_topics <-         import(filenames[8])
#FP7_webItem <-        import(filenames[9])
#FP7_webLink <-        import(filenames[10])


sapply(FP7_publications, class)



### 1.1.3. CORDIS reference data  ----
#++++++++++++++++++++++++++++++++++++++

#### Importar todos los archivos de Excel del directorio:

filenames <- list.files("./stores/EU_research_projects/reference_data", pattern="*.xls*", full.names=TRUE)
filenames


cordis_countries <-       import(filenames[1])
FP7_programmes <-         import(filenames[2])
#H2020_topic_keywords <-   import(filenames[3])
cordis_org_activity <-    import(filenames[4])
cordis_funding_scheme <-  import(filenames[5])
cordis_funding_scheme_names <-  import(filenames[6])


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2. Bases de datos de OECD REGPAT (registros de patentes)  ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Cargar las bases de datos de patentes:

#patentes_reg <- import("./stores/OECD_REGPAT/202202_PCT_App_reg.txt")
#export(patentes_reg,"./stores/OECD_REGPAT/202202_PCT_App_reg.rds")

#patentes_reg <- import("./stores/OECD_REGPAT/202202_PCT_App_reg.rds")
#head(patentes_reg)

#Cargar la base de datos armonizada de nombres de postulantes (HAN)
#HAN: Harmonised Applicant Names

# nombres_han <- import("./stores/OECD_REGPAT/OECD_HAN/202202_HAN_NAMES.txt")
# patentes_han <- import("./stores/OECD_REGPAT/OECD_HAN/202202_HAN_PATENTS.txt")
# export(nombres_han,"./stores/OECD_REGPAT/OECD_HAN/202202_HAN_Names.rds")
# export(patentes_han,"./stores/OECD_REGPAT/OECD_HAN/202202_HAN_Patents.rds")

nombres_han <- import("./stores/OECD_REGPAT/OECD_HAN/202202_HAN_Names.rds")
patentes_han <- import("./stores/OECD_REGPAT/OECD_HAN/202202_HAN_Patents.rds")


#colnames(patentes_reg)
colnames(nombres_han)
colnames(patentes_han)

#nrow(patentes_reg)
nrow(nombres_han)
nrow(patentes_han)

# patents_sample <- patentes_reg[1:10000,]


#Queremos agregar el nombre armonizado a la base de datos de patentes:
patentes_han <- left_join(patentes_han,nombres_han,by="HAN_ID")

table(patentes_han$Publn_auth)
#La base más completa es la HAN; se puede hacer un conteo simple de patentes


colnames(patentes_han)

num_patents <- patentes_han %>% 
  group_by(Clean_name,Person_ctry_code) %>% 
  summarize(num_patent= n())

table(duplicated(num_patents))

colnames(num_patents) <- c("name","country","num_patent")
num_patents$name <- toupper(num_patents$name)

#export(num_patents,"./stores/OECD_REGPAT/Cuenta_patentes.rds")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.3. Base de datos CWTS Leiden Ranking 2022 (excelencia científica)  ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

setwd("~/GitHub/MECA_BD_Final_project/")

#La primera vez lo cargo desde el Excel
#CWTS_ranking <- import("./stores/CWTS_Leiden_Ranking/CWTS Leiden Ranking 2022.xlsx")

#Lo guardo para hacer más óptimo el tamaño
#export(CWTS_ranking,"./stores/CWTS_Leiden_Ranking/CWTS_ranking_2022.rds")

#Cargo el archivo de ranking desde el .RDS
CWTS_ranking <- import("./stores/CWTS_Leiden_Ranking/CWTS_ranking_2022.rds")


sapply(CWTS_ranking, class)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. PREPARACIÓN BASES DE DATOS: INSTITUCIONES ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.0. Categorías de países ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


cordis_countries <- cordis_countries[cordis_countries$language=="en",]

EU_13 <- c(
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czechia",
  "Estonia",
  "Hungary",
  "Latvia",
  "Lithuania",
  "Malta",
  "Poland",
  "Romania",
  "Slovakia",
  "Slovenia")

EU_15 <- c(
  "Austria",
  "Belgium",
  "Denmark",
  "Finland",
  "France",
  "Germany", 
  "Greece",
  "Ireland",
  "Italy",
  "Luxembourg",
  "Netherlands",
  "Portugal",
  "Spain",
  "Sweden",
  "United Kingdom")


cordis_countries$country_cat <- case_when(
  cordis_countries$name %in% EU_13 ~ "EU13",
  cordis_countries$name %in% EU_15 ~ "EU15",
  TRUE ~ "NonEU"
)

table(cordis_countries$country_cat)

colnames(cordis_countries) <- c("country",
                                "isoCode",
                                "country_name",
                                "language",
                                "country_cat")

#Se hace un join con la tabla de organizaciones para añadirle esta información
H2020_organization <- left_join(H2020_organization,
                                cordis_countries[,c("country","country_name","country_cat")],
                                by="country")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1. Experiencia previa en FP7 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#skim(H2020_organization)
table(H2020_organization$role)
table(FP7_organization$role)


#Se calculan el número de participaciones y de coordinaciones en el FP7
org_particip_FP7 <- FP7_organization %>% 
  group_by(organisationID) %>% 
  summarize(num_particip_FP7= sum(role=="participant"),
            num_coord_FP7 = sum(role=="coordinator"))

#Se hace el join con la base de H2020
nrow(H2020_organization)
H2020_organization <- left_join(H2020_organization,org_particip_FP7,by="organisationID")

colSums(is.na(H2020_organization))

#A los que quedan con NA se les imputa 0 (no han participado ni coordinado)
H2020_organization$num_particip_FP7[is.na(H2020_organization$num_particip_FP7)] <- 0
H2020_organization$num_coord_FP7[is.na(H2020_organization$num_coord_FP7)] <- 0



# Puede ser útil: experiencia en la misma base de H2020 (partner / coordinator)

table(H2020_organization$role)

#Se calculan el número de participaciones y de coordinaciones en H2020
org_particip_H2020 <- H2020_organization %>% 
  group_by(organisationID) %>% 
  summarize(num_particip_H2020= sum(role %in% c("participant",
                                                "internationalPartner",
                                                "partner",
                                                "thirdParty")),
            num_coord_H2020 = sum(role=="coordinator"))


#Se hace el join con la base de H2020
nrow(H2020_organization)
H2020_organization <- left_join(H2020_organization,org_particip_H2020,by="organisationID")

colSums(is.na(H2020_organization))




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2. Ranking CWTS Leiden 2022 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 2.2.1. Preparación de información ----

#Listado único de organizaciones

organizations <- distinct(H2020_organization[,-(1:2)])
colnames(organizations)
nrow(organizations)

organizations[,c("contactForm",
"contentUpdateDate",
"SME",
"rcn",
"order",
"role",
"ecContribution",
"netEcContribution",
"totalCost",
"geolocation",
"active",
"endOfParticipation")] <- list(NULL)

organizations <- distinct(organizations)
nrow(organizations)

#Del archivo cargado del ranking se deja solo la última versión

ranking2022 <- CWTS_ranking[CWTS_ranking$Period=="2017–2020" & 
                              CWTS_ranking$Field =="All sciences" &
                              CWTS_ranking$Frac_counting==1,]

ranking2022$uni_country <- paste0(ranking2022$University,"_",ranking2022$Country)
ranking2022$uni_country <- toupper(ranking2022$uni_country)

nrow(ranking2022)

# ranking2022 <- distinct(ranking2022,University, .keep_all = TRUE)

#Ranking con base en el número total de publicaciones:
ranking2022 <- ranking2022[order(ranking2022$impact_P,decreasing = TRUE),]
ranking2022 <- ranking2022 %>% mutate(ranking_p= 1:n())

#Ranking con base en el número de publicaciones Top 1%
ranking2022 <- ranking2022[order(ranking2022$P_top1,decreasing = TRUE),]
ranking2022 <- ranking2022 %>% mutate(ranking_p_top1= 1:n())


### 2.2.2. Join por nombre directo con CWTS Ranking ----

# organizations <- left_join(organizations,
#                            cordis_countries[,c("country","country_name")],
#                            by="country")

organizations$uni_country <- toupper(paste0(organizations$name,"_",
                                            organizations$country_name))

nrow(organizations)
organizations <- left_join(organizations,
                           ranking2022[,c("uni_country","ranking_p","ranking_p_top1")],
                           by="uni_country")

universities <- organizations[organizations$activityType %in% c("HES"),]

colSums(is.na(universities))
nrow(universities)

print(paste0("El porcentaje de universidades encontradas es ",
             round(100 - (sum(is.na(universities$ranking_p_top1))/nrow(universities))*100,1),
             " %"))


nrow(H2020_organization)
H2020_organization <- left_join(H2020_organization,organizations[,c("organisationID","ranking_p_top1")]
                                ,by="organisationID")
nrow(H2020_organization)



#PROBLEMA COMPLEJO, INSTITUTIONS' NAMES DISAMBIGUATION.

# Solución para este caso específico: la excelencia científica se mide solo para
# las 50 top universidades, es decir, es una variable dummy para reflejar
# si está en el top 50 del CWTS (ver paper).
# En este caso, podemos crear un diccionario de nombres para las 50 universidades
# top.

# Posible trabajo futuro: incluir todas las universidades, adelantar trabajo
# de corrección de nombres.


#export(universities,"./stores/Nombres_universidades/Universidades.xlsx")
#export(ranking2022,"./stores/Nombres_universidades/Ranking 2022.xlsx")


### 2.2.3. Join con diccionario de nombres de universidades ----

dicc_unis <- import("./stores/Nombres_universidades/Diccionario_nombres_unis.xlsx")

colnames(dicc_unis)
colnames(dicc_unis)[2] <- "country_name"
head(dicc_unis)

dicc_unis <- left_join(dicc_unis,cordis_countries[,c("country","country_name")],
                       by="country_name")

dicc_unis$uni_country_rank <- toupper(paste0(dicc_unis$Name_ranking,"_",dicc_unis$country_name))
dicc_unis$uni_country_cordis <- toupper(paste0(dicc_unis$Name_horizon,"_",dicc_unis$country_name))

organizations$uni_country_cordis <- organizations$uni_country

nrow(organizations)

colnames(organizations)


#Se hace la equivalencia de nombres con el diccionario
organizations <- left_join(organizations,
                           dicc_unis[,c("uni_country_cordis","uni_country_rank")],
                           by="uni_country_cordis")


#Se hace la búsqueda del ranking con el nombre del diccionario:

organizations$uni_country2 <- organizations$uni_country 
organizations$uni_country <- organizations$uni_country_rank
ranking2022$ranking_p_top1_2 <- ranking2022$ranking_p_top1


nrow(organizations)
organizations <- left_join(organizations,
                           ranking2022[,c("uni_country","ranking_p_top1_2")],
                           by="uni_country")

organizations$uni_country <- organizations$uni_country2
organizations$uni_country2 <- NULL

organizations_ranking_dicc <- filter(organizations,!is.na(organizations$ranking_p_top1_2))

nrow(organizations_ranking_dicc)

organizations_ranking_dicc <- distinct(organizations_ranking_dicc,
                                       organisationID,.keep_all = TRUE)


#Se hace la unión con la base de H2020_orgs
nrow(H2020_organization)
H2020_organization <- left_join(H2020_organization,
                                organizations_ranking_dicc[,c("organisationID","ranking_p_top1_2")]
                                ,by="organisationID")
nrow(H2020_organization)



### 2.2.4. Unión de las diferentes búsquedas ----

H2020_organization$ranking_top1 <- case_when(
  !is.na(H2020_organization$ranking_p_top1) ~ H2020_organization$ranking_p_top1,
  !is.na(H2020_organization$ranking_p_top1_2) ~ H2020_organization$ranking_p_top1_2)

H2020_organization$ranking_p_top1 <- NULL
H2020_organization$ranking_p_top1_2 <- NULL


H2020_organization$ranking_top50 <- if_else(
  H2020_organization$ranking_top1 <= 50,TRUE,FALSE,missing= FALSE)

H2020_organization$ranking_EU_top50 <- if_else(
  H2020_organization$ranking_top1 < 250 & 
    H2020_organization$country_cat != "NonEU" ,TRUE,FALSE,missing= FALSE)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.3. Patentes OECD REGPAT ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 2.3.1. Join por nombre directo con OECD REGPAT ----


#Puede ser complejo sacar la info de la OCDE por los nombres

nrow(organizations)

#Hacer el join por institutción + país

organizations$name_pais <- paste0(organizations$name,"_",organizations$country)
num_patents$name_pais <- paste0(num_patents$name,"_",num_patents$country)

organizations <- left_join(organizations,
                           num_patents[,c("name_pais","num_patent")]
                           ,by="name_pais")
nrow(organizations)

sum(is.na(organizations$num_patent))

print(paste0("El porcentaje de organizaciones con patentes encontradas es ",
             round(100 - (sum(is.na(organizations$num_patent)) / 
                            nrow(organizations))*100,1), " %"))

nrow(H2020_organization)
H2020_organization <- left_join(H2020_organization,organizations[,c("organisationID","num_patent")]
                           ,by="organisationID")
nrow(H2020_organization)


### 2.3.2. Join con nombre corto de la misma base de datos ----

nomb_cortos <- import("./stores/Nombres_universidades/Diccionario_Patentes.xlsx")
colnames(nomb_cortos)[2] <- "name_pais"

nomb_cortos <- left_join(nomb_cortos,num_patents[,c("name_pais","num_patent")]
                         ,by="name_pais")

nrow(nomb_cortos)
colSums(is.na(nomb_cortos))
colnames(nomb_cortos)[3] <- "num_patent2"
nomb_cortos <- distinct(nomb_cortos)

nrow(H2020_organization)
H2020_organization <- left_join(H2020_organization,
                                nomb_cortos[,c("organisationID","num_patent2")],
                                by="organisationID")
nrow(H2020_organization)


### 2.3.3. Unión de las diferentes búsquedas ----

H2020_organization$num_patentes <- case_when(
  !is.na(H2020_organization$num_patent) ~ H2020_organization$num_patent,
  !is.na(H2020_organization$num_patent2) ~ H2020_organization$num_patent2)

H2020_organization$num_patent <- NULL
H2020_organization$num_patent2 <- NULL


### 2.3.4. Proxy: patentes en FP7 ----


colnames(FP7_Irps)
table(FP7_Irps$patentType)


#Se calcula el número de patentes por organizacion en FP7
patentes_proy_FP7 <- FP7_Irps %>% 
  group_by(organisationID) %>% 
  summarize(num_patentes_FP7 = n())

class(patentes_proy_FP7$organisationID) <- "character"


#Se une con la base de organizaciones H2020
nrow(H2020_organization)
H2020_organization <- left_join(H2020_organization,patentes_proy_FP7,by="organisationID")
nrow(H2020_organization)

rm(patentes_proy_FP7)

#A los que quedan con NA se les imputa 0 patentes
H2020_organization$num_patentes_FP7[is.na(H2020_organization$num_patentes_FP7)] <- 0

summary(H2020_organization$num_patentes_FP7)
hist(H2020_organization$num_patentes_FP7)


### 2.3.6. Proxy final: dummy "evidencia de patentes" ----

#En este caso, lo mejor es manejar una dummy que se llame "evidencia de patentes".
#Puede ser bastante imprecisa, teniendo en cuenta que no se hayan todos los nombres.


H2020_organization$evidencia_patente <- if_else(
  (H2020_organization$num_patentes_FP7 > 0) | (H2020_organization$num_patentes_FP7 >0),
  TRUE,FALSE,missing=FALSE)

table(H2020_organization$evidencia_patente)
table(H2020_organization$evidencia_patente,H2020_organization$role)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PREPARACIÓN BASES DE DATOS: CONSORCIOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1. Tamaño del consorcio ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tamano_consorc <- H2020_organization %>% 
  group_by(projectID) %>%
  summarize(consorc_size = n())

tamano_consorc$id <- tamano_consorc$projectID
class(tamano_consorc$id) <- "character"
tamano_consorc$projectID <- NULL

nrow(H2020_project)
H2020_project <- left_join(H2020_project,tamano_consorc,by="id")
nrow(H2020_project)

colSums(is.na(H2020_project))

#Se eliminan pocos registros que quedan con NA:
nrow(H2020_project)
H2020_project <- filter(H2020_project,!(is.na(H2020_project$consorc_size)))
nrow(H2020_project)

rm(tamano_consorc)



#Se repite para FP7 (se necesitará más adelante)

tamano_consorc <- FP7_organization %>% 
  group_by(projectID) %>%
  summarize(consorc_size = n())

tamano_consorc$id <- tamano_consorc$projectID
class(tamano_consorc$id) <- "character"
tamano_consorc$projectID <- NULL

nrow(FP7_project)
FP7_project <- left_join(FP7_project,tamano_consorc,by="id")
nrow(FP7_project)

colSums(is.na(FP7_project))

#Se eliminan pocos registros que quedan con NA:
nrow(FP7_project)
FP7_project <- filter(FP7_project,!(is.na(FP7_project$consorc_size)))
nrow(FP7_project)

rm(tamano_consorc)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2. Proporción por tipos de organización ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sum(is.na(H2020_organization$activityType))
table(H2020_organization$activityType)


tipos_socios_consorc <- H2020_organization %>% 
  group_by(projectID,activityType) %>%
  summarize(numPartners=n())

colnames(tipos_socios_consorc) <- c("id","activityType","numPartners")
nrow(tipos_socios_consorc)
colSums(is.na(tipos_socios_consorc))

#Se cambia la estructura de filas a columnas para agregarla a la base:

tipos_socios_consorc_w <- pivot_wider(tipos_socios_consorc, 
                                      names_from = activityType,
                                      names_prefix = "numPartn",
                                      names_sep = "_",
                                      values_from = numPartners)

colSums(is.na(tipos_socios_consorc_w))
tipos_socios_consorc_w[is.na(tipos_socios_consorc_w)] <- 0

colnames(tipos_socios_consorc_w)

nrow(H2020_project)
H2020_project <- left_join(H2020_project, tipos_socios_consorc_w,by="id")
nrow(H2020_project)

H2020_project[,c("numPartnOTH","numPartnNA","numPartnPUB")] <- list(NULL)

H2020_project$share_unis <- H2020_project$numPartnHES / H2020_project$consorc_size
H2020_project$share_resCen <- H2020_project$numPartnREC / H2020_project$consorc_size 
H2020_project$share_compan <- H2020_project$numPartnPRC / H2020_project$consorc_size


rm(tipos_socios_consorc)
rm(tipos_socios_consorc_w)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.3. Características de países representados ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Se hacen las cuentas agregadas según la categoría de país (EU13 / EU15 / Non-EU)

tipos_paises_consorc <- H2020_organization %>% 
  group_by(projectID,country_cat) %>%
  summarize(cuenta_country_cat=n())

colnames(tipos_paises_consorc) <- c("id","countryCat","cuenta_countryCat")
nrow(tipos_paises_consorc)
colSums(is.na(tipos_paises_consorc))

#Se cambia la estructura de filas a columnas para agregarla a la base:

tipos_paises_consorc <- pivot_wider(tipos_paises_consorc, 
                                      names_from = countryCat,
                                      names_prefix = "NumPartners_",
                                      values_from = cuenta_countryCat)

colSums(is.na(tipos_paises_consorc))
tipos_paises_consorc[is.na(tipos_paises_consorc)] <- 0

colnames(tipos_paises_consorc)

nrow(H2020_project)
H2020_project <- left_join(H2020_project, tipos_paises_consorc,by="id")
nrow(H2020_project)


H2020_project$share_EU13 <- H2020_project$NumPartners_EU13 / H2020_project$consorc_size
H2020_project$share_EU15 <- H2020_project$NumPartners_EU15 / H2020_project$consorc_size
H2020_project$share_nonEU <- H2020_project$NumPartners_NonEU / H2020_project$consorc_size

rm(tipos_paises_consorc)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.4. Visibilidad y centralidad del consorcio (degree / eigenvector) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 3.4.1. Tutorial----

# # install.packages("igraph", dependencies=TRUE)
# # library(igraph)
# 
# set.seed(8675309)
# 
# g <- sample_pa(50)
# plot(g)
# 
# g1 <- growing.random.game(50, m=2)
# g1 <- simplify(g1) # "Simplify" removes loops and multiple edges.
# plot(g1)
# 
# g2 <- erdos.renyi.game(50, 5/50)
# degree_distribution(g2)
# plot(g2)
# 
# #Remember, the degree of each node is just a count of how many edges attach to it. 
# #The distribution of the degree measure is a record of how frequently each To see 
# #the same information graphically, try displaying it as a histogram.
# hist(degree.distribution(g2))
# 
# #el <- read.table(file.choose(), sep=",")) # Read in the table
# #el <- as.matrix(el) 
# el <- as.matrix(read.table(file.choose(), sep=",")) # Load your edgelist as a two column matrix.
# g3 <- graph.edgelist(el, directed=TRUE)    # Convert it into an igraph object.
# plot(g3)
# V(g3)$name
# 
# fix(g3) # Use caution. It can cause grief if you change anything manually.
# print(g3, e=TRUE, v=TRUE)
# plot(g3)
# plot(g3, edge.width=E(g)$weight) # To plot using edge weights
# 
# 
# actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David", "Esmeralda"),
#                      age=c(48,33,45,34,21),
#                      gender=c("F","M","F","M","F"))
# 
# relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David", "David", "Esmeralda"),
#                         to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
#                         same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
#                         friendship=c(4,5,5,2,1,1), 
#                         advice=c(4,5,5,4,2,3))
# 
# g4 <- graph.data.frame(relations, directed=TRUE, vertices=actors)
# plot(g4)
# 
# deg <- degree(g4)            # Degree centrality
# 
# clo <- closeness(g4)         # Closeness centrality
# 
# bet <- betweenness(g4)       # Betweenness centrality
# 
# eig <- evcent(g4)$vector     # Eigenvector centrality
# 
# name <- get.vertex.attribute(g4, "name")
# 
# table <- cbind(name, deg, clo, bet, eig)
#                     
# table
# 
# hist(degree.distribution(g4)) 
# 
# object.name <- cbind(V(g4)$id, deg, clo, bet, eig)
# write.csv(object.name, file=paste("centrality.csv", sep=","))
# 
# # First, merge vectors into table, store as 'cent'
# cent <- cbind(deg, clo, bet, eig)
# 
# # Next, save them as a .csv file.
# write.csv(cent, file="Centrality.csv") # You may want to choose a working directory first.
# # If you need to find out where it went, use: getwd()  
# getwd()  
# 
# plot(g4) 
# tkplot(g4)
# rglplot(g4, layout=layout.fruchterman.reingold(g, dim=3))
# 
# 
# ### Leer la base
# #setwd("~/GitHub/MECA_BD_Final_project")
# 
# #prueba <-readRDS("./stores/H2020_orgs.rds") #174.005 Obs 29 var




### 3.4.2. Degree / Eigenvector----

prueba <- H2020_organization

library(dplyr)

# Identificamos cuántos cores tiene nuestra máquina
n_cores <- detectCores()
n_cores
cl <- makePSOCKcluster(n_cores-4) 
registerDoParallel(cl)

# Filtro de prueba para ver solo UK
# prueba <- prueba %>% filter(country=='UK')

# Se hace join con la misma tabla usando proyectos.
# Esto hace que se generen tantas filas como relaciones hayan entre
# organizaciones a través de proyectos.
prueba2 <- prueba %>% inner_join(prueba, by="projectID")

# Ahora filtramos todas las filas en donde la organización se
# repite pues no nos interesa tener relaciones cíclicas.
prueba2 <- prueba2 %>% filter(organisationID.x!=organisationID.y)

# Lo convertimos en el dataframe que espera la librería igraph
relationships <- prueba2 %>% dplyr::select(to=organisationID.x, from=organisationID.y)

# Ahora extraemos todas las organizaciones que tienen relaciones
# Para identificar los nodos de una mejor manera de llegar a
# graficarlos usando labels.
orgs <- prueba2 %>% distinct(organisationID.x, shortName.x, activityType.x, country.x)
orgs <- orgs %>% dplyr::select(organisationID=organisationID.x,
               shortName=shortName.x,
               activityType=activityType.x,
               country=country.x)

# Creamos el grafo y generamos las nuevas variables.
gpruebados <- graph.data.frame(relationships, directed=FALSE, vertices=orgs)

deg <- degree(gpruebados, mode="all")            # Degree centrality

clo <- closeness(gpruebados)         # Closeness centrality

bet <- betweenness(gpruebados)       # Betweenness centrality

eig <- evcent(gpruebados)$vector     # Eigenvector centrality

# Intentos de gráfica.
# Hay demasiada información.
# plot(gpruebados, vertex.label=NA, vertex.size=deg*2)
# plot(gpruebados, vertex.label=NA, vertex.size=5, layout=layout_with_fr,)
# plot(gpruebados, vertex.label=NA, vertex.size=5)

# Crear vector con nombres.
name <- get.vertex.attribute(gpruebados, "shortName")

# Creación de la tabla(matriz) con nuevas variables.
table <- cbind(name, deg, clo, bet, eig)

# Convertir matriz en una tabla y mover los nombres de las filas
# A una nueva columna.
rowtable <- table %>% as.data.frame() %>% tibble::rownames_to_column("organisationID")

org_centrality <- rowtable

colnames(org_centrality)
colnames(org_centrality) <- c("organisationID","name","netw_degree",
                              "netw_closeness" ,"netw_betweenness",
                              "netw_eigenvector")

org_centrality$name <- NULL

#Se une a la base de H2020_org:
nrow(H2020_organization)
H2020_organization <- left_join(H2020_organization,
                                org_centrality,
                                by="organisationID")
nrow(H2020_organization)

colSums(is.na(H2020_organization))

#Se analizan los NA:

H2020org_NA_netw <- filter(H2020_organization,(is.na(H2020_organization$netw_degree)))
H2020_consortia_NA_netw <- H2020_project[H2020_project$id %in% 
                                           H2020org_NA_netw$projectID, ]

#Son consorcios de 1, se les puede imputar 0

H2020_organization$netw_degree[is.na(H2020_organization$netw_degree)] <- 0
H2020_organization$netw_closeness[is.na(H2020_organization$netw_closeness)] <- 0
H2020_organization$netw_betweenness[is.na(H2020_organization$netw_betweenness)] <- 0
H2020_organization$netw_eigenvector[is.na(H2020_organization$netw_eigenvector)] <- 0

colSums(is.na(H2020_organization))


# Guardamos la nueva tabla.
#saveRDS(rowtable, './stores/tabla_variables_grafo.rds')

# ##Acquaintance
# 
# setwd("~/GitHub/MECA_BD_Final_project/")
# 
# 
# #### Importar todos los archivos de Excel del directorio:
# 
# filenames <- list.files("./stores/EU_research_projects/FP7_projects", pattern="*.xlsx", full.names=TRUE)
# filenames
# 
# 
# #FP7_euroSciVoc <-     import(filenames[1])
# #FP7_publications <-   import(filenames[2])
# #FP7_legalBasis <-     import(filenames[3])
# FP7_organization <-   import(filenames[4])
# FP7_project <-        import(filenames[5])
# #FP7_Irps <-           import(filenames[6])
# #FP7_reportSummaries<- import(filenames[7])
# #FP7_topics <-         import(filenames[8])
# #FP7_webItem <-        import(filenames[9])
# #FP7_webLink <-        import(filenames[10])
# 
# 
# acq <- FP7_organization %>% inner_join(FP7_organization, by="projectID")
# 
# # Ahora filtramos todas las filas en donde la organización se
# # repite pues no nos interesa tener relaciones cíclicas.
# acq <- acq %>% filter(organisationID.x!=organisationID.y)
# 
# # Lo convertimos en el dataframe que espera la librería igraph
# relationships_acq <- acq %>% dplyr::select(to=organisationID.x, from=organisationID.y)
# 
# # Ahora extraemos todas las organizaciones que tienen relaciones
# # Para identificar los nodos de una mejor manera de llegar a
# # graficarlos usando labels.
# orgs_acq <- acq %>% distinct(organisationID.x, shortName.x, activityType.x)
# orgs_acq <- orgs_acq %>% dplyr::select(organisationID=organisationID.x,
#                                shortName=shortName.x,
#                                activityType=activityType.x)
# 
# # Creamos el grafo y generamos las nuevas variables.
# g_acq <- graph.data.frame(relationships_acq, directed=FALSE, vertices=orgs_acq)
# 
# deg_acq <- degree(g_acq, mode="all")            # Degree centrality
# 
# clo_acq <- closeness(g_acq)         # Closeness centrality
# 
# bet_acq <- betweenness(g_acq)       # Betweenness centrality
# 
# eig_acq <- evcent(g_acq)$vector     # Eigenvector centrality
# 
# # Intentos de gráfica.
# # Hay demasiada información.
# # plot(g_acq, vertex.label=NA, vertex.size=deg*2)
# # plot(g_acq, vertex.label=NA, vertex.size=5, layout=layout_with_fr,)
# # plot(g_acq, vertex.label=NA, vertex.size=5)
# 
# # Crear vector con nombres.
# name_acq <- get.vertex.attribute(g_acq, "shortName")
# 
# # Creación de la tabla(matriz) con nuevas variables.
# table_acq <- cbind(name_acq, deg_acq, clo_acq, bet_acq, eig_acq)
# table_acq
# 
# library(tibble)
# 
# # Convertir matriz en una tabla y mover los nombres de las filas
# # A una nueva columna.
# rowtable_acq <- table_acq %>% as.data.frame() %>% tibble::rownames_to_column("organisationID")
# 
# # Guardamos la nueva tabla.
# saveRDS(rowtable_acq, './stores/prueba_acq.rds')
# 
# # Resumen de número de proyectos (Equivalente a número de acquaintances)
# FP7_orgs <- import(filenames[4])
# tablaNumProyectos <- FP7_orgs %>%
#   group_by(organisationID, name, country, city) %>%
#   summarize(numProyectos=n())
# 
# saveRDS(tablaNumProyectos, './stores/tabla_num_proyectos.rds')
# 
# histogram(tablaNumProyectos$numProyectos)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.5. Experiencia de trabajo previo ("Acquaintance") del consorcio ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Acquaintance:

#"Number of joint participations of project partners in FP7 projects (count)"


## Parejas de organizaciones de H2020:

#Hay que remover los consorcios de 1
H2020_individual_projects <- filter(H2020_project,H2020_project$consorc_size=="1")

  nrow(H2020_organization)


H2020_consortia <- H2020_organization[! H2020_organization$projectID %in% 
                                        H2020_individual_projects$id, ]

nrow(H2020_consortia)


#Se dejan solo el nombre del consorcio y de la organización:
H2020_consortia <- H2020_consortia[ , c("projectID","organisationID")]

#Se deben eliminar duplicados:

table(duplicated(H2020_consortia))
H2020_consortia <- distinct(H2020_consortia, .keep_all = TRUE)
table(duplicated(H2020_consortia))


#Ahora se separan en una lista las observaciones de cada consorcio
H2020_consortia_list <- split(H2020_consortia, f = H2020_consortia$projectID)
H2020_pairs_results <- list()

gc()


for (i in 1:length(H2020_consortia_list)){
  
  H2020_pairs_results[[i]] <- data.frame(combinations(nrow(H2020_consortia_list[[i]]),
                                                      2,
                                                      H2020_consortia_list[[i]][,2]))
  names(H2020_pairs_results)[i] <- names(H2020_consortia_list)[i]
}

H2020_consortia_pairs <- bind_rows(H2020_pairs_results, .id = "projectID")
H2020_consortia_pairs$pair <- paste0(H2020_consortia_pairs$X1,"_",H2020_consortia_pairs$X2)



## Parejas de organizaciones de FP7:

#Se calcula el tamaño de los consorcios de FP7


#Hay que remover los consorcios de 1

#Se guardan los consorcios de 1
FP7_individual_projects <- filter(FP7_project,FP7_project$consorc_size=="1")

nrow(FP7_organization)

#Se remueven los consorcios de 1
FP7_consortia <- FP7_organization[! FP7_organization$projectID %in% 
                                    FP7_individual_projects$id, ]

nrow(FP7_consortia)


#Se dejan solo el nombre del consorcio y de la organización:
FP7_consortia <- FP7_consortia[ , c("projectID","organisationID")]

#Se deben eliminar duplicados:

table(duplicated(FP7_consortia))
FP7_consortia <- distinct(FP7_consortia, .keep_all = TRUE)
table(duplicated(FP7_consortia))


#Se eliminan NAs
colSums(is.na(FP7_consortia))
FP7_consortia <- filter(FP7_consortia,!(is.na(FP7_consortia$organisationID)))

#OJO: Al eliminar duplicados o NAs me pueden quedar consorcios de 1 y me falla
#el For.

#Identificar nuevos consorcios de 1:

FP7_consortia_singles <- FP7_consortia %>% 
  group_by(projectID) %>% 
  summarize(consorc_size=n())

FP7_consortia_singles <- filter(FP7_consortia_singles,
                                FP7_consortia_singles$consorc_size=="1")

#Hay que remover los consorcios de 1
FP7_consortia <- FP7_consortia[! FP7_consortia$projectID %in% 
                                 FP7_consortia_singles$projectID, ]



#Ahora se separan en una lista las observaciones de cada consorcio
FP7_consortia_list <- split(FP7_consortia, f = FP7_consortia$projectID)
length(FP7_consortia_list)
FP7_pairs_results <- list()

gc()

# #Debuggin
# i=2327
# view(FP7_consortia_list[[i]])

for (i in 1:length(FP7_consortia_list)){
  
  FP7_pairs_results[[i]] <- data.frame(combinations(nrow(FP7_consortia_list[[i]]),
                                                    2,
                                                    FP7_consortia_list[[i]][,2]))
  names(FP7_pairs_results)[i] <- names(FP7_consortia_list)[i]
}

FP7_consortia_pairs <- bind_rows(FP7_pairs_results, .id = "projectID")
FP7_consortia_pairs$pair <- paste0(FP7_consortia_pairs$X1,"_",FP7_consortia_pairs$X2)



#Se calcula la cantidad de veces que cada pareja aparece en consorcios FP7
FP7_cuenta_parejas <- FP7_consortia_pairs %>%
  group_by(pair) %>% 
  summarize(cuenta_pareja_FP7=n())

#Se trae la cantidad de veces que cada pareja de H2020 estuvo en FP7
nrow(H2020_consortia_pairs)
H2020_consortia_pairs <- left_join(H2020_consortia_pairs,FP7_cuenta_parejas,by="pair")

colSums(is.na(H2020_consortia_pairs))

#A los que quedan con NA se les imputa 0 participaciones previas
H2020_consortia_pairs$cuenta_pareja_FP7[is.na(H2020_consortia_pairs$cuenta_pareja_FP7)] <- 0

#Se calcula el acquaintance de cada proyecto (consorcio):
#"Number of joint participations of project partners in FP7 projects (count)"

H2020_acquaintance <- H2020_consortia_pairs %>% 
  group_by(projectID) %>% 
  summarize(acquaintance=sum(cuenta_pareja_FP7))

colnames(H2020_acquaintance) <- c("id","acquaintance")


#Se agrega el acquaintance a la base de proyectos:

nrow(H2020_project)
H2020_project <- left_join(H2020_project,H2020_acquaintance,by = "id")

colSums(is.na(H2020_project))
#Los consorcios de 1 quedan con NAs, se les imputa 0

H2020_project$acquaintance[is.na(H2020_project$acquaintance)] <- 0

summary(H2020_project$acquaintance)


rm(list = ls()[grep("^FP7_consort", ls())])
rm(list = ls()[grep("H2020_consort", ls())])

rm(list=c("FP7_cuenta_parejas","FP7_pairs_results",
          "FP7_individual_projects","H2020_pairs_results",
          "H2020_individual_projects","H2020_acquaintance","i"))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. PREPARACIÓN BASES DE DATOS: PROYECTOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.1. Cuenta de patentes por proyecto H2020 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


colnames(H2020_Irps)
table(H2020_Irps$awardKind)


#Se calcula el número de patentes por proyecto H2020
patentes_proy_H2020 <- H2020_Irps %>% 
  group_by(projectID) %>% 
  summarize(num_patentes = n())

patentes_proy_H2020$id <- patentes_proy_H2020$projectID
class(patentes_proy_H2020$id) <- "character"
patentes_proy_H2020$projectID <- NULL



#Se une con la base de proyectos H2020
nrow(H2020_project)
H2020_project <- left_join(H2020_project,patentes_proy_H2020,by="id")
nrow(H2020_project)

rm(patentes_proy_H2020)

#A los que quedan con NA se les imputa 0 patentes
H2020_project$num_patentes[is.na(H2020_project$num_patentes)] <- 0



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.2. Cuenta de publicaciones por proyecto y tipo H2020 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(H2020_publications)
table(H2020_publications$isPublishedAs)


#Se calcula el número de publicaciones por proyecto y tipo H2020
public_proy_H2020 <- H2020_publications %>% 
  group_by(projectID,isPublishedAs) %>% 
  summarize(num_public = n())


#Se ajustan los nombres y clases de columna
colnames(public_proy_H2020) <- c("id","publicationType","numPublic")
class(public_proy_H2020$id) <- "character"


#Se codifican los tipos de publicación

public_proy_H2020$publicationType <- case_when(
  public_proy_H2020$publicationType == "Book chapters" ~ "BookChapt",
  public_proy_H2020$publicationType == "Conference proceedings" ~ "ConfProceed",
  public_proy_H2020$publicationType == "Monographic books" ~ "Books",
  public_proy_H2020$publicationType == "Non-peer reviewed articles" ~ "non-peerArticle",
  public_proy_H2020$publicationType == "Other" ~ "Other",
  public_proy_H2020$publicationType == "Peer reviewed articles" ~ "peerArticle",
  public_proy_H2020$publicationType == "Thesis dissertations" ~ "ThesisDiss")


#Se cambia la estructura de filas a columnas ("wider") para agregarla a la base:

public_proy_H2020_w <- pivot_wider(public_proy_H2020, 
                                      names_from = publicationType,
                                      names_prefix = "NPub_",
                                      values_from = numPublic)

#Se cambian los NA por 0
colSums(is.na(public_proy_H2020_w))
public_proy_H2020_w[is.na(public_proy_H2020_w)] <- 0


#Se hace el join con la base de proyectos:
nrow(H2020_project)
H2020_project <- left_join(H2020_project,public_proy_H2020_w,by="id")
nrow(H2020_project)
colSums(is.na(H2020_project))

colnames(H2020_project)

#Se cambian los NA por 0
H2020_project <- H2020_project %>% 
  mutate_at(c(
            "NPub_peerArticle",
            "NPub_ConfProceed" ,
            "NPub_Other",
            "NPub_non-peerArticle",
            "NPub_ThesisDiss" ,
            "NPub_Books",
            "NPub_BookChapt"),
            ~replace_na(.,0))

#Total de publicaciones por proyecto:

H2020_project$NPub_total <- rowSums(H2020_project[,c(
  "NPub_peerArticle",
  "NPub_ConfProceed" ,
  "NPub_Other",
  "NPub_non-peerArticle",
  "NPub_ThesisDiss" ,
  "NPub_Books",
  "NPub_BookChapt")])


H2020_project$NPub_resto <- H2020_project$NPub_total -  H2020_project$NPub_peerArticle

rm(public_proy_H2020)
rm(public_proy_H2020_w)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.3. Cuenta de otros entregables por proyecto y tipo H2020 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


colnames(H2020_deliverables)
table(H2020_deliverables$deliverableType)


#Se calcula el número de entregables por proyecto y tipo H2020
entregabl_proy_H2020 <- H2020_deliverables %>% 
  group_by(projectID,deliverableType) %>% 
  summarize(num_entregabl = n())

#Se ajustan los nombres y clases de columna
colnames(entregabl_proy_H2020) <- c("id","deliverableType","numEntregabl")
class(entregabl_proy_H2020$id) <- "character"


#Se codifican los tipos de entregables

table(entregabl_proy_H2020$deliverableType)

entregabl_proy_H2020$deliverableType <- case_when(
  entregabl_proy_H2020$deliverableType == "Demonstrators, pilots, prototypes" ~ "Demos_Prototyp",
  entregabl_proy_H2020$deliverableType == "Documents, reports" ~ "Docs_reports",
  entregabl_proy_H2020$deliverableType == "Open Research Data Pilot" ~ "OpenResData",
  entregabl_proy_H2020$deliverableType == "Other" ~ "Other",
  entregabl_proy_H2020$deliverableType == "Websites, patent fillings, videos etc." ~ "Websites_videos")


#Se cambia la estructura de filas a columnas ("wider") para agregarla a la base:

entregabl_proy_H2020_w <- pivot_wider(entregabl_proy_H2020, 
                                   names_from = deliverableType,
                                   names_prefix = "NEntreg_",
                                   values_from = numEntregabl)

#Se cambian los NA por 0
colSums(is.na(entregabl_proy_H2020_w))
entregabl_proy_H2020_w[is.na(entregabl_proy_H2020_w)] <- 0


#Se hace el join con la base de proyectos:
nrow(H2020_project)
H2020_project <- left_join(H2020_project,entregabl_proy_H2020_w,by="id")
nrow(H2020_project)
colSums(is.na(H2020_project))

colnames(H2020_project)

#Se cambian los NA por 0
H2020_project <- H2020_project %>% 
  mutate_at(c(
    "NEntreg_Docs_reports",
    "NEntreg_Other",
    "NEntreg_OpenResData"    ,
    "NEntreg_Websites_videos",
    "NEntreg_Demos_Prototyp"), ~replace_na(.,0))

#Total de publicaciones por proyecto:

H2020_project$NEntreg_total <- rowSums(H2020_project[,c(
  "NEntreg_Docs_reports",
  "NEntreg_Other",
  "NEntreg_OpenResData"    ,
  "NEntreg_Websites_videos",
  "NEntreg_Demos_Prototyp")])

rm(entregabl_proy_H2020)
rm(entregabl_proy_H2020_w)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.4. Recursos del proyecto ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

H2020_project <- import("./stores/H2020_projects.rds")

class(H2020_project$totalCost)
class(H2020_project$ecMaxContribution)

H2020_project$totalCost <- gsub(",","\\.",H2020_project$totalCost)
H2020_project$ecMaxContribution <- gsub(",","\\.",H2020_project$ecMaxContribution)


class(H2020_project$totalCost) <- "numeric"
class(H2020_project$ecMaxContribution) <- "numeric"

#Corrección del Total cost cuando es 0
#Hay varios registros con total cost = 0 pero EC contribution!=0.
#Se le imputa la EC contribution al total cost

sum(H2020_project$totalCost==0 & H2020_project$ecMaxContribution > 0)
H2020_project$totalCost[H2020_project$totalCost==0] <- H2020_project$ecMaxContribution[H2020_project$totalCost==0]
sum(H2020_project$totalCost==0 & H2020_project$ecMaxContribution > 0)

H2020_project$ln_totalCost <- log(H2020_project$totalCost)
H2020_project$ln_ecMaxContribution <- log(H2020_project$ecMaxContribution)

H2020_project$EC_cost_share <- H2020_project$ecMaxContribution/H2020_project$totalCost


colSums(is.na(H2020_project))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.5. Índice integrado del proyecto ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(H2020_project)
nrow(H2020_project)
colSums(is.na(H2020_project))

#Explorar las variables que conforman el índice:

summary(H2020_project$num_patentes)
hist(H2020_project$num_patentes)

summary(H2020_project$NPub_peerArticle)
hist(H2020_project$NPub_peerArticle)

summary(H2020_project$NPub_resto)
hist(H2020_project$NPub_resto)

summary(H2020_project$NEntreg_total)
hist(H2020_project$NEntreg_total)

summary(H2020_project$EC_cost_share)
hist(H2020_project$EC_cost_share)

summary(H2020_project$ln_ecMaxContribution)
hist(H2020_project$ln_ecMaxContribution)

summary(H2020_project$ecMaxContribution)
hist(H2020_project$ecMaxContribution)

summary(H2020_project$ln_totalCost)
hist(H2020_project$ln_totalCost)

summary(H2020_project$totalCost)
hist(H2020_project$totalCost)

#Se escalan las variables para el índice:

H2020_project$num_patentes_s <- scale(H2020_project$num_patentes,center = TRUE, scale = TRUE)
H2020_project$NPub_peerArticle_s <- scale(H2020_project$NPub_peerArticle  ,center = TRUE, scale = TRUE)
H2020_project$NPub_resto_s <- scale(H2020_project$NPub_resto ,center = TRUE, scale = TRUE)
H2020_project$NEntreg_total_s <- scale(H2020_project$NEntreg_total ,center = TRUE, scale = TRUE)
H2020_project$EC_cost_share_s <- scale(H2020_project$EC_cost_share ,center = TRUE, scale = TRUE)
H2020_project$ecMaxContribution_s <- scale(H2020_project$ecMaxContribution ,center = TRUE, scale = TRUE)
H2020_project$totalCost_s <- scale(H2020_project$totalCost ,center = TRUE, scale = TRUE)


#Explorar las variables escaladas que conforman el índice:

summary(H2020_project$num_patentes_s)
hist(H2020_project$num_patentes_s)

summary(H2020_project$NPub_peerArticle_s)
hist(H2020_project$NPub_peerArticle_s)

summary(H2020_project$NPub_resto_s)
hist(H2020_project$NPub_resto_s)

summary(H2020_project$NEntreg_total_s)
hist(H2020_project$NEntreg_total_s)

summary(H2020_project$EC_cost_share_s)
hist(H2020_project$EC_cost_share_s)

summary(H2020_project$ecMaxContribution_s)
hist(H2020_project$ecMaxContribution_s)

summary(H2020_project$totalCost_s)
hist(H2020_project$totalCost_s)


#Se calcula el índice:


# H2020_project$indice_integrado <- 
#   H2020_project$num_patentes * 10 + 
#   H2020_project$NPub_peerArticle * 60 +
#   H2020_project$NPub_resto * 10 +
#   H2020_project$NEntreg_total*20
#   
# summary(H2020_project$indice_integrado)
# hist(H2020_project$indice_integrado)
# boxplot(H2020_project$indice_integrado)

H2020_project$indice_integrado_s <- 
  log(H2020_project$num_patentes_s * 10 + 
        H2020_project$NPub_peerArticle_s * 60 +
        H2020_project$NPub_resto_s * 10 +
        H2020_project$NEntreg_total_s*20 + 
        100)

summary(H2020_project$indice_integrado_s)
hist(H2020_project$indice_integrado_s)
boxplot(H2020_project$indice_integrado_s)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. UNIÓN FINAL DE BASES DE DATOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Añadir características de organizaciones y consorcios a la base de proyectos


#Coordinador (experiencia, país, ranking ,patentes)
#Ranking, patentes
#Experiencias previas
#Experiencia previa individual, agregada a nivel de consorcio (suma)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.1. Características del coordinador ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

H2020_coordinators <- H2020_organization[H2020_organization$role=="coordinator",]

colnames(H2020_coordinators)


H2020_coordinators <- 
  H2020_coordinators[,c("projectID","num_coord_FP7","country_cat",
                        "ranking_top1","ranking_top50","ranking_EU_top50",
                        "evidencia_patente")]

colnames(H2020_coordinators) <- c("id","coord_exper_FP7","coord_country_cat",
                                  "coord_ranking_p1","coord_top50_rank",
                                  "coord_top50_EU","coord_evid_patentes")

nrow(H2020_project)
H2020_project <- left_join(H2020_project,H2020_coordinators,by="id")

colSums(is.na(H2020_project))

rm(H2020_coordinators)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.2. Características de experiencia previa (en FP7) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

exp_prev_H2020 <- H2020_organization %>% 
  group_by(projectID) %>% 
  summarize(particip_consorc_FP7 = sum(num_particip_FP7),
            coordin_consorc_FP7 = sum(num_coord_FP7))

colnames(exp_prev_H2020)[1] <- "id"
colnames(exp_prev_H2020)

nrow(H2020_project)
H2020_project <- left_join(H2020_project,exp_prev_H2020,by="id")

colSums(is.na(H2020_project))

rm(exp_prev_H2020)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.3. Métricas de red (centralidad, grado, eigenvector) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Se calculan las métricas totales de redes para los consorcios (suma)

class(H2020_organization$netw_degree) <- "numeric"
class(H2020_organization$netw_closeness) <- "numeric"
class(H2020_organization$netw_betweenness) <- "numeric"
class(H2020_organization$netw_eigenvector) <- "numeric"


network_metrics_H2020 <- H2020_organization %>% 
  group_by(projectID) %>% 
  summarize(netw_degree = sum(netw_degree),
            netw_closeness = sum(netw_closeness),
            netw_betweenness = sum(netw_betweenness),
            netw_eigenvector = sum(netw_eigenvector))

colnames(network_metrics_H2020)[1] <- "id"
colnames(network_metrics_H2020)

nrow(H2020_project)
H2020_project <- left_join(H2020_project,network_metrics_H2020,by="id")

colSums(is.na(H2020_project))

rm(network_metrics_H2020)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.4. Características de ranking ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

suma_ranking_top50 <- H2020_organization %>% 
  group_by(projectID) %>% 
  summarize(ranking_top50_consorc = sum(ranking_top50))

colnames(suma_ranking_top50)[1] <- "id"

nrow(H2020_project)
H2020_project <- left_join(H2020_project,suma_ranking_top50,by="id")

rm(suma_ranking_top50)


suma_ranking_EU_top50 <- H2020_organization %>% 
  group_by(projectID) %>% 
  summarize(rank_EU_top50_consorc = sum(ranking_EU_top50))

colnames(suma_ranking_EU_top50)[1] <- "id"

nrow(H2020_project)
H2020_project <- left_join(H2020_project,suma_ranking_EU_top50,by="id")

rm(suma_ranking_EU_top50)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.5. Características de patentes ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

suma_evid_patent <- H2020_organization %>% 
  group_by(projectID) %>% 
  summarize(evid_patent_consorc = sum(evidencia_patente))

colnames(suma_evid_patent)[1] <- "id"

nrow(H2020_project)
H2020_project <- left_join(H2020_project,suma_evid_patent,by="id")

rm(suma_evid_patent)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.6. Características del tipo de proyecto (funding scheme) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(H2020_project)
table(H2020_project$fundingScheme)

esquemas_financ <- H2020_project %>% 
  group_by(fundingScheme) %>% 
  summarize(n())

colnames(cordis_funding_scheme)[1] <- "fundingScheme"

esquemas_financ <- left_join(esquemas_financ,
                             cordis_funding_scheme_names[,c("fundingScheme",
                                                            "fundingScheme_name",
                                                            "fS_type")],
                             by="fundingScheme")

nrow(H2020_project)
H2020_project <- left_join(H2020_project,cordis_funding_scheme_names[,c("fundingScheme",
                                                        "fundingScheme_name",
                                                        "fS_type")],
                   by="fundingScheme")


H2020_project$consorcUnit <- if_else(H2020_project$consorc_size==1,TRUE,FALSE)





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.7. Cálculo calificación de los jurados (estimación regresión paper) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
colnames(H2020_project)

H2020_project$coord_exper_FP7_dummy <- if_else(H2020_project$coord_exper_FP7>0,1,0)
H2020_project$log_eigenvector <- log(H2020_project$netw_eigenvector+1)

H2020_project$expert_score_hat <- 
  H2020_project$log_eigenvector * 0.516 +
  H2020_project$coord_exper_FP7_dummy * 0.262 +
  H2020_project$acquaintance * (-0.001) +
  H2020_project$ranking_top50_consorc * 0.109 +
  H2020_project$share_resCen * 0.007 +
  H2020_project$share_unis * (-0.012) +
  H2020_project$NumPartners_EU13 * (-0.222) + 
  H2020_project$consorc_size * (0.148) + 
  10.571

summary(H2020_project$expert_score_hat)
hist(H2020_project$expert_score_hat)





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.8. Exportación ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Control de NA
nrow(H2020_project)
colSums(is.na(H2020_project))

H2020_project$coord_ranking_p1[is.na(H2020_project$coord_ranking_p1)] <- 0


export(H2020_organization,"./stores/H2020_organizations.rds")
export(H2020_project,"./stores/H2020_projects.rds")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. SIMULACIÓN BASE DE DATOS TEST ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6.1. Creación de los consorcios aleatorios ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Condiciones:
# 1. Paises europeos
# 2. Mínimo 3 paises dentro del consorcio no tienen el mismo país

H2020 <-readRDS("./stores/H2020_orgs.rds")

orgs <- H2020 %>% 
  filter(country_cat != 'NonEU') %>%
  distinct(organisationID, name, activityType, country)

# 6.1.1. Pruebas con sampling ----

# Grupos por país.
orgs %>% group_by(country)

# Muestras por país.
new_df <- orgs %>% group_by(country) %>% sample_n(2, replace = TRUE)

# Otros métodos: Usando data.table (no funciona).
library("data.table")
new_df <- orgs[, .SD[sample(x = .N, size = 5)], by=col1]

# Otros métodos: Usando plyr (no funciona).
library("plyr")
new_df <- ddply(data_frame,.(country),function(x) x[sample(nrow(x),5),])

# 6.1.2. For loop con memoria. ----
new_df <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(new_df) <- c('organisationID', 'name', 'activityType', 'country', 'consortium')

# Crear muestra de todo el dataframe para dejarlo desordenado.
orgs <- orgs[sample(1:nrow(orgs)), ]

# Memoria de ids usados.
used <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(used) <- c('organisationID')

consortiumCounter <- 1
for (i in 1:nrow(orgs)) {
  if (orgs[i,1] %in% used$organisationID) {
    next
  }
  
  # Consorcio actual.
  currGroup <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(currGroup) <- c('organisationID', 'name', 'activityType', 'country')
  
  # Paises del consorcio actual.
  currCountries <- data.frame(matrix(ncol = 1, nrow = 0))
  colnames(currCountries) <- c('country')
  
  # Agregar organización actual al consorcio.
  currGroup[nrow(currGroup) + 1, ] <- orgs[i,]
  currCountries[nrow(currCountries) + 1, ] <- orgs[i,4]
  used[nrow(used) + 1, ] <- orgs[i,1]
  
  # Iterar en el resto de la lista.
  for (j in i:nrow(orgs)) {
    # Solo pueden pertenecer al consorcio si no están en otro consorcio
    # y no están en el mismo país.
    if (orgs[j,4] %in% currCountries$country || orgs[j,1] %in% used$organisationID) {
      next
    }
    
    # Agregar organización actual al consorcio.
    currGroup[nrow(currGroup) + 1, ] <- orgs[j,]
    currCountries[nrow(currCountries) + 1, ] <- orgs[i,4]
    used[nrow(used) + 1, ] <- orgs[j,1]
    if (nrow(currGroup) >= 3) {
      # Cuando ya hayan al menos 3, se agrega el consorcio al data frame.
      for (k in 1:nrow(currGroup)) {
        org <- currGroup[k, ]
        row <- c(org$organisationID, org$name, org$activityType, org$country)
        row <- append(row, consortiumCounter)
        new_df[nrow(new_df) + 1,] <- row
      }
      consortiumCounter <- consortiumCounter + 1
      break
    }
  }
}

saveRDS(new_df, './stores/consorcios_europeos_aleatorios.rds')

# Agregar consorcios fuera de EU a los nuevos consorcios.

# Obtener paises no euro.
nonEurOrgs <- H2020 %>% 
  filter(country_cat == 'NonEU') %>%
  distinct(organisationID, name, activityType, country)

# Desordenar lista.
nonEurOrgs <- nonEurOrgs[sample(1:nrow(nonEurOrgs)), ]

nonEurOrgs <- nonEurOrgs %>% 
  mutate(consortium=sample(1:consortiumCounter, nrow(nonEurOrgs), replace=TRUE))

consorcios_test <- rbind(new_df, nonEurOrgs)

# Convertir consorcios en numérico.
consorcios_test <- consorcios_test %>% transform(consortium = as.numeric(consortium))

# Máximo número de consorcios.
max(consorcios_test$consortium)

# Guardar base.
saveRDS(consorcios_test, './stores/consorcios_test.rds')


end <- Sys.time()
end - start

#rm(list=ls())
#gc()




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6.2. Enriquecer la información de los consorcios (sección 3) ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

consorcios_test <- import("./stores/consorcios_test.rds")









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. ESTADÍSTICAS DESCRIPTIVAS CON LA BASE COMPLETA ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

train <- readRDS("~/Desktop/BIG_DATA/MECA_BD_Final_project/stores/train.rds")


install.packages("pacman")

## llamar librerias de la sesion
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret) # Classification And REgression Training


##Resumen de las variables

skim(train$EC_cost_share)
summary(EC_cost_share)

skim(train$EC_cost_share)
summary(EC_cost_share)

skim(train$totalCost)
summary(totalCost)

skim(train$ecMaxContribution)
summary(ecMaxContribution)

skim(train$consorc_size)
summary(consorc_size)


skim(train$numPartnPRC)
summary(numPartnPRC)

skim(train$numPartnPRC)
summary(consorc_size)

##Otra manera con más información

install.packages("psych") 
require(psych) 


describe(totalCost)
describe(consorc_size)
describe(numPartnPRC)
describe(ecMaxContribution)
describe(numPartnREC)
describe(numPartnPRC)
describe(numPartnHES)
describe(NumPartners_EU13)
describe(NumPartners_EU15)
describe(NumPartners_NonEU)
describe(acquaintance)
describe(num_patentes)
describe(share_unis)
describe(share_resCen)
describe(share_compan)
describe(share_EU13)
describe(share_EU15)
describe(share_nonEU)
describe(NPub_peerArticle)
describe(NPub_ConfProceed)
describe(NPub_Other)
describe(`NPub_non-peerArticle`)
describe(NPub_ThesisDiss)
describe(NPub_Books)
describe()
describe()
describe()
describe()




proyectos <- import("./stores/H2020_projects.rds")
organizaciones <- import("./stores/H2020_organizations.rds")
# 
# 
# 





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 8. MODELOS DE REGRESIÓN / ESTIMACIÓN VARIABLES Y ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 8.1 Separación de bases de datos y preparación ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#train <- H2020_project
train <- proyectos
 
colnames(train)

train[ , c("legalBasis","ecSignatureDate","nature","objective",
                    "contentUpdateDate","rcn","grantDoi","masterCall","subCall",
                    "topics")] <- list(NULL)

export(train,"./stores/train.rds")

# Análisis preliminar: ¿correlación entre nuestro índice y el puntaje de expertos?
cor(train$indice_integrado_s,train$expert_score_hat)
cor.test(train$indice_integrado_s,train$expert_score_hat)
scatter.smooth(train$indice_integrado_s,train$expert_score_hat)

reg1 <- lm(expert_score_hat ~ indice_integrado_s,data=train)
stargazer(reg1,type="text")

summary(train$expert_score_hat)
summary(train$indice_integrado_s)



## 8.1.1. Separación de bases de datos y preparación ----


# Revisar: Generamos las particiones

set.seed(100)
split1 <- createDataPartition(train$expert_score_hat, p = .7)[[1]]
length(split1) 

other <- train[-split1,]
Tr_train <- train[split1,]

split2 <- createDataPartition(other$expert_score_hat, p = 1/3)[[1]]

Tr_eval <- other[ split2,]
Tr_test <- other[-split2,]

nrow(Tr_train)
nrow(Tr_eval)
nrow(Tr_test)

nrow(Tr_train)+nrow(Tr_eval)+nrow(Tr_test)==nrow(train)

rm(list=c("other","split1","split2"))





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 8.2 Modelos: recursos del proyecto ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


colnames(train)
colSums(is.na(train))

#Lógica:

#Características de tamaño y conformación
#Características de países representados
#Caraceterísticas de familiaridad y visibilidad de la red
#Características del coordinador
#Características de excelencia cientifica del consorcio (ranking)
#Características de patentes del consorcio.

#OJO:
#Degree y Eigenvector no deberían estar juntos
#Uni_share y priv_share no deberían ir juntos (correlación negativa, paper)

regresores <- "~ consorc_size + consorcUnit + fS_type +
                          share_unis + share_resCen + 
                          share_EU13 + share_EU15 + share_nonEU + 
                          acquaintance + log_eigenvector + 
                          coord_exper_FP7 + coord_country_cat + 
                          coord_ranking_p1 + coord_top50_rank + 
                          coord_top50_EU + coord_evid_patentes + 
                          ranking_top50_consorc + rank_EU_top50_consorc +
                          evid_patent_consorc +
                          expert_score_hat"


form_lntotalcost <- as.formula(paste0("ln_totalCost",regresores))
form_totalcost <-  as.formula(paste0("totalCost",regresores))
form_lnECcontrib <- as.formula(paste0("ln_ecMaxContribution",regresores))
form_ECcontrib <-  as.formula(paste0("ecMaxContribution",regresores))


reg_lntotalcost <- lm(form_lntotalcost,data=Tr_train)
reg_totalcost <-  lm(form_totalcost,data=Tr_train)
reg_lnECcontrib <- lm(form_lnECcontrib,data=Tr_train)
reg_ECcontrib <- lm(form_ECcontrib,data=Tr_train)


stargazer(reg_lntotalcost,reg_totalcost,reg_lnECcontrib,reg_ECcontrib,type="text")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 8.3 Modelos: patentes ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


form_patentes <- as.formula(paste0("num_patentes",regresores))

reg_patentes <- lm(form_patentes,data=Tr_train)

stargazer(reg_patentes,type="text")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 8.4 Modelos: publicaciones ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

form_articl <- as.formula(paste0("NPub_peerArticle",regresores))
form_tot_publs <- as.formula(paste0("NPub_total",regresores))
form_otras_pubs <- as.formula(paste0("NPub_resto",regresores))

reg_articl <- lm(form_articl,data=Tr_train)
reg_tot_publs <- lm(form_tot_publs,data=Tr_train)
reg_otras_pubs <- lm(form_otras_pubs,data=Tr_train)

stargazer(reg_articl,reg_tot_publs,reg_otras_pubs,type="text")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 8.5 Modelos: otros entregables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

form_entreg <- as.formula(paste0("NEntreg_total",regresores))

reg_entreg <- lm(form_entreg,data=Tr_train)

stargazer(reg_entreg,type="text")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 8.6 Modelos: estimación índice de calificación ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

colnames(train)

form_indice_integrado <- as.formula(paste0("indice_integrado_s",regresores))

reg_indice_integrado <- lm(form_indice_integrado,data=Tr_train)

stargazer(reg_indice_integrado,type="text")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 9. CÁLCULO ÍNDICE AGREGADO ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++








#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 10. CLASIFICACIÓN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++








#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 11. PREDICCIÓN FINAL CON BASE DE TEST ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
