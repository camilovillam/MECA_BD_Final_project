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
       xgboost)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. CARGA DE LAS BASES DE DATOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


setwd("~/GitHub/MECA_BD_Final_project/")


## 1.1. Bases de datos de proyectos europeos ----

### 1.1.1. Horizon 2020 (H2020) ----


#### Importar todos los archivos de Excel del directorio:

filenames <- list.files("./stores/EU_research_projects/H2020_projects", pattern="*.xlsx", full.names=TRUE)
filenames


H2020_euroSciVoc <-     import(filenames[1])
H2020_legalBasis <-     import(filenames[2])
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
H2020_reportSummaries<- import(filenames[14])
H2020_topics <-         import(filenames[15])
H2020_webItem <-        import(filenames[16])
H2020_webLink <-        import(filenames[17])



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


setwd("~/GitHub/MECA_BD_Final_project/")


#### Importar todos los archivos de Excel del directorio:

filenames <- list.files("./stores/EU_research_projects/FP7_projects", pattern="*.xlsx", full.names=TRUE)
filenames


FP7_euroSciVoc <-     import(filenames[1])
FP7_publications <-   import(filenames[2])
FP7_legalBasis <-     import(filenames[3])
FP7_organization <-   import(filenames[4])
FP7_project <-        import(filenames[5])
FP7_Irps <-           import(filenames[6])
FP7_reportSummaries<- import(filenames[7])
FP7_topics <-         import(filenames[8])
FP7_webItem <-        import(filenames[9])
FP7_webLink <-        import(filenames[10])


sapply(FP7_publications, class)



### 1.1.3. CORDIS reference data  ----

#### Importar todos los archivos de Excel del directorio:

filenames <- list.files("./stores/EU_research_projects/reference_data", pattern="*.xls*", full.names=TRUE)
filenames


cordis_countries <-       import(filenames[1])
FP7_programmes <-         import(filenames[2])
H2020_topic_keywords <-   import(filenames[3])
cordis_org_activity <-    import(filenames[4])
cordis_funding_scheme <-  import(filenames[5])


## 1.2. Bases de datos de OECD REGPAT (registros de patentes)  ----





## 1.3 Base de datos del CWTS Leiden Ranking 2022 (excelencia científica)  ----

setwd("~/GitHub/MECA_BD_Final_project/")

#La primera vez lo cargo desde el Excel
#CWTS_ranking <- import("./stores/CWTS_Leiden_Ranking/CWTS Leiden Ranking 2022.xlsx")

#Lo guardo para hacer más óptimo el tamaño
export(CWTS_ranking,"./stores/CWTS_Leiden_Ranking/CWTS_ranking_2022.rds")

#Cargo el archivo de ranking desde el .RDS
CWTS_ranking <- import("./stores/CWTS_Leiden_Ranking/CWTS_ranking_2022.rds")


sapply(CWTS_ranking, class)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. EXPLORACIÓN INICIAL DE LOS DATOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# view(train_prop)
# view(test_prop)
# 
# table(train_prop$l3)
# table(train_prop$property_type)
# table(train_prop$operation_type)
# table(train_prop$currency)
# table(train_prop$ad_type)
# 
# 
# table(test_prop$l3)
# table(test_prop$property_type)
# table(test_prop$operation_type)
# table(test_prop$currency)
# table(test_prop$ad_type)
# 
# 
# #Exploración de las bases de datos:
# skim(train_prop)
# skim(test_prop)
# 
# #Comparación de variables (columnas) entre las dos bases de datos:
# compare_df_cols(train_prop, test_prop)
# 
# #Resumen de diferencias
# all_equal(train_prop, test_prop)


orglist <- data.frame(table(H2020_organization$name))
orgtype <- data.frame(table(H2020_organization$activityType))
pubtype <- data.frame(table(H2020_publications$isPublishedAs))
delivtype <- data.frame(table(H2020_deliverables$deliverableType))
IPRtype <- data.frame(table(H2020_Irps$type))

skim(H2020_project)

cols_factor <- c("status","fundingScheme")
H2020_project[cols_factor] <- lapply(H2020_project[cols_factor], factor)
sapply(H2020_project, class)

fundingSchemes <- data.frame(table(H2020_project$fundingScheme))


#Cuántos proyectos tienen al menos una publicación?

public_proy_tipo <- H2020_publications %>% 
    group_by(projectID,projectAcronym,isPublishedAs) %>%
    summarize(num_public=n())

public_proy <- H2020_publications %>% 
  group_by(projectID,projectAcronym) %>%
  summarize(num_public=n())

colnames(public_proy) <- c("id","acronym","numPublics")

nrow(H2020_project)


H2020_project <- 
  full_join(H2020_project, public_proy,
             by = c("id","acronym"))

nrow(H2020_project)

summary(H2020_project$numPublics)

proj_sin_pubs <- subset(H2020_project$numPublics)

proj_sin_pubs <- H2020_project[is.na(H2020_project$numPublics),]




#Número de socios por proyecto:


socios_proy_tipo <- H2020_organization %>% 
  group_by(projectID,projectAcronym,activityType) %>%
  summarize(num_org=n())

socios_proy <- H2020_organization %>% 
  group_by(projectID,projectAcronym) %>%
  summarize(num_org=n())

colnames(socios_proy) <- c("id","acronym","numPartners")

nrow(H2020_project)

H2020_project <- 
  full_join(H2020_project, socios_proy,
            by = c("id","acronym"))

nrow(H2020_project)

summary(H2020_project$numPartners)

hist(table(H2020_project$numPartners))




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PREPARACIÓN BASES DE DATOS: INSTITUCIONES ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##3.1. Listado único de organizaciones ----


organizations <- distinct(H2020_organization[,3:14])



##3.2 Ranking ----

ranking2022 <- CWTS_ranking[CWTS_ranking$Period=="2017–2020" & 
                              CWTS_ranking$Field =="All sciences" &
                              CWTS_ranking$Frac_counting==1,]

nrow(ranking2022)

#Ranking con base en el número total de publicaciones:
ranking2022 <- ranking2022[order(ranking2022$impact_P,decreasing = TRUE),]
ranking2022 <- ranking2022 %>% mutate(ranking_p= 1:n())

#Ranking con base en el número de publicaciones Top 1%
ranking2022 <- ranking2022[order(ranking2022$P_top1,decreasing = TRUE),]
ranking2022 <- ranking2022 %>% mutate(ranking_p_top1= 1:n())


ranking2022$name <- toupper(ranking2022$University)


organizations_ranking <- left_join(organizations,ranking2022[,c("name","ranking_p","ranking_p_top1")],by="name")

universities <- organizations_ranking[organizations_ranking$activityType %in% c("HES"),]

colSums(is.na(universities))
nrow(universities)

print(paste0("El porcentaje de universidades encontradas es ", round(100 - (3681/4157)*100,1), " %"))


#PROBLEMA COMPLEJO, NAME DISAMBIGUATION.

# Solución para este caso específico: la excelencia científica se mide solo para
# las 50 top universidades, es decir, es una variable dummy para reflejar
# si está en el top 50 del CWTS (ver paper).
# En este caso, podemos crear un diccionario de nombres para las 50 universidades
# top.
# Posible trabajo futuro: incluir todas las universidades, adelantar trabajo
# de corrección de nombres.


export(universities,"./stores/Nombres_universidades/Universidades.xlsx")
export(ranking2022,"./stores/Nombres_universidades/Ranking 2022.xlsx")


##3.3 Patentes ----




#3.4 Experiencia previa en FP7 ----


skim(H2020_organization)
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




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. PREPARACIÓN BASES DE DATOS: CONSORCIOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++








#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. PREPARACIÓN BASES DE DATOS: PROYECTOS ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#5.1. Cuenta de patentes por proyecto H2020 ----




#Se calculan el número de participaciones y de coordinaciones en el FP7
org_particip_FP7 <- FP7_organization %>% 
  group_by(organisationID) %>% 
  summarize(num_particip_FP7= sum(role=="participant"),
            num_coord_FP7 = sum(role=="coordinator"))


#5.2. Cuenta de publicaciones por proyecto y tipo H2020 ----




#5.3. Cuenta de otros entregables por proyecto y tipo H2020 ----


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. MODELOS DE PREDICCIÓN VARIABLES Y ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. CÁLCULO ÍNDICE AGREGADO ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 8. CLASIFICACIÓN ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++







#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
