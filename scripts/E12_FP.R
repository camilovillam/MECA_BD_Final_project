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


## 1.1. Bases de datos de proyectos europeos ----

### 1.1.1. Horizon 2020 (H2020) ----


#### Importar todos los archivos de Excel del directorio:

setwd("~/GitHub/MECA_BD_Final_project/stores/EU_research_projects/H2020_projects")

filenames <- list.files(".", pattern="*.xlsx", full.names=TRUE)
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




### 1.1.3. CORDIS reference data  ----




## 1.2. Bases de datos de OECD REGPAT (registros de patentes)  ----




### 1.3 Base de datos del CWTS Leiden Ranking 2022 (excelencia científica)  ----




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




