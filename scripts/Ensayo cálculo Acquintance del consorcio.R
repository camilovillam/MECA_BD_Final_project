library(gtools)

?permute

#EJEMPLO H2020

#H2020_organizations_sample <- H2020_organization[1:68,c(2,5)]


#Hay que remover los consorcios de 1; hacerlo automático
#Ya hay un campo con tamaño del consorcio, o debe haberlo
#H2020_organizations_sample <- H2020_organizations_sample[-c(13,28),]
H2020_organizations_sample <- filter(H2020_organizations,!(H2020_project$consorc_size==1))


H2020_consortia_list <- split(H2020_organizations_sample, f = H2020_organizations_sample$projectAcronym)

H2020_pairs_results <- list()

for (i in 1:length(H2020_consortia_list)){

  H2020_pairs_results[[i]] <- data.frame(combinations(nrow(H2020_consortia_list[[i]]),
                                                2,
                                                H2020_consortia_list[[i]][,2]))
  names(H2020_pairs_results)[i] <- names(H2020_consortia_list)[i]
}

H2020_consortia_pairs <- bind_rows(H2020_pairs_results, .id = "projectAcronym")
H2020_consortia_pairs$pair <- paste0(H2020_consortia_pairs$X1,"_",H2020_consortia_pairs$X2)


#Ésta no se necesita acá, es más la de FP7
cuenta_parejas_H2020 <- H2020_consortia_pairs %>%
  group_by(pair) %>% 
  summarize(cuenta_pareja=n())


consortia_pairs <- left_join(consortia_pairs,cuenta_parejas_h2020,by="pair")

acquaintance <- consortia_pairs %>% 
  group_by(projectAcronym) %>% 
  summarize(acquaintance=sum(cuenta_pareja))


