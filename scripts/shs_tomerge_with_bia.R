library(data.table)

setwd("C:/Users/P826495/Documents/Projet_biodiv_paper/SHSS/")

shs <- fread("for_BIA_list_ISIN_top95issuers_internal_restricted.csv", encoding = "UTF-8")
# shsr <- fread("for_BIA_list_ISIN_top95issuers_ok_to_send.csv", header = T)
map_ini <- fread("ptf_BdF_BIA_mapping.csv")

#on somme les détentions par ISIN
colnames(shs)[ colnames ( shs) == "Nom de l'émetteur"] <- "Nom_emetteur"
shs_summed_by_isin <- shs[, .(Nom_emetteur[1], total_detenu = sum(`valeur du fait`)) ,by="Identifiant"]
#check
sum(shs_summed_by_isin$total_detenu)/sum(shs$`valeur du fait`)

#On retire les ISINs non couverts par C4F dans la base map
map <- subset(map_ini, BIA_coverage == "Covered")

#On merge les détentions par ISIN avec la map de C4F (pour avoir les noms d'émetteurs C4F)
shs_map <- merge(shs_summed_by_isin,map, by.x="Identifiant", by.y="portfolio_isin")
#check
sum(shs_map$total_detenu)/sum(shs$`valeur du fait`)
#on doit couvrir 94.73% de la base shs initiale (top95)

#On somme les détentions par nom d'émetteur C4F (variable issuer_name)
final_shs_map <- shs_map[, .(amount_held = sum(total_detenu)) , by="entity_analyzed_name"]
#check
sum(final_shs_map$amount_held)/sum(shs$`valeur du fait`)
#save
#write.csv(final_shs_map, "shs_tomerge_with_bia.csv")

###########################################
#SHS LISTED SHARES ONLY 
#on somme les détentions par ISIN en ne gardant que les actions
shs_shares_summed_by_isin <- shs[`Code Classification instrument ESA 2010`=="F_511", .(Nom_emetteur[1], total_detenu = sum(`valeur du fait`)) ,by="Identifiant"]
#check
sum(shs_shares_summed_by_isin$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_511"])

#On merge les détentions par ISIN avec la map de C4F (pour avoir les noms d'émetteurs C4F)
shs_shares_map <- merge(shs_shares_summed_by_isin,map, by.x="Identifiant", by.y="portfolio_isin")
#check
sum(shs_shares_map$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_511"])

#On somme les détentions par nom d'émetteur C4F (variable issuer_name)
final_shs_shares_map <- shs_shares_map[, .(amount_held = sum(total_detenu)) , by="entity_analyzed_name"]
#check
sum(final_shs_shares_map$amount_held)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_511"])

#save
#write.csv(final_shs_shares_map, "shs_tomerge_with_bia_F_511.csv")

###########################################
#SHS DEBT SECURITIES ONLY 
#on somme les détentions par ISIN en ne gardant que les debt securities F_31
shs_debt31_summed_by_isin <- shs[`Code Classification instrument ESA 2010`=="F_31" , .(Nom_emetteur[1], total_detenu = sum(`valeur du fait`)) ,by="Identifiant"]

#check
sum(shs_debt31_summed_by_isin$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_31"|
                                                                     shs$`Code Classification instrument ESA 2010`=="F_32"])

#On merge les détentions par ISIN avec la map de C4F (pour avoir les noms d'émetteurs C4F)
shs_debt31_map <- merge(shs_debt31_summed_by_isin,map, by.x="Identifiant", by.y="portfolio_isin")
#check
sum(shs_debt31_map$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_31"])

#On somme les détentions par nom d'émetteur C4F (variable issuer_name)
final_shs_debt31_map <- shs_debt31_map[, .(amount_held = sum(total_detenu)) , by="entity_analyzed_name"]
#check
sum(final_shs_debt31_map$amount_held)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_31"])

#check
sum(final_shs_debt31_map$amount_held)+sum(final_shs_shares_map$amount_held)==sum(final_shs_map$amount_held)
#save
#write.csv(final_shs_debt31_map, "shs_tomerge_with_bia_F_32.csv")

###########################################
#SHS DEBT SECURITIES ONLY 
#on somme les détentions par ISIN en ne gardant que les debt securities F_32
shs_debt32_summed_by_isin <- shs[`Code Classification instrument ESA 2010`=="F_32" , .(Nom_emetteur[1], total_detenu = sum(`valeur du fait`)) ,by="Identifiant"]

#check
sum(shs_debt32_summed_by_isin$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_32"])

#On merge les détentions par ISIN avec la map de C4F (pour avoir les noms d'émetteurs C4F)
shs_debt32_map <- merge(shs_debt32_summed_by_isin,map, by.x="Identifiant", by.y="portfolio_isin")
#check
sum(shs_debt32_map$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_32"])

#On somme les détentions par nom d'émetteur C4F (variable issuer_name)
final_shs_debt32_map <- shs_debt32_map[, .(amount_held = sum(total_detenu)) , by="entity_analyzed_name"]
#check
sum(final_shs_debt32_map$amount_held)/sum(shs$`valeur du fait`[shs$`Code Classification instrument ESA 2010`=="F_32"])

#check
sum(final_shs_debt32_map$amount_held)+sum(final_shs_shares_map$amount_held)==sum(final_shs_map$amount_held)
#save
#write.csv(final_shs_debt32_map, "shs_tomerge_with_bia_F_32.csv")


###########################################
#SHS S_122 (Deposit taking corporations except central banks) ONLY 
#on somme les détentions par ISIN en ne gardant que les détentions des S_122
shs_S_122_summed_by_isin <- shs[`Code Secteur détenteur`=="S_122" , .(Nom_emetteur[1], total_detenu = sum(`valeur du fait`)) ,by="Identifiant"]
#check
sum(shs_S_122_summed_by_isin$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_122"])

#On merge les détentions par ISIN avec la map de C4F (pour avoir les noms d'émetteurs C4F)
shs_S_122_map <- merge(shs_S_122_summed_by_isin,map, by.x="Identifiant", by.y="portfolio_isin")
#check
sum(shs_S_122_map$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_122"])

#On somme les détentions par nom d'émetteur C4F (variable issuer_name)
final_shs_S_122_map <- shs_S_122_map[, .(amount_held = sum(total_detenu)) , by="entity_analyzed_name"]
#check
sum(final_shs_S_122_map$amount_held)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_122"])

#save
#write.csv(final_shs_S_122_map, "shs_tomerge_with_bia_S_122.csv")

###########################################
#SHS S_124 (Non-MMF Investment funds) ONLY 
#on somme les détentions par ISIN en ne gardant que les détentions des S_124
shs_S_124_summed_by_isin <- shs[`Code Secteur détenteur`=="S_124" , .(Nom_emetteur[1], total_detenu = sum(`valeur du fait`)) ,by="Identifiant"]
#check
sum(shs_S_124_summed_by_isin$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_124"])

#On merge les détentions par ISIN avec la map de C4F (pour avoir les noms d'émetteurs C4F)
shs_S_124_map <- merge(shs_S_124_summed_by_isin,map, by.x="Identifiant", by.y="portfolio_isin")
#check
sum(shs_S_124_map$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_124"])

#On somme les détentions par nom d'émetteur C4F (variable issuer_name)
final_shs_S_124_map <- shs_S_124_map[, .(amount_held = sum(total_detenu)) , by="entity_analyzed_name"]
#check
sum(final_shs_S_124_map$amount_held)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_124"])

#save
#write.csv(final_shs_S_124_map, "shs_tomerge_with_bia_S_124.csv")
###########################################
#SHS S_128 (Insurance corporations) ONLY 
#on somme les détentions par ISIN en ne gardant que les détentions des S_128
shs_S_128_summed_by_isin <- shs[`Code Secteur détenteur`=="S_128" , .(Nom_emetteur[1], total_detenu = sum(`valeur du fait`)) ,by="Identifiant"]
#check
sum(shs_S_128_summed_by_isin$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_128"])

#On merge les détentions par ISIN avec la map de C4F (pour avoir les noms d'émetteurs C4F)
shs_S_128_map <- merge(shs_S_128_summed_by_isin,map, by.x="Identifiant", by.y="portfolio_isin")
#check
sum(shs_S_128_map$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_128"])

#On somme les détentions par nom d'émetteur C4F (variable issuer_name)
final_shs_S_128_map <- shs_S_128_map[, .(amount_held = sum(total_detenu)) , by="entity_analyzed_name"]
#check
sum(final_shs_S_128_map$amount_held)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_128"])

#save
#write.csv(final_shs_S_128_map, "shs_tomerge_with_bia_S_128.csv")

###########################################
#SHS for OTHERS (S_125W: Other financial corporations excluding financial vehicle corporations;
# S_125A: Financial vehicle corporations; S_123: Money market funds (MMF)) 

#on somme les détentions par ISIN en ne gardant que les détentions des other_holders
shs_other_holders_summed_by_isin <- shs[`Code Secteur détenteur`=="S_125W"|
                                          `Code Secteur détenteur`=="S_125A"|
                                          `Code Secteur détenteur`=="S_123"
                                        , .(Nom_emetteur[1], total_detenu = sum(`valeur du fait`)) ,by="Identifiant"]
#check
sum(shs_other_holders_summed_by_isin$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_125W"|
                                                                            shs$`Code Secteur détenteur`=="S_125A"|
                                                                            shs$`Code Secteur détenteur`=="S_123"])

#On merge les détentions par ISIN avec la map de C4F (pour avoir les noms d'émetteurs C4F)
shs_other_holders_map <- merge(shs_other_holders_summed_by_isin,map, by.x="Identifiant", by.y="portfolio_isin")
#check
sum(shs_other_holders_map$total_detenu)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_125W"|
                                                                   shs$`Code Secteur détenteur`=="S_125A"|
                                                                   shs$`Code Secteur détenteur`=="S_123"])

#On somme les détentions par nom d'émetteur C4F (variable issuer_name)
final_shs_other_holders_map <- shs_other_holders_map[, .(amount_held = sum(total_detenu)) , by="entity_analyzed_name"]
#check
sum(final_shs_other_holders_map$amount_held)/sum(shs$`valeur du fait`[shs$`Code Secteur détenteur`=="S_125W"|
                                                                        shs$`Code Secteur détenteur`=="S_125A"|
                                                                        shs$`Code Secteur détenteur`=="S_123"])

#save
#write.csv(final_shs_other_holders_map, "shs_tomerge_with_bia_other_holders.csv")

sum(final_shs_other_holders_map$amount_held)+
  sum(final_shs_S_122_map$amount_held)+
  sum(final_shs_S_124_map$amount_held)+
  sum(final_shs_S_128_map$amount_held)==sum(final_shs_map$amount_held)

############################################
#base avec toutes les info
colnames(final_shs_debt31_map)[2] <- "amount_held_debtST"
colnames(final_shs_debt32_map)[2] <- "amount_held_debtLT"
colnames(final_shs_shares_map)[2] <- "amount_held_shares"
colnames(final_shs_S_122_map)[2] <- "amount_held_S_122"
colnames(final_shs_S_124_map)[2] <- "amount_held_S_124"
colnames(final_shs_S_128_map)[2] <- "amount_held_S_128"
colnames(final_shs_other_holders_map)[2] <- "amount_held_other_holders"

final_shs_map <- merge(final_shs_map, final_shs_debt31_map, all.x=T)
final_shs_map <- merge(final_shs_map, final_shs_debt32_map, all.x=T)
final_shs_map <- merge(final_shs_map, final_shs_shares_map, all.x=T)
final_shs_map <- merge(final_shs_map, final_shs_S_122_map, all.x=T)
final_shs_map <- merge(final_shs_map, final_shs_S_124_map, all.x=T)
final_shs_map <- merge(final_shs_map, final_shs_S_128_map, all.x=T)
final_shs_map <- merge(final_shs_map, final_shs_other_holders_map, all.x=T)


#check
sum(final_shs_map$amount_held_debtST, na.rm=T)+sum(final_shs_map$amount_held_debtLT, na.rm=T)+sum(final_shs_map$amount_held_shares,na.rm=T)
sum(final_shs_map$amount_held_S_128, na.rm=T)+sum(final_shs_map$amount_held_S_122,na.rm=T)+
  sum(final_shs_map$amount_held_S_124, na.rm=T)+sum(final_shs_map$amount_held_other_holders,na.rm=T)
sum(final_shs_map$amount_held)  

#replace NAs by zeros
final_shs_map[is.na(final_shs_map)] <- 0

write.csv(final_shs_map, "shs_tomerge_with_bia.csv")

