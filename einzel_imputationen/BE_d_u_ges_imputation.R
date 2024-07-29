
# BE_d_u_ges_imputation.R
# Julian Bischof
# 03.08.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# d_u_ges ist der mittlere U-Wert des Dachs (opak und transparent kombiniert)
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & bak_grob>=0 & d_u_ges>=0)
####

# hk_geb_clean <- DB_BE_clean$hk_geb
# bgf_clean <- DB_BE_clean$bgf
# d_u_ges_clean <- DB_BE_clean$d_u_ges
#
# # min(bgf_clean)
# # min(hk_geb_clean)
# # min(d_u_ges_clean)

hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
d_u_ges <- DB_BE_clean$d_u_ges
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)
bak_grob <- DB_BE_clean$bak_grob
bak <- DB_BE_clean$bak
N_Clean <- nrow(DB_BE_clean)



#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem d_u_ges = -7
DB_BE_unclean_d_u_ges <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & bak_grob>=0 & d_u_ges==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    d_u_ges_imputated <- DB_BE$d_u_ges == -7
    table <- table(d_u_ges_imputated)
    d_u_gesd_u_ges_n_imputated <- table[2]
    
    d_u_ges_imputated[d_u_ges_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt



#............................................................................
#
# 2
#Auswertung####
#
#............................................................................

#----------------------------------------
# 2.1 Regressionsmodel 

#Regression linear, Faktoren werden einzeln berücksichtigt
# Reg <- lm(d_u_ges ~ as.factor(uk_geb) : as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.6167 # NAs
# Reg <- lm(d_u_ges ~ as.factor(uk_geb) : as.factor(bak_grob), weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.3721 # NAs
Reg <- lm(d_u_ges ~ as.factor(hk_geb) + as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.3072
# Reg <- lm(d_u_ges ~ as.factor(hk_geb) : as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.4383 # NAs
# Reg <- lm(d_u_ges ~ as.factor(hk_geb) : as.factor(bak_grob), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.2222 # NAs

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/d_u_ges.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/d_u_ges.rds")

# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation 
#     Berechnung der d_u_ges in der bisher -7 steht für die Zeilen in denen prediktoren vorhanden, also >=0
data_for_predict <- DB_BE_unclean_d_u_ges

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 4) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$d_u_ges[DB_BE$d_u_ges==(-7) & DB_BE$bak>=0 & DB_BE$hk_geb>=0] <- rounded_predict #überschreibt d_u_ges in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden



#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

d_u_ges <- DB_BE$d_u_ges
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, d_u_ges, d_u_ges_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_d_u_ges_imputiert.csv')

