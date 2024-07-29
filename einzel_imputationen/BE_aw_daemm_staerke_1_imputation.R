# BE_aw_daemm_staerke_1_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# aw_daemm_staerke_1: Außenwanddämmstärke des überwiegenden Außenwandaufbaus
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & bak_grob>=0 & aw_daemm_staerke_1>=0)
####

aw_daemm_staerke_1 <- DB_BE_clean$aw_daemm_staerke_1
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# min(bak)
# min(hk_geb)
# min(bak_grob)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem bak_grob = -7
DB_BE_unclean_aw_daemm_staerke_1 <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & bak_grob>=0 & aw_daemm_staerke_1==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    aw_daemm_staerke_1_imputated <- DB_BE$aw_daemm_staerke_1 == -7
    table <- table(aw_daemm_staerke_1_imputated)
    aw_daemm_staerke_1_n_imputated <- table[2]
    
    aw_daemm_staerke_1_imputated[aw_daemm_staerke_1_imputated == TRUE] <- 1
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

# Reg <- lm(aw_daemm_staerke_1 ~ as.factor(hk_geb) * as.factor(bak_grob), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)   # R-squared:  0.08138
Reg <- lm(aw_daemm_staerke_1 ~ as.factor(hk_geb) * as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.2693 # NAs not effecting prediction for Imputation!!!!
# Reg <- lm(aw_daemm_staerke_1 ~ as.factor(hk_geb) : as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.2693 #NAs
# Reg <- lm(aw_daemm_staerke_1 ~ as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.03334
# Reg <- lm(aw_daemm_staerke_1 ~ as.factor(uk_geb) * as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.609 # NAs requiered for predict

# uk_geb is not working for predictor, as in the clean set are some categories missing... therefore the prediction does not work for these...
# Also with the chosen option NAs are in the equation, these do not affect the prediction!


sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/aw_daemm_staerke_1.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/aw_daemm_staerke_1.rds")

# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und q25_1 = -7
#     Berechnung der q25_1 in der bisher -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_aw_daemm_staerke_1

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 0) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$aw_daemm_staerke_1[DB_BE$aw_daemm_staerke_1==(-7) & DB_BE$hk_geb>=0 & DB_BE$bak>=0] <- rounded_predict #überschreibt q25_1 in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden

#table(DB_BE$aw_daemm_staerke_1)


#----------------------------------------
# Alle -8 zu 0, da hier nicht gedämmt wurde

DB_BE$aw_daemm_staerke_1[DB_BE$aw_daemm_staerke_1==(-8)] <- 0


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

aw_daemm_staerke_1 <- DB_BE$aw_daemm_staerke_1
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, aw_daemm_staerke_1, aw_daemm_staerke_1_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_aw_daemm_staerke_1_imputiert.csv')


