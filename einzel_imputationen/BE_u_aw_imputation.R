# BE_u_aw_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# u_aw: U-Wert der Außenwand des Gebäudes
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bak>=0 & aw_flantgedges>=0 & aw_daemm_staerke_1>=0 & aw_konstr_1>=0 & u_aw>=0)
####

#eda.shape(DB_BE_clean$u_aw)
u_aw <- DB_BE_clean$u_aw
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
aw_konstr_1 <- DB_BE_clean$aw_konstr_1
aw_flantgedges <- DB_BE_clean$aw_flantgedges
aw_daemm_staerke_1 <- DB_BE_clean$aw_daemm_staerke_1

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# min(bak)
# min(hk_geb)
# min(aw_konstr_1)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem aw_konstr_1 = -7
DB_BE_unclean_u_aw <- subset(DB_BE, hk_geb>=0 & bak>=0 & aw_flantgedges>=0 & aw_daemm_staerke_1>=0 & aw_konstr_1>=0 & u_aw==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    u_aw_imputated <- DB_BE$u_aw == -7
    table <- table(u_aw_imputated)
    u_aw_n_imputated <- table[2]
    
    u_aw_imputated[u_aw_imputated == TRUE] <- 1
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

# Reg <- lm(u_aw ~ as.factor(hk_geb) * as.factor(bak) * aw_flantgedges * aw_daemm_staerke_1 * as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)   # R-squared:  0.9752 # NAs # -9 bis 17.7
# Reg <- lm(u_aw ~ as.factor(hk_geb) * as.factor(bak_grob) * aw_flantgedges * aw_daemm_staerke_1 * as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.8447 # NAs
# Reg <- lm(u_aw ~ as.factor(hk_geb) : as.factor(bak) : aw_flantgedges : aw_daemm_staerke_1 : as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.3967 #NAs
# Reg <- lm(u_aw ~ as.factor(hk_geb) * as.factor(bak) * aw_daemm_staerke_1 * as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.9422 # NAs -1.39 bis 2.57
# Reg <- lm(u_aw ~ as.factor(hk_geb) * as.factor(bak) * aw_flantgedges * as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.9604 # NAs -0.7 bis 3.84
# Reg <- lm(u_aw ~ as.factor(hk_geb) * as.factor(bak) * aw_flantgedges * aw_daemm_staerke_1, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.9402 # NAs -4.25 bis 5.97
# Reg <- lm(u_aw ~ as.factor(bak) * aw_flantgedges * aw_daemm_staerke_1 * as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.958 # NAs -1.3 bis 1.78
Reg <- lm(u_aw ~ as.factor(bak_grob) * aw_flantgedges * aw_daemm_staerke_1 * as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.8194
# Reg <- lm(u_aw ~ as.factor(hk_geb) * aw_flantgedges * aw_daemm_staerke_1 * as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.5394


# uk_geb is not working for predictor, as in the clean set are some categories missing... therefore the prediction does not work for these...

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/u_aw.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/u_aw.rds")

# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und q25_1 = -7
#     Berechnung der q25_1 in der bisher -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_u_aw

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 4) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- min(rounded_predict[rounded_predict>0]) #korrigiere negative berechnete Werte zum kleinsten gültigen Wert aus predict
DB_BE$u_aw[DB_BE$u_aw==(-7) & DB_BE$hk_geb>=0 & DB_BE$bak>=0 & DB_BE$aw_flantgedges>=0 & DB_BE$aw_daemm_staerke_1>=0 & DB_BE$aw_konstr_1>=0] <- rounded_predict #überschreibt q25_1 in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden
 
#table(DB_BE$u_aw)
#min(rounded_predict[rounded_predict>0])


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

u_aw <- DB_BE$u_aw
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, u_aw, u_aw_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_u_aw_imputiert.csv')

