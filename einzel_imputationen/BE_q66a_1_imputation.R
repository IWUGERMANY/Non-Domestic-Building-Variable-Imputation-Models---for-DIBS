# BE_q66a_1_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# q66a_1: Unter Raumhöhe oder lichter Höhe eines Geschosses oder eines Raumes 
# verstehen wir den mittleren, senkrechten Abstand zwischen der Fußbodenoberkante 
# und der Unterkante der Decke. Bei großen Hallen mit nur einem Geschoss ist das 
# der Abstand von der Fußbodenoberkante bis Unterkante des Daches.
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & n_og>=0 & geb_f_hoehe_mittel_iwu>=0 & q66a_1>=0)
####

q66a_1 <- DB_BE_clean$q66a_1
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
n_og <- DB_BE_clean$n_og
geb_f_hoehe_mittel_iwu <- DB_BE_clean$geb_f_hoehe_mittel_iwu

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# min(n_og)
# min(hk_geb)
# min(geb_f_hoehe_mittel_iwu)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem geb_f_hoehe_mittel_iwu = -7
DB_BE_unclean_q66a_1 <- subset(DB_BE, hk_geb>=0 & n_og>=0 & geb_f_hoehe_mittel_iwu>=0 & q66a_1==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    q66a_1_imputated <- DB_BE$q66a_1 == -7
    table <- table(q66a_1_imputated)
    q66a_1_n_imputated <- table[2]
    
    q66a_1_imputated[q66a_1_imputated == TRUE] <- 1
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

# Reg <- lm(q66a_1 ~ as.factor(hk_geb) + n_og + geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.08872
# Reg <- lm(q66a_1 ~ as.factor(hk_geb) : n_og : geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.04292
# Reg <- lm(q66a_1 ~ as.factor(uk_geb) + n_og + geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.1884 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(uk_geb) : n_og : geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.1362 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(uk_geb) + n_og, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)                     # R-squared:  0.1743 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(uk_geb) * n_og, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)                     # R-squared:  0.2021 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(uk_geb) : n_og, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)                     # R-squared:  0.1466 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(uk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)                            # R-squared:  0.1715 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)                            # R-squared:  0.06519
# Reg <- lm(q66a_1 ~ as.factor(uk_geb) * geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)     # R-squared:  0.2585 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(uk_geb) : geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)   # R-squared:  0.1868 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(uk_geb) + geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)   # R-squared:  0.1759 # NAs -> Problem predict as uk_geb is new
# Reg <- lm(q66a_1 ~ as.factor(hk_geb) * geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)   # R-squared:    0.1008
Reg <- lm(q66a_1 ~ as.factor(hk_geb) * n_og * geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)   # R-squared:    0.1424

# uk_geb is not working for predictor, as in the clean set are some categories missing... therefore the prediction does not work for these...


sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/q66a_1__room_height.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/q66a_1__room_height.rds")


# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und q25_1 = -7
#     Berechnung der q25_1 in der bisher -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_q66a_1

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 0) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$q66a_1[DB_BE$q66a_1==(-7) & DB_BE$hk_geb>=0 & DB_BE$geb_f_hoehe_mittel_iwu>=0] <- rounded_predict #überschreibt q25_1 in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden

#table(DB_BE$q66a_1)


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

q66a_1 <- DB_BE$q66a_1
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, q66a_1, q66a_1_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_q66a_1_imputiert.csv')

