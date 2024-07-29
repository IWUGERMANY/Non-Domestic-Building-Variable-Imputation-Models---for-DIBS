# BE_n_og_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# n_og: Anzahl oberirdischer Geschosse
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, uk_geb>=0 & dachform_be>=0 & n_og>=0)
####

hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
n_og <- DB_BE_clean$n_og
#geb_flaeche <- DB_BE_clean$geb_flaeche
dachform_be <- DB_BE_clean$dachform_be

geb_f_hoehe_mittel_iwu <- DB_BE_clean$geb_f_hoehe_mittel_iwu

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# min(n_og)
# min(hk_geb)
# min(geb_flaeche)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem gbf = -7
DB_BE_unclean_n_og <- subset(DB_BE, uk_geb>=0 & dachform_be>=0 & n_og==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    n_og_imputated <- DB_BE$n_og == -7
    table <- table(n_og_imputated)
    n_og_n_imputated <- table[2]
    
    n_og_imputated[n_og_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................

#----------------------------------------
# 2.1 Regressionsmodel n_og_clean und hk_geb_clean zu n_og_clean

#Regression linear, Faktoren werden einzeln berücksichtigt
# Reg <- lm(n_og ~ as.factor(hk_geb)) # R-squared:  0.1889
# Reg <- lm(n_og ~ as.factor(hk_geb) + geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)               # R-squared:  0.4179
# Reg <- lm(n_og ~ as.factor(hk_geb) : geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)               # R-squared:  0.4295
# Reg <- lm(n_og ~ as.factor(hk_geb) * geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)               # R-squared:  0.4375
# Reg <- lm(n_og ~ as.factor(uk_geb) * geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)               # R-squared:  0.4894 # NAs
# Reg <- lm(n_og ~ as.factor(uk_geb) : geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)               # R-squared:  0.4636
# Reg <- lm(n_og ~ as.factor(uk_geb) + geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)               # R-squared:  0.4491
# Reg <- lm(n_og ~ as.factor(uk_geb) : geb_f_hoehe_mittel_iwu : as.factor(dachform_be), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.5787 # NAs
Reg <- lm(n_og ~ as.factor(hk_geb) : geb_f_hoehe_mittel_iwu : as.factor(dachform_be), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.5106

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/n_og.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/n_og.rds")

# summary(Reg)
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn n_og und hk_geb vorhanden und n_og = -7
#     Berechnung der n_og in der bisher -7 steht für die Zeilen in denen n_og und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_n_og

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 2) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$n_og[DB_BE$n_og==(-7) & DB_BE$dachform_be>=0 & DB_BE$uk_geb>=0] <- rounded_predict #überschreibt n_og in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden

#table(DB_BE$n_og)

#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

n_og <- DB_BE$n_og
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, n_og, n_og_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_n_og_imputiert.csv')

