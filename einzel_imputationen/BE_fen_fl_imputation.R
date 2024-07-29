# BE_fen_fl_imputation.R
# Julian Bischof
# 09.08.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# fen_fl: Fensterfläche des gesamten Gebäudes in m² aus der Hüllflächenberechnung 
# über die Geodaten und Breitenerhebung qD1 und qXD1.
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bak>=0 & nrf_2>=0 & geb_f_hoehe_mittel_iwu>=0 & fen_fl>=0 & fen_flant_1 >= 0)
#### 

hk_geb <- DB_BE_clean$hk_geb
fen_fl <- DB_BE_clean$fen_fl
geb_flaeche <- DB_BE_clean$geb_flaeche

aw_fl <- DB_BE_clean$aw_fl
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
geb_f_hoehe_mittel_iwu <- DB_BE_clean$geb_f_hoehe_mittel_iwu
nrf_2 <- DB_BE_clean$nrf_2
fen_flant_1 <- DB_BE_clean$fen_flant_1

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)



# min(fen_fl)
# min(hk_geb)
# min(geb_flaeche)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem gbf = -7
DB_BE_unclean_fen_fl <- subset(DB_BE, hk_geb>=0 & bak>=0 & nrf_2>=0 & geb_f_hoehe_mittel_iwu>=0 & fen_flant_1>=0 & fen_fl==-7)

    # Counting number of imputations in this variable and marking them for later analysis
    fen_fl_imputated <- DB_BE$fen_fl == -7
    table <- table(fen_fl_imputated)
    fen_fl_n_imputated <- table[2]
    
    fen_fl_imputated[fen_fl_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................

#----------------------------------------
# 2.1 Regressionsmodel fen_fl_clean und hk_geb_clean zu fen_fl_clean

#Regression linear, Faktoren werden einzeln berücksichtigt
# Reg <- lm(fen_fl ~ as.factor(uk_geb) * aw_fl, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.5302  # lm(y ~ x1*x2) is the same thing as lm(y ~ x1 + x2 + x1:x2)
# Reg <- lm(fen_fl ~ as.factor(uk_geb) : as.factor(bak) + geb_f_hoehe_mittel_iwu + nrf_2, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.7425 # NAs in Reg
# Reg <- lm(fen_fl ~ as.factor(uk_geb) : as.factor(bak) : fen_flant_1 + geb_f_hoehe_mittel_iwu + nrf_2, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.8043 # NAs in Reg
Reg <- lm(fen_fl ~ as.factor(hk_geb) : as.factor(bak_grob) : fen_flant_1 + geb_f_hoehe_mittel_iwu + nrf_2, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.6443 # 

# Reg <- lm(fen_fl ~ fen_flant_1 + geb_f_hoehe_mittel_iwu + nrf_2, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.5268 # 
# Reg <- lm(fen_fl ~ as.factor(uk_geb) : as.factor(bak_grob) + geb_f_hoehe_mittel_iwu + nrf_2, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.6259 # NAs in Reg


#TEST Reg vs. manuelle Mittelwerte nach hk_geb_clean
#Reg <- lm(fen_fl_clean ~ as.factor(hk_geb_clean)) # Alternative zur Mittelwertbestimmung in Abhänigigkeit von hk_geb_clean und zuweisung unten!!! Verfahren einfach über Regressionsimputationsweg!

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/fen_fl.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/fen_fl.rds")

# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn fen_fl und hk_geb vorhanden und fen_fl = -7
#     Berechnung der fen_fl in der bisher -7 steht für die Zeilen in denen fen_fl und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_fen_fl

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 4) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$fen_fl[DB_BE$fen_fl==(-7) & DB_BE$geb_flaeche>=0 & DB_BE$hk_geb>=0] <- rounded_predict #überschreibt fen_fl in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden

#table(DB_BE$fen_fl)

#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

fen_fl <- DB_BE$fen_fl
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, fen_fl, fen_fl_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_fen_fl_imputiert.csv')

