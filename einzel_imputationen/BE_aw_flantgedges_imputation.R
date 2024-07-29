# BE_aw_flantgedges_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# aw_flantgedges: Außenwandflächeanteil gedämmt gesammt
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & bak_grob>=0 & aw_flantgedges>=0)
####

aw_flantgedges <- DB_BE_clean$aw_flantgedges
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
DB_BE_unclean_aw_flantgedges <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & bak_grob>=0 & aw_flantgedges==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    aw_flantgedges_imputated <- DB_BE$aw_flantgedges == -7
    table <- table(aw_flantgedges_imputated)
    aw_flantgedges_n_imputated <- table[2]
    
    aw_flantgedges_imputated[aw_flantgedges_imputated == TRUE] <- 1
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

# Reg <- lm(aw_flantgedges ~ as.factor(hk_geb) * as.factor(bak_grob), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)   # R-squared:  0.1318
Reg <- lm(aw_flantgedges ~ as.factor(hk_geb) * as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.3271 # NAs not affecting prediction!
# Reg <- lm(aw_flantgedges ~ as.factor(uk_geb) * as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.5505 # NAs requiered for predict


# uk_geb is not working for predictor, as in the clean set are some categories missing... therefore the prediction does not work for these...

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/aw_flantgedges.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/aw_flantgedges.rds")

# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und q25_1 = -7
#     Berechnung der q25_1 in der bisher -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_aw_flantgedges

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 4) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$aw_flantgedges[DB_BE$aw_flantgedges==(-7) & DB_BE$hk_geb>=0 & DB_BE$bak>=0] <- rounded_predict #überschreibt q25_1 in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden

#table(DB_BE$aw_flantgedges)


#----------------------------------------
# Alle -8 zu 0, da hier nicht gedämmt wurde

DB_BE$aw_flantgedges[DB_BE$aw_flantgedges==(-8)] <- 0


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

aw_flantgedges <- DB_BE$aw_flantgedges
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, aw_flantgedges, aw_flantgedges_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_aw_flantgedges_imputiert.csv')


