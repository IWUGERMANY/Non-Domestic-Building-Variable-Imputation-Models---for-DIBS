
# BE_q25_1_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# q25_1 ist die Maximale Personenbelegung
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bgf>=0 & q25_1>=0)
####

# hk_geb_clean <- DB_BE_clean$hk_geb
# bgf_clean <- DB_BE_clean$bgf
# q25_1_clean <- DB_BE_clean$q25_1
#
# # min(bgf_clean)
# # min(hk_geb_clean)
# # min(q25_1_clean)

hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bgf <- DB_BE_clean$bgf
q25_1 <- DB_BE_clean$q25_1
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)
bak_grob <- DB_BE_clean$bak_grob
bak <- DB_BE_clean$bak
N_Clean <- nrow(DB_BE_clean)

# min(bgf)
# min(hk_geb)
# min(q25_1)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem q25_1 = -7
DB_BE_unclean_q25_1 <- subset(DB_BE, uk_geb>=0 & bak>=0 & bgf>=0 & q25_1==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    q25_1_imputated <- DB_BE$q25_1 == -7
    table <- table(q25_1_imputated)
    q25_1_n_imputated <- table[2]
    
    q25_1_imputated[q25_1_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................

#----------------------------------------
# 2.1 Regressionsmodel bgf_clean und hk_geb_clean zu q25_1_clean

#Regression linear, Faktoren werden einzeln berücksichtigt
# Reg <- lm(q25_1_clean ~ as.factor(hk_geb_clean) + bgf_clean)
# Reg <- lm(q25_1 ~ as.factor(hk_geb) : bgf, weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.1291 # faktor spezifische Personenanzahl je hk_geb * bgf
# Reg <- lm(q25_1 ~ as.factor(hk_geb) + bgf, weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.08747
# Reg <- lm(q25_1 ~ as.factor(uk_geb) : bgf, weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.1804 # NA in REG! -> Faktor 'as.factor(uk_geb)' hat neue Stufen 6.07
# Reg <- lm(q25_1 ~ as.factor(uk_geb) : as.factor(bak_grob) : bgf, weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.1964 # NA in REG! -> Faktor 'as.factor(uk_geb)' hat neue Stufen 6.07
# Reg <- lm(q25_1 ~ as.factor(uk_geb) : as.factor(bak) : bgf, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.2287 # NA in REG! -> Faktor 'as.factor(uk_geb)' hat neue Stufen 6.07
Reg <- lm(q25_1 ~ as.factor(hk_geb) : as.factor(bak_grob) : bgf, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.1444

#TEST Reg vs. manuelle Mittelwerte nach hk_geb_clean
#Reg <- lm(q25_1_clean ~ as.factor(hk_geb_clean)) # Alternative zur Mittelwertbestimmung in Abhänigigkeit von hk_geb_clean und zuweisung unten!!! Verfahren einfach über Regressionsimputationsweg!

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/q25_1__max_occupancy.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/q25_1__max_occupancy.rds")

# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und q25_1 = -7
#     Berechnung der q25_1 in der bisher -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_q25_1

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 0) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$q25_1[DB_BE$q25_1==(-7) & DB_BE$bgf>=0 & DB_BE$hk_geb>=0] <- rounded_predict #überschreibt q25_1 in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

q25_1 <- DB_BE$q25_1
bgf <- DB_BE$bgf
uk_geb <- DB_BE$uk_geb
hk_geb <- DB_BE$hk_geb
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, q25_1, q25_1_imputated, bgf, uk_geb, hk_geb)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_q25_1_imputiert.csv')

# write it as xlsx
library("writexl")
write_xlsx(Imputierte_Variable,"DB_BE_q25_1_imputiert.xlsx")

