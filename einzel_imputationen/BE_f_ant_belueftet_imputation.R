# BE_f_ant_belueftet_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# f_ant_belueftet: Anteil Nutzungsfläche zentral mechanisch belüftet
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bak>=0 & uk_geb>=0 & bak_grob>=0 & qh1==3 & f_ant_belueftet>=0) # wenn qh1 = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

#eda.shape(DB_BE_clean$f_ant_belueftet)
f_ant_belueftet <- DB_BE_clean$f_ant_belueftet
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# min(bak)
# min(hk_geb)
# min(aw_konstr_1)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem aw_konstr_1 = -7
DB_BE_unclean_f_ant_belueftet <- subset(DB_BE, hk_geb>=0 & bak>=0 & uk_geb>=0 & bak_grob>=0 & qh1==3 & f_ant_belueftet==-7) # Wenn f_ant_belueftet = - 7 ist auch qh1 = -7

# Auf Basis des Würfels wird Gebäuden mit gewürfelter RLT eine Fläche zugewiesen (siehe Klammern oben)


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................

#----------------------------------------
# 2.1 Regressionsmodel 

#Regression linear, Faktoren werden einzeln berücksichtigt
# lm() ~ hk_geb, uk_geb, bak, bak_grob;  wenn qh1 = 3, sonst 0 # Alle f_ant_belueftet = -7 sind auch qh1 = -7!!! daher Regression ohne Einbezug qh1

# Reg <- lm(f_ant_belueftet ~ as.factor(bak) * as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.4461 # NAs
# Reg <- lm(f_ant_belueftet ~ as.factor(bak_grob) * as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)  # R-squared:  0.2568 # NAs
# Reg <- lm(f_ant_belueftet ~ as.factor(bak) * as.factor(uk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.6918 # NAs
# Reg <- lm(f_ant_belueftet ~ as.factor(bak) : as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.4461 # NAs
Reg <- lm(f_ant_belueftet ~ as.factor(bak) : as.factor(uk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.6918 # NAs not affecting prediction
# Reg <- lm(f_ant_belueftet ~ as.factor(bak) + as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.2621
# Reg <- lm(f_ant_belueftet ~ as.factor(bak_grob) : as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)  # R-squared:  0.2568 # NAs
# Reg <- lm(f_ant_belueftet ~ as.factor(bak_grob) + as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)  # R-squared:  0.2171 

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/f_ant_belueftet.txt")
print("Also the regression equation contains NAs, it has been used for the imputation, as these combinations do not exist in the DataNWG BRE data and therfore do not affect the prediction")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/f_ant_belueftet.rds")

# summary(Reg)
# 
# model_equation(Reg)


#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und f_ant_belueftet nach Würfeln immer noch = -7
#     Berechnung der f_ant_belueftet nach Würfeln immer noch = -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_f_ant_belueftet

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 0) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- min(rounded_predict[rounded_predict>0]) #korrigiere negative berechnete Werte zum kleinsten gültigen Wert aus predict
DB_BE$f_ant_belueftet[DB_BE$f_ant_belueftet==(-7) & qh1==3 & DB_BE$uk_geb>=0 & DB_BE$bak>=0] <- rounded_predict #überschreibt variable in der bisher -7 steht 
print("ACHTUNG: WENN qh1 keine RLT zu einer f_ant_belueftet zuwürfelt, kann hier DB_BE_unclean eine leere Menge sein, was zu fehlendes-Argument und Länge 0 Fehlern führt")


# table(DB_BE$f_ant_belueftet)
# min(rounded_predict[rounded_predict>0])
# table(data_for_predict)

#----------------------------------------
# Alle verbleibenden -7 zu 0, da diese in qh1 keine zentrale mechanische Belüftung zugewürfelt bekommen haben
DB_BE$f_ant_belueftet[DB_BE$f_ant_belueftet==(-7)] <- 0

#----------------------------------------
# Alle -8 zu 0, da hier keine zentrale mechanische Belüftung existiert

DB_BE$f_ant_belueftet[DB_BE$f_ant_belueftet==(-8)] <- 0


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

f_ant_belueftet <- DB_BE$f_ant_belueftet
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, f_ant_belueftet)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_f_ant_belueftet_imputiert.csv')

