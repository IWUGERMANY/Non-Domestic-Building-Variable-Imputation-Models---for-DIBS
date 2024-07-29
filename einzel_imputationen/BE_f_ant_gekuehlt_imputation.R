# BE_f_ant_gekuehlt_imputation.R
# Julian Bischof
# 16.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# f_ant_gekuehlt: Anteil Nutzungsfläche zentral mechanisch gekühlt
#-----------------------------------------


# -7 kommt nur vor, wenn auch eine zentrale Kältenanlage als vorhanden angegeben wurde 
# d.h. lm() kann aus der teilmenge von Gebäuden mit qI1 = 3 (zentrale Kälteanlage vorhanden) ermittelt werden
# -8 = 0; da dann keine zentrale Kälteanlage -> 0% mechanisch zentral gekühlte Fläche
# lm() ~ hk_geb, uk_geb, bak, bak_grob aus teilmenge qi1 = 3


#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bak>=0 & uk_geb>=0 & bak_grob>=0 & qi1==3 & f_ant_gekuehlt>=0) # wenn qi1 = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

#eda.shape(DB_BE_clean$f_ant_gekuehlt)
f_ant_gekuehlt <- DB_BE_clean$f_ant_gekuehlt
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
DB_BE_unclean_f_ant_gekuehlt <- subset(DB_BE, hk_geb>=0 & bak>=0 & uk_geb>=0 & bak_grob>=0 & qi1==3 & f_ant_gekuehlt==-7) # Wenn f_ant_gekuehlt = - 7 ist auch qi1 = -7


    # Counting number of imputations in this variable and marking them for later analysis
    f_ant_gekuehlt_imputated <- DB_BE$f_ant_gekuehlt == -7
    table <- table(f_ant_gekuehlt_imputated)
    f_ant_gekuehlt_n_imputated <- table[2]
    
    f_ant_gekuehlt_imputated[f_ant_gekuehlt_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


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
# lm() ~ hk_geb, uk_geb, bak, bak_grob;  wenn qi1 = 3, sonst 0 # Alle f_ant_gekuehlt = -7 sind auch qi1 = -7!!! daher Regression ohne Einbezug qi1

# Reg <- lm(f_ant_gekuehlt ~ as.factor(bak) * as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.5198 # NAs
# Reg <- lm(f_ant_gekuehlt ~ as.factor(bak_grob) * as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)  # R-squared:  0.2639 # NAs
# Reg <- lm(f_ant_gekuehlt ~ as.factor(bak) * as.factor(uk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.7435 # NAs in predict possbile
Reg <- lm(f_ant_gekuehlt ~ as.factor(bak) : as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.5198 # NAs
# Reg <- lm(f_ant_gekuehlt ~ as.factor(bak) : as.factor(uk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.7435 # NAs in predict possbile
# Reg <- lm(f_ant_gekuehlt ~ as.factor(bak) + as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)       # R-squared:  0.1639
# Reg <- lm(f_ant_gekuehlt ~ as.factor(bak_grob) : as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)  # R-squared:  0.2639 #NAs
# Reg <- lm(f_ant_gekuehlt ~ as.factor(bak_grob) + as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)  # R-squared:  0.0613


sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/f_ant_gekuehlt.txt")
print("Also the regression equation contains NAs, it has been used for the imputation, as these combinations do not exist in the DataNWG BRE data and therfore do not affect the prediction")
print("Damit die Zuweisung der Kälteanlagen passt, muss hier nur im Fall f_ant_gekuehlt==-7 & qi1==3 imputiert werden")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/f_ant_gekuehlt.rds")

# summary(Reg)
# 
# model_equation(Reg)



#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und f_ant_gekuehlt nach Würfeln immer noch = -7
#     Berechnung der f_ant_gekuehlt nach Würfeln immer noch = -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_f_ant_gekuehlt

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 0) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- min(rounded_predict[rounded_predict>0]) #korrigiere negative berechnete Werte zum kleinsten gültigen Wert aus predict
DB_BE$f_ant_gekuehlt[DB_BE$f_ant_gekuehlt==(-7) & qi1==3 & DB_BE$uk_geb>=0 & DB_BE$bak>=0] <- rounded_predict #überschreibt variable in der bisher -7 steht 
print("ACHTUNG: WENN qh1 keine RLT zu einer f_ant_gekuehlt zuwürfelt, kann hier DB_BE_unclean eine leere Menge sein, was zu fehlendes-Argument und Länge 0 Fehlern führt")
# table(DB_BE$f_ant_gekuehlt)
# min(rounded_predict[rounded_predict>0])
# table(data_for_predict)

#----------------------------------------
# Alle verbleibenden -7 zu 0, da diese in qh1 keine zentrale mechanische Belüftung zugewürfelt bekommen haben
DB_BE$f_ant_belueftet[DB_BE$f_ant_belueftet==(-7)] <- 0


#----------------------------------------
# Alle -8 zu 0, da hier keine zentrale mechanische Belüftung existiert

DB_BE$f_ant_gekuehlt[DB_BE$f_ant_gekuehlt==(-8)] <- 0


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

f_ant_gekuehlt <- DB_BE$f_ant_gekuehlt
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, f_ant_gekuehlt, f_ant_gekuehlt_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_f_ant_gekuehlt_imputiert.csv')

