# BE_d_fl_wueoa_imputation.R
# Julian Bischof
# 29.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# d_fl_wueoa: wärmeübertragende Fläche des oberen Gebäudeabschlusses
#-----------------------------------------

# unter anderem abhänig von fd_ant_kond. Da die drei -7 in d_fl_wueoa (hier) genau aus fd_ant_kond kommen, 
# kann auch gleich d_fl_wueoa in Abhängikeit von (siehe lm unten) imputiert werden


#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bak>=0 & dachform_be>=0 & d_fl_wueoa>=0) # bak_grob automatisch drin wenn bak
####

#eda.shape(DB_BE_clean$d_fl_wueoa)
d_fl_wueoa <- DB_BE_clean$d_fl_wueoa
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
dachform_be <- DB_BE_clean$dachform_be
geb_flaeche <- DB_BE_clean$geb_flaeche # Vollstänig vorliegend

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# min(bak)
# min(hk_geb)
# min(dachform_be)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem dachform_be = -7
DB_BE_unclean_d_fl_wueoa <- subset(DB_BE, hk_geb>=0 & bak>=0 & dachform_be>=0 & d_fl_wueoa==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    d_fl_wueoa_imputated <- DB_BE$d_fl_wueoa == -7
    table <- table(d_fl_wueoa_imputated)
    d_fl_wueoa_n_imputated <- table[2]
    
    d_fl_wueoa_imputated[d_fl_wueoa_imputated == TRUE] <- 1
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
# Reg <- lm(d_fl_wueoa ~ as.factor(bak) * as.factor(hk_geb) * as.factor(dachform_be), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.1817 # 
# Reg <- lm(d_fl_wueoa ~ as.factor(hk_geb) * as.factor(dachform_be), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.08478 # 
# Reg <- lm(d_fl_wueoa ~ as.factor(bak) * as.factor(dachform_be), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.04773 # 
# Reg <- lm(d_fl_wueoa ~ as.factor(bak) * as.factor(hk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.1431 # 

# Reg <- lm(d_fl_wueoa ~ as.factor(bak) * as.factor(hk_geb) * as.factor(dachform_be) * geb_flaeche, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.9999 # NAs
# Reg <- lm(d_fl_wueoa ~ as.factor(dachform_be) * geb_flaeche, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.9995 # 
# Reg <- lm(d_fl_wueoa ~ geb_flaeche, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.9993 # 
Reg <- lm(d_fl_wueoa ~ as.factor(hk_geb) * as.factor(dachform_be) * geb_flaeche, weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.9996 # 

# uk_geb is not working for predictor, as in the clean set are some categories missing... therefore the prediction does not work for these...

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/d_fl_wueoa.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/d_fl_wueoa.rds")

# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und q25_1 = -7
#     Berechnung der q25_1 in der bisher -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_d_fl_wueoa

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 4) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- min(rounded_predict[rounded_predict>0]) #korrigiere negative berechnete Werte zum kleinsten gültigen Wert aus predict
DB_BE$d_fl_wueoa[DB_BE$d_fl_wueoa==(-7) & DB_BE$hk_geb>=0 & DB_BE$bak>=0 & DB_BE$dachform_be>=0] <- rounded_predict #überschreibt q25_1 in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden

#table(DB_BE$d_fl_wueoa)
#min(rounded_predict[rounded_predict>0])


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

d_fl_wueoa <- DB_BE$d_fl_wueoa
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, d_fl_wueoa, d_fl_wueoa_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_d_fl_wueoa_imputiert.csv')

