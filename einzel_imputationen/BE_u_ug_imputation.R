
# BE_u_ug_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# u_ug ist der U-Wert des unteren Gebäudeabschlusses
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & bak_grob>=0 & u_ug>=0)
####

# hk_geb_clean <- DB_BE_clean$hk_geb
# bgf_clean <- DB_BE_clean$bgf
# u_ug_clean <- DB_BE_clean$u_ug
#
# # min(bgf_clean)
# # min(hk_geb_clean)
# # min(u_ug_clean)

hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
u_ug <- DB_BE_clean$u_ug
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)
bak_grob <- DB_BE_clean$bak_grob
bak <- DB_BE_clean$bak
N_Clean <- nrow(DB_BE_clean)



#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem u_ug = -7
DB_BE_unclean_u_ug <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & bak_grob>=0 & u_ug==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    u_ug_imputated <- DB_BE$u_ug == -7
    table <- table(u_ug_imputated)
    u_ug_n_imputated <- table[2]
    
    u_ug_imputated[u_ug_imputated == TRUE] <- 1
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
# Reg <- lm(u_ug ~ as.factor(uk_geb) : as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.8336 # NAs
# Reg <- lm(u_ug ~ as.factor(uk_geb) : as.factor(bak_grob), weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.6457 # NAs
Reg <- lm(u_ug ~ as.factor(hk_geb) + as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)) # R-squared:  0.6444
# Reg <- lm(u_ug ~ as.factor(hk_geb) : as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.7481 # NAs
# Reg <- lm(u_ug ~ as.factor(hk_geb) : as.factor(bak_grob), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.5722 # NAs

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/u_ug.txt")
print("Also the regression equation contains NAs, it has been used for the imputation, as these combinations do not exist in the DataNWG BRE data and therfore do not affect the prediction")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/u_ug.rds")

# summary(Reg)
# 
# model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation 
#     Berechnung der u_ug in der bisher -7 steht für die Zeilen in denen prediktoren vorhanden, also >=0
data_for_predict <- DB_BE_unclean_u_ug

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 4) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$u_ug[DB_BE$u_ug==(-7) & DB_BE$bak>=0 & DB_BE$hk_geb>=0] <- rounded_predict #überschreibt u_ug in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden



#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

u_ug <- DB_BE$u_ug
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, u_ug, u_ug_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_u_ug_imputiert.csv')

