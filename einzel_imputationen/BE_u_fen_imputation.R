# BE_u_fen_imputation.R
# Julian Bischof
# 09.08.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# u_fen: U-Wert der Fenster
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bak>=0 & glasart_1>=0 & u_fen>=0)
####

u_fen <- DB_BE_clean$u_fen
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
glasart_1 <- DB_BE_clean$glasart_1

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)

# min(bak)
# min(hk_geb)
# min(fen_glasart_1)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem fen_glasart_1 = -7
DB_BE_unclean_u_fen <- subset(DB_BE, hk_geb>=0 & bak>=0 & glasart_1>=0 & u_fen==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    u_fen_imputated <- DB_BE$u_fen == -7
    table <- table(u_fen_imputated)
    u_fen_n_imputated <- table[2]
    
    u_fen_imputated[u_fen_imputated == TRUE] <- 1
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


# Reg <- lm(u_fen ~ as.factor(hk_geb) * as.factor(glasart_1), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)                  # R-squared:  0.5061 # NAs
# Reg <- lm(u_fen ~ as.factor(hk_geb) * as.factor(glasart_1) * as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.7183 # NAs
# Reg <- lm(u_fen ~ as.factor(hk_geb) * as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)                 # R-squared:  0.4286 # NAs
# Reg <- lm(u_fen ~ as.factor(hk_geb) : as.factor(glasart_1) : as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.7183 # NAs
Reg <- lm(u_fen ~ as.factor(hk_geb) + as.factor(glasart_1) + as.factor(bak), weights = (HRF/Sum_HRF_BE_clean)*N_Clean) # R-squared:  0.6467

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/u_fen.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/u_fen.rds")

model_equation(Reg)

#----------------------------------------
# 2.2 Regressionsimputation wenn bgf und hk_geb vorhanden und q25_1 = -7
#     Berechnung der q25_1 in der bisher -7 steht für die Zeilen in denen bgf und hk_geb vorhanden, also >=0
data_for_predict <- DB_BE_unclean_u_fen

rounded_predict <- round(predict.lm(Reg, data_for_predict, se.fit = FALSE), digits = 4) #berechne und runde fehlender Werte
rounded_predict[rounded_predict<0] <- 0 #korrigiere negative berechnete Werte zu 0
DB_BE$u_fen[DB_BE$u_fen==(-7) & DB_BE$hk_geb>=0 & DB_BE$bak>=0 & DB_BE$glasart_1>=0] <- rounded_predict #überschreibt q25_1 in der bisher -7 steht für die Zeilen in denen gbr und hk_geb vorhanden

#table(DB_BE$u_fen)

#Sicherheitshalber ersetzen von -8 durch u_aw (imputiert)

DB_BE$u_fen[DB_BE$u_fen==(-8)] <- DB_BE$u_aw[DB_BE$u_fen==(-8)]

#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

u_fen <- DB_BE$u_fen
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, u_fen, u_fen_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_u_fen_imputiert.csv')
