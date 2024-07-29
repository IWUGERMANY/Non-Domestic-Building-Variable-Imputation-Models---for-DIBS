# BE_k_erz_art_rk_imputation.R
# Julian Bischof
# 29.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# k_erz_art_rk: cooling_supply_system
#-----------------------------------------


# -8 bleibt -8, d.h. -8 bedeutet es wird nicht gekühlt--> daher kein Kühlsystem könnte auch Null gesetzt werden.
# AUßer: Für alle Fälle, welche f_ant_gekuehlt > 0 und qi1==3 und hier -8 sind, diese bekommen eine -7 zugewiesen und im nächsten
#        Schritt imputiert
# -7 nur mit k_erz_art_rk auffüllen, wenn f_ant_gekuehlt > 0 und qi1==3



#............................................................................
#
# 0 Korrektur von k_erz_art_rk auf Basis von anderen Angaben 
# f_ant_gekuehlt > 0 und qi1==3 und hier -8 sind, diese bekommen eine -7 zugewiesen####
#
#............................................................................

# t <- DB_BE$k_erz_art_rk[DB_BE$f_ant_gekuehlt>0 & DB_BE$qi1==3 & DB_BE$k_erz_art_rk==-8]
# table(t)

DB_BE$k_erz_art_rk[DB_BE$f_ant_gekuehlt>0 & DB_BE$qi1==3 & DB_BE$k_erz_art_rk==-8] <- -7 #  



#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, k_erz_art_rk!=-7 & hk_geb > 0 & bak > 0 & f_ant_gekuehlt > 0) # wenn k_erz_art_rk = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

k_erz_art_rk <- DB_BE_clean$k_erz_art_rk
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
f_ant_gekuehlt <- DB_BE_clean$f_ant_gekuehlt

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


    # Counting number of imputations in this variable and marking them for later analysis
    k_erz_art_rk_imputated <- DB_BE$k_erz_art_rk == -7
    table <- table(k_erz_art_rk_imputated)
    k_erz_art_rk_n_imputated <- table[2]
    
    k_erz_art_rk_imputated[k_erz_art_rk_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 k_erz_art_rk auf Basis multinominal logistc regression multinom() of the nnet package
# Alle weitern Zuweisungen zu Variablen, welche auf zur RLT gehören werden nur im Fall von k_erz_art_rk==3 imputiert! Damit konsistent.

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$k_erz_art_rk <- relevel(as.factor(DB_BE_clean$k_erz_art_rk), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

# Reg <- multinom(k_erz_art_rk ~ as.factor(hk_geb) * as.factor(bak_grob) * f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 1604.069 # NAs
# Reg <- multinom(k_erz_art_rk ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 1413.811 # NAs
# Reg <- multinom(k_erz_art_rk ~ as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 1313.815 # NAs
# Reg <- multinom(k_erz_art_rk ~ as.factor(hk_geb), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 1368.291 # NAs
Reg <- multinom(k_erz_art_rk ~ as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 1223.488 # NAs

# Reg <- multinom(k_erz_art_rk ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 2000) # AIC: 2063.718 # NAs
# Reg <- multinom(k_erz_art_rk ~ as.factor(bak) * f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 1226.759 # NAs


# # Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/k_erz_art_rk.txt")
print("Also the regression equation contains NAs, it has been used for the imputation, as these combinations do not exist in the DataNWG BRE data and therfore do not affect the prediction")
print(summary(Reg))
sink()  # returns output to the console
#summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/k_erz_art_rk.rds")


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of k_erz_art_rk by a dice the has the 
#       levels_k_erz_art_rk (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_k_erz_art_rk <- levels(DB_BE_clean$k_erz_art_rk)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$k_erz_art_rk[i]==-7){
    DB_BE$k_erz_art_rk[i] <- sample(levels_k_erz_art_rk, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4],predicted.probs[i,5],predicted.probs[i,6],predicted.probs[i,7],predicted.probs[i,8],predicted.probs[i,9]))
  }
}



#
#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

k_erz_art_rk <- DB_BE$k_erz_art_rk
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, k_erz_art_rk, k_erz_art_rk_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_k_erz_art_rk_imputiert.csv')

