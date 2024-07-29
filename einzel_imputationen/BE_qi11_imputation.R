# BE_qi11_imputation.R
# Julian Bischof
# 29.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# qi11: cooling_emision_system
#-----------------------------------------


# -8 bleibt -8, d.h. -8 bedeutet es wird nicht gekühlt--> daher kein Kühlsystem/Übertrager könnte auch Null gesetzt werden.
# AUßer: Für alle Fälle, welche f_ant_gekuehlt > 0 und qi1==3 und hier -8 sind, diese bekommen eine -7 zugewiesen und im nächsten
#        Schritt imputiert
# -7 nur mit qi11 auffüllen, wenn f_ant_gekuehlt > 0 und qi1==3



#............................................................................
#
# 0 Korrektur von qi11 auf Basis von anderen Angaben 
# f_ant_gekuehlt > 0 und qi1==3 und hier -8 sind, diese bekommen eine -7 zugewiesen####
#
#............................................................................

# t <- DB_BE$qi11[DB_BE$f_ant_gekuehlt>0 & DB_BE$qi1==3 & DB_BE$qi11==-8]
# table(t)

DB_BE$qi11[DB_BE$f_ant_gekuehlt>0 & DB_BE$qi1==3 & DB_BE$qi11==-8] <- -7 #  



#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, qi11!=-7 & hk_geb > 0 & bak > 0 & f_ant_gekuehlt > 0) # wenn qi11 = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

qi11 <- DB_BE_clean$qi11
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
f_ant_gekuehlt <- DB_BE_clean$f_ant_gekuehlt

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


    # Counting number of imputations in this variable and marking them for later analysis
    qi11_imputated <- DB_BE$qi11 == -7
    table <- table(qi11_imputated)
    qi11_n_imputated <- table[2]
    
    qi11_imputated[qi11_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 qi11 auf Basis multinominal logistc regression multinom() of the nnet package
# Alle weitern Zuweisungen zu Variablen, welche auf zur RLT gehören werden nur im Fall von qi11==3 imputiert! Damit konsistent.

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$qi11 <- relevel(as.factor(DB_BE_clean$qi11), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

# Reg <- multinom(qi11 ~ as.factor(hk_geb) * as.factor(bak_grob) * f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 743.3479 # NAs
# Reg <- multinom(qi11 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 704.6904 # NAs
# Reg <- multinom(qi11 ~ as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 692.6454 # NAs
# Reg <- multinom(qi11 ~ as.factor(hk_geb), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 686.6701 # NAs
# Reg <- multinom(qi11 ~ as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 641.8652  # NAs

# Reg <- multinom(qi11 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 2000) # AIC: 908.394 # NAs
Reg <- multinom(qi11 ~ as.factor(bak) * f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 603.4769 # NAs
# Reg <- multinom(qi11 ~ f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 688.1849 # NAs


# # Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qi11__cooling_emision_system.txt")
print("Also the regression equation contains NAs, it has been used for the imputation, as these combinations do not exist in the DataNWG BRE data and therfore do not affect the prediction")
print(summary(Reg))
sink()  # returns output to the console
#summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qi11__cooling_emision_system.rds")


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of qi11 by a dice the has the 
#       levels_qi11 (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_qi11 <- levels(DB_BE_clean$qi11)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$qi11[i]==-7){
    DB_BE$qi11[i] <- sample(levels_qi11, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4]))
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

qi11 <- DB_BE$qi11
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, qi11, qi11_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_qi11_imputiert.csv')

