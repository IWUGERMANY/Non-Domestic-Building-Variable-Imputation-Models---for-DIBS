# BE_qd8_imputation.R
# Julian Bischof
# 30.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# qd8: main shading system (sun protection)
#-----------------------------------------




#............................................................................
#
# 0 Korrektur von qd8 auf Basis von anderen Angaben ####
#
#............................................................................

# qd8 wird -8 (TNZ) wenn in qd1 für die überwiegende Fassadenart gleich 6 = "keine Fenster" angegeben wurde

DB_BE$qd8[DB_BE$qd8==-8] <- 6 #Daher wird nun auch qd8 für diese Fälle zu 6 = "Keine Sonnenschutzvorrichtung" gesetzt



#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, qd8!=-7 & hk_geb > 0 & bak > 0) # hk_geb und bak indizieren Vollständigkeit von uk_geb und bak_grob
####

qd8 <- DB_BE_clean$qd8
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
ebf <- DB_BE_clean$ebf

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


    # Counting number of imputations in this variable and marking them for later analysis
    qd8_imputated <- DB_BE$qd8 == -7
    table <- table(qd8_imputated)
    qd8_n_imputated <- table[2]
    
    qd8_imputated[qd8_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 qd8 auf Basis multinominal logistc regression multinom() of the nnet package

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$qd8 <- relevel(as.factor(DB_BE_clean$qd8), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

Reg <- multinom(qd8 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 11123 # 
# Reg <- multinom(qd8 ~ as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 12491 # 
# Reg <- multinom(qd8 ~ as.factor(hk_geb), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 11267 # 
# Reg <- multinom(qd8 ~ as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 12007  # 
# 
# Reg <- multinom(qd8 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 2000) # AIC: 10138 # NAs


# # Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qd8__main_shading_system.txt")
print(summary(Reg))
sink()  # returns output to the console
# summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qd8__main_shading_system.rds")


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of qd8 by a dice the has the 
#       levels_qd8 (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_qd8 <- levels(DB_BE_clean$qd8)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$qd8[i]==-7){
    DB_BE$qd8[i] <- sample(levels_qd8, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4],predicted.probs[i,5],predicted.probs[i,6]))
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

qd8 <- DB_BE$qd8
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, qd8, qd8_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_qd8_imputiert.csv')

