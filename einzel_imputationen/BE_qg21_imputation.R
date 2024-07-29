# BE_qg21_imputation.R
# Julian Bischof
# 05.05.2022
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# qg21: DHW generation
#-----------------------------------------




#............................................................................
#
# 0 Korrektur von qg21 auf Basis von anderen Angaben ####
#
#............................................................................


#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, qg21!=-7 & hk_geb > 0 & bak > 0) # hk_geb und bak indizieren Vollständigkeit von uk_geb und bak_grob
####

qg21 <- DB_BE_clean$qg21
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
ebf <- DB_BE_clean$ebf

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


    # Counting number of imputations in this variable and marking them for later analysis
    qg21_imputated <- DB_BE$qg21 == -7
    table <- table(qg21_imputated)
    qg21_n_imputated <- table[2]
    
    qg21_imputated[qg21_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 qg21 auf Basis multinominal logistc regression multinom() of the nnet package

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$qg21 <- relevel(as.factor(DB_BE_clean$qg21), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

Reg <- multinom(qg21 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 9989.386  # 
# Reg <- multinom(qg21 ~ as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 11057.98 # 
# Reg <- multinom(qg21 ~ as.factor(hk_geb), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 10485.01  # 
# Reg <- multinom(qg21 ~ as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 10884.91   # 
# 
# Reg <- multinom(qg21 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 2000) # AIC: 9352.209 # NAs


# # Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qg21__DWH_generation.txt")
print(summary(Reg))
sink()  # returns output to the console
# summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qg21__DWH_generation.rds")



# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of qg21 by a dice the has the 
#       levels_qg21 (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_qg21 <- levels(DB_BE_clean$qg21)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$qg21[i]==-7){
    DB_BE$qg21[i] <- sample(levels_qg21, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4],predicted.probs[i,5]))
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

qg21 <- DB_BE$qg21
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, qg21, qg21_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_qg21_imputiert.csv')

