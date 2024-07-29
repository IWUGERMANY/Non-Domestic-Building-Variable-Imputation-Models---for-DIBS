# BE_qg13_imputation.R
# Julian Bischof
# 30.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# qg13: heating_emision_system
#-----------------------------------------

# -88 sind wie -7 zu imputieren. -88 reprsentieren TNZ durch in Version 1 des Fragebogens fehlerhafte Filterführung. 
# Da in DE davon auzugehen ist, dass ein GEG-Relevantes Gebäude immer eine Beheizung hat und damit auch ein Wärmeübergabesystem, 
# werden die -88 zu -7 und ebenso imputiert.


# HC am 29.März 2021
#
# „-88“ ist ein speziell vergebener Missing mit der Bedeutung: „TNZ wegen falscher Filterführung“. 
# Dieser Missing wurde unter anderem für qg13 vergeben.
#
# Aufgrund einer falsch programmierten Filterführung in Fragebogenversion 1 (aber nur dort, 
# d.h. nicht mehr in den folgenden Versionen) wurde nicht allen Befragten die zu qG13 korrespondierende Frage vorgelegt. 
# Betroffen sind Fälle, für die folgende  Bedingung erfüllt ist:
#   
#   (qg14 = 1) or ((qg14 = 2) and (qG15A_1 = -7) and (qG16 = 1)) or ((qG14 = -7) and (qG16 = 1)).




#............................................................................
#
# 0 Korrektur von qg13 auf Basis von anderen Angaben ####
#
#............................................................................

# t <- DB_BE$qg13[DB_BE$ebf>0 & DB_BE$qi1==3 & DB_BE$qg13==-8]
# table(t)

DB_BE$qg13[DB_BE$qg13==-88] <- -7 #  



#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, qg13!=-7 & hk_geb > 0 & bak > 0 & ebf > 0) # wenn qg13 = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

qg13 <- DB_BE_clean$qg13
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
ebf <- DB_BE_clean$ebf

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


    # Counting number of imputations in this variable and marking them for later analysis
    qg13_imputated <- DB_BE$qg13 == -7
    table <- table(qg13_imputated)
    qg13_n_imputated <- table[2]
    
    qg13_imputated[qg13_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 qg13 auf Basis multinominal logistc regression multinom() of the nnet package
# Alle weitern Zuweisungen zu Variablen, welche auf zur RLT gehören werden nur im Fall von qg13==3 imputiert! Damit konsistent.

# SETTING UP add levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$qg13 <- relevel(as.factor(DB_BE_clean$qg13), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

# Reg <- multinom(qg13 ~ as.factor(hk_geb) * as.factor(bak_grob) * ebf, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 7706.182 # NAs
# Reg <- multinom(qg13 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 7960.028 # NAs
# Reg <- multinom(qg13 ~ as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 9206.573 # NAs
# Reg <- multinom(qg13 ~ as.factor(hk_geb), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 8801.929 # NAs
# Reg <- multinom(qg13 ~ as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 8810.756  # NAs

Reg <- multinom(qg13 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 2000) # AIC: 7679.055 # NAs
# Reg <- multinom(qg13 ~ as.factor(bak) * ebf, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 8659.253 # 
# Reg <- multinom(qg13 ~ ebf, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 9773.734 # 


# # Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qg13__heating_emision_system.txt")
print("Also the regression equation contains NAs, it has been used for the imputation, as these combinations do not exist in the DataNWG BRE data and therfore do not affect the prediction")
print(summary(Reg))
sink()  # returns output to the console
# summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/qg13__heating_emision_system.rds")


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of qg13 by a dice the has the 
#       levels_qg13 (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_qg13 <- levels(DB_BE_clean$qg13)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$qg13[i]==-7){
    DB_BE$qg13[i] <- sample(levels_qg13, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4],predicted.probs[i,5],predicted.probs[i,6],predicted.probs[i,7]))
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

qg13 <- DB_BE$qg13
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, qg13, qg13_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_qg13_imputiert.csv')

