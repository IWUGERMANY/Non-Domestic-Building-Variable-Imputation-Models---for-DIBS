# BE_w_erz_art_imputation.R
# Julian Bischof
# 26.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# w_erz_art: Haupt-Wärmeerzeuger im Gebäude
#-----------------------------------------


#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, w_erz_art!=-7) # wenn w_erz_art = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

w_erz_art <- DB_BE_clean$w_erz_art
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 w_erz_art auf Basis multinominal logistc regression multinom() of the nnet package
# Alle weitern Zuweisungen zu Variablen, welche auf zur RLT gehören werden nur im Fall von w_erz_art==3 imputiert! Damit konsistent.

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$w_erz_art <- relevel(as.factor(DB_BE_clean$w_erz_art), ref = "10")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

# Reg <- multinom(w_erz_art ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # zu viele Gewichte
Reg <- multinom(w_erz_art ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 11687.21
# Reg <- multinom(w_erz_art ~ as.factor(hk_geb) : as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # zu viele Gewichte
# Reg <- multinom(w_erz_art ~ as.factor(hk_geb) : as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # zu viele Gewichte
# Reg <- multinom(w_erz_art ~ as.factor(hk_geb), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 12093.03  #
# Reg <- multinom(w_erz_art ~ as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 13336.83 #


# # Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/w_erz-art.txt")
print(summary(Reg))
sink()  # returns output to the console
# summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/w_erz-art.rds")


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of w_erz_art by a dice the has the 
#       levels_w_erz_art (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_w_erz_art <- levels(DB_BE_clean$w_erz_art)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$w_erz_art[i]==-7){
    DB_BE$w_erz_art[i] <- sample(levels_w_erz_art, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4],predicted.probs[i,5],predicted.probs[i,6],predicted.probs[i,7],predicted.probs[i,8],predicted.probs[i,9],predicted.probs[i,10],predicted.probs[i,11],predicted.probs[i,12],predicted.probs[i,13],predicted.probs[i,14],predicted.probs[i,15],predicted.probs[i,16],predicted.probs[i,17],predicted.probs[i,18],predicted.probs[i,19],predicted.probs[i,20]))
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

w_erz_art <- DB_BE$w_erz_art
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, w_erz_art)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_w_erz_art_imputiert.csv')

