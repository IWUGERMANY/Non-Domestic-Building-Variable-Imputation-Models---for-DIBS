# BE_w_erz_art_et_imputation.R
# Julian Bischof
# 28.07.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# w_erz_art_et: heating_supply_system 
#-----------------------------------------



# # Erstellung fehlender w_erz_art_et auf Basis von w_erz_art und energietraeger
# 
# # compute w_erz_art_et = w_erz_art * 10 + energietraeger (Kodierungsverkettung!)
# DB_BE$w_erz_art_et[DB_BE$w_erz_art_et==-7] <- as.factor((as.numeric(DB_BE$w_erz_art[DB_BE$w_erz_art_et==-7]) * 10) + as.numeric(DB_BE$energietraeger[DB_BE$w_erz_art_et==-7]))
# # DB_BE$w_erz_art_et <- as.factor((as.numeric(DB_BE$w_erz_art) * 10) + as.numeric(DB_BE$energietraeger))
# 


#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, w_erz_art_et!=-7) # wenn energietraeger = 3 ist überhaut nur eine zentrale Belüftung vorhanden
####

w_erz_art_et <- DB_BE_clean$w_erz_art_et
hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob

N_Clean <- nrow(DB_BE_clean)
HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)


    # Counting number of imputations in this variable and marking them for later analysis
    w_erz_art_et_imputated <- DB_BE$w_erz_art_et == -7
    table <- table(w_erz_art_et_imputated)
    w_erz_art_et_n_imputated <- table[2]
    
    w_erz_art_et_imputated[w_erz_art_et_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



#----------------------------------------
# 2.3 w_erz_art_et auf Basis multinominal logistc regression multinom() of the nnet package
# Alle weitern Zuweisungen zu Variablen, welche auf zur RLT gehören werden nur im Fall von w_erz_art_et==3 imputiert! Damit konsistent.

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$w_erz_art_et <- relevel(as.factor(DB_BE_clean$w_erz_art_et), ref = "108")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model

# Reg <- multinom(w_erz_art_et ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # zu viele Gewichte
# Reg <- multinom(w_erz_art_et ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # zu viele Gewichte
# Reg <- multinom(w_erz_art_et ~ as.factor(hk_geb) : as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # zu viele Gewichte
# Reg <- multinom(w_erz_art_et ~ as.factor(hk_geb) : as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # zu viele Gewichte
Reg <- multinom(w_erz_art_et ~ as.factor(hk_geb), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 18090.63  #
# Reg <- multinom(w_erz_art_et ~ as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 19644.87 # NAs


# # Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/w_erz_art_et.txt")
print("Also the regression equation contains NAs, it has been used for the imputation, as these combinations do not exist in the DataNWG BRE data and therfore do not affect the prediction")
print(summary(Reg))
sink()  # returns output to the console
#summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/w_erz_art_et.rds")


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
# predicted.probs <- predict(Reg,new.data=DB_BE, type="probs") # new.data berechnet nur die Wahrscheinlichkeiten für den Eingangsdatensatz
predicted.probs <- predict(Reg,newdata=DB_BE, type="probs") # newdata berechnet alle Wahrscheinlichkeiten des neuen Datensatzes


# THIRD for each row overwrite the -7 of energietraeger by a dice the has the 
#       levels_energietraeger (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_w_erz_art_et <- levels(DB_BE_clean$w_erz_art_et)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$w_erz_art_et[i]==-7){
    DB_BE$w_erz_art_et[i] <- sample(levels_w_erz_art_et, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4],predicted.probs[i,5],predicted.probs[i,6],predicted.probs[i,7],predicted.probs[i,8],predicted.probs[i,9],predicted.probs[i,10],predicted.probs[i,11],predicted.probs[i,12],predicted.probs[i,13],predicted.probs[i,14],predicted.probs[i,15],predicted.probs[i,16],predicted.probs[i,17],predicted.probs[i,18],predicted.probs[i,19],predicted.probs[i,20],predicted.probs[i,21],predicted.probs[i,22],predicted.probs[i,23],predicted.probs[i,24],predicted.probs[i,25],predicted.probs[i,26],predicted.probs[i,27],predicted.probs[i,28],predicted.probs[i,29],predicted.probs[i,30],predicted.probs[i,31],predicted.probs[i,32],predicted.probs[i,33],predicted.probs[i,34],predicted.probs[i,35],predicted.probs[i,36],predicted.probs[i,37],predicted.probs[i,38],predicted.probs[i,39],predicted.probs[i,40],predicted.probs[i,41],predicted.probs[i,42],predicted.probs[i,43],predicted.probs[i,44],predicted.probs[i,45],predicted.probs[i,46]))
    #,predicted.probs[i,47],predicted.probs[i,48],predicted.probs[i,49],predicted.probs[i,50],predicted.probs[i,51],predicted.probs[i,52],predicted.probs[i,53],predicted.probs[i,54],predicted.probs[i,55],predicted.probs[i,56],predicted.probs[i,57],predicted.probs[i,58],predicted.probs[i,59],predicted.probs[i,60],predicted.probs[i,61],predicted.probs[i,62],predicted.probs[i,63],predicted.probs[i,64],predicted.probs[i,65],predicted.probs[i,66],predicted.probs[i,67],predicted.probs[i,68],predicted.probs[i,69],predicted.probs[i,70],predicted.probs[i,71],predicted.probs[i,72],predicted.probs[i,73],predicted.probs[i,74],predicted.probs[i,75],predicted.probs[i,76],predicted.probs[i,77],predicted.probs[i,78],predicted.probs[i,79],predicted.probs[i,80],predicted.probs[i,81]
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

w_erz_art_et <- DB_BE$w_erz_art_et
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, w_erz_art_et, w_erz_art_et_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_w_erz_art_et_imputiert.csv')

