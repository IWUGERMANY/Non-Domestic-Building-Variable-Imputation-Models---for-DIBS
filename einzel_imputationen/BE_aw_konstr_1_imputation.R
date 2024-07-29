# BE_aw_konstr_1_imputation.R
# Julian Bischof
# 19.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# aw_konstr_1: Fassadenbauweise des Gebäudes -> 1 massiv, 2 leicht, 3 Fassadensystem
aw_konstr_1 <- DB_BE$aw_konstr_1
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bak>=0 & n_og>=0 & aw_konstr_1>=0)
####

hk_geb <- DB_BE_clean$hk_geb
uk_geb <- DB_BE_clean$uk_geb
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob
aw_konstr_1 <- DB_BE_clean$aw_konstr_1
n_og <- DB_BE_clean$n_og


N_Clean <- nrow(DB_BE_clean)
Sum_HRF_BE_clean <- sum(HRF)
HRF <- DB_BE_clean$HRF

# min(bak)
# min(hk_geb)
# min(aw_konstr_1)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem aw_konstr_1 = -7
DB_BE_unclean_aw_konstr_1 <- subset(DB_BE, hk_geb>=0 & bak>=0 & n_og>=0 & aw_konstr_1==-7)


    # Counting number of imputations in this variable and marking them for later analysis
    aw_konstr_1_imputated <- DB_BE$aw_konstr_1 == -7
    table <- table(aw_konstr_1_imputated)
    aw_konstr_1_n_imputated <- table[2]
    
    aw_konstr_1_imputated[aw_konstr_1_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................


#----------------------------------------
# 2.3 aw_konstr_1 auf Basis multinominal logistc regression multinom() of the nnet package

# SETTING UP ad levles to factors
#####################################################
# Setting the basline for the multinom() 
DB_BE_clean$aw_konstr_1 <- relevel(as.factor(DB_BE_clean$aw_konstr_1), ref = "1")


# FIRST Prepare the glm model
#####################################################
# Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
require(nnet)

# Training the multinomial model
 
# Reg <- multinom(aw_konstr_1 ~ as.factor(hk_geb) * n_og, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 2551.312
# Reg <- multinom(aw_konstr_1 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean, maxit = 1000, MaxNWts = 1000)*N_Clean) # AIC: 2345.004 # NAs
Reg <- multinom(aw_konstr_1 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 2333.879
# Reg <- multinom(aw_konstr_1 ~ as.factor(hk_geb) : as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean, maxit = 1000, MaxNWts = 1000)*N_Clean) # AIC: 2344.881 # NAs
# Reg <- multinom(aw_konstr_1 ~ as.factor(hk_geb) * as.factor(bak) * n_og, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 2512.148 # NAs
# Reg <- multinom(aw_konstr_1 ~ as.factor(hk_geb) * as.factor(bak_grob) * n_og, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean, maxit = 1000, MaxNWts = 1000)*N_Clean) # AIC: 2333.16 # one predictor more than chosen option with very close AIC

# Checking the model
sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/aw_konstr_1.txt")
print(summary(Reg))
sink()  # returns output to the console
#summary(Reg)

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/aw_konstr_1.rds")

# mlogit() Multinomial logistic regression
# multinom function of the package nnet https://stats.stackexchange.com/questions/175782/how-to-perform-a-logistic-regression-for-more-than-2-response-classes-in-r


# SECOND Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = probs)!
#predicted.probs <- predict(Reg,new.data=DB_BE, type="probs")

predicted.probs <- predict(Reg,newdata=DB_BE, type="probs")

# THIRD for each row overwrite the -7 of aw_konstr_1 by a dice the has the 
#       levels_aw_konstr_1 (Ausprägungen (1, 2, 3)) mit der 
#       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
#####################################################
levels_aw_konstr_1 <- levels(DB_BE_clean$aw_konstr_1)
rows_DB_BE <- nrow(DB_BE)

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$aw_konstr_1[i]==-7){
    #DB_BE$aw_konstr_1[i] <- sample(levels_aw_konstr_1, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3]))
    DB_BE$aw_konstr_1[i] <- sample(levels_aw_konstr_1, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3], predicted.probs[i,4],predicted.probs[i,5],predicted.probs[i,6], predicted.probs[i,7],predicted.probs[i,8],predicted.probs[i,9], predicted.probs[i,10],predicted.probs[i,11],predicted.probs[i,12], predicted.probs[i,13],predicted.probs[i,14],predicted.probs[i,15],predicted.probs[i,16]))
  }
}

#table(DB_BE$aw_konstr_1)


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

aw_konstr_1 <- DB_BE$aw_konstr_1
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, aw_konstr_1, aw_konstr_1_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_aw_konstr_1_imputiert.csv')

