# BE_dachform_be_imputation.R
# Julian Bischof
# 26.07.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# dachform_be: Dachform/typ des Gebäudes
#-----------------------------------------


#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#----------------------------------------
# 1.1 Sauberer Datensatz für Regressionsmodel

DB_BE_clean <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & dachform_be>=0)
####

hk_geb <- DB_BE_clean$hk_geb
dachform_be <- DB_BE_clean$dachform_be
geb_flaeche <- DB_BE_clean$geb_flaeche
bak <- DB_BE_clean$bak
bak_grob <- DB_BE_clean$bak_grob

uk_geb <- DB_BE_clean$uk_geb

HRF <- DB_BE_clean$HRF
Sum_HRF_BE_clean <- sum(HRF)
N_Clean <- nrow(DB_BE_clean)


# min(dachform_be)
# min(hk_geb)
# min(geb_flaeche)


#----------------------------------------
# 1.2 Datensatz aus zu imputierenden Zeilen

#Generieren von Un-Clean Subset d.h. das Subset in welchem gbf = -7
DB_BE_unclean_dachform_be <- subset(DB_BE, hk_geb>=0 & uk_geb>=0 & bak>=0 & dachform_be==-7)

    # Counting number of imputations in this variable and marking them for later analysis
    dachform_be_imputated <- DB_BE$dachform_be == -7
    table <- table(dachform_be_imputated)
    dachform_be_n_imputated <- table[2]
    
    dachform_be_imputated[dachform_be_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................



# FIRST in Steildach (1) und Flachdach (0)
####################################################
dachform_be[dachform_be == 2] <- 0 # Codierung von Flachdach (2) wird zu 0

dachform_be <- factor(dachform_be,
                      levels=c(0,1),
                      labels=c("Flachdach","Steildach"))
table(dachform_be)


#----------------------------------------
# 2.1 Regressionsmodel dachform_be_clean und hk_geb_clean zu dachform_be_clean

# Comparing optional models: "Once you have created several possible models, you can use AIC to compare them. 
# Lower AIC scores are better, and AIC penalizes models that use more parameters. 
# So if two models explain the same amount of variation, the one with fewer parameters will 
# have a lower AIC score and will be the better-fit model. 
# https://www.scribbr.com/statistics/akaike-information-criterion/

#Reg <- glm(dachform_be ~ as.factor(hk_geb))                         # AIC: 7122.2
#Reg <- glm(dachform_be ~ as.factor(hk_geb) * geb_flaeche)           # AIC: 6890.3
#Reg <- glm(dachform_be ~ as.factor(hk_geb) : geb_flaeche)           # AIC: 6997.4
#Reg <- glm(dachform_be ~ as.factor(uk_geb), family=binomial)        # AIC: 6481.3
#Reg <- glm(dachform_be ~ as.factor(uk_geb), family=binomial, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)      # AIC: 5308 #https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages
Reg <- glm(dachform_be ~ as.factor(uk_geb), family=quasibinomial, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)   
#Reg <- glm(dachform_be ~ as.factor(uk_geb) * geb_flaeche)           # AIC: 6700.4 # NAs # lm(y ~ x1*x2) is the same thing as lm(y ~ x1 + x2 + x1:x2)
#Reg <- glm(dachform_be ~ as.factor(uk_geb) : geb_flaeche)           # AIC: 6981.7
#Reg <- glm(dachform_be ~ as.factor(hk_geb) : as.factor(bak_grob))   # AIC: 6975.7 # NAs

#glm() binomial model without weights for testing best option of predictors, than with weights (binomial) for AIC and than (quasibinomial) for prediction model, as weights are non-integer values!!!! 
#https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages

sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/dachform.txt")
print(summary(Reg))
print(model_equation(Reg))
sink()  # returns output to the console

# Save Model for later R use
saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/dachform.rds")

# summary(Reg)
# model_equation(Reg)


# SECOND 13.2.2 Assessing the impact of predictors on the probability of an outcome
#        in predict() ist exp(bj)^n schon automatisch drin und es kommen wahrscheinlichkeiten raus (type = response)! P321 
#####################################################
#Add a probability of beeing 1 (Steildach) to DB_BE -->

prob_dachform_be_steil <- predict(Reg, newdata=DB_BE, type="response") 
#DB_BE$prob_dachform_be_steil <- predict(Reg, newdata=DB_BE, type="response") 

# THIRD get subset of missing dachform (or do above with unclean data set and use here onwards)
#####################################################
#Generieren von Un-Clean Subset d.h. das Subset in welchem gbf = -7
DB_BE_unclean_dachform_be <- subset(DB_BE, uk_geb>=0 & dachform_be==-7)

# FORUTH for each row overwrite the -7 of dachform by a dice the has the 
#        Ausprägungen (1=steildach und 0=flachdach) mit der 
#       Frequency zu 1 = prob (siehe oben) und zu 2 = 1 - prob
#####################################################
#Damit wird für jede einzelen Zeile mit einem individuellen Würfel gewürfelt
levels_dachform <- c(1,2) # 1 ist weiterhin steildach, aber 0 wird wieder zu Ursprungskoodierung von Flachdach

rows_DB_BE <- nrow(DB_BE)  

#i=1

for(i in 1:rows_DB_BE){
  if(DB_BE$dachform_be[i]==-7){
    DB_BE$dachform_be[i] <- sample(levels_dachform, size = 1, replace = TRUE, prob = c(prob_dachform_be_steil[i],1-prob_dachform_be_steil[i]))
  }
}


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

dachform_be <- DB_BE$dachform_be
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, dachform_be, dachform_be_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_dachform_be_imputiert.csv')

