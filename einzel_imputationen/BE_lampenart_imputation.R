# BE_lampenart_imputation.R
# Julian Bischof
# 15.03.2022
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# lampenart: überwiegende Lampenart, bezogen auf die Nutzungsfläche des Gebäudes. 
# Bei LED-Lampen wird noch zusätzlich unterschieden, ob es sich um eine 
# LED-Ersatzlampe oder um eine LED-Speziallampe handelt.
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

    DB_BE_clean <- subset(DB_BE, hk_geb>=0 & bak>=0 &lampenart>=0)
    
    
    lampenart <- DB_BE_clean$lampenart
    hk_geb <- DB_BE_clean$hk_geb
    HRF <- DB_BE_clean$HRF
    bak <- DB_BE_clean$bak
    Sum_HRF_BE_clean <- sum(HRF)
    bak_grob <- DB_BE_clean$bak_grob
    
    N_Clean <- nrow(DB_BE_clean)

# #............................................................................
# #
# # 2
# #Auswertung####
# #
# #............................................................................
# 


    #----------------------------------------
    # 1.2 Datensatz aus zu imputierenden Zeilen
    
    #Generieren von Un-Clean Subset d.h. das Subset in welchem XXX = -7
    DB_BE_unclean_lampenart <- subset(DB_BE, hk_geb>=0 & bak_grob>=0 & lampenart==-7)
    
    
        # Counting number of imputations in this variable and marking them for later analysis
        lampenart_imputated <- DB_BE$lampenart == -7
        table <- table(lampenart_imputated)
        lampenart_n_imputated <- table[2]
        
        lampenart_imputated[lampenart_imputated == TRUE] <- 1
        # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt
        
        
        
    
    
    #----------------------------------------
    # 2.3 lampenart auf Basis multinominal logistc regression multinom() of the nnet package
    
    # SETTING UP add levels to factors
    #####################################################
    # Setting the basline for the multinom() 
    DB_BE_clean$lampenart <- relevel(as.factor(DB_BE_clean$lampenart), ref = "1")
    
    
    # FIRST Prepare the glm model
    #####################################################
    # Loading the nnet package requiered for the multinom() funktion for multinominal logistic regression
    require(nnet)
    
    # Training the multinomial model
    Reg <- multinom(lampenart ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 8453.951
    
    # Checking the model
    summary(Reg)
    
    sink("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/lampenart.txt")
    print(summary(Reg))
    sink()  # returns output to the console
    
    # Save Model for later R use
    saveRDS(Reg, "D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Reg_equations/lampenart.rds")
    
    
    # mlogit() Multinomial logistic regression
    # multinom function of the package nnet https://stats.stackexchange.com/questions/175782/how-to-perform-a-logistic-regression-for-more-than-2-response-classes-in-r
    
    
    # SECOND Assessing the impact of predictors on the probability of an outcome
    #        in predict() ist exp(bj)^n schon berücksichtigt und als Ergebniss werden Wahrscheinlichkeiten ausgegeben (type = probs)!
    # predicted.probs <- predict(Reg,new.data=DB_BE, type="probs")
    
    predicted.probs <- predict(Reg,newdata=DB_BE, type="probs")
    
    # THIRD for each row overwrite the -7 of lampenart by a dice the has the 
    #       levels_lampenart (Ausprägungen (1, 2, 3)) mit der 
    #       Frequency/Probability zu 1,2,3 = prob (Die Probabilities werden in df predicted.probs für jede Ausprägung in je eine Spalte abgelegt)
    #####################################################
    levels_lampenart <- levels(DB_BE_clean$lampenart)
    rows_DB_BE <- nrow(DB_BE)
    
    #i=1
    
    for(i in 1:rows_DB_BE){
      if(DB_BE$lampenart[i]==-7){
        DB_BE$lampenart[i] <- sample(levels_lampenart, size = 1, replace = TRUE, prob = c(predicted.probs[i,1],predicted.probs[i,2],predicted.probs[i,3],predicted.probs[i,4]))
      }
    }
    
    #table(DB_BE$lampenart)
    
    

#............................................................................
#
# 3
#Speichern####
#
#............................................................................

    # Imputierte Variable zum Speichern vorbereiten
    
    lampenart <- DB_BE$lampenart
    Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, lampenart, lampenart_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar
    
    # write it as a csv file
    write.csv(Imputierte_Variable, 'DB_BE_lampenart_imputiert.csv')

