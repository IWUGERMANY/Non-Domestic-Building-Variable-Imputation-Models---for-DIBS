# BE_d_fl_be_imputation.R
# Julian Bischof
# 10.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# d_fl_be: Schätzung der Dachfläche aus der Grundfläche (Geodaten) und der 
# Dachform (Breitenerhebung); Faktor 1,51/1,33 für das Steildach nach 
# [Loga et al. 2005] Kurzverfahren Energieprofil
#-----------------------------------------


      #............................................................................
      #
      # 2
      #Auswertung####
      #
      #............................................................................
      


#----------------------------------------
# Berechnung von d_fl_be
# Berechnen flach = fläche, Steildach (Strukturtabelle) sharepoint/Auswertung/generierte Merkmale Steildach: *1,51/1,33 nach Loga et.al. 2005 Kurzverfahren Energieprofil

nrows_DB_BE <- nrow(DB_BE)

# i=1

for(i in 1:nrows_DB_BE){
  if(DB_BE$dachform_be[i]==1 & DB_BE$d_fl_be[i]==-7){
    DB_BE$d_fl_be[i] <- DB_BE$geb_flaeche[i] * (1.51/1.33)
  } else {if(DB_BE$dachform_be[i]==2 & DB_BE$d_fl_be[i]==-7){
    DB_BE$d_fl_be[i] <- DB_BE$geb_flaeche[i]
  }
    
  }
}

# Berechnen flach = fläche, Steildach (Strukturtabelle) sharepoint/Auswertung/generierte Merkmale Steildach: *1,51/1,33 nach Loga et.al. 2005 Kurzverfahren Energieprofil




#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

d_fl_be <- DB_BE$d_fl_be
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, d_fl_be)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_d_fl_be_imputiert.csv')

