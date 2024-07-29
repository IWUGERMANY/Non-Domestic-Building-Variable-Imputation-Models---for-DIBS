# BE_unteraw_fl_imputation.R
# Julian Bischof
# 11.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# unteraw_fl: Außenwandfläche gegen Erdreich des gesamten Gebäudes in m²
#-----------------------------------------


    # Counting number of imputations in this variable and marking them for later analysis
    unteraw_fl_imputated <- DB_BE$unteraw_fl == -7
    table <- table(unteraw_fl_imputated)
    unteraw_fl_n_imputated <- table[2]
    
    unteraw_fl_imputated[unteraw_fl_imputated == TRUE] <- 1
    # Der indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#----------------------------------------
# Berechung auf Basis bereits imputierter Werte
# Hier keine Reg. bzw. Mittwelwertschätzer, sonder berechung der unteraw_fl für alle unteraw_fl == -7
DB_BE$unteraw_fl[DB_BE$unteraw_fl==(-7)] <- DB_BE$n_ug[DB_BE$unteraw_fl==(-7)] * (DB_BE$unter_hoehe[DB_BE$unteraw_fl==(-7)]+0.3) * DB_BE$geb_f_umfang[DB_BE$unteraw_fl==(-7)]
table(DB_BE$unteraw_fl)

# unteraw_fl = n_ug * ((unter_hoehe)+0.3) * geb_f_umfang
# The floor slaps are assumed to have a hight of 0.3 m




#----------------------------------------
# Alle -8 zu 0 

DB_BE$unteraw_fl[DB_BE$unteraw_fl==(-8)] <- 0


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

unteraw_fl <- DB_BE$unteraw_fl
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, unteraw_fl, unteraw_fl_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_unteraw_fl_imputiert.csv')
