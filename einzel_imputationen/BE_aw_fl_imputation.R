# BE_aw_fl_imputation.R
# Julian Bischof
# 09.08.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# aw_fl: Außenwandfläche des gesamten Gebäudes gegen Außenluft in m² aus der 
# Hüllflächenberechnung über die Geodaten und Breitenerhebung.
#-----------------------------------------


    # Counting number of imputations in this variable and marking them for later analysis
    aw_fl_imputated <- DB_BE$aw_fl == -7
    table <- table(aw_fl_imputated)
    aw_fl_n_imputated <- table[2]
    
    aw_fl_imputated[aw_fl_imputated == TRUE] <- 1
    # Der Indikator ob imputated (1) oder nicht (0) wird unten zur speichernden Variable dazu gespielt


#............................................................................
#
# 2
#Auswertung####
#
#............................................................................

# Berechnung auf Basis f_fl_geo_iwu und fen_flant_1 (Fensterflächenanteil der größten Bauweise). Fen_flanteges weist an den selben Stellen wie aw_fl -7 auf. 
# Daher kann nur qd1 angewandt werden. Damit wird angenommen, dass die zweitgrößte Fassdenbauweise einen Fensterflächenanteil von 0 hat.
DB_BE$aw_fl[DB_BE$aw_fl==(-7)] <- DB_BE$f_fl_geo_iwu[DB_BE$aw_fl==(-7)] * ((100 - DB_BE$fen_flant_1[DB_BE$aw_fl==(-7)])/100)

table(DB_BE$aw_fl)



#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

aw_fl <- DB_BE$aw_fl
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, aw_fl, aw_fl_imputated)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_aw_fl_imputiert.csv')

