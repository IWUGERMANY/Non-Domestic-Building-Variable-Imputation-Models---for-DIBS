# BE_ebf_imputation.R
# Julian Bischof
# 25.02.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# ebf: Maßzahl der Energiebezugsfläche als beheizte und/oder gekühlte NRF in m² 
# aus der Breitenerhebung. Berechnung aus NRF_2 * f_ant_beheizt!
# Wenn das Gebäude nicht beheizt ist, soll generell keine beheizte Fläche ausgewiesen werden, 
# das Merkmal erhält die Ausprägung TNZ (-8).
#-----------------------------------------

# f_ant_beheizt ist der NGF zu EBF Umrechnungsfaktor (Anteil konditionierte Fläche)
min(DB_BE$f_ant_beheizt) #keine Missings

# Hier keine Reg. bzw. Mittwelwertschätzer, sonder umrechnung der imputierten nrf_2 für alle ebf == -7
DB_BE$ebf[DB_BE$ebf==(-7)] <- DB_BE$nrf_2[DB_BE$ebf==(-7)]*DB_BE$f_ant_beheizt[DB_BE$ebf==(-7)]
# table(DB_BE$ebf)
# min(DB_BE$ebf)


#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

ebf <- DB_BE$ebf
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, ebf)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_ebf_imputiert.csv')

