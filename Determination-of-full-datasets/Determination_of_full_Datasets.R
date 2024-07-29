# BE Determination of Full Datasets for Simulation and Validation
# Julian Bischof
# 18.08.2021
# Updated 11.12.2023
# UTF 8
# Abhänigkeiten/Dependecies: BE_imputation.R; JBs_R_Functions.R; All R-Scripts in Folder einzel_imputationen

#............................................................................
#
# Ermitteln Anzahl Voller Datensätze in BE (ohne Missings = -7)#####
# Determination of number of full DIBS simulation data sets without missings
#
#............................................................................

#Alle Variablen, welche an DIBS gehen 
#All DIBS input Variables from DataNWG-BRE
DB_BE_full <- filter(DB_BE, (scr_gebaeude_id >= 0 | scr_gebaeude_id == -8) & 
                       (plz >= 0 | plz == -8) & 
                       (hk_geb >= 0 | hk_geb == -8) & 
                       (uk_geb >= 0 | uk_geb == -8) & 
                       (q25_1 >= 0 | q25_1 == -8) & 
                       (aw_fl >= 0 | aw_fl == -8) &  
                       (unteraw_fl >= 0 | unteraw_fl == -8) & 
                       (qd1 >= 0 | qd1 == -8) & 
                       (geb_f_flaeche_n_iwu >= 0 | geb_f_flaeche_n_iwu == -8)& 
                       (geb_f_flaeche_o_iwu >= 0 | geb_f_flaeche_o_iwu == -8)& 
                       (geb_f_flaeche_s_iwu >= 0 | geb_f_flaeche_s_iwu == -8)& 
                       (geb_f_flaeche_w_iwu >= 0 | geb_f_flaeche_w_iwu == -8)& 
                       (d_fl_be >= 0 | d_fl_be == -8)& 
                       (nrf_2 >= 0 | nrf_2 == -8)& 
                       (ebf >= 0 | ebf == -8)& 
                       (n_og >= 0 | n_og == -8)& 
                       (geb_f_hoehe_mittel_iwu >= 0 | geb_f_hoehe_mittel_iwu == -8)& 
                       (qf1 >= 0 | qf1 == -8)& 
                       (lampenart >= 0 | lampenart == -8)& 
                       (glasart_1 >= 0 | glasart_1 == -8)& 
                       (qd8 >= 0 | qd8 == -8)& 
                       (u_fen >= 0 | u_fen == -8)& 
                       (u_aw >= 0 | u_aw == -8)& 
                       (d_u_ges >= 0 | d_u_ges == -8)& 
                       (u_ug >= 0 | u_ug == -8)& 
                       (n_ug >= 0 | n_ug == -8)& 
                       (bak_grob >= 0 | bak_grob == -8)& 
                       (qh1 >= 0 | qh1 == -8)& 
                       (qh3_1 >= 0 | qh3_1 == -8)& 
                       (aw_konstr_1 >= 0 | aw_konstr_1 == -8)& 
                       (freie_kuehlung >= 0 | freie_kuehlung == -8)& 
                       (w_erz_art_et >= 0 | w_erz_art_et == -8)& 
                       (k_erz_art_rk >= 0 | k_erz_art_rk == -8)& 
                       (qg13 >= 0 | qg13 == -8)& 
                       (qi11 >= 0 | qi11 == -8)) 
print("Anzahl Gebäude mit vollem DIBS-Datensatz ohne Missing (-7, -66, -88):")
print(nrow(DB_BE_full))
# 2011
print("Anzahl Gebäude mit unvollständigem DIBS-Datensatz MIT Missing (-7, -66, -88):")
print(rows_DB_BE - nrow(DB_BE_full))
# 3092

# Abgleich mit den verfügbaren Verbrauchsdaten 
colnames(DB_TE_Verbrauch)[1] <- "scr_gebaeude_id_ver" # Rename Geb ID of column name as it contains an Ä initionally
DB_BE_full_ver <- merge(DB_BE_full, DB_TE_Verbrauch, by.x=c("scr_gebaeude_id"), by.y = c("scr_gebaeude_id_ver")) # Match the two dfs
DB_BE_full_ver_full <- DB_BE_full_ver[DB_BE_full_ver$energy_consumption_heating_clean > 0 & DB_BE_full_ver$energy_consumption_electricity_clean > 0] #Reduce merged df to buildings that have heat and electiricity measuerd consumption data

print("Anzahl Gebäude mit vollem DIBS-Datensatz und Verbrauchsdaten (Wärme und Strom) ohne Missing (-7, -66, -88):")
print(nrow(DB_BE_full_ver_full))
# 195

print("Anzahl Gebäude mit vollem Verbrauchsdaten (Wärme und Strom) generell in Tiefenerhebung ohne Missing (-7, -66, -88):")
xxx <- (DB_TE_Verbrauch$energy_consumption_heating_clean > 0) & (DB_TE_Verbrauch$energy_consumption_electricity_clean > 0)
xxx <- xxx[xxx==TRUE]
print(length(xxx))
#412



# #............................................................................
# #
# # Ermitteln Anzahl Voller fehlender Fälle je Variabele in BE (ohne Missings = -7)#####
# # Determination of number of missing cases of each variable
# #
# #............................................................................

# #Alle Variablen, welche an DIBS gehen 
# #All DIBS input Variables from DataNWG-BRE

# table(DB_BE$scr_gebaeude_id)


# variable_name <- "n_og"
# df_input <- DB_BE


# table(DB_BE$n_og)


# n_df <- nrow(df_input)
# filter_application <- filter(df_input, (variable_name >= 0 | variable_name == -8))
# n_variable <- nrow(filter_application)
# n_missings_in_variable <- n_df - n_variable
# print("Number of missing cases in the variable")
# print(variable_name)
# print(n_missings_in_variable)


# #_______________________________________________________
# ###  ####
# n_missings <- function(df_input, variable_name){ 
#     n_df <- nrow(df_input)
#     filter_application <- filter(df_input, (variable_name >= 0 | variable_name == -8))
#     n_variable <- nrow(filter_application)
#     n_missings_in_variable <- n_df - n_variable
#     print("Number of missing cases in the variable")
#     print(variable_name)
#     print(n_missings_in_variable)
# }
# ###





table(DB_BE$scr_gebaeude_id) # No missing
table(DB_BE$plz) # No missing
table(DB_BE$hk_geb) # No missing
table(DB_BE$uk_geb) # No missing
table(DB_BE$q25_1) #  948
table(DB_BE$aw_fl) # 122
t <- table(DB_BE$unteraw_fl) # 139
table(DB_BE$qd1) # No missing
t <- table(DB_BE$geb_f_flaeche_n_iwu) # No missing
t <- table(DB_BE$geb_f_flaeche_o_iwu) # No missing
t <- table(DB_BE$geb_f_flaeche_s_iwu) # No missing
t <- table(DB_BE$geb_f_flaeche_w_iwu) # No missing
t <- table(DB_BE$d_fl_be) # 31
t <- table(DB_BE$nrf_2) # No missing
t <- table(DB_BE$ebf) # No missing
table(DB_BE$n_og) # 97
t <- table(DB_BE$geb_f_hoehe_mittel_iwu) # No missing
table(DB_BE$qf1) # 57
table(DB_BE$lampenart) # 207
table(DB_BE$glasart_1) # No missing
table(DB_BE$qd8) # 140
table(DB_BE$u_fen) # 433
table(DB_BE$u_aw) # 643
table(DB_BE$d_u_ges) # 1575
table(DB_BE$u_ug) # 1625
table(DB_BE$n_ug) # No missing
table(DB_BE$bak_grob) # No missing
table(DB_BE$qh1) # 72
table(DB_BE$qh3_1) # 44
table(DB_BE$aw_konstr_1) # 2
table(DB_BE$freie_kuehlung) # 184
table(DB_BE$w_erz_art_et) # 352
table(DB_BE$k_erz_art_rk) # 20
table(DB_BE$qg13) # 96
table(DB_BE$qi11) # 28

    # Resulting in 6,815 missing values in 178,605 total DIBS input variable values in the large data set (35 Variables * 5,103 Buildings = 178,605)
    # -> 3.82 % of all variables for DIBS input are missing!

table(DB_BE$unter_hoehe) # 139
table(DB_BE$qg21) # 173


# Hilfsvariablen
table(DB_BE$geb_flaeche)


