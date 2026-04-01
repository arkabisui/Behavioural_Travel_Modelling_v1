# ################################################################# #
# PART (d) and (e): Mode split and security impact calculations      #
# Run AFTER estimating MNL and NL (Car+Air nest) models above        #
# Paste your estimated coefficients from model output below          #
# ################################################################# #

# ---------------------------------------------------------------
# PASTE YOUR ESTIMATED COEFFICIENTS HERE from model output
# ---------------------------------------------------------------
asc_car   = 0           # fixed base
asc_air   = 0           # replace with your estimate
asc_rail  = 0           # replace with your estimate
b_IVTT    = 0           # replace with your estimate
b_OVTT    = 0           # replace with your estimate
b_COST    = 0           # replace with your estimate
b_FREQ    = 0           # replace with your estimate
theta     = 0           # replace with NL Car+Air nest parameter estimate

# ---------------------------------------------------------------
# LOS values from the assignment table (Part d)
# replace with actual values from the table in assignment
# ---------------------------------------------------------------
IVTT_c = 0;  OVTT_c = 0;  COST_c = 0;  FREQ_c = 0
IVTT_a = 0;  OVTT_a = 0;  COST_a = 0;  FREQ_a = 0
IVTT_r = 0;  OVTT_r = 0;  COST_r = 0;  FREQ_r = 0

total_demand = 1000

# ---------------------------------------------------------------
# MNL utilities and probabilities
# ---------------------------------------------------------------
V_car_MNL  = asc_car  + b_IVTT*IVTT_c + b_OVTT*OVTT_c + b_COST*COST_c + b_FREQ*FREQ_c
V_air_MNL  = asc_air  + b_IVTT*IVTT_a + b_OVTT*OVTT_a + b_COST*COST_a + b_FREQ*FREQ_a
V_rail_MNL = asc_rail + b_IVTT*IVTT_r + b_OVTT*OVTT_r + b_COST*COST_r + b_FREQ*FREQ_r

denom_MNL  = exp(V_car_MNL) + exp(V_air_MNL) + exp(V_rail_MNL)
P_car_MNL  = exp(V_car_MNL)  / denom_MNL
P_air_MNL  = exp(V_air_MNL)  / denom_MNL
P_rail_MNL = exp(V_rail_MNL) / denom_MNL

cat("--- MNL Mode Split (Part d) ---\n")
cat("Car  :", round(P_car_MNL  * total_demand), "trips\n")
cat("Air  :", round(P_air_MNL  * total_demand), "trips\n")
cat("Rail :", round(P_rail_MNL * total_demand), "trips\n")

# ---------------------------------------------------------------
# NL utilities and probabilities (Car+Air nest)
# ---------------------------------------------------------------
V_car_NL  = asc_car  + b_IVTT*IVTT_c + b_OVTT*OVTT_c + b_COST*COST_c + b_FREQ*FREQ_c
V_air_NL  = asc_air  + b_IVTT*IVTT_a + b_OVTT*OVTT_a + b_COST*COST_a + b_FREQ*FREQ_a
V_rail_NL = asc_rail + b_IVTT*IVTT_r + b_OVTT*OVTT_r + b_COST*COST_r + b_FREQ*FREQ_r

logsum_nest  = theta * log(exp(V_car_NL/theta) + exp(V_air_NL/theta))

denom_NL     = exp(logsum_nest) + exp(V_rail_NL)
P_nest_NL    = exp(logsum_nest) / denom_NL
P_rail_NL    = exp(V_rail_NL)   / denom_NL

P_car_given_nest = exp(V_car_NL/theta) / (exp(V_car_NL/theta) + exp(V_air_NL/theta))
P_air_given_nest = exp(V_air_NL/theta) / (exp(V_car_NL/theta) + exp(V_air_NL/theta))

P_car_NL  = P_nest_NL * P_car_given_nest
P_air_NL  = P_nest_NL * P_air_given_nest

cat("\n--- NL Mode Split (Part d) ---\n")
cat("Car  :", round(P_car_NL  * total_demand), "trips\n")
cat("Air  :", round(P_air_NL  * total_demand), "trips\n")
cat("Rail :", round(P_rail_NL * total_demand), "trips\n")

# ---------------------------------------------------------------
# Part (e): +30 min airline OVTT shock
# ---------------------------------------------------------------
OVTT_a_new = OVTT_a + 30

# MNL after shock
V_air_MNL_new  = asc_air + b_IVTT*IVTT_a + b_OVTT*OVTT_a_new + b_COST*COST_a + b_FREQ*FREQ_a
denom_MNL_new  = exp(V_car_MNL) + exp(V_air_MNL_new) + exp(V_rail_MNL)
P_car_MNL_new  = exp(V_car_MNL)      / denom_MNL_new
P_air_MNL_new  = exp(V_air_MNL_new)  / denom_MNL_new
P_rail_MNL_new = exp(V_rail_MNL)     / denom_MNL_new

cat("\n--- MNL After Security Shock (Part e) ---\n")
cat("Car  :", round(P_car_MNL_new  * total_demand), "trips |",
    round((P_car_MNL_new - P_car_MNL) / P_car_MNL * 100, 2), "% change\n")
cat("Air  :", round(P_air_MNL_new  * total_demand), "trips |",
    round((P_air_MNL_new - P_air_MNL) / P_air_MNL * 100, 2), "% change\n")
cat("Rail :", round(P_rail_MNL_new * total_demand), "trips |",
    round((P_rail_MNL_new - P_rail_MNL) / P_rail_MNL * 100, 2), "% change\n")

# NL after shock
V_air_NL_new     = asc_air + b_IVTT*IVTT_a + b_OVTT*OVTT_a_new + b_COST*COST_a + b_FREQ*FREQ_a
logsum_nest_new  = theta * log(exp(V_car_NL/theta) + exp(V_air_NL_new/theta))
denom_NL_new     = exp(logsum_nest_new) + exp(V_rail_NL)
P_nest_NL_new    = exp(logsum_nest_new) / denom_NL_new
P_rail_NL_new    = exp(V_rail_NL)       / denom_NL_new

P_car_given_nest_new = exp(V_car_NL/theta)      / (exp(V_car_NL/theta) + exp(V_air_NL_new/theta))
P_air_given_nest_new = exp(V_air_NL_new/theta)  / (exp(V_car_NL/theta) + exp(V_air_NL_new/theta))

P_car_NL_new = P_nest_NL_new * P_car_given_nest_new
P_air_NL_new = P_nest_NL_new * P_air_given_nest_new

cat("\n--- NL After Security Shock (Part e) ---\n")
cat("Car  :", round(P_car_NL_new  * total_demand), "trips |",
    round((P_car_NL_new - P_car_NL) / P_car_NL * 100, 2), "% change\n")
cat("Air  :", round(P_air_NL_new  * total_demand), "trips |",
    round((P_air_NL_new - P_air_NL) / P_air_NL * 100, 2), "% change\n")
cat("Rail :", round(P_rail_NL_new * total_demand), "trips |",
    round((P_rail_NL_new - P_rail_NL) / P_rail_NL * 100, 2), "% change\n")