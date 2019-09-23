# Calculating VPD and other intermediary atmospheric values
# John Godlee (johngodlee@gmail.com)
# 2018_11_22

# Vapour pressure (millibars)
vp <- function(T_c){
    return(0.6108 * exp(17.27 * T_c / (T_c + 237.3))) 
}

# Relative humidity (%)
rh <- function(T_c, Td_c){
    es_calc <- vp(T_c)  # Saturation vp
    ea_calc <- vp(Td_c)  # Actual vp
    return((ea_calc / es_calc) * 100) 
}

# Vapour pressure deficit (kPa)
vpd <- function(T_c, Td_c){ 
    es_calc <- vp(T_c)
    ea_calc <- vp(Td_c) 
    return(0.1 * (ea_calc - es_calc))
}

