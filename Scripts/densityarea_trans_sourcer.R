###############################################
#Density-area transmission function sourcer
###############################################

require(ggplot2)
require(deSolve)
require(cowplot)
require(gridExtra)
require(ggpubr)

source(paste(getwd(), "/simulate_densityarea_trans_fun.R", sep = ''))
source(paste(getwd(), "/R0land_fun.R", sep = ''))
source(paste(getwd(), "/R0patch_fun.R", sep = ''))
source(paste(getwd(), "/densityarea_fun.R", sep = ''))
source(paste(getwd(), "/simulate_onerun.R", sep = ''))
source(paste(getwd(), "/halflife_fun.R", sep = ''))
source(paste(getwd(), "/sensitivity_calcs_fun.R", sep = ''))

source(paste(getwd(), "/landuse_ODE.R", sep = ''))