setwd("/Users/pichugina/Work/Diffusion_Absorption_model")
pathFiles_1='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F1'
pathFiles_0.5='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.5'
pathFiles_0.25='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.25'
pathFiles_0.125='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.125'

library(reshape2)
library(tidyverse)
library(dplyr)
source("ExpApproximation.R")
source("ReadFiles_to_data_frame.R")
source("one_data_frame_plot.R")
source("several_data_frame_plot.R")