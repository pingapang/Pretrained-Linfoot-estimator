library(mets) # contains twin data on bmi
library(tidyverse)
library(reticulate)

# specify the environment you want to use, should have tensorflow etc installed
use_condaenv("/opt/miniconda3/envs/tensorflow")

data(twinbmi)

mz <- twinbmi %>% 
  dplyr::filter(zyg == "MZ") %>% 
  pivot_wider(names_from = num, values_from = bmi) %>% 
  filter(!is.na(`1`) & !is.na(`2`)) %>% 
  dplyr::select(`1`:`2`) 

linfoot(mz)
cor(mz)

dz <- twinbmi %>% 
  dplyr::filter(zyg == "DZ") %>% 
  pivot_wider(names_from = num, values_from = bmi) %>% 
  filter(!is.na(`1`) & !is.na(`2`)) %>% 
  dplyr::select(`1`:`2`) 


linfoot(dz)
cor(dz)

# what happens when you log-transform the data
dz_log <- dz
dz_log$'1' <- log(dz$'1')
dz_log$'2' <- log(dz$'2')
linfoot(dz)
linfoot(dz_log)

mz_log <- mz
mz_log$'1' <- log(mz$'1')
mz_log$'2' <- log(mz$'2')
linfoot(mz)
linfoot(mz_log)

