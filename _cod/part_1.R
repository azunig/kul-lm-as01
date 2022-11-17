library(readr)
library(ggplot2)

#getwd()
fol_main <- '/Users/aronvz/Documents/github/kul-lm-as01/'
fol_src  <- '_src/'
fol_out  <- '_out/'
fil_raw  <- 'salary.gender.txt'
setwd(fol_main)

salary.gender.txt <- read.table(paste0(fol_src,fil_raw), 
                        header=TRUE, 
                        skip = 27,
                        quote="\"", sep=",")


