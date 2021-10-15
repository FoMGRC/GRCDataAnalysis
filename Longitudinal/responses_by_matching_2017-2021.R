## longitudinal questions ###########################################################

library(readxl)
library(data.table)

## HELPER #########################################################################################

replace_entry <- function(dat, column, search, replace, ignore_case = FALSE){

    dat[grep(search, x = dat[[column]], ignore.case = ignore_case), column] <- replace

    return(dat)
}

replace_in_list <- function(ll, search, replace, ignore_case = FALSE){

    ll <- sapply(ll, function(x) ifelse(grepl(search, x, ignore.case = ignore_case), replace, x))

    # ll <- rapply(ll, function(x) gsub(pattern = search, x = x, replacement = replace, ignore.case = T), how = "replace")

    return(ll)
}

## DATA ###########################################################################################

parsed_dir <- '~/Documents/grc_data/Parsed/'

survey_list <- list()
survey_list[['2017_18']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2017-18.tsv'), as.is=T)
survey_list[['2018_19']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2018-19.tsv'), as.is=T)
survey_list[['2019_20']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2019-20.tsv'), as.is=T)
survey_list[['2020_21']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2020-21.tsv'), as.is=T)
survey_list[['2021_22']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2021-22.tsv'), as.is=T)
print(sapply(survey_list, ncol))

setwd('~/Documents/grc_data/Longitudinal/')
matching <- read.delim('All GRC Survey Question Correspondences 2017-2021 - Sheet2.tsv', as.is=T)

## MATCH ##########################################################################################

