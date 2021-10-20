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
survey_list[['X2017_18']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2017-18.tsv'), as.is=T, check.names=FALSE)
survey_list[['X2018_19']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2018-19.tsv'), as.is=T, check.names=FALSE)
survey_list[['X2019_20']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2019-20.tsv'), as.is=T, check.names=FALSE)
survey_list[['X2020_21']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2020-21.tsv'), as.is=T, check.names=FALSE)
survey_list[['X2021_22']] <- read.delim(paste0(parsed_dir, '/', 'GRC_Survey_Cleaned_2021-22.tsv'), as.is=T, check.names=FALSE)
print(sapply(survey_list, ncol))

setwd('~/Documents/grc_data/Longitudinal/')
matching <- read.delim('All GRC Survey Question Correspondences 2017-2021 - Sheet2.tsv', as.is=T)

## MATCH ##########################################################################################

matched_lists <- list()

# go through each match
for (i in 1:nrow(matching)){

    # skip if without manual short form
    if (matching$Short_Form[i] == ''){ next }

    # remove all ':'
    matching[i, ] <- gsub(':', '', matching[i, ])

    matched_lists[[matching$Short_Form[i]]] <- list()

    for (j in colnames(matching)[2:ncol(matching)]){
        if (is.na(matching[i, j])){ next }

        matched_lists[[matching$Short_Form[i]]][[j]] <- survey_list[[j]][[matching[i, j]]]
    }

}

# additional columns
additional_cols <- c('gender.minority', 'sexual.minority', 'racialized', 'international',
    'sch.tot.value', 'sch.topup', 'support.family', 'support.ta', 'support.employment',
    'support.loans', 'support.savings', 'support.none', 'scholarship.applied',
    'scholarship.held', 'sch.tot.value', 'sch.tot.value.held', 'sch.topup', 'sch.topup.held')

for (i in 1:length(additional_cols)){

    matched_lists[[additional_cols[i]]] <- list()

    for (j in names(survey_list)){
        if (additional_cols[i] %in% colnames(survey_list[[j]])){
            matched_lists[[additional_cols[i]]][[j]] <- survey_list[[j]][[additional_cols[i]]]
        }
    }
}


