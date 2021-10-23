## longitudinal questions ###########################################################

library(readxl)
library(data.table)
library(plyr)
library(BoutrosLab.plotting.general)

## DATA ###########################################################################################

work_dir <- '~/Documents/grc_data/2021-22/'
setwd(work_dir)

date <- Sys.Date()

latest_rds <- list.files('./', '*matched_lists_2017-2021.RDS')
latest_rds <- latest_rds[length(latest_rds)]

matched_lists <- readRDS(latest_rds)

## HELP ###########################################################################################


## TRANSFER #######################################################################################


## AWARENESS ######################################################################################

## SUPPORT ########################################################################################

# Supprt Living Expenses
raw <- matched_lists[['Supprt Living Expenses']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year'))
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'No'))
raw_df_stack <- raw_df_stack[raw_df_stack$variable == 'Yes', ]
write.table(raw_df_stack, 'can_live_on_funding_yes_2017-21.tsv', col.names=T, row.names=F, quote=T, sep='\t')



## EMPLOYMENT #####################################################################################

## SCHOLARSHIP ####################################################################################

## LIVING #########################################################################################

## TRANSPORT ######################################################################################

## MENTAL HEALTH ##################################################################################

# Anxiety or Depression
raw <- matched_lists[['Anxiety or Depression']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year'))
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'No'))
raw_df_stack <- raw_df_stack[raw_df_stack$variable == 'Yes', ]
colnames(raw_df_stack)[colnames(raw_df_stack) == 'variable'] <- 'Do you struggle with anxiety or depression?'
write.table(raw_df_stack, 'anxiety_depression_yes_2019-21.tsv', col.names=T, row.names=F, quote=T, sep='\t')

# Financial Impact Mental Health
raw <- matched_lists[['Financial Negative Mental Health']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack_negative <- reshape2::melt(raw_df, id.vars = c('year', 'order'))

raw <- matched_lists[['Financial Impact Mental Health']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
colnames(raw_df) <- colnames(raw_df)[ncol(raw_df):1]
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df) + 2
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))

raw_df_stack <- rbind(raw_df_stack_negative, raw_df_stack)
raw_df_stack$variable <- factor(raw_df_stack$variable)


