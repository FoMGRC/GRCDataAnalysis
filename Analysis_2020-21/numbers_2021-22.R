## longitudinal questions ###########################################################

library(readxl)
library(data.table)
library(BoutrosLab.plotting.general)

## HELPER #########################################################################################

## DATA ###########################################################################################

work_dir <- '~/Documents/grc_data/2021-22/'

date <- Sys.Date()

setwd(work_dir)

parsed_file <- "~/Documents/grc_data/Parsed/GRC_Survey_Cleaned_2021-22.tsv"
dat <- read.delim(parsed_file, as.is=T, check.names=F)

## TRANSFER #######################################################################################

q1 <- 'If MSc, do you intend to transfer to the PhD program?'
q2 <- 'Does/did the lack of financial security during graduate school discourage your decision to transfer to a PhD?'

tat <- table(dat[[q1]], dat[[q2]])
tat <- as.data.frame(rbind(tat))

write.table(tat, 'master_transfer_by_financial_discourage.tsv', col.names=T, row.names=T, quote=T, sep='\t')

## SUPPORT ########################################################################################

q2 <- 'Can you support all of your day-to-day living expenses exclusively from your graduate funding (i.e stipend and awards/top-ups)?'
q1 <- 'How much additional monetary support would you need to meet your day-to-day expenses each year (above your graduate living allowance)?'

tat <- table(dat[[q1]], dat[[q2]])
tat <- as.data.frame(rbind(tat))

write.table(tat, 'need_extra_by_can_live.tsv', col.names=T, row.names=T, quote=T, sep='\t')

## EMPLOYMENT #####################################################################################

q1 <- 'How many hours per week do you work as a teaching assistant?'
q2 <- 'How many hours per week do you work at your side job (excluding teaching assistantships)?'

tat <- table(dat[[q1]] > 0, dat[[q2]] > 0)
tat <- as.data.frame(rbind(tat))

tatt <- tat / nrow(dat)
tatt <- cbind(tat, tatt)

write.table(tat, 'need_extra_by_can_live.tsv', col.names=T, row.names=T, quote=T, sep='\t')
