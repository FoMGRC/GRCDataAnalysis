## 2020-21 student finance survey analysis ###########################################################

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
# Load survey data #
file <- "~/Documents/grc_data/Raw/Faculty of Medicine Graduate Student Survey_2017_18 (Responses).xlsx"
# only read first sheet (doesn't really work)
dat <- read_excel(file, na = c("", "NA", "N/A"), sheet = 1)
print(dim(dat))

# Stop if duplicated answers exist
stopifnot(!any(duplicated(dat)))
# dat <- dat[-which(duplicated(x = dat[-1])),]
# dat <- dat[-which(is.na(dat$dept)),]

# remove those with no timestamp
dat <- dat[!is.na(dat$Timestamp), ]
print(dim(dat))

# remove semi-colon from column names
colnames(dat) <- gsub(':', '', colnames(dat))

## CLEANING #######################################################################################

# gender minority
dat$gender.minority <- ifelse(dat[['What is your gender?']] != 'Female' & dat[['What is your gender?']] != 'Male', TRUE, FALSE)

# Add short dept name # no RSI
print(table(dat[["What department are you in?"]]))
dat$dept.short <- factor(dat[["What department are you in?"]], labels = c("BCHM", "Imm.", "LMP", "MBP", "MoGen", "NutriSci", "PharmTox", "Phys", "IMS"))

# area of research
# make every letter after space upper case
q <- 'What is your area of research (select up to 3 that best apply)?'
dat[[q]] <- tools::toTitleCase(dat[[q]])

# Research location
dat[["What is your primary research location (select 1)"]] <- tools::toTitleCase(dat[["What is your primary research location (select 1)"]])
table(dat[["What is your primary research location (select 1)"]])

dat <- replace_entry(dat, 'What is your primary research location (select 1)', 'none', NA, ignore_case = T)
dat <- replace_entry(dat, 'What is your primary research location (select 1)', 'camh|addiction', 'Centre for Addiction and Mental Health (CAMH)', ignore_case = T)
dat <- replace_entry(dat, 'What is your primary research location (select 1)', 'sick children|PGCRL', 'Hospital for Sick Children/PGCRL', ignore_case = T)
dat <- replace_entry(dat, 'What is your primary research location (select 1)', 'MaRS', 'MaRS (PMCRT/OICR)', ignore_case = T)
dat <- replace_entry(dat, 'What is your primary research location (select 1)', 'Princess Margaret Hospital', 'Princess Margaret Cancer Centre', ignore_case = T)

table(dat[["What is your primary research location (select 1)"]])

# Awards applied for
dat$sch.apply <- dat[["What award(s) did you apply for?"]]
# Cleaning fields with extra entries
dat$sch.apply <- gsub(pattern = ", | & R", 
                      x = dat$sch.apply,
                      replacement = ";")
# Split and convert field into list
dat$sch.apply <- strsplit(x = as.character(dat$sch.apply), split = ";")
table(unlist(dat$sch.apply))

# Replace in list
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Banting & Best Diabetes.*|BBDC.*|Banting.*", x = x, replacement = "Banting & Best Diabetes Centre", ignore.case = T), how = "replace")
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "CF.*", x = x, replacement = "CF Canada", ignore.case = T), how = "replace")
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "connaught.*", x = x, replacement = "Connaught", ignore.case = T), how = "replace")
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "HSRLCE.*", x = x, replacement = "HSRLCE", ignore.case = T), how = "replace")

dat$sch.apply <- replace_in_list(dat$sch.apply, "internal|department", 'Department Internal Award', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "Trillium|OTS", 'Ontario Trillium', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "OSOFT", 'Ontario Student Opportunity Trust Fund (OSOTF)', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "research specific", 'Research Area Awards', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "Restracomp|Sick kids|sickkids|Retracomp", 'SickKids Restracomp', ignore_case = T)

dat$sch.apply <- replace_in_list(dat$sch.apply, "vanier", 'Vanier', ignore_case = T)

table(unlist(dat$sch.apply))

dat[['What award(s) did you apply for?']] <- sapply(dat$sch.apply, paste, collapse=", ")
dat <- dat[, colnames(dat) != 'sch.apply']

# Awards hold
dat$sch.hold <- dat[['What type of scholarship(s) did you receive?']]
# Cleaning fields with extra entries
dat$sch.hold <- gsub(pattern = ", | & R",
                      x = dat$sch.hold,
                      replacement = ";")
# Split and convert field into list
dat$sch.hold <- strsplit(x = as.character(dat$sch.hold), split = ";")
table(unlist(dat$sch.hold))

# Replace in list
dat$sch.hold <- replace_in_list(dat$sch.hold, "GSEF|endowment", 'Faculty of Medicine Graduate Student Endowment Fund', ignore_case = T)
dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "Banting & Best Diabetes.*|BBDC.*|Banting.*", x = x, replacement = "Banting & Best Diabetes Centre", ignore.case = T), how = "replace")
dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "Cecil Yip.*", x = x, replacement = "Cecil Yip", ignore.case = T), how = "replace")
dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "CF.*", x = x, replacement = "CF Canada", ignore.case = T), how = "replace")
dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "connaught.*", x = x, replacement = "Connaught", ignore.case = T), how = "replace")
dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "HSRLCE.*", x = x, replacement = "HSRLCE", ignore.case = T), how = "replace")
dat$sch.hold <- replace_in_list(dat$sch.hold, "Trillium|OTS", 'Ontario Trillium', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "OSOFT", 'Ontario Student Opportunity Trust Fund (OSOTF)', ignore_case = T)

dat$sch.hold <- replace_in_list(dat$sch.hold, "research specific", 'Research Area Awards', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "u of t|university of toronto|UofT|UTF", 'UofT Awards', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "Restracomp|Sick kids|sickkids|Retracomp", 'SickKids Restracomp', ignore_case = T)

dat$sch.hold <- replace_in_list(dat$sch.hold, "Stimulus|building|internal|MBP|department", 'Internal Award', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "government", 'International Award', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "entrance", 'Entrance Scholarship', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "Restracomp", 'SickKids Restracomp', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "SMH|Michael", 'St. Michael\'s Hospital', ignore_case = T)

table(unlist(dat$sch.hold))

dat[["What type of scholarship(s) did you receive?"]] <- sapply(dat$sch.hold, paste, collapse=", ")
dat <- dat[, colnames(dat) != 'sch.hold']

# scholarship value
q <- 'What was the total value of your scholarship(s) for the 2016/17 academic year?'
dat[[q]] <- gsub('\\,\\$', '', dat[[q]])
dat$sch.tot.value <- as.numeric(dat[[q]])
table(dat$sch.tot.value)

q <- 'What was the monetary value beyond your base stipend that you received as a result of winning any awards (i.e. "top-ups") for the 2016/17 academic year?'
dat$sch.topup <- as.numeric(dat[[q]])
table(dat$sch.topup)

# jobs
q <- 'If externally employed, how many hours per week do you work on average (outside of graduate work)?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

# housing
q <- 'Approximately what is your monthly rent and/or mortgage expenses?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'Approximately how much do you spend per month on additional essential expenses (dental, health, optical, food, transit, etc)?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

write.table(dat, file="~/Documents/grc_data/Parsed/GRC_Survey_Cleaned_2017-18.tsv", sep='\t', quote=T, row.names=F, col.names=T)
