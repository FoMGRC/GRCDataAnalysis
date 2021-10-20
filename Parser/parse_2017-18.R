## 2017-18 student finance survey analysis ###########################################################

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

dat[['If MSc, do you intend to transfer to the PhD program?']] <- ifelse(dat[['What degree program are you in?']] != 'MSc', NA, dat[['If MSc, do you intend to transfer to the PhD program?']])

# Add short dept name # no RSI
print(table(dat[["What department are you in?"]]))
dat$dept.short <- factor(dat[["What department are you in?"]], labels = c("BCHM", "Imm.", "LMP", "MBP", "MoGen", "NutriSci", "PharmTox", "Phys", "IMS"))

# international
dat$international <- dat[['Are you an domestic or international student?']] == 'International'

q <- 'If an International student, do you think your higher tuition fees cause undue stress between you and  your supervisor?'
dat[[q]] <- ifelse(dat$international, dat[[q]], NA)

# support
q <- 'What are the sources of your additional support, if any (select all that apply)?'
dat$support.family <- grepl('Parents/Spouse/Relative', dat[[q]], ignore.case = T)
dat$support.ta <- grepl('Teaching Assistantship', dat[[q]], ignore.case = T)
dat$support.employment <- grepl('employment|work', dat[[q]], ignore.case = T)
dat$support.loans <- grepl('Loans', dat[[q]], ignore.case = T)
dat$support.savings <- grepl('Personal savings', dat[[q]], ignore.case = T)
dat$support.none <- grepl('I do not receive', dat[[q]], ignore.case = T)
# filter for those that cannot support on stipend
q <- 'Can you support your all of your day-to-day living expenses exclusively from your graduate funding (stipend/awards)?'
dat$support.family.no <- ifelse(dat[[q]] == 'No', dat$support.family, NA)
dat$support.ta.no <- ifelse(dat[[q]] == 'No', dat$support.ta, NA)
dat$support.employment.no <- ifelse(dat[[q]] == 'No', dat$support.employment, NA)
dat$support.loans.no <- ifelse(dat[[q]] == 'No', dat$support.loans, NA)
dat$support.savings.no <- ifelse(dat[[q]] == 'No', dat$support.savings, NA)
dat$support.none.no <- ifelse(dat[[q]] == 'No', dat$support.none, NA)

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

dat$scholarship.applied <- dat[['What award(s) did you apply for?']] != 'NA'

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

dat$scholarship.held <- dat[["What type of scholarship(s) did you receive?"]] != 'NA'

# scholarship value
q <- 'What was the total value of your scholarship(s) for the 2016/17 academic year?'
dat[[q]] <- gsub('\\,\\$', '', dat[[q]])
dat$sch.tot.value <- as.numeric(dat[[q]])
table(dat$sch.tot.value)
dat$sch.tot.value.held <- ifelse(dat$scholarship.held, dat$sch.tot.value, NA)

q <- 'What was the monetary value beyond your base stipend that you received as a result of winning any awards (i.e. "top-ups") for the 2016/17 academic year?'
dat$sch.topup <- as.numeric(dat[[q]])
table(dat$sch.topup)
dat$sch.topup.held <- ifelse(dat$scholarship.held, dat$sch.topup, NA)

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

# support
q <- 'What is the approximate annual monetary value of your additional support?'
dat[[q]][is.na(dat[[q]])] <- '0.0'
table(dat[[q]])

write.table(dat, file="~/Documents/grc_data/Parsed/GRC_Survey_Cleaned_2017-18.tsv", sep='\t', quote=T, row.names=F, col.names=T)
