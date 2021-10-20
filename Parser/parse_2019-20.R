## 2019-20 student finance survey analysis ###########################################################

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
file <- "~/Documents/grc_data/Raw/Faculty of Medicine Graduate Student Survey_2019_20 (Responses).xlsx"
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

# international status
dat$international <- ifelse(dat[['Are you an international or domestic student?']] == 'International', TRUE, FALSE)

# gender minority
dat$gender.minority <- ifelse(dat[['Gender']] != 'Female' & dat[['Gender']] != 'Male', TRUE, FALSE)

# degree program
dat <- replace_entry(dat, 'What degree program are you in?', '^PhD$', 'Direct-entry PhD')
dat <- replace_entry(dat, 'What degree program are you in?', '^PhD after MSc$', 'Direct-entry PhD')
dat <- replace_entry(dat, 'What degree program are you in?', 'MHSc', 'MSc')
table(dat[['What degree program are you in?']])

dat[['If MSc, do you intend to transfer to the PhD program?']] <- ifelse(dat[['What degree program are you in?']] != 'MSc', NA, dat[['If MSc, do you intend to transfer to the PhD program?']])

# international
dat$international <- dat[['Are you an international or domestic student?']] == 'International'

# support
q <- 'If you require additional monetary support for your day-to-day expenses, what are the sources? (select all that apply)'
dat$support.family <- grepl('Parents', dat[[q]], ignore.case = T)
dat$support.ta <- grepl('Teaching Assistant|T.A.', dat[[q]], ignore.case = T)
dat$support.employment <- grepl('employment|work|job', dat[[q]], ignore.case = T)
dat$support.loans <- grepl('Loans', dat[[q]], ignore.case = T)
dat$support.savings <- grepl('Personal savings', dat[[q]], ignore.case = T)
dat$support.none <- grepl('I do not receive', dat[[q]], ignore.case = T)
# filter for those that cannot support on stipend
q <- 'Can you support all of your day-to-day living expenses exclusively from your graduate funding (i.e stipend and awards)?'
dat$support.family.no <- ifelse(dat[[q]] == 'No', dat$support.family, NA)
dat$support.ta.no <- ifelse(dat[[q]] == 'No', dat$support.ta, NA)
dat$support.employment.no <- ifelse(dat[[q]] == 'No', dat$support.employment, NA)
dat$support.loans.no <- ifelse(dat[[q]] == 'No', dat$support.loans, NA)
dat$support.savings.no <- ifelse(dat[[q]] == 'No', dat$support.savings, NA)
dat$support.none.no <- ifelse(dat[[q]] == 'No', dat$support.none, NA)

# Add short dept name # no RSI
print(table(dat[["What department are you in?"]]))
dat$dept.short <- factor(dat[["What department are you in?"]], labels = c("BCHM", "Imm.", "LMP", "MBP", "MoGen", "NutriSci", "PharmTox", "Phys", "IMS"))

# area of research
# make every letter after space upper case
q <- 'What is your area of research (select up to 3 that best apply)?'
dat[[q]] <- tools::toTitleCase(dat[[q]])

# Research location
dat[["What is your primary research location?"]] <- tools::toTitleCase(dat[["What is your primary research location?"]])

table(dat[["What is your primary research location?"]])

dat[grep("undefined|undecided|TRP|capstone|off campus|n/a", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- NA
dat[grep("Mouse Imaging|MICe", x = dat[["What is your primary research location?"]]), "What is your primary research location?"] <- "The Mouse Imaging Centre"
dat[grep("Pharmacy Building", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Leslie Dan Faculty of Pharmacy"

table(dat[["What is your primary research location?"]])

# Awards applied for
dat$sch.apply <- dat[["What award(s) did you apply for to be held in the 2018-2019 academic year?"]]
# Cleaning fields with extra entries
dat$sch.apply <- gsub(pattern = ", | & R", 
                      x = dat$sch.apply,
                      replacement = ";")
# Split and convert field into list
dat$sch.apply <- strsplit(x = as.character(dat$sch.apply), split = ";")
table(unlist(dat$sch.apply))

# Replace in list
dat$sch.apply <- replace_in_list(dat$sch.apply, "already|apparently|international|could not apply|ineligible", 'Did not apply', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "entrance", 'Entrance Scholarship', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "internal", 'Department Internal Award', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "Restracomp", 'SickKids Restracomp', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "GSEF|endowment", 'Faculty of Medicine Graduate Student Endowment Fund', ignore_case = T)

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "CBS.*|Canadian Blood Services.*", x = x, replacement = "Canadian Blood Services", ignore.case = T), how = "replace")
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Banting & Best Diabetes.*|BBDC.*", x = x, replacement = "Banting & Best Diabetes Centre", ignore.case = T), how = "replace")
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "u of t.*", x = x, replacement = "UofT Awards", ignore.case = T), how = "replace")
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "MITACS.*", x = x, replacement = "MITACS", ignore.case = T), how = "replace")

table(unlist(dat$sch.apply))

dat[['What award(s) did you apply for to be held in the 2018-2019 academic year?']] <- sapply(dat$sch.apply, paste, collapse=", ")
dat <- dat[, colnames(dat) != 'sch.apply']

dat$scholarship.applied <- dat[['What award(s) did you apply for to be held in the 2018-2019 academic year?']] != 'NA' &
    dat[['What award(s) did you apply for to be held in the 2018-2019 academic year?']] != 'Did not apply'

# Awards hold
dat$sch.hold <- dat[["What type of scholarship(s) did you receive?"]]
# Cleaning fields with extra entries
dat$sch.hold <- gsub(pattern = ", | & R",
                      x = dat$sch.hold,
                      replacement = ";")
# Split and convert field into list
dat$sch.hold <- strsplit(x = as.character(dat$sch.hold), split = ";")
table(unlist(dat$sch.hold))

# Replace in list
dat$sch.hold <- replace_in_list(dat$sch.hold, "GSEF|endowment", 'Faculty of Medicine Graduate Student Endowment Fund', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "Stimulus|building|internal|MBP", 'Internal Award', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "government", 'International Award', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "entrance", 'Entrance Scholarship', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "Restracomp", 'SickKids Restracomp', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "SMH|Michael", 'St. Michael\'s Hospital', ignore_case = T)
dat$sch.hold <- replace_in_list(dat$sch.hold, "u of t|university of toronto|UofT|UTF", 'UofT Awards', ignore_case = T)

dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "CBS.*|Canadian Blood Services.*", x = x, replacement = "Canadian Blood Services", ignore.case = T), how = "replace")
dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "ALS", x = x, replacement = "ALS Canada", ignore.case = T), how = "replace")
dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "SM", x = x, replacement = "Canadian Blood Services", ignore.case = T), how = "replace")
dat$sch.hold <- rapply(dat$sch.hold, function(x) gsub(pattern = "MITACS.*", x = x, replacement = "MITACS", ignore.case = T), how = "replace")

table(unlist(dat$sch.hold))

dat[["What type of scholarship(s) did you receive?"]] <- sapply(dat$sch.hold, paste, collapse=", ")
dat <- dat[, colnames(dat) != 'sch.hold']
dat$scholarship.held <- dat[["What type of scholarship(s) did you receive?"]] != 'NA'

# scholarship value
q <- 'What was the total value of your scholarship(s) for the 2018/19 academic year?'
dat$sch.tot.value <- as.numeric(dat[[q]])
table(dat$sch.tot.value)
dat$sch.tot.value.held <- ifelse(dat$scholarship.held, dat$sch.tot.value, NA)

q <- 'What was the monetary value beyond your base stipend that you received as a result of winning any awards (i.e. "top-ups") for the 2018/19 academic year?'
dat$sch.topup <- as.numeric(dat[[q]])
table(dat$sch.topup)
dat$sch.topup.held <- ifelse(dat$scholarship.held, dat$sch.topup, NA)

# jobs
q <- 'How many hours per week do you work as a teaching assistant?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How many hours per week do you work at your side job (excluding teaching assistantships)?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

# OSAP
q <- 'Approximately what is your monthly rent and/or mortgage expenses?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How much do you spend on transit per month?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How long on average is your commute (minutes)?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How do you support yourself financially?'
dat <- replace_entry(dat, q, "loan", "Loans", ignore_case = T)

q <- 'Are you satisfied with your housing options?'
table(dat[[q]])
dat <- replace_entry(dat, q, '100|great place', 'Yes', ignore_case = T)
dat <- replace_entry(dat, q, 'somewhat|expensive|better|deal|afford|far|okay|hard|unaffordable|long-term|support', 'Could be better', ignore_case = T)
table(dat[[q]])

write.table(dat, file="~/Documents/grc_data/Parsed/GRC_Survey_Cleaned_2019-20.tsv", sep='\t', quote=T, row.names=F, col.names=T)
