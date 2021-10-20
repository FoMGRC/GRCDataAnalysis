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
file <- "~/Documents/grc_data/Raw/Faculty of Medicine Graduate Student Survey_2020_21 (Responses).xlsx"
# only read first sheet (doesn't really work)
dat <- read_excel(file, na = c("", "NA", "N/A"), sheet = 1)
print(dim(dat))

# Stop if duplicated answers exist
stopifnot(!any(duplicated(dat)))
# dat <- dat[-which(duplicated(x = dat[-1])),]
# dat <- dat[-which(is.na(dat$dept)),]

# remove those with no timestamp
dat <- dat[!is.na(dat$Timestamp), ]

# Stop if duplicated answers exist
stopifnot(!any(duplicated(dat))) 
# dat <- dat[-which(duplicated(x = dat[-1])),]

## CLEANING #######################################################################################

# remove semi-colon from column names
colnames(dat) <- gsub(':', '', colnames(dat))

dat$racialized <- ifelse(dat[['Do you identify as a racialized person?']] == 'YES', TRUE,
                    ifelse(dat[['Do you identify as a racialized person?']] == 'NO', FALSE, NA))
# racial / ethnicity
# Taiwanese
# Mixed race
# mixed race
# Eastern European
# Jewish - I call this "racializ…
# Ashkenazi Jewish
# Afghan
# Jewish
# African
# Israeli
# Manitoban (Canada) for s…
# Caribbean/West Indian
# Indo-Caribbean
# Turkish

# disability
table(dat[["Do you have a physical disability?"]])
dat <- replace_entry(dat, "Do you have a physical disability?", "Hearing|Mental|Epilepsy|I have", "Yes, but not physical")
table(dat[["Do you have a physical disability?"]])


# Add short dept name
print(table(dat[["What department are you in?"]]))
dat$dept.short <- factor(dat[["What department are you in?"]], labels = c("BCHM", "Imm.", "LMP", "MBP", "MoGen", "NutriSci", "PharmTox", "Phys", "RSI", "IMS"))

# transfer
dat[['If MSc, do you intend to transfer to the PhD program?']] <- ifelse(dat[['What degree program are you in?']] != 'Thesis-based Masters', NA, dat[['If MSc, do you intend to transfer to the PhD program?']])
q <- 'Does/did the lack of financial security during graduate school discourage your decision to transfer to a PhD?'
dat[[q]] <- ifelse(dat[['What degree program are you in?']] != 'Thesis-based Masters', NA, dat[[q]])

# international
dat$international <- dat[['Are you an international or domestic student?']] == 'International'
dat[['What made you decide to come to Canada for graduate school?']] <- ifelse(dat$international,
    dat[['What made you decide to come to Canada for graduate school?']], NA)
dat[['My department provided enough help with my transition to Canada.']] <- ifelse(dat$international,
    dat[['My department provided enough help with my transition to Canada.']], NA)

# support
q <- 'If you require additional monetary support for your day-to-day expenses, what are the sources?'
dat$support.family <- grepl('Parents', dat[[q]], ignore.case = T)
dat$support.ta <- grepl('Teaching Assistant|T.A.', dat[[q]], ignore.case = T)
dat$support.employment <- grepl('employment|work|job', dat[[q]], ignore.case = T)
dat$support.loans <- grepl('Loans', dat[[q]], ignore.case = T)
dat$support.savings <- grepl('Personal savings', dat[[q]], ignore.case = T)
dat$support.none <- grepl('I do not receive', dat[[q]], ignore.case = T)
# filter for those that cannot support on stipend
q <- 'Can you support all of your day-to-day living expenses exclusively from your graduate funding (i.e stipend and awards/top-ups)?'
dat$support.family.no <- ifelse(dat[[q]] == 'No', dat$support.family, NA)
dat$support.ta.no <- ifelse(dat[[q]] == 'No', dat$support.ta, NA)
dat$support.employment.no <- ifelse(dat[[q]] == 'No', dat$support.employment, NA)
dat$support.loans.no <- ifelse(dat[[q]] == 'No', dat$support.loans, NA)
dat$support.savings.no <- ifelse(dat[[q]] == 'No', dat$support.savings, NA)
dat$support.none.no <- ifelse(dat[[q]] == 'No', dat$support.none, NA)


# area of research
# make every letter after space upper case
dat[["What is your area of research?"]] <- tools::toTitleCase(dat[["What is your area of research?"]])

# Research location
table(dat[["What is your primary research location?"]])

dat[grep("Baycrest|Rotman|RRI", x = dat[["What is your primary research location?"]]), "What is your primary research location?"] <- "Rotman Research Institute (Baycrest)"
dat[grep("Sick|PGCRL", x = dat[["What is your primary research location?"]]), "What is your primary research location?"] <- "Hospital for Sick Children/PGCRL"
dat[grep("Princess|mars|oicr|Ontario", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "MaRS (PMCRT/OICR)"
dat[grep("camh|addiction", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Centre for Addiction and Mental Health (CAMH)"
dat[grep("krembil", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Toronto Western Hospital/Krembil"
dat[grep("blood|CBS", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Canadian Blood Services"
dat[grep("lunenfeld", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Mount Sinai Hospital/LTRI"
dat[grep("500|Rehabilitation Sciences|RSI", x = dat[["What is your primary research location?"]], ignore.case = F), "What is your primary research location?"] <- "Rehabilitation Sciences Institute"
dat[grep("General|TGH", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Toronto General Hospital"
dat[grep("Toronto Rehab|TRI|Rumsey", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Toronto Rehab (Downtown)"
dat[grep("Leslie Dan", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Leslie Dan Faculty of Pharmacy"
dat[grep("Holland", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Holland Bloorview"

table(dat[["What is your primary research location?"]])

# make every letter after space upper case
dat[["What is your area of research?"]] <- tools::toTitleCase(dat[["What is your area of research?"]])

# Awards applied for
dat$sch.apply <- dat[["What graduate award(s) did you apply for to be held in the 2019-2020 academic year?"]]
# Cleaning fields with extra entries
dat$sch.apply <- gsub(pattern = ", | & R", 
                      x = dat$sch.apply,
                      replacement = ";")
# Split and convert field into list
dat$sch.apply <- strsplit(x = as.character(dat$sch.apply), split = ";")

# Replace in list
dat$sch.apply <- replace_in_list(dat$sch.apply, "already|funded|eligible|none", 'Did not apply', ignore_case = T)

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Alzheimer Society.*", x = x, replacement = "Alzheimer Society of Canada", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "are Disease.*", x = x, replacement = "Rare Disease Foundation", ignore.case = T), how = "replace")

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Banting & Best Diabetes.*|BBDC.*", x = x, replacement = "Banting & Best Diabetes Centre", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "CBS.*", x = x, replacement = "CBS", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "FRQS.*", x = x, replacement = "FRQS Doctoral fellowship", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "OHTN Student.*", x = x, replacement = "OHTN Student Leader Award", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Ontario Respiratory.*", x = x, replacement = "Ontario Respiratory Society Fellowship", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Ted Rogers.*", x = x, replacement = "Ted Rogers Centre for Heart Research Education Fund", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "CIHR Fellowship.*", x = x, replacement = "CIHR Fellowship", ignore.case = T), how = "replace")

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Ydessa Hendeles Memorial Scholarchip.*", x = x, replacement = "Ydessa Hendeles Memorial Scholarchip", ignore.case = T), how = "replace")

table(unlist(dat$sch.apply))

dat[["What graduate award(s) did you apply for to be held in the 2019-2020 academic year?"]] <- sapply(dat$sch.apply, paste, collapse=", ")
dat <- dat[, colnames(dat) != 'sch.apply']
# binary applied
dat$scholarship.applied <- dat[["What graduate award(s) did you apply for to be held in the 2019-2020 academic year?"]] != 'Did not apply' &
     dat[["What graduate award(s) did you apply for to be held in the 2019-2020 academic year?"]] != 'NA'

q <- 'What type of scholarship(s) did you receive during the 2019/2020 academic year?'
scholarships.won.columns <- grep(x=colnames(dat), pattern=q, fixed=TRUE)
smallDat <- dat[,scholarships.won.columns]
colnames(smallDat) <- gsub(colnames(smallDat), pat="What type of scholarship\\(s\\) did you receive during the 2019/2020 academic year\\? \\[|\\]", rep="")
dat <- cbind(dat, smallDat)
# awards received
dat$scholarship.held <- rowSums(dat[, scholarships.won.columns] == '0.0' | is.na(dat[, scholarships.won.columns])) != length(scholarships.won.columns)

q <- 'What was the total value ($) of all scholarships you received during the 2019/2020 academic year?'
dat$sch.tot.value <- as.numeric(dat[[q]])
table(dat$sch.tot.value)
dat$sch.tot.value.held <- ifelse(dat$scholarship.held, dat$sch.tot.value, NA)

q <- 'What was the monetary value beyond your base stipend that you received as a result of winning any awards (i.e. "top-ups") for the 2019/2020 academic year?'
dat$sch.topup <- as.numeric(dat[[q]])
table(dat$sch.topup)
dat$sch.topup.held <- ifelse(dat$scholarship.held, dat$sch.topup, NA)

q <- 'How much has your monthly income ($) changed due to the COVID-19 pandemic?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How much have your monthly expenses ($) changed due to the COVID-19 pandemic?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How much money did you receive in OSAP loans?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How much money did you receive in OSAP grants?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How many hours per week do you work as a teaching assistant?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'How many hours per week do you work at your side job (excluding teaching assistantships)?'
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- 'Approximately what are your individual monthly housing expenses (i.e. rent, mortgage, maintenance fees, utilities, etc.)?'
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

write.table(dat, file="~/Documents/grc_data/Parsed/GRC_Survey_Cleaned_2020-21.tsv", sep='\t', quote=T, row.names=F, col.names=T)
