## 2021-22 student finance survey analysis ###########################################################

library(readxl)
library(data.table)

## HELPER #########################################################################################

is_multi_select <- function(dat){

}

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
file <- "~/Documents/grc_data/Raw/Faculty of Medicine Graduate Student Survey_2021_22 (Responses).xlsx"
# only read first sheet (doesn't really work)
dat <- read_excel(file, na = c("", "NA", "N/A"), sheet = 'Form Responses 1')
print(dim(dat))

# Stop if duplicated answers exist
stopifnot(!any(duplicated(dat)))
# dat <- dat[-which(duplicated(x = dat[-1])),]
# dat <- dat[-which(is.na(dat$dept)),]

# remove those with no timestamp
dat <- dat[!is.na(dat$Timestamp), ]

## CLEANING #######################################################################################

# remove semi-colon from column names
colnames(dat) <- gsub(':', '', colnames(dat))

# Add short dept name
print(table(dat[["What department are you in?"]]))
dat$dept.short <- factor(dat[["What department are you in?"]], labels = c("BCHM", "Imm.", "LMP", "MBP", "MoGen", "NutriSci", "PharmTox", "Phys", "RSI", "IMS"))

# gender identify
# table(dat[["Gender Identity"]])
dat$gender.minority <- ifelse(dat[['Gender Identity']] == 'man' | dat[['Gender Identity']] == 'woman', FALSE, TRUE)
# additional responses:
# - transmasculine
# - Trans/non-binary
# - trans, maverique

# sexual identify
# table(dat[["Gender Identity:"]])
dat$sexual.minority <- ifelse(dat[['Sexual Identity']] == 'straight (heterosexual)', FALSE, TRUE)
# additional responses
# - greyromantic

dat$racialized <- ifelse(dat[['Do you identify as a racialized person?']] == 'YES', TRUE,
                    ifelse(dat[['Do you identify as a racialized person?']] == 'NO', FALSE, NA))
# racial / ethnicity
# additional responses:
# Jewish
# Ashkenazi
# Ashkenazi Jewish
# Mediterranean (Italian, Greek, Turkish)
# White - Tatar
# Portuguese
# South Asian - Indian
# Askenazi
# Israeli
# Armenian
# Bangladeshi

# disability
# table(dat[["Do you have any disabilities?"]])
# dat <- replace_entry(dat, "Do you have any disabilities?", "Please specify", NA)
# dat <- replace_entry(dat, "Do you have any disabilities?", "Long-term", NA)

# international
dat$international <- dat[['Are you an international or domestic student?']] == 'International'
dat[['What made you decide to come to Canada for graduate school?']] <- ifelse(dat$international,
    dat[['What made you decide to come to Canada for graduate school?']], NA)
dat[['My department provided enough help with my transition and integration in Canada.']] <- ifelse(dat$international,
    dat[['My department provided enough help with my transition and integration in Canada.']], NA)
dat[['My department/faculty provided enough help with the application process for a study permit.']] <- ifelse(dat$international,
    dat[['My department/faculty provided enough help with the application process for a study permit.']], NA)
dat[['How much money do you send back home? (Annually)']] <- ifelse(dat$international,
    dat[['How much money do you send back home? (Annually)']], NA)

# support
q <- 'If you require additional monetary support for your day-to-day expenses, what are the sources?'
dat$support.family <- grepl('Parents|boyfriend', dat[[q]], ignore.case = T)
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

# transfer
dat[['If MSc, do you intend to transfer to the PhD program?']] <- ifelse(dat[['What degree program are you in?']] != 'Thesis-based Masters', NA, dat[['If MSc, do you intend to transfer to the PhD program?']])
q <- 'Does/did the lack of financial security during graduate school discourage your decision to transfer to a PhD?'
dat[[q]] <- ifelse(dat[['What degree program are you in?']] != 'Thesis-based Masters', NA, dat[[q]])

# area of research
dat <- replace_entry(dat, "What is your area of research?", "Structural biology", "Structural Biology")
dat <- replace_entry(dat, "What is your area of research?", "Structural Protein Biology", "Structural Biology")
dat <- replace_entry(dat, "What is your area of research?", "Pediatrics/ nutrition/ public health ", "Pediatrics, Nutrition, Public Health")
# make every letter after space upper case
dat[["What is your area of research?"]] <- tools::toTitleCase(dat[["What is your area of research?"]])

# Research location
# table(dat[["What is your primary research location?"]])
dat <- replace_entry(dat, "What is your primary research location?", "no location", NA)
# impossible to place UHN...
dat <- replace_entry(dat, "What is your primary research location?", "UHN", NA)
# no location
dat <- replace_entry(dat, "What is your primary research location?", "Literature-based", NA)
dat <- replace_entry(dat, "What is your primary research location?", "Unknown: haven't began", NA)
dat <- replace_entry(dat, "What is your primary research location?", "NA", NA)
dat[grep("Toronto Rehab|TRI|Rumsey", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Toronto Rehabilitation Institute"
dat[grep("General|TGH", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Toronto General Hospital"
dat[grep("500|Rehabilitation Sciences|RSI", x = dat[["What is your primary research location?"]], ignore.case = F), "What is your primary research location?"] <- "Rehabilitation Sciences Institute"
dat[grep("Leslie Dan", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Leslie Dan Faculty of Pharmacy"
dat[grep("Peter Gilgan", x = dat[["What is your primary research location?"]]), "What is your primary research location?"] <- "Hospital for Sick Children/PGCRL"
dat[grep("Holland", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Holland Bloorview"
# new
dat <- replace_entry(dat, "What is your primary research location?", "Structural Genomics Consortium", "Toronto General Hospital")
dat <- replace_entry(dat, "What is your primary research location?", "Ted Rogers Centre for Heart Research", "MaRS/PMCRT/OICR")
table(dat[["What is your primary research location?"]])

# international student reasons to Canada for grad school
# new responses
# Personal situation
# wanted to experience living…
# immigration
# Wanted to experience life in…
# important lab in my field of in…
# My advisor
# English-speaking country as…
# Not as violent as US, pays a…
# To see this side of the world…
# chasing dreams
# For my PI
# I live here

# monthly covid change
q <- "How much has your monthly income ($) changed due to the COVID-19 pandemic in 2020-2021?"
table(dat[[q]])
dat[[q]] <- as.numeric(dat[[q]])

q <- "How much have your monthly expenses ($) changed due to the COVID-19 pandemic in 2020-2021?"
table(dat[[q]])
dat[[q]] <- as.numeric(dat[[q]])

# additional monetary support
# new responses
# Awards
# Boyfriend
# city child care subsidy
# Resident top up
# OSAP does not support you…
# COVID Relief (CERB, CRB)
# disability
# gov't funding
# Note I usually take OSAP bu…
# As an international student fr…


# Awards applied for
q <- "What graduate award(s) did you apply for to be held in the 2020-2021 academic year?"
dat$sch.apply <- dat[[q]]

# Cleaning fields with extra entries
dat$sch.apply <- gsub(pattern = ", | & R",
                      x = dat$sch.apply,
                      replacement = ";")
# Split and convert field into list
dat$sch.apply <- strsplit(x = as.character(dat$sch.apply), split = ";")

# Replace in list
dat$sch.apply <- replace_in_list(dat$sch.apply, "2021|first|holding|not apply|entering|had CIHR|had the Restracomp|new|late June|not eligible|inelligible|NA", 'Did not apply', ignore_case = T)
dat$sch.apply <- replace_in_list(dat$sch.apply, "internal", 'Department &/or hospital-specific award', ignore_case = T)

dat$sch.apply <- replace_in_list(dat$sch.apply, "University-Wide", 'SGS University-Wide Awards')
dat$sch.apply <- replace_in_list(dat$sch.apply, "TPI", 'The Philanthropic Initiative Awards')
dat$sch.apply <- replace_in_list(dat$sch.apply, "homeland|external|peterborough", 'Other Awards', ignore_case = TRUE)
dat$sch.apply <- replace_in_list(dat$sch.apply, "Blood Services", 'Canadian Blood Services')
dat$sch.apply <- replace_in_list(dat$sch.apply, "CIHR Fellowship", 'CIHR Fellowship')
dat$sch.apply <- replace_in_list(dat$sch.apply, "CIMVHR", "The Canadian Institute for Military and Veteran Health Research")
dat$sch.apply <- replace_in_list(dat$sch.apply, "endMS", "Multiple Sclerosis Society of Canada")
dat$sch.apply <- replace_in_list(dat$sch.apply, "MITACS", "MITACS", ignore_case = TRUE)
dat$sch.apply <- replace_in_list(dat$sch.apply, "completion", "SGS Doctoral Completion Award", ignore_case = TRUE)
dat$sch.apply <- replace_in_list(dat$sch.apply, "VSRP", "Vision Science Research Program", ignore_case = TRUE)
dat$sch.apply <- replace_in_list(dat$sch.apply, "UTCSP", "University of Toronto Centre for the Study of Pain", ignore_case = TRUE)
table(unlist(dat$sch.apply))

dat[[q]] <- sapply(dat$sch.apply, paste, collapse=", ")
dat <- dat[, colnames(dat) != 'sch.apply']

dat$scholarship.applied <- dat[[q]] != 'Did not apply' & dat[[q]] != 'NA'

# individual scholarships
q <- 'What type of scholarship(s) did you receive during the 2020/2021 academic year?'
scholarships.won.columns <- grep(x=colnames(dat), pattern=q, fixed=TRUE)
smallDat <- dat[,scholarships.won.columns]
colnames(smallDat) <- gsub(colnames(smallDat), pat="What type of scholarship\\(s\\) did you receive during the 2020/2021 academic year\\? \\[|\\]", rep="")
dat <- cbind(dat, smallDat)
# awards received
dat$scholarship.held <- rowSums(dat[, scholarships.won.columns] == '0.0') != length(scholarships.won.columns)

# total value of scholarship
q <- "What was the total value ($) of all scholarships you received during the 2020/2021 academic year?"
dat$sch.tot.value <- as.numeric(dat[[q]])
table(dat$sch.tot.value)
dat$sch.tot.value.held <- ifelse(dat$scholarship.held, dat$sch.tot.value, NA)

# total value of topup
q <- "What was the monetary value, beyond your base stipend, that you received as a result of winning any awards (i.e. \"top-ups\") for the 2020/2021 academic year?"
dat$sch.topup <- as.numeric(dat[[q]])
table(dat$sch.topup)
dat$sch.topup.held <- ifelse(dat$scholarship.held, dat$sch.topup, NA)

# covid CERB
q <- "How much money ($) did you receive from CERB?"
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- "If you received CERB, how did it impact your financial situation?"
# dat <- replace_entry(dat, q, "CESB, not CERB", NA)
# dat <- replace_entry(dat, q, "I received and award", NA)

# did not apply to OSAP
q <- "If you DID NOT apply for OSAP, what was the reason?"
dat <- replace_entry(dat, q, "wealthy|enough savings|nonexistent|stipend covers", "I chose not to apply", ignore_case = TRUE)
dat <- replace_entry(dat, q, "ago|not eligible|Ineligible|no longer qualified", "I did not think I was eligible", ignore_case = TRUE)
dat <- replace_entry(dat, q, "province|BC|Ontario|Alberta|overseas", "I was not considered an Ontario Resident", ignore_case = TRUE)
dat <- replace_entry(dat, q, "debt|loan", "I did not want more debt")
dat <- replace_entry(dat, q, "household income|family income", "I did not think I was eligible")
dat <- replace_entry(dat, q, "not a student|in school", "I chose not to apply")
dat <- replace_entry(dat, q, "I did apply for OSAP", "N/A - I applied for OSAP")
dat <- replace_entry(dat, q, "I was in international", "I am an International student")
table(dat[[q]])

# responses that clearly applied, captured in next question


# applied but didnt get OSAP
q <- "If you APPLIED but you did not receive funds from OSAP in 2020-2021, what was the reason?"
dat <- replace_entry(dat, q, "first year", NA)
dat <- replace_entry(dat, q, "received funds", "I applied and received funds")
dat <- replace_entry(dat, q, "n/a", "N/A I did not apply")
dat <- replace_entry(dat, q, "Not eligible", "Uncertain/Do not know")
# dat <- replace_entry(dat, q, "stipend", "Stipend altered my financial eligibility")
# n/a
# Not eligible
# Ineligible due to stipend
# Not been out of high school f…
# Stipend made me ineligible
# stipend plus parent income
# was not offered loans due to…
# Currently in First Year. Recei…
# I received funds but was enr…
# I think I filled out my stipend…

# received OSAP
q <- "If you received OSAP, how did it impact your financial situation?"
dat <- replace_entry(dat, q, "receive|qualif", NA)
# Short term has helped but in…
# I did not receive OSAP
# did not receive OSAP
# I wasn't qualified for OSAP.
# Applied but did not receive l…
# No OSAP received
# positively impacted anxiety,…
# appreciated the money but i…
# Did not receive funding
# Helped pay for health relate…

# osap money
q <- "How much money ($) did you receive in OSAP loans?"
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- "How much money ($) did you receive in OSAP grants?"
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

# housing expenses
q <- "Approximately what are your individual monthly housing expenses (i.e. rent, mortgage, maintenance fees, utilities, etc.)?"
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

# transportation
q <- "What are your main modes of transportation?"
# dat <- replace_entry(dat, q, "Shuttle", 'Hospital Shuttle', ignore_case = TRUE)
# dat <- replace_entry(dat, q, "Car rental", 'Drive', ignore_case = TRUE)
# dat <- replace_entry(dat, q, "Partner drives|uber", 'Ride Share', ignore_case = TRUE)
dat <- replace_entry(dat, q, "have not taken transport", NA)
# Hospital Shuttle
# Car rental
# Hospital Shuttle Bus
# Partner drives
# Uber
# have not taken transport recent…

q <- "How much do you spend on transit per month?"
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- "How long on average is your commute (please report for one-way in minutes)?"
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

q <- "Which of the following transportation subsidy proposals would interest you?"
# table(dat[[q]])
# TTC pass with decent discount
# GO Transit pass
# Full reimbursement based o…
# I would vote for a subsidized…
# tiered subsidy based on dist…
# I submit my transit recipiet a…
# Mandatory TTC Pass in Tuiti…
# GO Pass for self-identified c…
# I could use a better GO Tran…
# GO transit pass
# Free TTC Pass for all Toront…
# BikeShare has been an exce…
# Something in partnership wit…
# Significantly discounted TTC…
# Graduate students should h…
# Don't include it int he tuition f…
# UPASS at BC universities ar…

# health care
q <- "Approximately what are your individual monthly health care expenses not covered by the U of T insurance plan (i.e. left over dental costs, emergency dental, physiotherapy, additional eye care, counselling, etc.)?"
dat[[q]] <- as.numeric(dat[[q]])
table(dat[[q]])

# opinions
q <- "Do you believe stipends should be tied to standardized metrics, i.e., the cost of living in Toronto?"
dat <- replace_entry(dat, q, "receive|qualif", NA)

print(dim(dat))

write.table(dat, file="~/Documents/grc_data/Parsed/GRC_Survey_Cleaned_2021-22.tsv", sep='\t', quote=T, row.names=F, col.names=T)
