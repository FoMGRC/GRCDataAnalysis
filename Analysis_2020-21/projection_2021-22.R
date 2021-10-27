## longitudinal questions ###########################################################

library(readxl)
library(data.table)
library(BoutrosLab.plotting.general)

## HELPER #########################################################################################

forward_compound <- function(value, percent){

    for (j in percent){
        value <- value * (1+j)
    }

    return(value)

}

backward_compound <- function(value, percent){

    for (j in rev(percent)){
        value <- value / (1 + j)
    }

    return(value)

}

convert_year_to_2019 <- function(value_df, cpi_df){

    before_2019 <- value_df[value_df$year < 2019, ]

    for (i in 1:nrow(before_2019)){

        before_2019$value[i] <- forward_compound(before_2019$value[i],
            cpi_df$value[before_2019$year[i] < cpi_df$year & cpi_df$year <= 2019])

    }


    at_2019 <- value_df[value_df$year == 2019, ]

    after_2019 <- value_df[value_df$year > 2019, ]

    for (i in 1:nrow(after_2019)){

        after_2019$value[i] <- backward_compound(after_2019$value[i],
            cpi_df$value[cpi_df$year > 2019 & cpi_df$year <= after_2019$year[i]])

    }


    return(c(before_2019$value, at_2019$value, after_2019$value))
}

# convert_2019_to_year <- function(value_df, cpi_df){

#     at_2019 <- value_df[value_df$year == 2019, ]

#     after_2019 <- value_df[value_df$year > 2019, ]

#     for (i in 1:nrow(after_2019)){

#         after_2019$value[i] <- forward_compound(at_2019$value,
#             cpi_df$value[cpi_df$year > 2019 & cpi_df$year <= after_2019$year[i]])

#     }

#     before_2019 <- value_df[value_df$year < 2019, ]

#     for (i in 1:nrow(before_2019)){

#         before_2019$value[i] <- backward_compound(at_2019$value,
#             cpi_df$value[before_2019$year[i] < cpi_df$year & cpi_df$year <= 2019])

#     }


#     return(c(before_2019$value, at_2019$value, after_2019$value))
# }

## DATA ###########################################################################################

work_dir <- '~/Documents/grc_data/2021-22/'

date <- Sys.Date()

setwd(work_dir)

projection_file <- "2021 External Stats.xlsx - TSV for R.tsv"
projection <- read.delim(projection_file, as.is=T)

## RANDOM #########################################################################################

mbm <- c(24911, 24794.5, 24568, 24552.5, 24652)
x <- 1:5
lm(mbm ~ x)

## PROJECT ########################################################################################

# sanity check
limat_year_df <- projection[, c('Year', 'LIM.AT.Year')]
cpi_df <- projection[, c('Year', 'CPI.Percent')]
colnames(limat_year_df) <- c('year', 'value')
colnames(cpi_df) <- c('year', 'value')
stopifnot(all.equal(projection$LIM.AT.2019, convert_year_to_2019(limat_year_df, cpi_df)))

# sanity check
mbm_year_df <- projection[, c('Year', 'MBM.Year')]
colnames(mbm_year_df) <- c('year', 'value')
stopifnot(all.equal(projection$MBM.2019, convert_year_to_2019(mbm_year_df, cpi_df)))

# phd
phd_year_df <- projection[, c('Year', 'PhD.Year')]
colnames(phd_year_df) <- c('year', 'value')
projection$PhD.2019 <- convert_year_to_2019(phd_year_df, cpi_df)

# msc
msc_year_df <- projection[, c('Year', 'MSc.Year')]
colnames(msc_year_df) <- c('year', 'value')
projection$MSc.2019 <- convert_year_to_2019(msc_year_df, cpi_df)

## PLOT ###########################################################################################

projection <- projection[, colnames(projection) !=  'CPI.Percent']
projection <- projection[, !grepl('LIM.AT', colnames(projection))]

# only plot until 2026
projection <- projection[projection$Year <= 2025, ]
projection <- projection[projection$Year >= 2015, ]

projection$order <- 1:nrow(projection)
projection_stack <- reshape2::melt(projection, id.vars = c('Year', 'order'))

# separate out stipend projections
projection_stack$variable <- as.character(projection_stack$variable)
projected_val <- projection_stack$Year > 2021 & grepl('PhD|MSc', projection_stack$variable)
projection_stack$variable[projected_val] <- paste0(projection_stack$variable[projected_val], '.Projection')
# add another 2021 for plotting
val_2021 <- projection_stack$Year == 2021 & grepl('PhD|MSc', projection_stack$variable)
projection_2021 <- projection_stack[val_2021, ]
projection_2021$variable <- paste0(projection_2021$variable, '.Projection')
projection_stack <- rbind(projection_stack, projection_2021)

projection_stack <- projection_stack[order(projection_stack$variable, projection_stack$order), ]

projection_stack$variable <- factor(projection_stack$variable,
    c('MBM.Year', 'MBM.2019', 'PhD.Year', 'PhD.2019', 'MSc.Year', 'MSc.2019',
        'PhD.Year.Projection', 'PhD.2019.Projection',
        'MSc.Year.Projection', 'MSc.2019.Projection'))

# legend
projection_key <- list(
    text = list(
        lab = c('MBM Year Adjusted', 'MBM 2019 Constant $',
        		'PhD Living Allowance', 'PhD Living Allowance 2019 Constant $',
        		'MSc Living Allowance', 'MSc Living Allowance 2019 Constant $',
                'Proposed Increase',
                'Proposed Increase 2019 Constant $'),
        cex = 1.4,
        col = c('#b471ab', '#4b297b', rep(c("#B92F5A", "#D9713E"), each = 2), '#403f3f', '#7ca9ba')
        ),
    lines = list(
        lty = rep(c(1, 2), 4),
        col = c('#b471ab', '#4b297b', rep(c("#B92F5A", "#D9713E"), each = 2), '#403f3f', '#7ca9ba'),
        lwd = 4,
        cex = 0.5
        ),
    padding.text = 3
    )

line_cols <- c('#b471ab', '#4b297b',
    rep(c("#B92F5A", "#D9713E"), each = 2),
    rep(c('#403f3f', '#7ca9ba'), 2))

create.scatterplot(
    main = 'Living Allowance vs. LIM-AT Projection',
    main.cex = 0 ,
    data = projection_stack,
    formula = value ~ order,
    groups = variable,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Dollar Value ($)',
    ylab.cex = 1.4,
    # xaxis.lab = ifelse(1:length(projection$Year) %in% seq(1, max(projection$order), 2), projection$Year, ''),
    xaxis.lab = projection$Year,
    xlimits = c(0.5, nrow(projection) + 0.5),
    xat = seq(1, max(projection$order), 1),
    xaxis.cex = 1.4,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 1.3,
    ylimits = c(16000, 41000),
    yaxis.tck = c(0.5, 0),
    type = c('p', 'l'),
    pch = rep(c(19, 15, 17, 15, 17), each = 2),
    lty = rep(c(1, 2), 10),
    col = line_cols,
    lwd = c(rep(2, 6), 4, 1, 4, 1),
    xaxis.fontface = 1,
    yaxis.fontface = 1,
    xlab.fontface = 1,
    ylab.fontface = 1,
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
	add.rectangle = TRUE,
	xleft.rectangle = c(4, 7),
	ybottom.rectangle = c(0, 0),
	xright.rectangle = c(7, 50),
	ytop.rectangle = rep(50000, 2),
	col.rectangle = c('#99C19A', '#FAE5A1'),
	alpha.rectangle = c(0.25, 0.5),
    # LEGEND
    legend = list(
        inside = list(
        	fun = draw.key,
        	args = list(key = projection_key),
            x = 0.02,
            y = 0.95)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_projection_076_2_years_then_follow', '.pdf'),
    width = 10,
    height = 6,
    resolution = 300
    )

write.table(projection_stack, paste0(date, '_projection_0635_5_years_then_follow.tsv'), col.names=T, row.names=F, quote=T, sep='\t')
