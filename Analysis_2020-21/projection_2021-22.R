## longitudinal questions ###########################################################

library(readxl)
library(data.table)
library(BoutrosLab.plotting.general)

## HELPER #########################################################################################

## DATA ###########################################################################################

work_dir <- '~/Documents/grc_data/Projection/'

date <- Sys.Date()

setwd(work_dir)

projection_file <- "2021 External Stats.xlsx - TSV for R.tsv"
projection <- read.delim(projection_file, as.is=T)

## PLOT ###########################################################################################

projection$order <- 1:nrow(projection)
projection_stack <- reshape2::melt(projection, id.vars = c('Year', 'order'))

projection_stack$group <- gsub('(.*)\\.(.*)', '\\1', projection_stack$variable)
projection_stack$type <- gsub('(.*)\\.(.*)', '\\2', projection_stack$variable)
projection_stack$group <- factor(projection_stack$group, c('LIM.AT', 'PhD', 'MSc'))
projection_stack$type <- factor(projection_stack$type, c('2019', 'Year'))

# legend
projection_key <- list(
    text = list(
        lab = c('LIM-AT Year Adjusted', 'LIM-AT 2019 Constant $',
        		'PhD Living Allowance', 'PhD Living Allowance 2019 Constant $',
        		'MSc Living Allowance', 'MSc Living Allowance 2019 Constant $'),
        cex = 0.8,
        col = rep(default.colours(3, 'spiral.morning'), each = 2)
        ),
    lines = list(
        lty = rep(c(1, 2), 3),
        col = rep(default.colours(3, 'spiral.morning'), each = 2),
        lwd = 2,
        cex = 0.5
        ),
    points = list(
        pch = rep(c(1, 0, 2), each = 2),
        col = rep(default.colours(3, 'spiral.morning'), each = 2),
        cex = 0.5
        ),
    padding.text = 2
    )

create.scatterplot(
    main = 'Living Allowance vs. LIM-AT Projection',
    main.cex = 1,
    data = projection_stack,
    formula = value ~ order,
    groups = variable,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Dollar Value ($)',
    ylab.cex = 1,
    xaxis.lab = projection$Year,
    xlimits = c(0.5, nrow(projection) + 0.5),
    xat = projection$order,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(16000, 31000),
    yaxis.tck = c(0.5, 0),
    type = c('p', 'l'),
    pch = rep(c(1, 0, 2), each = 2),
    lty = rep(c(2, 1), 3),
    col = rep(default.colours(3, 'spiral.morning'), each = 2),
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
	xleft.rectangle = c(6, 9),
	ybottom.rectangle = c(0, 0),
	xright.rectangle = c(9, 13),
	ytop.rectangle = rep(35000, 2),
	col.rectangle = c('yellowgreen', 'grey75'),
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
    filename = paste0(date, '_projection_yearly_vs_2019', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )
