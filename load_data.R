######################################
# Shiny App * UK Eat Out - Load Data #
######################################
# this script should be run as a cronjob in August only:
# 0 12-18 * 8 * Rscript --no-save --no-restore --verbose /home/datamaps/shiny/shiny-uk_eatout/load_data.R > out.Rout 2 > /home/datamaps/cronjobs/uk_eatout.out

pkgs <- c('popiFun', 'data.table', 'fst')
lapply(pkgs, require, char = TRUE)

add_loca_name <- function(dt, geo, keep.codes = TRUE, coords = FALSE, saf = TRUE){
    yn <- names(dt)
    cols <- c('type', 'location_id', 'name')
    if(coords) cols <- c(cols, 'x_lon', 'y_lat')
    y <- read_fst(file.path(geouk_path, 'locations'), columns = cols, as.data.table = TRUE)
    y <- y[type == geo][, type := NULL][, name := factor(name)]
    cols <- c('id', geo)
    if(coords) cols <- c(cols,  paste0(geo, c('x', 'y')))
    setnames(y, cols)
    dt <- y[dt, on = c(id = geo)]
    if(keep.codes){
        if(saf) dt[, id := factor(id)]
        setnames(dt, c('id', geo), c(geo, paste0(geo, 'n')))
        setcolorder(dt, c(yn[1:which(yn == geo)], paste0(geo, 'n'), yn[(which(yn == geo) + 1):length(yn)]))
    } else {
        dt[, id := NULL]
        setnames(dt, geo, paste0(geo, 'n'))
    }
    dt
}

y <- fread(
        'https://raw.githubusercontent.com/hmrc/eat-out-to-help-out-establishments/master/data/participating-establishments/restaurants.csv',
        na.strings = ''
)
setnames(y, tolower(names(y)))
y[name == `line 1`, `:=`(`line 1` = `line 2`, `line 2` = NA )]

y <- capitalize(y, 'name')
y <- capitalize(y, 'line 1')
y <- capitalize(y, 'line 2')
y[, address := paste0(trimws(`line 1`), ifelse(is.na(`line 2`), '', paste(',', trimws(`line 2`))))][is.na(`line 1`), address := '']
y[, `:=`(`line 1` = NULL, `line 2` = NULL )]
cols <- c('WARD', 'PAR', 'PCS', 'PCD')
pc <- read_fst(file.path(geouk_path, 'postcodes'), columns = c('postcode', cols), as.data.table = TRUE)
pc[y, on = 'postcode']

cols <- c(cols, 'PCT')
for(t in cols) y <- add_loca_name(y, t)
y[, c('town', 'county') := NULL]
#===
pc <- read_fst(file.path(geouk_path, 'postcodes'), columns = c('postcode', 'x_lon', 'y_lat'), as.data.table = TRUE)
y <- pc[y, on = 'postcode'][order(postcode)]
y[, npc := 1:.N, postcode][, overlaps := 0]
y[npc %% 2 == 0, overlaps := npc / 2 ]
y[npc > 1 & npc %% 2 > 0, overlaps := -(npc - 1) / 2 ]
y[, `:=`( x_lon = x_lon + overlaps * 0.00005, y_lat = y_lat + overlaps * 0.00005 )][, c('npc', 'overlaps') := NULL]
#===
setcolorder(y, c('name', 'address'))

y[, is_chain := 'blue']
txt <- c(
    'burger ?king', 'costa coffee', 'domino', '^gusto italian$', 'kfc', 'm.?c.?donald', 
    'papa johns', '^pret.*manger$', 'starbucks', 'subway', 'taco bell', 'wasabi'
)
for(tx in txt) y[grepl(tx, tolower(name)), is_chain := 'red']

write_fst(y, file.path(app_path, 'uk_eatout', 'dataset'))

#===
# bnd <- list()
# for(x in cols){
#     y <- readRDS(file.path(bnduk_path, 'rds', 's05', x))
#     bnd[[x]] <- y
# }
# saveRDS(bnd, file.path(app_path, 'uk_eatout', 'boundaries'))
#===

rm(list = ls())
gc()

