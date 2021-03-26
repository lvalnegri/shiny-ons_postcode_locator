lcn.tpe <- c('Postcode [Bounding Box]' = 'PCU', 'Postcode Sector ' = 'PCS', 'Postcode District' = 'PCD', 'Post Town' = 'PCT', 'Ward' = 'WARD', 'Parish' = 'PAR')

build_list_loca <- function(x, tpe, cname = NA){
    yl <- read_fst_idx(file.path(geouk_path, 'locations'), tpe)
    if(is.na(cname)) cname <- tpe
    yl <- yl[x, on = c(location_id = cname)][, .(id = location_id, name)][order(name)]
    y <- as.list(yl$id)
    names(y) <- yl$name
    y
}

addLegendFixedCustom <- function(map, colors, labels, sizes = 20, opacity = 0.5, radius = 50, ...){
    colorAdditions <- paste0(colors, ';margin-top:4px;margin-bottom:4px;border-radius:', radius, '%;width:', sizes, 'px;height:', sizes, 'px')
    labelAdditions <- paste0(
        '<div style=display:inline-block;height:', sizes, 'px;margin-top:4px;margin-bottom:4px;line-height:', sizes, 'px;>',
        labels, 
        '</div>'
    )
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, ...))
}

get_postcodes_map <- function(id
                              
    ){
        lcn <- read_fst(file.path(geouk_path, 'locations'), as.data.table = TRUE)
        tpe <- lcn[location_id == id, as.character(type)]
        if(!tpe %in% c('PCS', 'PCD', 'PCT', 'WARD', 'PAR')) stop('Sorry, location type not implemented.')

        y <- readRDS(file.path(bnduk_path, 'postcodes', 'ch', id))
        grps <- c('Concave Hull', 'Output Areas', 'Postcodes (active)', 'Postcodes (terminated)')
        mp <- basemap(bbox = y@bbox) %>% 
            addPolygons(
                data = y,
                group = grps[1]
            )
        
        y <- readRDS(file.path(bnduk_path, 'postcodes', 'oa', id))
        mp <- mp %>% 
            addPolygons(
                data = y, 
                group = grps[2],
                color = 'green'
            )
        
        cid <- c(NA, id)
        switch(tpe,
            'PCS' = { fname <- 'pcds' },        
            'PCD' = { 
                fname <- 'pcds'
                cid <- id
            },        
            'PCT' = { fname <- 'pcat' },        
            'WARD' = { fname <- 'ladw' },        
            'PAR' = { fname <- 'ladp' }      
        )
        y <- read_fst_idx(
                file.path(geouk_path, paste0('postcodes_', fname)), 
                cid, 
                c('postcode', 'is_active', 'x_lon', 'y_lat')
        )
        mp <- mp %>% 
            addCircles(
                data = y[is_active == 1],
                lng = ~x_lon, lat = ~y_lat,
                group = grps[3],
                radius = 5,
                weight = 1,
                color = 'darkgreen',
                opacity = 1,
                fillColor = 'green',
                fillOpacity = 0.5,
                label = ~postcode
            )
        if(nrow(y[is_active == 0]) > 0){
            mp <- mp %>% 
                addCircles(
                    data = y[is_active == 0],
                    lng = ~x_lon, lat = ~y_lat,
                    group = grps[4],
                    radius = 8,
                    weight = 1,
                    color = 'darkred',
                    opacity = 1,
                    fillColor = 'red',
                    fillOpacity = 0.5,
                    label = ~postcode
                )
        } else { grps <- grps[1:3] }
        
        mp <- mp %>% 
            addLayersControl( baseGroups = names(tiles.lst), overlayGroups = grps )
        
        # mp <- mp %>% 
            

        # mp <- mp %>% 
        # 	addLegendFixedCustom(
        #         colors = c('orange', 'lightblue'),
        #         labels = c('Big Chains', 'Other Shops'),
        # 		opacity = 1,
        #         title = '',
        #         position = 'bottomright',
        # 	)
        
        mp
}
