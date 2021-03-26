##################################################
# Shiny App * UK Postcodes Visualizer - server.R #
##################################################

server <- function(input, output) {

    output$ui_1st <- renderUI({
        
        switch(input$cbo_geo,
            
            'PCU' = { 
                tagList(
                    textInput('xx1_pcu', 'Enter a valid postcode:', placeholder = 'Example: SW12 8AA'),
                    tags$style('#xx1_pcu',"{background-color:#D6FFDE;}")
                )
            },
            
            { pickerInput('xx1_rgn', 'REGION:', rgns.lst, 'London') }
               
        )
        
    })
    
    output$ui_2nd <- renderUI({
        
        switch(input$cbo_geo,
            
            'PCU' = { sliderInput('xx2_pcu', 'Distance (miles):', min = 0.2, max = 2, value = 0.6, step = 0.2) },
               
            { input_xx2_lcn(input$xx1_rgn, input$cbo_geo) }
               
        )
        
        
    })
    
    dt <- eventReactive(input$btn_go, {
        
        # FIND OBJECT ----
        switch(input$cbo_geo,
            
            'PCU' = {
                if(nchar(input$xx1_pcu) <= 5) return(NULL)
                yc <- clean_postcode(data.table(postcode = input$xx1_pcu))
                yc <- pc[postcode == yc$postcode]
                if(nrow(yc) == 0){
                    return(NULL)
                } else {
                    yb <- bounding_box(yc$y_lat, yc$x_lon, as.numeric(input$xx2_pcu))
                    yd <- dts[ x_lon >= yb[1, 1] & x_lon <= yb[1, 2] & y_lat >= yb[2, 1] & y_lat <= yb[2, 2] ]
                }
            },
            
            { yd <- dts[get(input$cbo_geo) == input$xx2_lcn] }
               
        )

        # CREATE MAP ----
        
        if(input$cbo_geo == 'PCU'){
            mps <- mp %>% 
                addPulseMarkers(
                    lng = yc$x_lon, 
                    lat = yc$y_lat,
                    label = yc$postcode,
                    icon = makePulseIcon(color = 'black', iconSize = 12, animate = TRUE, heartbeat = 2)
                )
        } else {
            y <- readRDS(file.path(bnduk_path, 'postcodes', 'oa', input$xx2_lcn))
            yb <- c(y@bbox[1, 1], y@bbox[2, 1], y@bbox[1, 2], y@bbox[2, 2])
            dim(yb) <- c(2,2)
            dimnames(yb) <- list(c('lng', 'lat'), c('min', 'max'))
            mps <- mp %>% addPolygons(data = y, color = 'black', weight = 3, opacity = 0.8, fillColor = 'red', fillOpacity = 0.1)
        }
        
        mps <- mps %>%
            fitBounds(yb[1, 1], yb[2, 1], yb[1, 2], yb[2, 2]) %>%
            addMarkers(
                data = yd, 
                lng = ~x_lon, lat = ~y_lat,
                icon = ~markers[is_chain],
                markerOptions(riseOnHover = TRUE),
                label = lapply(
                    1:nrow(yd),
                    function(x)
                        HTML(paste0('<p style="font-weight:bold;color:red;font-size:14px;">', 
                                    yd[x, name], '</p>', 
                                    yd[x, address], '<br>', 
                                    yd[x, postcode], '<br>',
                                    yd[x, WARDn], ', ', yd[x, PCTn]
                        ))
                ),
                labelOptions = lbl.options,
                popup = lapply(
                    1:nrow(yd),
                    function(x)
                        HTML(paste0('<p style="font-weight:bold;font-size:14px;">
                                    <a href="http://www.google.co.uk/search?q=', 
                                    yd[x, name], '+', yd[x, address], '+', yd[x, postcode], '+', yd[x, WARDn],
                                    '" target="_blank">', 
                                    yd[x, name], '</a></p>'
                        ))
                )
                
            )
        
        # CREATE TABLE ----
        tbl <- datatable( 
            yd[, .(name, address, postcode, Ward = WARDn, Town = PCTn)],
            rownames = NULL, 
            selection = 'none',
            class = 'stripe nowrap hover compact row-border',
            extensions = c('Buttons', 'Scroller'),
            options = list(
                scrollX = TRUE,
                scrollY = 500,
                scroller = TRUE,
                buttons = c('copy', 'csv', 'print'),
                ordering = TRUE,
                deferRender = TRUE,
                dom = 'Biftp'
            )
        )
        
        # RETURN ----
        list('mps' = mps, 'tbl' = tbl, 'cnt' = nrow(yd))
        
    })
    
    output$ui_nrs <- renderText({
        if(is.null(dt())) return("The postcode you entered is invalid")
        HTML(paste('<p>Your query returned', formatC(dt()$cnt, big.mark = ','),'restaurants.</p>'))
    })

    output$out_map <- renderLeaflet({
        if(is.null(dt())) return(mp)
        dt()$mps
    })
    
    output$out_txt <- renderText({
        ifelse(is.null(dt()), 'The text you entered is not a postcode or postcode not found', '')
    })
    
    output$out_tbl <- renderDT({
        if(is.null(dt())) return(NULL)
        dt()$tbl
    })

}