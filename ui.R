##############################################
# Shiny App * UK Postcodes Visualizer - ui.R #
##############################################

fluidPage(

    titlePanel('UK Postcodes Visualizer'),

    sidebarLayout(

        sidebarPanel(

            pickerInput('cbo_geo', 'GEOGRAPHY:', lcn.tpe),

            uiOutput('ui_1st'),

            uiOutput('ui_2nd'),

            tags$br(),
            column(12, 
                actionButton('btn_go', 'DRAW MAP',
                    icon = icon('search-location'),
                    style = "color:#fff;background-color:#337ab7;border-color:#2e6da4;font-weight:600"
                ),
                align = 'center', style = "margin-bottom:30px;margin-top:-10px"
            ), 
            
            uiOutput('ui_nrs'),
            HTML(paste('<hr><p>The dataset has been updated on AUG-20 and contains', formatC(nrow(dts), big.mark = ','), 'postcodes.</p>')),
            HTML(
                '<p>Data: <a href="https://bit.ly/hmrc_eatout" target="_blank">HMRC</a></p>',
                '<p>App Code: <a href="https://bit.ly/datamaps-uk_eatout_github" target="_blank">GitHub</a></p>
                 <br>',
                
                '<p>Contains <a href="https://bit.ly/ONS_geoportal" target="_blank">OS</a>
                    and <a href="https://bit.ly/royal_mail_paf" target="_blank">Royal Mail</a> data.</p>
                 <p>All content is available under the 
                    <a href="https://bit.ly/open_gov_licence_v3" target="_blank">
                        Open Government Licence v3.0
                    </a></p>
                 <p>&copy; Crown copyright and database rights [2020]</p>
                 <p>&copy; Royal Mail copyright and database rights [2020]</p>
                 <br>',
                
                '<p>Postcode Sectors and Districts Polygons are only approximations of the real ones, here depicted as suitable unions of
                    <a href="https://bit.ly/ons-output_areas" target="_blank">ONS Output Areas</a> 
                    (see the <a href="https://bit.ly/datamaps-create_parent_boundaries" target="_blank">relevant code</a>).
                    Hence some shop could fall outside the designated polygon (for more technical information, see FAQs in the <a href="https://bit.ly/onspd-user_guide" target="_blank">ONSPD User Guide</a>).</p>
                 <br>',
                
                '<p>Post Towns have been built using <a href="https://bit.ly/wiki-postcode_areas" target="_blank">Wikipedia resources</a>, and are based on Postcode Districts.</p>'
            ),

            width = 3

        ),

        mainPanel(

            tabPanel('Map', tags$br(), withSpinner(leafletOutput('out_map', height = '600px') ) ),

        )

    )

)
