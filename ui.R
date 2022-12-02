

bootstrapPage(
  
  
  htmlTemplate(filename = "www/index.html",
               panel_gen = fluidPage(
                 column(width = 3,
                        material_card(title = "",
                                      pickerInput(label = "State", inputId = "var", choices = unique(base_color$STATE_NAME),
                                                  multiple = T,
                                                  options = opts,
                                                  selected = unique(base_color$STATE_NAME)
                                      ),
                                      pickerInput(label = "Stressor type", inputId = "stress", choices = unique(stressor$stressor),
                                                  multiple = T,
                                                  options = opts,
                                                  selected = unique(stressor$stressor)
                                      ),
                                      pickerInput(label = "Year", inputId = "year", choices = unique(stressor$year),
                                                  multiple = T,
                                                  options = opts,
                                                  selected = unique(stressor$year)
                                      ),
                                      pickerInput(label = "Period of the year", inputId = "period", choices = unique(stressor$months),
                                                  multiple = T,
                                                  options = opts,
                                                  selected = unique(stressor$months)
                                      ),
                                      reactableOutput("bees_gen")
                        )
                 ),
                 column(width = 9,
                        column(
                          width = 8,
                          material_card(title = "",mapboxerOutput("mapa", width = 470, height = 820))
                        ),
                        column(width = 4,
                               material_card(title = "",
                                             echarts4rOutput("grafico1", height = 250),
                                             echarts4rOutput("grafico2", height = 250)
                                             
                                             )
                        )
                 ),
                 column(width = 12,
                        material_card(title = "",
                                      prettyRadioButtons(
                                        inputId = "type_table",
                                        label = "Group by:", 
                                        choices = c("State", "Year"),
                                        inline = T
                                      ),
                                      reactableOutput("table_conc")
                                      
                        )
                 )
               )
  )
  
  
)







