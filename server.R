

function(input, output, session) {
  
  
  
  
  
  output$mapa <- renderMapboxer({
    est_selec <- base_color |> 
      set_color()
    
    
    mapboxer(
      attributionControl = FALSE,
      style = style, token = token_mapa,
      bounds = sf::st_bbox(est_selec)) |> 
      add_source(as_mapbox_source(est_selec), id) |> 
      add_fill_layer(
        fill_opacity = 0.75,
        fill_sort_key = 1,
        fill_outline_color = "gray",
        fill_color = c("get", "color"),
        source = id
        #,popup = "<p><strong>Valor total:</strong> {{valor_mon}}</p>"
      ) |> 
      add_text_control(
        text = "<h2> Average honeycombs by state </h2>",
        pos = "top-left",
        css_text = "background: transparent; color: black"
        
      )
  })
  
  
  
  observeEvent(input$var, {
    
    
    est_selec <- base_color |> 
      tidyr::replace_na(list(ingreso_medio = 0)) |> 
      set_color(input$var)
    
    
    mapboxer_proxy("mapa") |> 
      set_data(est_selec, id) |> 
      fit_bounds(sf::st_bbox(est_selec)) |> 
      update_mapboxer()
  }) 
  
  
  
  output$bees_gen <- renderReactable({
    
    tabla <-
      stressor |> 
      filter(
        state %in% input$var &
        months %in% input$period &
        year %in% input$year &
        stressor %in% input$stress
      ) |> 
      group_by(state) |> 
      summarise(
        `% colonies afected` = mean(stress_pct, na.rm = T)
      ) |> 
      na.omit() 
    
    tabla |> 
      reactable(
        compact = T,
        highlight = T,
        bordered = T,
        defaultPageSize = 7,
        style = list(fontSize = 11),
        columns = list(
          state = colDef(header = icon("globe")),
          `% colonies afected` = colDef(
            cell = reactablefmtr::data_bars(
              data = tabla,
              text_position = 'outside-base',
              box_shadow = TRUE,
              round_edges = TRUE,
              number_fmt = scales::comma,
              fill_color = MetBrewer::met.brewer('VanGogh3'),
              bias = 1.5,
              icon = "bug",
              background = 'transparent',
              bar_height = 3
            )
          )
        )
      )
    
    
    
  })
  
  output$table_conc <- renderReactable({
    
    x <-
      bees[2:length(bees)] |> 
      filter(
        state %in% input$var &
          months %in% input$period &
          year %in% input$year 
      ) |> 
      rename(State = state, Year = year) |> 
      filter(State != "United States") |> 
      group_by_at(
        input$type_table
      ) |> 
      summarise(
        `Mean colonies` = mean(colony_n, na.rm = T),
        `Losted colonies (%)` = mean(colony_lost_pct, na.rm = T)/100,
        `Added colonies` = mean(colony_added, na.rm = T),
        `Renovated colonies (%)` = mean(colony_reno_pct, na.rm = T)/100
      ) 
    
    
    if (input$type_table == "State"){
      
      x |> 
        mutate(
          state2 =State
        ) |>
        filter(state2 != "Other States") |> 
        reactable(
          defaultColDef = colDef(
            headerStyle = list(background = "#364559", color = "#fff"),
            format = colFormat(digits = 0, separators = TRUE )
          ),
          highlight = T,
          columns = list(
            state2 = colDef(
              name = "State Geometry",
              cell = function(value){
                img_src <- knitr::image_uri(sprintf("states_plot/%s.png", value) )
                image <- img(src = img_src, height = "15px", alt = value)
                tagList(
                  div(style = list(display = "inline-block", width = "45px"), image)
                )
              }
            ),
            `Losted colonies (%)` = colDef(
              align = 'left',
              minWidth = 250,
              cell = 
                reactablefmtr::data_bars(
                  data = x,
                  fill_color = '#EEEEEE',
                  number_fmt = scales::percent,
                  text_position = 'outside-end',
                  max_value = max(x$`Losted colonies (%)`+.03),
                  min_value = min(x$`Losted colonies (%)`-.03),
                  icon = 'circle',
                  icon_color = '#a35a5d',
                  icon_size = 15,
                  text_color = '#a35a5d',
                  round_edges = TRUE
                )
            ),
            `Renovated colonies (%)` = colDef(
              align = 'left',
              minWidth = 250,
              cell = 
                reactablefmtr::data_bars(
                  data = x,
                  fill_color = '#EEEEEE',
                  number_fmt = scales::percent,
                  text_position = 'outside-end',
                  max_value = max(x$`Renovated colonies (%)`+.03),
                  min_value = min(x$`Renovated colonies (%)`-.03),
                  icon = 'circle',
                  icon_color = '#a35a5d',
                  icon_size = 15,
                  text_color = '#a35a5d',
                  round_edges = TRUE
                )
            ),
            `Mean colonies` = colDef(
              align = 'left',
              minWidth = 250,
              cell = reactablefmtr::data_bars(
                data = x,
                fill_color = '#5a7ca3',
                background = '#FFFFFF',
                bar_height = 5,
                number_fmt = scales::comma,
                text_position = 'outside-end',
                max_value = max(x$`Mean colonies`),
                icon_size = 15,
                text_color = '#000',
                round_edges = TRUE
              )
            ),
            `Added colonies` = colDef(
              align = 'left',
              minWidth = 250,
              cell = reactablefmtr::data_bars(
                data = x,
                fill_color = '#5a7ca3',
                background = '#FFFFFF',
                bar_height = 5,
                number_fmt = scales::comma,
                text_position = 'outside-end',
                max_value = max(x$`Added colonies`),
                icon_size = 15,
                text_color = '#000',
                round_edges = TRUE
              )
            )
          )
        )
      
      
    }else{
      
      x |> 
        reactable(
          defaultColDef = colDef(
            headerStyle = list(background = "#364559", color = "#fff"),
            format = colFormat(digits = 0, separators = TRUE )
          ),
          highlight = T,
          columns = list(
            Year = colDef(format = colFormat(separators = F)),
            `Losted colonies (%)` = colDef(
              align = 'left',
              minWidth = 250,
              cell = 
                reactablefmtr::data_bars(
                  data = x,
                  fill_color = '#EEEEEE',
                  number_fmt = scales::percent,
                  text_position = 'outside-end',
                  max_value = max(x$`Losted colonies (%)`+.03),
                  min_value = min(x$`Losted colonies (%)`-.03),
                  icon = 'circle',
                  icon_color = '#a35a5d',
                  icon_size = 15,
                  text_color = '#a35a5d',
                  round_edges = TRUE
                )
            ),
            `Renovated colonies (%)` = colDef(
              align = 'left',
              minWidth = 250,
              cell = 
                reactablefmtr::data_bars(
                  data = x,
                  fill_color = '#EEEEEE',
                  number_fmt = scales::percent,
                  text_position = 'outside-end',
                  max_value = max(x$`Renovated colonies (%)`+.03),
                  min_value = min(x$`Renovated colonies (%)`-.03),
                  icon = 'circle',
                  icon_color = '#a35a5d',
                  icon_size = 15,
                  text_color = '#a35a5d',
                  round_edges = TRUE
                )
            ),
            `Mean colonies` = colDef(
              align = 'left',
              minWidth = 250,
              cell = reactablefmtr::data_bars(
                data = x,
                fill_color = '#5a7ca3',
                background = '#FFFFFF',
                bar_height = 5,
                number_fmt = scales::comma,
                text_position = 'outside-end',
                max_value = max(x$`Mean colonies`),
                icon_size = 15,
                text_color = '#000',
                round_edges = TRUE
              )
            ),
            `Added colonies` = colDef(
              align = 'left',
              minWidth = 250,
              cell = reactablefmtr::data_bars(
                data = x,
                fill_color = '#5a7ca3',
                background = '#FFFFFF',
                bar_height = 5,
                number_fmt = scales::comma,
                text_position = 'outside-end',
                max_value = max(x$`Added colonies`),
                icon_size = 15,
                text_color = '#000',
                round_edges = TRUE
              )
            )
          )
        )
      
      
    }
    
    
  })
  
  
  output$grafico1 <- renderEcharts4r({
    
    bees |> 
      filter(
        state %in% input$var &
          months %in% input$period &
          year %in% input$year 
      ) |> 
      group_by(months) |> 
      summarise(
        `Colony means` = mean(colony_n, na.rm = T)
      ) |> 
      e_charts(months, dispose = FALSE) |> 
      e_bar(
        `Colony means`
      ) |> 
      e_theme("auritus") |> 
      e_tooltip(trigger = "axis",textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |> 
      e_title(
        "Number of colonies by months",
        left = "center",
        textStyle = list(
          color = "gray",
          fontFamily = "Roboto Condensed",
          fontSize = 12
        )
      ) |>
      e_color(color = "#fcba03") |> 
      e_legend(right = 1,bottom = 1, orient = "vertical",textStyle = list(fontFamily = "Roboto Condensed", 
                                                               color = "gray",
                                                               fontSize = 12))
    
    
  
    })
  
  output$grafico2 <- renderEcharts4r({
    
    stressor |> 
      filter(
        state %in% input$var &
          months %in% input$period &
          year %in% input$year &
          stressor %in% input$stress
      ) |> 
      group_by(
        stressor
      ) |> 
      summarise(
        `Stressor efectivity (%)` = round(mean(stress_pct, na.rm = T))
      ) |> 
      arrange(desc(`Stressor efectivity (%)` )) |> 
      e_charts(stressor, dispose = FALSE) |> 
      e_bar(
        `Stressor efectivity (%)`
      ) |> 
      e_theme("auritus") |> 
      e_tooltip(trigger = "axis",textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |> 
      e_title(
        "Principal stressors for bees",
        left = "center",
        textStyle = list(
          color = "gray",
          fontFamily = "Roboto Condensed",
          fontSize = 12
        )
      ) |>
      e_color(color = "#ad5b53") |> 
      e_legend(right = 1,bottom = 1, orient = "vertical",textStyle = list(fontFamily = "Roboto Condensed", 
                                                               color = "gray",
                                                               fontSize = 12))
    
    
    
  })
  
  
  
}