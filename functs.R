






set_color <- function(base_color, .ESTADO = elecciones ) {
  if (length(.ESTADO) == 1){
    base_color |> 
      filter(STATE_NAME %in% .ESTADO) |> 
      mutate(color = "#467582" )
  }else{
    base_color |> 
      filter(STATE_NAME %in% .ESTADO) |> 
      tidyr::replace_na(list(mean_col=0)) |> 
      mutate(color = scales::col_numeric("Blues", mean_col, na.color = "gray")(mean_col))
  }
}



