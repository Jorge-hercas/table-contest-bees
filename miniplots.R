








x <- 
states |> 
  left_join(
    bees |> 
      group_by(state) |> 
      summarise(
        total = mean(colony_n, na.rm = T)
      ),
    by = c("STATE_NAME" = "state")
  ) |> 
  mutate(
    color = scales::col_numeric("Spectral", domain = total)(total)
  )


for (i in 1:nrow(x)){
 
  y <- 
    x[i,]
  
  plot <- 
    y |> 
    ggplot() +
    geom_sf(color = "grey", fill = alpha(y$color,0.4)) +
    theme_void()
  
  
  ggsave(filename = paste0("states_plot/",y$STATE_NAME, ".png"),plot = plot) 
  
  
}






