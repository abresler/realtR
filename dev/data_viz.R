data <- 
  get_mortage_rates(return_wide = F)

hchart(data,
       "spline",
       hcaes(x = dateData, y = value, group = typeRate)) %>%
  hc_tooltip(sort = TRUE, table = TRUE) %>%
  hc_legend(align = "left",
            layout = "vertical",
            verticalAlign = "top") %>%
  hc_tooltip(sort = TRUE, table = TRUE)