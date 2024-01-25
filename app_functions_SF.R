summarise_sapflow <- function(x){
  create_summary_table <- list(
    Xmin = ~min(.x, na.rm = TRUE),
    Xmean = ~mean(.x, na.rm = TRUE),
    Xmax = ~max(.x, na.rm = TRUE),
    Xtotal = ~sum(.x, na.rm = TRUE)
  )

  x2 <- x %>%
    group_by(Tree, Cable) %>%
    summarise(across(where(is.numeric), create_summary_table))

  x3 <- x2 %>%
    pivot_longer(3:ncol(x2), names_to = "var", values_to = "value") %>%
    separate(var, into = c("measure", "sumstat"), sep = "X") %>%
    pivot_wider(names_from = sumstat, values_from = value) %>%
    mutate(Change = max - min) %>%
    mutate(measure = str_sub(measure, end = -2))

  return(x3)
}
