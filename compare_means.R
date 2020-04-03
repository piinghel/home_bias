compare_means <- function(data = NULL,
                          credit_agency = "sp",
                          category = "AAA",
                          method = "t.test",
                          title = "Standard & Poor's (S&P)",
                          label_y = -14,
                          pallette = "Dark2",
                          alternative = "two.sided",
                          var_equal = FALSE,
                          return_statistic = FALSE) {
  # filter by cateogory
  summary_df <-
    data %>% select(pd_log, region, !!dplyr::sym(credit_agency)) %>% na.omit() %>%
    filter(!!dplyr::sym(credit_agency) == category)
  
  # boxplot
  p <- ggboxplot(
    data = summary_df,
    x = "region",
    y = "pd_log",
    color = "region",
    palette = pallette,
    size = .05
  ) +
    # Add p-value and and specify method
    stat_compare_means(
      method = method,
      label.y = label_y,
      method.args = list(alternative = alternative,
                         var.equal = var_equal)
    ) +
    labs(x = "Region", y = "Probability of Default (Logarithmic)") +
    ggtitle(paste(title, ":", category)) +
    theme_gray()
  
  
  
  if (return_statistic == TRUE) {
    # t test
    res <-
      t.test(as.numeric(pd_log) ~ region,
             data = summary_df,
             var.equal = var_equal)
    # make data frame
    tbl <- tibble(
      rating_agency = credit_agency,
      rating_category = category,
      t_stat = res$statistic,
      p_value = res$p.value,
      conf_lower_bound = res$conf.int[[1]],
      conf_upper_bound = res$conf.int[[2]],
      sd_error = res$stderr,
      means_europe = res$estimate[[1]],
      means_us = res$estimate[[2]],
      difference_mean = res$estimate[[1]] - res$estimate[[2]]
    )
    
    return(list(figure = p, statistics = tbl))
  }
  else{
    return(list(figure = p))
    
  }
  
}