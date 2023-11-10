library(modelsummary)

# load the models in estimation plotting with 0.01
m2 %>% tidy %>%
  filter(term %in% c("temp","temp_2","prcp", "prcp_2")) %>%
  mutate(model = "Base",
         residual.df = m2$df.residual,
         n = m2$model %>% nrow) %>%

  bind_rows(m2.isat %>% as.lm %>% tidy %>%
              filter(term %in% c("temp","temp_2", "prcp", "prcp_2")) %>%
              mutate(model = "Base IIS",
                     residual.df = m2.isat$df,
                     n = m2.isat$n)) %>%

  bind_rows(am2 %>% tidy %>%
              filter(term %in% c("temp","temp_2","temp_int","temp_2_int","prcp", "prcp_2","prcp_int", "prcp_2_int")) %>%
              mutate(model = "Adaptation",
                     residual.df = am2$df.residual,
                     n = am2$model %>% nrow)) %>%

  bind_rows(am2.isat %>% as.lm %>% tidy %>%
              filter(term %in% c("temp","temp_2","temp_int","temp_2_int","prcp", "prcp_2","prcp_int", "prcp_2_int")) %>%
              mutate(model = "Adaptation IIS",
                     residual.df = am2.isat$df,
                     n = am2.isat$n)) %>%


  relocate(model,term, estimate, std.error, statistic, p.value, n, residual.df) %>%
  writexl::write_xlsx("JPS JoE Estimates Table 4.1.xlsx")

