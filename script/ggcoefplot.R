ggcoefplot <- function(model){
        broom.mixed::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>% 
                filter(!str_detect(term, "Intercept")) %>% 
                mutate(term = fct_reorder(term, estimate)) %>% 
                ggplot() +
                aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high) +
                geom_pointrange() +
                scale_y_log10() +
                coord_flip() +
                geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
                labs(y = "Odds ratio",
                     x = NULL,
                     title = "Coefficient plot showing ORs for TOM")
}
