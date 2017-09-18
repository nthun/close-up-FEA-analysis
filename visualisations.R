# Show emotions by stimuli
emotion_occurance %>% 
        group_by(stimulus, emotion) %>% 
        summarise(Mean = mean(value, na.rm = T),
                  Se = sd(value, na.rm = T)/sqrt(n())) %>% 
        ggplot() +
        aes(x = emotion, y = Mean, group = stimulus, fill = stimulus, ymin = Mean - Se, ymax = Mean + Se) +
        geom_text(aes(label = Mean %>% round(2) %>% percent()), hjust = -.2) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(NULL, labels = percent_format(), limits = c(0,1)) +
        xlab(NULL) +
        facet_wrap(~stimulus) +
        theme_minimal() +
        theme(legend.position="bottom") +
        ggtitle("Prevalence of elicited emotion by film version")

# Emotions by stimuli by empathy group
emotion_occurance %>% 
        group_by(Empathy_group, stimulus, Close_up, emotion) %>%  
        summarise(Mean = mean(value, na.rm = T), # For 0/1 data, mean percentage = sum/n
                  Se = sd(value, na.rm = T)/sqrt(n())) %>% 
        mutate(Mean = if_else(Mean == 0, NA_real_, Mean), # 0 -> NA, to make plot nicer
               Se = if_else(Se == 0, NA_real_, Se)) %>% 
        ggplot() +
        aes(x = emotion, y = Mean, group = Empathy_group, fill = Empathy_group, ymin = Mean - Se, ymax = Mean + Se) +
        geom_errorbar(position = position_dodge()) +                
        geom_col(position = "dodge") +
        facet_wrap(~stimulus, nrow = 1) +
        coord_flip() +
        scale_y_continuous(labels=percent) +
        theme_minimal() +
        labs(x = NULL, y = NULL) +
        ggtitle("Prevalence of elicited emotion by empathy group and film version")


# Scale boxplots
narr %>% 
        gather(key, value, -TestId) %>% 
        drop_na %>% 
        ggplot() +
        aes(y = value, x = key) +
        geom_boxplot()

emp %>% 
        gather(key, value, -TestId, -Empathy_group) %>% 
        drop_na %>% 
        ggplot() +
        aes(y = value, x = key) +
        geom_boxplot()

# Enjoyment by stimuli
narr %>% 
        left_join(seen, by = "TestId") %>% 
        gather(variable, value, -TestId, -stimulus, -Close_up) %>% 
        filter(variable == "Enjoyment") %>% 
        group_by(variable, stimulus) %>% 
        summarise(Mean = mean(value, na.rm = T),
                  Sd = sd(value, na.rm = T),
                  N = n(),
                  Se = Sd/sqrt(N)) %>% 
        ggplot() +
                aes(x = stimulus, y = Mean, ymin = Mean - Se, ymax = Mean + Se, group = stimulus, fill = stimulus) +
                geom_errorbar(position = position_dodge(), width = .5) +
                geom_col(position = position_dodge()) +
                labs(y = "Mean enjoyment(SE)", x = NULL) +
                theme_minimal() 
        

# Enjoyment by stimuli by empathy group
narr %>% 
        left_join(seen, by = "TestId") %>% 
        left_join(emp %>% select(TestId, Empathy_group), by = "TestId") %>% 
        gather(variable, value, -TestId, -stimulus, -Close_up, -Empathy_group) %>% 
        filter(variable == "Enjoyment") %>% 
        group_by(variable, stimulus, Empathy_group) %>% 
        summarise(Mean = mean(value),
                  Sd = sd(value),
                  N = n(),
                  Se = Sd/sqrt(N)) %>% 
        ggplot() +
                aes(x = stimulus, y = Mean, group = Empathy_group, fill = Empathy_group) +
                geom_errorbar(aes(ymin = Mean - Se, ymax = Mean + Se), position = position_dodge(.75), width = .5) +
                geom_col(position = "dodge", width = .75) +        
                labs(y = "Mean enjoyment (SE)", x = NULL) +
                theme_minimal() +
                annotate("segment", x=c(0.75,0.75,1.25),xend=c(0.75,1.25,1.25), y= c(5,5.2,5.2), yend=c(5.2,5.2,5), size = 1.1) +
                annotate("text",x=1,y=5.36,label="p < .01") +
                annotate("segment", x=c(2.75,2.75,3.25),xend=c(2.75,3.25,3.25), y= c(5.5,5.7,5.7), yend=c(5.7,5.7,5.5), size = 1.1) +
                annotate("text",x=3,y=5.86,label="p < .05")

# Enjoyment and Sum Empathy by stimulus
narr %>% 
        left_join(seen, by = "TestId") %>% 
        left_join(emp, by = "TestId") %>% 
        ggplot() +
                aes(x = `Sum Empathy`, y = Enjoyment, linetype = stimulus, shape = stimulus, color = stimulus) +
                geom_point(size = 2) +
                geom_smooth(method  = lm, se = F, size = 1.2) +
                theme_minimal()

# Physiological reactions by stimuli based on single values by the whole stimuli
sdata %>% 
        group_by(stimulus, metric) %>% 
        summarise(Mean = mean(value),
                  Sd = sd(value),
                  N = n(),
                  Se = Sd/sqrt(N)) %>% 
        ungroup() %>% 
        ggplot() +
                aes(x = metric, y = Mean, group = stimulus, fill = stimulus) +
                geom_bar(stat = "identity", position = "dodge") +
                geom_errorbar(aes(ymax = Mean + Se, ymin = Mean -Se), position = "dodge") +
                facet_wrap(~metric, scales = "free")

