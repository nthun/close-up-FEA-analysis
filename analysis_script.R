options(encoding = "UTF-8")

library(MASS)
library(tidyverse)
library(magrittr)
library(ggjoy)
library(tidytext)
library(hunspell)

# Read data
tom_df <- readxl::read_xlsx("data/tom_final.xlsx", 1)
stimuli_df <- read_csv("data/00_stimuli_presentation_order_per_tester_default.csv")

# Dictionaries
hungarian_dict <- dictionary("data/dictionaries/hu_HU.dic")
hungarian_stopwords <- read_lines("data/dictionaries/hungarian_stopwords.dat") %>% data_frame(stem = .)
# Load sentiment dictionaries (downloaded from http://opendata.hu/dataset/hungarian-sentiment-lexicon, 2016/07/08)
senti_pos <- read_lines("data/dictionaries/PrecoPos.txt") %>% data_frame(stem = ., sentiment = 1L)
senti_neg <- read_lines("data/dictionaries/PrecoNeg.txt") %>% data_frame(stem = ., sentiment = -1L)
senti <- bind_rows(senti_pos, senti_neg)

films <- c("FD_0CU", "FD_5CU", "FD_10CU")
film_titles <- c("0 close-ups", "5 close-ups", "10 close-ups")
cu_levels <- c("No close-ups", "Contains close-ups")

# Transform data
seen <- 
        stimuli_df %>% 
        gather(TestId, seen, -stimulus) %>% 
        filter(stimulus %in% films) %>% 
        drop_na() %>% 
        select(-seen) %>% 
        mutate(Close_up = if_else(stimulus == films[1], cu_levels[1], cu_levels[2]) %>% factor(levels = cu_levels),
               stimulus = stimulus %>% factor(., levels = films, labels = film_titles))

df <- 
        tom_df %>% 
        left_join(seen, by = "TestId")
        
# Showing densities
df %>% 
        select(Close_up, Affective_female:Intention_male) %>% 
        gather(tom_type, value, -Close_up) %>% 
        ggplot() +
        aes(x = value, y = tom_type) +
        geom_joy() +
        facet_wrap(~Close_up) +
        ggtitle("Density of tom categories by movie version")

glm_aff_fem <- glm(Affective_female ~ stimulus + offset(log(Length)), family = "poisson",data = df)
summary(glm_aff_fem)
glm_aff_fem <- glm(Affective_female ~ Close_up + offset(log(Length)), family = "poisson",data = df)
summary(glm_aff_fem)
AER::dispersiontest(glm_aff_fem, alternative = "greater")

# We have to run a negative binomial regression, since dispersion is 1.9 (variance is almost 2x the mean). This parameter was calculated using quasipoisson family.
glm_nb_aff_fem <- glm.nb(Affective_female ~ stimulus + offset(log(Length)), data = df)
glm_nb_aff_fem <- glm.nb(Affective_female ~ Close_up + offset(log(Length)), data = df)
summary(glm_nb_aff_fem)
anova(glm_nb_aff_fem)

df %>% 
        group_by(stimulus) %>% 
        summarise(Mean = mean(Affective_female, na.rm = T),
                  Se = sd(Affective_female, na.rm = T)/sqrt(n())) %>% 
        ggplot(data = .) +
                aes(x = stimulus, y = Mean, ymin = Mean - Se, ymax = Mean + Se) +
                geom_errorbar(width = .5) +
                geom_col() +
                theme_minimal() +
                labs(x = NULL, y = "Affective mentalizaton to the female character") +
                ggtitle("Differences in the mean affective mentalization response to the female character (SE)")
        

## Preparing the text for sentiment analysis
# The text is already annotated by magyarlanc API, so we just load the files
words <- 
        data_frame(file = list.files("data/magyarlanc annotation")) %>% 
        rowwise() %>% 
        mutate(annotation = map(., ~read_tsv(paste0("data/magyarlanc annotation/", file), col_names = c("word", "stem", "type", "full_annot")), trim_ws = TRUE, encoding = "UTF-8")) %>% 
        mutate(TestId = file %>% gsub(".txt","", .)) %>% 
        ungroup() %>% 
        unnest(annotation) %>% 
        filter(!type %in% c("PUNCT","X","_","CONJ","INTJ","NUM")) %>% # Remove punctuation and others
        anti_join(hungarian_stopwords, by = "stem") %>% # Remove hungarian stopwords
        rowwise() %>% 
        mutate( spelling = hunspell_check(stem, dict = hungarian_dict), # Spell check
                stem = if_else(!spelling,  # For incorrect spelling, use the first suggestion
                               hunspell_suggest(words = stem, dict = hungarian_dict)[[1]][1],
                               stem
                               ),
                corrected = !spelling
        ) %>%
        left_join(senti, by = "stem") %>% # Add sentiment lexicon
        select(-file, -full_annot, -spelling) %>%
        group_by(TestId)
# TODO: Correct the words BEFORE taking apart with magyarlanc


# Average setiment by stimuli
words %>%
        summarise(sentiment = sum(sentiment, na.rm = T)) %>% # Add up sentiments into one num/tester
        left_join(df, by = "TestId") %>% 
        lm(sentiment/Length ~ stimulus, data = .) %>%
        summary()

words %>%
        select(TestId, word, sentiment) %>% 
        drop_na(sentiment) %>% 
        mutate(sentiment = recode(sentiment, `1` = "positive", `-1` = "negative")) %>% 
        left_join(seen) %>% 
        group_by(TestId, stimulus) %>% 
        count(sentiment) %>% 
        spread(sentiment, n) %>% 
        mutate_at(vars(negative, positive), funs(if_else(is.na(.), 0L, .))) %>% 
        left_join(tom_df %>% select(TestId, Length)) %>% 
        glm.nb(positive ~ stimulus + offset(log(Length)), data = .) %>%
        summary()
        
        
words %>%
        summarise(sentiment = sum(sentiment, na.rm = T)) %>% # Add up sentiments into one num/tester
        left_join(df, by = "TestId") %>% 
        ggplot() +
                aes(y = stimulus, x = sentiment) +
                geom_joy() +
                ggtitle("Density of sentiment scores by film version") +
                theme_minimal()

words %>%
        summarise(sentiment = sum(sentiment, na.rm = T)) %>% # Add up sentiments into one num/tester
        left_join(df, by = "TestId") %>% 
        group_by(Close_up, stimulus) %>% 
        summarise(Mean = mean(sentiment, na.rm = T),
                  Se = sd(sentiment, na.rm = T)/sqrt(n())) %>% 
        ggplot() +
                aes(x = stimulus, y = Mean, ymax = Mean + Se, ymin = Mean - Se) +
                geom_errorbar(width = .5) +
                geom_col() +
                ggtitle("Average sentiment scores by film version") +
                theme_minimal() +
                labs(x = NULL, y = "Mean sentiment score")

words %>% 
        count(sentiment) %>% # Count the sentiment data (separately for both sentiments)
        left_join(df, by = "TestId") %>% 
        mutate(sentiment = sentiment %>% factor(levels = c(-1L,1L), labels = c("Negative","Positive"))) %>% 
        drop_na(sentiment) %>% 
        ggplot() +
                aes(x = n, y = stimulus, fill = sentiment) +
                geom_joy() +
                facet_wrap(~sentiment) +
                ggtitle("Density of negative and positive sentiment scores by film version")

words %>% 
        count(sentiment) %>% # Count the sentiment data (separately for both sentiments)
        left_join(df, by = "TestId") %>% 
        mutate(sentiment = sentiment %>% factor(levels = c(-1L,1L), labels = c("Negative","Positive"))) %>% 
        group_by(stimulus, sentiment) %>% 
        summarise(Mean = mean(n, na.rm = T),
                  Se = sd(n, na.rm = T)/sqrt(n())) %>% 
        drop_na(sentiment) %>% 
        ggplot() +
                aes(x = stimulus, y = Mean, ymax = Mean + Se, ymin = Mean - Se, group = sentiment, fill = sentiment) +
                geom_errorbar(position = "dodge", width = .5) +
                geom_col(position = "dodge", width = .5) +
                ggtitle("Average sentiment scores by film version, separately for sentiments") +
                labs(x = NULL, y = "Mean aggregated sentiment") +
                theme_minimal()




