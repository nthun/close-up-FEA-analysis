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

# Transform data
seen <- 
        stimuli_df %>% 
        gather(TestId, seen, -stimulus) %>% 
        filter(stimulus %in% films) %>% 
        drop_na() %>% 
        select(-seen) %>% 
        mutate(stimulus = stimulus %>% factor(., levels = films, labels = film_titles))

df <- 
        tom_df %>% 
        left_join(seen, by = "TestId")
        
# Showing densities
df %>% 
        select(stimulus, Affective_female:Intention_male) %>% 
        gather(tom_type, value, -stimulus) %>% 
        ggplot() +
        aes(x = value, y = tom_type) +
        geom_joy() +
        facet_wrap(~stimulus) +
        ggtitle("Density of tom categories by movie version")

glm_aff_fem <- glm(Affective_female ~ stimulus + offset(log(Length)), family = "poisson",data = df)
summary(glm_aff_fem)
AER::dispersiontest(glm_aff_fem, alternative = "greater")

# We have to run a negative binomial regression, since dispersion is 1.9 (variance is almost 2x the mean). This parameter was calculated using quasipoisson family.
glm_nb_aff_fem <- glm.nb(Affective_female ~ stimulus + offset(log(Length)), data = df)
summary(glm_nb_aff_fem)


df %>% 
        group_by(stimulus) %>% 
        summarise(Mean = mean(Affective_female, na.rm = T),
                  Se = sd(Affective_female, na.rm = T)/sqrt(n())) %>% 
        ggplot(data = .) +
                aes(x = stimulus, y = Mean, ymin = Mean - Se, ymax = Mean + Se) +
                geom_errorbar(width = .5) +
                geom_col()



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

words %>%
        summarise(sentiment = sum(sentiment, na.rm = T)) %>% # Add up sentiments into one num/tester
        left_join(df, by = "TestId") %>% 
        lm(sentiment/Length ~ stimulus, data = .) %>%
        summary()

words %>%
        summarise(sentiment = sum(sentiment, na.rm = T)) %>% # Add up sentiments into one num/tester
        left_join(df, by = "TestId") %>% 
        ggplot() +
                aes(y = stimulus, x = sentiment) +
                geom_joy() +
                ggtitle("Density of sentiment scores by film version")

words %>%
        summarise(sentiment = sum(sentiment, na.rm = T)) %>% # Add up sentiments into one num/tester
        left_join(df, by = "TestId") %>% 
        group_by(stimulus) %>% 
        summarise(Mean = mean(sentiment, na.rm = T),
                  Se = sd(sentiment, na.rm = T)/sqrt(n())) %>% 
        ggplot() +
                aes(x = stimulus, y = Mean, ymax = Mean + Se, ymin = Mean - Se) +
                geom_errorbar(width = .5) +
                geom_col() +
                ggtitle("Average sentiment scores by film version")

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
                geom_errorbar(position = "dodge") +
                geom_col(position = "dodge") +
                ggtitle("Average sentiment scores by film version, separately for sentiments")



