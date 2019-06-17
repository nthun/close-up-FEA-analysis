options(encoding = "UTF-8")

# library(MASS)
library(tidyverse)
library(tidytext)
library(hunspell)
library(lmerTest)
library(ggbeeswarm)

source("script/calclulate_overdispersion.R")
source("script/ggcoefplot.R")

theme_set(theme_light())

# Read data
tom_df <- readxl::read_xlsx("data/tom_final.xlsx", 1)
stimuli_df <- read_csv("data/00_stimuli_presentation_order_per_tester_default.csv")

# Dictionaries
hungarian_dict <- dictionary("data/dictionaries/hu_HU.dic")
hungarian_stopwords <- read_lines("data/dictionaries/hungarian_stopwords.dat") %>% 
                        tibble(stem = .)
# Load sentiment dictionaries (downloaded from http://opendata.hu/dataset/hungarian-sentiment-lexicon, 2016/07/08)
senti_pos <- read_lines("data/dictionaries/PrecoPos.txt") %>% 
                tibble(stem = ., sentiment = 1L)
senti_neg <- read_lines("data/dictionaries/PrecoNeg.txt") %>% 
                tibble(stem = ., sentiment = -1L)
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
        mutate(Close_up = if_else(stimulus == films[1], cu_levels[1], cu_levels[2]) %>%
                          factor(levels = cu_levels),
               stimulus = factor(stimulus, levels = films, labels = film_titles))

df <- 
        tom_df %>% 
        left_join(seen, by = "TestId") %>% 
        rename(characters = Length) %>% 
        mutate(words = str_count(Story, '\\w+'))

df_long <-
        df %>% 
        gather(tom_type, value, Affective_female:Intention_male) %>% 
        separate(tom_type, c("tom_subtype", "object"), sep = "_",remove = FALSE) %>% 
        mutate(object = fct_relevel(object, "male"),
               tom_subtype = fct_relevel(tom_subtype, "Intention"))

df_long %>% 
        mutate(tom_type = fct_reorder(tom_type, value)) %>% 
        ggplot() +
        aes(x = tom_type, y = value, fill = tom_type) +
        geom_boxplot(width = .5, outlier.size = 1) +
        geom_quasirandom(size = 1, alpha = .3) +
        scale_fill_viridis_d() +
        coord_flip() +
        facet_wrap(~stimulus) +
        labs(x = NULL,
             title = "Density of tom categories by movie version")
       
# Distribution of TOM
df_long %>% 
        group_by(stimulus, TestId, characters, words) %>% 
        summarise(tom = sum(value)) %>% 
        ggplot() +
        aes(x = tom) +
        geom_histogram(binwidth = .5)

# Distribution of words
df %>% 
        ggplot() +
        aes(x = log(words)) +
        geom_histogram(bins = 20)

# Association between words and characters
ggplot(df) +
        aes(x = words, y = characters) +
        geom_point()


# Hypothesis tests ------------------------------------------------------------------
# All TOM
all_tom_pois <-
        df_long %>% 
        group_by(stimulus, TestId, characters, words) %>% 
        summarise(tom = sum(value)) %>% 
        glm(tom ~ stimulus, 
            offset = log(words), 
            family = "poisson", 
            data = .)

summary(all_tom_pois)
broom.mixed::tidy(all_tom_pois, exponentiate = TRUE, conf.int = TRUE)
calclulate_overdispersion(all_tom_pois) # Calculate overdiscpersion using Bolker's function
# There is a considerable overdispersion, so we need to use negative binomial regression
# We have to run a negative binomial regression, since dispersion is significantly larger than 1 (variance is 1.4x the mean). 

all_tom_nb <-
        df_long %>% 
        group_by(stimulus, TestId, characters, words) %>% 
        summarise(tom = sum(value)) %>% 
        MASS::glm.nb(tom ~ stimulus + 
                     offset(log(words)), 
            data = .)

summary(all_tom_nb)
broom.mixed::tidy(all_tom_nb, exponentiate = TRUE, conf.int = TRUE)

# TOM by object
object_tom_pois <-
        df_long %>% 
        group_by(stimulus, TestId, object, characters, words) %>% 
        summarise(tom = sum(value)) %>% 
        glmer(tom ~ stimulus * object + (1|TestId), 
            offset = log(words), 
            family = "poisson", 
            data = .)

summary(object_tom_pois)
broom.mixed::tidy(object_tom_pois, exponentiate = TRUE, conf.int = TRUE)
calclulate_overdispersion(object_tom_pois) # No overdispersion

ggcoefplot(object_tom_pois)

# TOM by object and type
type_tom_pois <-
        df_long %>% 
        group_by(stimulus, TestId, object, tom_subtype, characters, words) %>% 
        summarise(tom = sum(value)) %>% 
        glmer(tom ~ stimulus * object * tom_subtype + (1|TestId), 
              offset = log(words),
              family = "poisson", 
              data = .)

summary(type_tom_pois)
broom.mixed::tidy(type_tom_pois, exponentiate = TRUE, conf.int = TRUE)

ggcoefplot(type_tom_pois)

calclulate_overdispersion(type_tom_pois) # No overdispersion
        

## Preparing the text for sentiment analysis
# The text is already annotated by magyarlanc API, so we just load the files
words <- 
        tibble(file = list.files("data/magyarlanc annotation")) %>% 
        rowwise() %>% 
        mutate(annotation = map(., ~read_tsv(paste0("data/magyarlanc annotation/", file), col_names = c("word", "stem", "type", "full_annot")), trim_ws = TRUE, encoding = "UTF-8")) %>% 
        mutate(TestId = str_remove(file, ".txt")) %>% 
        ungroup() %>% 
        unnest(annotation) %>% 
        filter(!type %in% c("PUNCT","X","_","CONJ","INTJ","NUM")) %>% # Remove punctuation and others
        # anti_join(hungarian_stopwords, by = "stem") %>% # Remove hungarian stopwords
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
        lm(sentiment/characters ~ stimulus, data = .) %>%
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
        left_join(tom_df %>% select(TestId, characters)) %>% 
        glm.nb(positive ~ stimulus + offset(log(characters)), data = .) %>%
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




