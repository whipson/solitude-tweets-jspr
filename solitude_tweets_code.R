#' This script reproduces the results obtained in Table 1 and all figures.
#' Due to the privacy agreement with other authors, we do not share the raw tweets that produced these results. 
#' Upon reasonable request, we will share this data with other researchers.

# Load tidyverse library for data manipulation and plotting functions
# install.packages("tidyverse")
library(tidyverse)
# Load ggpubr for arranging plots
# install.packages("ggpubr")
library(ggpubr)

# Load data. Each .csv file is gives the comparative PMI scores between pairs of solitude, lonely, and alone.
# Note: These files must be in your working directory for the code below to run properly!

# Original data

solitude_v_lonely <- read_tsv("lexicon-solitude-vs-lonelyall-words-noNEG-T+T25-v2.txt") %>%
  rename(pmi = 'PMI score',
         freq_neg = 'Freq. in NEG',
         freq_pos = 'Freq. in POS',
         word = Term) %>%
  mutate(frequency = freq_pos + freq_neg)

solitude_v_alone <- read_tsv("lexicon-solitude-vs-aloneall-words-noNEG-T+T25-v2.txt") %>%
  rename(pmi = 'PMI score',
         freq_neg = 'Freq. in NEG',
         freq_pos = 'Freq. in POS',
         word = Term) %>%
  mutate(frequency = freq_pos + freq_neg)

lonely_v_alone <- read_tsv("lexicon-lonelyall-vs-aloneall-words-noNEG-T+T25-v2.txt") %>%
  rename(pmi = 'PMI score',
         freq_neg = 'Freq. in NEG',
         freq_pos = 'Freq. in POS',
         word = Term) %>%
  mutate(frequency = freq_pos + freq_neg)

# filtered alone phrases
#' See appendix. Our comparison with 'alone' tweets focused mostly on those which used 'alone' in specific key phrases
#' This ensured that we did not cast too broad a net and included tweets that used 'alone' in a different context

solitude_v_alone2 <- read_tsv("lexicon-solitude-vs-aloneallfilt-words-noNEG-T+T25-v2.txt") %>%
  rename(pmi = 'PMI score',
         freq_neg = 'Freq. in NEG',
         freq_pos = 'Freq. in POS',
         word = Term) %>%
  mutate(frequency = freq_pos + freq_neg)

lonely_v_alone2 <- read_tsv("lexicon-lonelyall-vs-aloneallfilt-words-noNEG-T+T25-v2.txt") %>%
  rename(pmi = 'PMI score',
         freq_neg = 'Freq. in NEG',
         freq_pos = 'Freq. in POS',
         word = Term) %>%
  mutate(frequency = freq_pos + freq_neg)

## Lexica ##

# VAD (https://saifmohammad.com/WebPages/nrc-vad.html)
#' Download VAD from the above link and place files into working directory.

valence <- read_tsv("v.scores", col_names = FALSE) %>%
  rename(word = "X1",
         valence = "X2")

arousal <- read_tsv("a.scores", col_names = FALSE) %>%
  rename(word = "X1",
         arousal = "X2")

dominance <- read_tsv("d.scores", col_names = FALSE) %>%
  rename(word = "X1",
         dominance = "X2")

# We combine each dimension into one dataframe for ease of use.

vad <- valence %>%
  inner_join(arousal) %>%
  inner_join(dominance)

# Emotion Density (http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm)
#' Download NRC EL from link above and place files into working directory

emo <- read_tsv("emolex.txt", col_names = FALSE) %>%
  rename(word = X1, dimension = X2, score = X3) %>%
  pivot_wider(id_cols = word, names_from = dimension, values_from = score) # each emotion becomes a column

# Joining Lexica with Data set
#' We run a series of left_joins (dplyr) to associate each word in the corpus with the same word and score in the lexica.
#' Words not present in the lexica receive a score of NA (missing)

solitude_v_lonely <- solitude_v_lonely %>%
  left_join(vad) %>%
  left_join(emo) %>%
  select(word, pmi, valence, arousal, dominance, anger, fear, sadness, joy, frequency)

solitude_v_alone <- solitude_v_alone %>%
  left_join(vad) %>%
  left_join(emo) %>%
  select(word, pmi, valence, arousal, dominance, anger, fear, sadness, joy, frequency)

lonely_v_alone <- lonely_v_alone %>%
  left_join(vad) %>%
  left_join(emo) %>%
  select(word, pmi, valence, arousal, dominance, anger, fear, sadness, joy, frequency)

solitude_v_alone2 <- solitude_v_alone2 %>%
  left_join(vad) %>%
  left_join(emo) %>%
  select(word, pmi, valence, arousal, dominance, anger, fear, sadness, joy, frequency)

lonely_v_alone2 <- lonely_v_alone2 %>%
  left_join(vad) %>%
  left_join(emo) %>%
  select(word, pmi, valence, arousal, dominance, anger, fear, sadness, joy, frequency)

# Filter for words that occurred in the set of tweets at least 500 times
#' See shiny app to explore different cutoffs (https://whipson.shinyapps.io/Solitude_Tweets/)

solitude_v_lonely_filt <- solitude_v_lonely %>%
  filter(frequency >= 500)

lonely_v_alone_filt <- lonely_v_alone %>%
  filter(frequency >= 500)

solitude_v_alone_filt <- solitude_v_alone %>%
  filter(frequency >= 500)

lonely_v_alone_filt2 <- lonely_v_alone2 %>%
  filter(frequency >= 500)

solitude_v_alone_filt2 <- solitude_v_alone2 %>%
  filter(frequency >= 500)

# --- Linear regressions --- #
# There are 21 linear regressions in total

# Solitude versus Loneliness

sol_lon_val_fit <- lm(valence ~ pmi, data = solitude_v_lonely_filt)
summary(sol_lon_val_fit)

sol_lon_aro_fit <- lm(arousal ~ pmi, data = solitude_v_lonely_filt)
summary(sol_lon_aro_fit)

sol_lon_dom_fit <- lm(dominance ~ pmi, data = solitude_v_lonely_filt)
summary(sol_lon_dom_fit)

sol_lon_ang_fit <- glm(anger ~ pmi, family = "binomial", data = solitude_v_lonely_filt)
summary(sol_lon_ang_fit)

sol_lon_fear_fit <- glm(fear ~ pmi, family = "binomial", data = solitude_v_lonely_filt)
summary(sol_lon_fear_fit)

sol_lon_sad_fit <- glm(sadness ~ pmi, family = "binomial", data = solitude_v_lonely_filt)
summary(sol_lon_sad_fit)

sol_lon_joy_fit <- glm(joy ~ pmi, family = "binomial", data = solitude_v_lonely_filt)
summary(sol_lon_joy_fit)

# Using specific 'alone' phrases #
# Solitude versus Alone

sol_alo_val_fit2 <- lm(valence ~ pmi, data = solitude_v_alone_filt2)
summary(sol_alo_val_fit2)

sol_alo_aro_fit2 <- lm(arousal ~ pmi, data = solitude_v_alone_filt2)
summary(sol_alo_aro_fit2)

sol_alo_dom_fit2 <- lm(dominance ~ pmi, data = solitude_v_alone_filt2)
summary(sol_alo_dom_fit2)

sol_alo_ang_fit2 <- glm(anger ~ pmi, family = "binomial", data = solitude_v_alone_filt2)
summary(sol_alo_ang_fit2)

sol_alo_fear_fit2 <- glm(fear ~ pmi, family = "binomial", data = solitude_v_alone_filt2)
summary(sol_alo_fear_fit2)

sol_alo_sad_fit2 <- glm(sadness ~ pmi, family = "binomial", data = solitude_v_alone_filt2)
summary(sol_alo_sad_fit2)

sol_alo_joy_fit2 <- glm(joy ~ pmi, family = "binomial", data = solitude_v_alone_filt2)
summary(sol_alo_joy_fit2)

# lonely versus alone

lon_alo_val_fit2 <- lm(valence ~ pmi, data = lonely_v_alone_filt2)
summary(lon_alo_val_fit2)

lon_alo_aro_fit2 <- lm(arousal ~ pmi, data = lonely_v_alone_filt2)
summary(lon_alo_aro_fit2)

lon_alo_dom_fit2 <- lm(dominance ~ pmi, data = lonely_v_alone_filt2)
summary(lon_alo_dom_fit2)

lon_alo_ang_fit2 <- glm(anger ~ pmi, family = "binomial", data = lonely_v_alone_filt2)
summary(lon_alo_ang_fit2)

lon_alo_fear_fit2 <- glm(fear ~ pmi, family = "binomial", data = lonely_v_alone_filt2)
summary(lon_alo_fear_fit2)

lon_alo_sad_fit2 <- glm(sadness ~ pmi, family = "binomial", data = lonely_v_alone_filt2)
summary(lon_alo_sad_fit2)

lon_alo_joy_fit2 <- glm(joy ~ pmi, family = "binomial", data = lonely_v_alone_filt2)
summary(lon_alo_joy_fit2)

# --- Plotting --- #

# Figure 1

p1 <- solitude_v_lonely_filt %>%
  ggplot(aes(x = valence, y = pmi)) +
  geom_point(alpha = .25, color = "dodgerblue2", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-3, 0, 6),
                     labels = c("Lonely", 0, "Solitude")) +
  labs(x = "Valence",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p2 <- solitude_v_lonely_filt %>%
  ggplot(aes(x = arousal, y = pmi)) +
  geom_point(alpha = .25, color = "#00CD66", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-3, 0, 6),
                     labels = c("Lonely", 0, "Solitude")) +
  labs(x = "Arousal",
       y = NULL) +
  theme_minimal(base_size = 14)

p3 <- solitude_v_lonely_filt %>%
  ggplot(aes(x = dominance, y = pmi)) +
  geom_point(alpha = .25, color = "#EE3B3B", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-3, 0, 6),
                     labels = c("Lonely", 0, "Solitude")) +
  labs(x = "Dominance",
       y = NULL) +
  theme_minimal(base_size = 14)

fig1 <- ggarrange(p1, p2, p3, nrow = 1)
fig1
ggsave("fig1.png", fig1, width = 9)

# Figure 2

p4 <- solitude_v_alone_filt %>%
  ggplot(aes(x = valence, y = pmi)) +
  geom_point(alpha = .25, color = "dodgerblue2", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  5),
                     labels = c("Alone", 0, "Solitude")) +
  labs(x = "Valence",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p5 <- solitude_v_alone_filt %>%
  ggplot(aes(x = arousal, y = pmi)) +
  geom_point(alpha = .25, color = "#00CD66", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  5),
                     labels = c("Alone", 0, "Solitude")) +
  labs(x = "Arousal",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p6 <- solitude_v_alone_filt %>%
  ggplot(aes(x = dominance, y = pmi)) +
  geom_point(alpha = .25, color = "#EE3B3B", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  5),
                     labels = c("Alone", 0, "Solitude")) +
  labs(x = "Dominance",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

fig2 <- ggarrange(p4, p5, p6, nrow = 1)
annotate_figure(fig2,
                top = text_grob("Solitude vs. Alone", face = "bold", size = 18),
                bottom = text_grob("Words with higher PMI difference co-occur more with 'solitude'. Words with lower PMI difference co-occur more with 'alone'\nFive highest and lowest PMI difference scores are highlighted", hjust = 0, x = .01))

# Figure 3

p7 <- lonely_v_alone_filt %>%
  ggplot(aes(x = valence, y = pmi)) +
  geom_point(alpha = .25, color = "dodgerblue2", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  3.5),
                     labels = c("Alone", 0, "Lonely")) +
  labs(x = "Valence",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p8 <- lonely_v_alone_filt %>%
  ggplot(aes(x = arousal, y = pmi)) +
  geom_point(alpha = .25, color = "#00CD66", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  3.5),
                     labels = c("Alone", 0, "Lonely")) +
  labs(x = "Arousal",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

p9 <- lonely_v_alone_filt %>%
  ggplot(aes(x = dominance, y = pmi)) +
  geom_point(alpha = .25, color = "#EE3B3B", position = "jitter") +
  geom_smooth(method = 'lm', color = "black") +
  scale_y_continuous(breaks = c(-4, 0,  3.5),
                     labels = c("Alone", 0, "Lonely")) +
  labs(x = "Dominance",
       y = "Tendency of Co-occurrence") +
  theme_minimal(base_size = 14)

fig3 <- ggarrange(p7, p8, p9, nrow = 1)
annotate_figure(fig3,
                top = text_grob("Lonely vs. Alone", face = "bold", size = 18),
                bottom = text_grob("Words with higher PMI difference co-occur more with 'lonely'. Words with lower PMI difference co-occur more with 'alone'\nFive highest and lowest PMI difference scores are highlighted", hjust = 0, x = .01))
