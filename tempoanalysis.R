###Tempo stuff
source("analysis.R")
source("easy_util.R")

#add notes/sec; filter wjd_inphrase to JUST lick & line
wjd_inphrase_ll <- wjd_inphrase %>%
  dplyr::mutate(notespersec = (N)/(dur)) %>%
  filter(MLA_main_type == c("lick", "line"))

#tempo against genre
genre_tempo_boxplot <- ggplot(wjd_inphrase_ll, aes(x = factor(style), y = avgtempo)) + 
  geom_boxplot()

genre_tempo_violinplot <- ggplot(wjd_inphrase_ll, aes(x = style, y = avgtempo)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = .05, fill = "black", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)

genre_tempo_hist <- ggplot(wjd_inphrase_ll, aes(x = avgtempo)) +
  geom_histogram(fill = "white", color = "black") +
  facet_grid(style ~ .)

#need to rethink tempo class groups - maybe split based on trad. conducting classifications
class_tempo_hist <- ggplot(wjd_inphrase_ll, aes(x = avgtempo)) +
  geom_histogram(fill = "white", color = "black") +
  facet_grid(tempoclass ~ .)

#tempo class plot where facet is n-gram
ngram_tempo_plot <- function(data = wjd_inphrase_ll, 
                             min_N = 3, 
                             min_freq = 1, 
                             max_pos = 30, 
                             fix_scale = FALSE,
                             standardize = FALSE,
                             facetting = TRUE,
                             easiness = "combined_easiness",
                             easiness_label = "Mean Difficulty",
                             smooth = "poly", poly_degree = 3){
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  if(standardize){
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
  }  
  #browser()
  data <- 
    data %>% 
    filter(N >= min_N, in_phrase, freq >= min_freq, phrase_pos <= max_pos, phrase_len >= N) %>%  
    group_by(N, phrase_pos, tempoclass) %>% 
    summarise(easiness_mean = mean(easiness), 
              easiness_sd = sd(easiness), 
              n = n(), 
              easiness_se = easiness_sd/sqrt(n())) %>% 
    ungroup() 
  mean_eff_size <- 
    data %>% 
    group_by(N) %>% 
    summarise(r = diff(range(easiness_mean)), mem = mean(easiness_mean)) %>% 
    print() %>% 
    summarise(d = mean(r), max_d = max(r), min_d = min(r), med_d = median(r), mem = mean(mem)) %>% 
    print() %>% 
    pull(med_d)
  #browser()
  cor <- my_cor_test(data %>% filter(N == 4), "phrase_pos", "easiness_mean") 
  mean_cor <- 
    map_dfr(3:10, function(x) {
      my_cor_test(data %>% filter(N == x), "phrase_pos", "easiness_mean")}) %>% 
    summarise(mean_cor = mean(estimate)) %>% 
    print()
  
  printf("Data points: %d, mean eff. size: %f", sum(data$n), mean_eff_size)
  
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = easiness_mean, group = factor(tempoclass), color = factor(tempoclass))) 
  q <- q + geom_errorbar(aes(ymin = easiness_mean - easiness_se, ymax = easiness_mean + easiness_se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~N) 
    }
    else{
      q <- q + facet_wrap(~N, scales = "free_y") 
    }
    q <- q + theme(legend.position = "none") 
  }
  if(smooth == "poly"){
    f <- sprintf("y ~ poly(x, %d)", poly_degree)
    q <- q + geom_smooth(method = "lm", formula = f) 
  }
  else{
    q <- q + geom_smooth() 
    
  }
  q <- q + theme_bw() 
  q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + geom_point()
  q
}

#tempo class plot where facet is tempo class
class_tempo_plot <- function(data = wjd_inphrase_ll, 
                             min_N = 3, 
                             min_freq = 1, 
                             max_pos = 30, 
                             fix_scale = FALSE,
                             standardize = FALSE,
                             facetting = TRUE,
                             easiness = "combined_easiness",
                             easiness_label = "Mean Difficulty",
                             smooth = "poly", poly_degree = 3){
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  if(standardize){
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
  }  
  #browser()
  data <- 
    data %>% 
    filter(N == min_N, in_phrase, freq >= min_freq, phrase_pos <= max_pos, phrase_len >= N) %>%  
    group_by(N, phrase_pos, tempoclass) %>% 
    summarise(easiness_mean = mean(easiness), 
              easiness_sd = sd(easiness), 
              n = n(), 
              easiness_se = easiness_sd/sqrt(n())) %>% 
    ungroup() 
  mean_eff_size <- 
    data %>% 
    group_by(N) %>% 
    summarise(r = diff(range(easiness_mean)), mem = mean(easiness_mean)) %>% 
    print() %>% 
    summarise(d = mean(r), max_d = max(r), min_d = min(r), med_d = median(r), mem = mean(mem)) %>% 
    print() %>% 
    pull(med_d)
  #browser()
  cor <- my_cor_test(data %>% filter(N == 4), "phrase_pos", "easiness_mean") 
  mean_cor <- 
    map_dfr(3:10, function(x) {
      my_cor_test(data %>% filter(N == x), "phrase_pos", "easiness_mean")}) %>% 
    summarise(mean_cor = mean(estimate)) %>% 
    print()
  
  printf("Data points: %d, mean eff. size: %f", sum(data$n), mean_eff_size)
  
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = easiness_mean, color = factor(tempoclass))) 
  q <- q + geom_errorbar(aes(ymin = easiness_mean - easiness_se, ymax = easiness_mean + easiness_se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~tempoclass) 
    }
    else{
      q <- q + facet_wrap(~tempoclass, scales = "free_y") 
    }
    q <- q + theme(legend.position = "none") 
  }
  if(smooth == "poly"){
    f <- sprintf("y ~ poly(x, %d)", poly_degree)
    q <- q + geom_smooth(method = "lm", formula = f) 
  }
  else{
    q <- q + geom_smooth() 
    
  }
  q <- q + theme_bw() 
  q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + geom_point()
  q
}

