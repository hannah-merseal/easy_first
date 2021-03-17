###Tempo stuff
source("analysis.R")
source("easy_util.R")

#add notes/sec and quartiles; filter wjd_inphrase to JUST lick & line, split tempi
wjd_inphrase_ll <- wjd_inphrase %>%
  dplyr::mutate(notespersec = (N)/(dur)) %>%
  filter(MLA_main_type == c("lick", "line")) %>%
  mutate(tempo_quartile = ntile(avgtempo, 4)) %>%
  mutate(tempo_trad = cut(avgtempo, 
                          breaks = c(0, 60, 80, 110, 
                                     120, 160, 200, 500),
                          labels = c("largo", "adagio", "andante", 
                                     "moderato", "allegro", "presto", "prestissimo"))) %>%
  mutate(notespersec_quartile = ntile(notespersec, 4))

#exploring the tempo class variable
genre_tempo_boxplot <- ggplot(wjd_inphrase_ll, aes(x = factor(style), y = avgtempo)) + 
  geom_boxplot()

genre_tempo_violinplot <- ggplot(wjd_inphrase_ll, aes(x = style, y = avgtempo)) +
  geom_violin(trim = FALSE) + 
  #geom_boxplot(width = .05, fill = "black", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "red", shape = 21, size = 2.5)

genre_tempo_hist <- ggplot(wjd_inphrase_ll, aes(x = avgtempo)) +
  geom_histogram(fill = "white", color = "black") +
  facet_grid(style ~ .)

class_tempo_hist <- ggplot(wjd_inphrase_ll, aes(x = avgtempo)) +
  geom_histogram(fill = "white", color = "black") +
  facet_grid(tempoclass ~ .)

#tempo quartile plot where facet is n-gram
ngram_tempo_quart_plot <- function(data = wjd_inphrase_ll, 
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
    group_by(N, phrase_pos, tempo_quartile) %>% 
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
    ggplot(aes(x = phrase_pos, y = easiness_mean, group = factor(tempo_quartile), color = factor(tempo_quartile))) 
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

#tempo quartile plot where facet is tempo quartile, n-gram = 3
quartile_tempo_plot <- function(data = wjd_inphrase_ll, 
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
    group_by(N, phrase_pos, tempo_quartile) %>% 
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
    ggplot(aes(x = phrase_pos, y = easiness_mean, color = factor(tempo_quartile))) 
  q <- q + geom_errorbar(aes(ymin = easiness_mean - easiness_se, ymax = easiness_mean + easiness_se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~tempo_quartile) 
    }
    else{
      q <- q + facet_wrap(~tempo_quartile, scales = "free_y") 
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

#trad. tempo class plot where facet is n-gram
ngram_tempo_trad_plot <- function(data = wjd_inphrase_ll, 
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
    group_by(N, phrase_pos, tempo_trad) %>% 
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
    ggplot(aes(x = phrase_pos, y = easiness_mean, group = factor(tempo_trad), color = factor(tempo_trad))) 
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

#trad. tempo class plot where facet is tempo class, n-gram = 3
trad_tempo_plot <- function(data = wjd_inphrase_ll, 
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
    group_by(N, phrase_pos, tempo_trad) %>% 
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
    ggplot(aes(x = phrase_pos, y = easiness_mean, color = factor(tempo_trad))) 
  q <- q + geom_errorbar(aes(ymin = easiness_mean - easiness_se, ymax = easiness_mean + easiness_se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~tempo_trad) 
    }
    else{
      q <- q + facet_wrap(~tempo_trad, scales = "free_y") 
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

#note density plot where facet is n-gram
ngram_note_density_plot <- function(data = wjd_inphrase_ll, 
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
    group_by(N, phrase_pos, notespersec_quartile) %>% 
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
    ggplot(aes(x = phrase_pos, y = easiness_mean, group = factor(notespersec_quartile), color = factor(notespersec_quartile))) 
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

#note density plot where facet is density quartile, n-gram = 3
note_density_quartile_plot <- function(data = wjd_inphrase_ll, 
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
    group_by(N, phrase_pos, notespersec_quartile) %>% 
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
    ggplot(aes(x = phrase_pos, y = easiness_mean, color = factor(notespersec_quartile))) 
  q <- q + geom_errorbar(aes(ymin = easiness_mean - easiness_se, ymax = easiness_mean + easiness_se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~notespersec_quartile) 
    }
    else{
      q <- q + facet_wrap(~notespersec_quartile, scales = "free_y") 
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