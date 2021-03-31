###Tempo Stuff
source("analysis.R")
source("easy_util.R")
library(hexbin)

##list of important plots
#ngram_tempoclass_plot(): new tempo classes, facet is n-gram
##"Data points: 404068, mean eff. size: 0.566627"

#tempoclass_plot(): new tempo classes, facet is tempo class
##"Data points: 62593, mean eff. size: 0.642087"

#ngram_tempo_quart_plot(): tempo quartiles, facet is n-gram
##"Data points: 404068, mean eff. size: 0.416973"

#quartile_tempo_plot(): tempo quartiles, facet is quartile
##"Data points: 62593, mean eff. size: 0.369672"

#ngram_tempo_trad_plot(): classical tempi, facet is n-gram (do not use)
##"Data points: 404068, mean eff. size: 1.123177"

#trad_tempo_plot(): classical tempi, facet is class (do not use)
##"Data points: 62593, mean eff. size: 1.053371"


#add notes/sec and quartiles; filter wjd_inphrase to JUST lick & line, split tempi, make new bins for tempoclass
wjd_inphrase_ll <- wjd_inphrase %>%
  dplyr::mutate(notespersec = (N)/(dur)) %>%
  filter(MLA_main_type == c("lick", "line")) %>%
  mutate(tempo_quartile = ntile(avgtempo, 4)) %>%
  mutate(tempo_trad = cut(avgtempo, 
                          breaks = c(0, 60, 80, 110, 
                                     120, 160, 200, 500),
                          labels = c("largo", "adagio", "andante", 
                                     "moderato", "allegro", "presto", "prestissimo"))) %>%
  mutate(notespersec_quartile = ntile(notespersec, 4)) %>%
  mutate(tempoclass3 = recode(tempoclass,
                             "SLOW" = "SLOW",
                             "MEDIUM SLOW" = "MEDIUM",
                             "MEDIUM" = "MEDIUM",
                             "MEDIUM UP" = "UP",
                             "UP" = "UP")) %>%
  mutate(tempoclass3 = factor(tempoclass3, levels = c("SLOW", "MEDIUM", "UP")))

#the tempo class variable is now split into 3 bins instead of 5
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

#new tempo class plot where facet is n-gram
ngram_tempoclass_plot <- function(data = wjd_inphrase_ll, 
                                   min_N = 3, 
                                   min_freq = 1, 
                                   max_pos = 30, 
                                   fix_scale = TRUE,
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
    group_by(N, phrase_pos, tempoclass3) %>% 
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
    ggplot(aes(x = phrase_pos, y = easiness_mean, group = factor(tempoclass3), color = factor(tempoclass3))) 
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

#tempo class plot where facet is new tempo class, n-gram = 4
#add lick & line to this one -> color by lick/line
#then do beta coefficient from new_paper_production (plot_linear_betas)
tempoclass_plot <- function(data = wjd_inphrase_ll, 
                                min_N = 3, 
                                min_freq = 1, 
                                max_pos = 30, 
                                fix_scale = TRUE,
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
    group_by(N, phrase_pos, tempoclass3) %>% 
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
    ggplot(aes(x = phrase_pos, y = easiness_mean, color = factor(tempoclass3))) 
  q <- q + geom_errorbar(aes(ymin = easiness_mean - easiness_se, ymax = easiness_mean + easiness_se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~tempoclass3) 
    }
    else{
      q <- q + facet_wrap(~tempoclass3, scales = "free_y") 
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

#tempo class linear betas
plot_linear_betas_tempoclass <- function(data = wjd_inphrase_ll, 
                              MLA_main_types = c("lick", "line"),
                              easiness = "combined_easiness",
                              max_pos_range = 10:30, min_N = 3, 
                              p_thresh = .001,
                              alpha = 1){
  #browser()
  lb_SLOW <- get_linear_betas(data = data %>% filter(tempoclass3 == "SLOW"), 
                             min_N = min_N, 
                             easiness = easiness, 
                             MLA_main_type = MLA_main_types,
                             max_pos_range = max_pos_range) %>% mutate(tempoclass3 = "SLOW")
  lb_MEDIUM <- get_linear_betas(data = data %>% filter(tempoclass3 == "MEDIUM"), 
                               min_N = min_N, 
                               easiness = easiness,
                               MLA_main_type = MLA_main_types,
                               max_pos_range = max_pos_range) %>% mutate(tempoclass3 = "MEDIUM")
  lb_UP <- get_linear_betas(data = data %>% filter(tempoclass3 == "UP"), 
                                min_N = min_N, 
                                easiness = easiness,
                                MLA_main_type = MLA_main_types,
                                max_pos_range = max_pos_range) %>% mutate(tempoclass3 = "UP")
  lb <- bind_rows(lb_SLOW , lb_MEDIUM, lb_UP) %>% mutate(comb_tempoclass = sprintf("%s-%s", tempoclass3, MLA_main_type))
  assign("lb", lb, globalenv())
  q <- lb %>% filter(p.value < p_thresh) %>% 
    ggplot(aes(x = max_pos, y = estimate)) 
  q <- q + geom_ribbon(aes(ymin = estimate-std.error, ymax = estimate + std.error, fill = tempoclass3, group = comb_tempoclass), 
                       alpha = alpha) 
  q <- q + geom_line(aes(linetype = MLA_main_type, group = comb_tempoclass), colour = "black") 
  q <- q + facet_wrap(~N) 
  q <- q + get_default_theme(keep_legend = T)  
  q <- q + theme(legend.key.size = unit(0.5, "cm")) 
  q <- q + theme(legend.key.width = unit(1.0, "cm")) 
  q <- q + theme(legend.position = c(.85, .1), legend.background = element_rect(colour = "black")) 
  q <- q + geom_hline(yintercept = 0)
  q <- q + labs(x = "End Position", y = get_easiness_label(easiness))
  q
}

#tempo quartile plot where facet is n-gram
ngram_tempo_quart_plot <- function(data = wjd_inphrase_ll, 
                             min_N = 3, 
                             min_freq = 1, 
                             max_pos = 30, 
                             fix_scale = TRUE,
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
                             fix_scale = TRUE,
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
                                   fix_scale = TRUE,
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
                                fix_scale = TRUE,
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
                                  fix_scale = TRUE,
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
                            fix_scale = TRUE,
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

#average tempo (x) by note density (notes/sec, y)
tempoXdensity_plot <- ggplot(wjd_inphrase_ll, aes(x = avgtempo, y = notespersec)) +
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue", high = "red") +
  stat_smooth(method = "lm", level = .99, color = "purple")

#trend analysis for note density
