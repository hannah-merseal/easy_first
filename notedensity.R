###Note Density
source("analysis.R")
source("easy_util.R")
source("tempoanalysis.R")
library(hexbin)

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
plot_linear_betas_notedensity <- function(data = wjd_inphrase_ll, 
                                          MLA_main_types = c("lick", "line"),
                                          easiness = "combined_easiness",
                                          max_pos_range = 10:30, min_N = 3, 
                                          p_thresh = .001,
                                          alpha = 1){
  #browser()
  lb_Q1 <- get_linear_betas(data = data %>% filter(notespersec_quartile == 1), 
                            min_N = min_N, 
                            easiness = easiness, 
                            MLA_main_type = MLA_main_types,
                            max_pos_range = max_pos_range) %>% mutate(notespersec_quartile = 1)
  lb_Q2 <- get_linear_betas(data = data %>% filter(notespersec_quartile == 2), 
                            min_N = min_N, 
                            easiness = easiness, 
                            MLA_main_type = MLA_main_types,
                            max_pos_range = max_pos_range) %>% mutate(notespersec_quartile = 2)
  lb_Q3 <- get_linear_betas(data = data %>% filter(notespersec_quartile == 3), 
                            min_N = min_N, 
                            easiness = easiness, 
                            MLA_main_type = MLA_main_types,
                            max_pos_range = max_pos_range) %>% mutate(notespersec_quartile = 3)
  lb_Q4 <- get_linear_betas(data = data %>% filter(notespersec_quartile == 4), 
                            min_N = min_N, 
                            easiness = easiness, 
                            MLA_main_type = MLA_main_types,
                            max_pos_range = max_pos_range) %>% mutate(notespersec_quartile = 4)
  lb <- bind_rows(lb_Q1 , lb_Q2, lb_Q3, lb_Q4) %>% mutate(comb_notedensity = sprintf("%s-%s", notespersec_quartile, MLA_main_type))
  assign("lb", lb, globalenv())
  q <- lb %>% filter(p.value < p_thresh) %>% 
    ggplot(aes(x = max_pos, y = estimate)) 
  q <- q + geom_ribbon(aes(ymin = estimate-std.error, ymax = estimate + std.error, fill = factor(notespersec_quartile), group = comb_notedensity), 
                       alpha = alpha) 
  q <- q + geom_line(aes(linetype = MLA_main_type, group = comb_notedensity), colour = "black") 
  q <- q + facet_wrap(~N) 
  q <- q + get_default_theme(keep_legend = T)  
  q <- q + theme(legend.key.size = unit(0.5, "cm")) 
  q <- q + theme(legend.key.width = unit(1.0, "cm")) 
  q <- q + theme(legend.position = c(.85, .1), legend.background = element_rect(colour = "black")) 
  q <- q + geom_hline(yintercept = 0)
  q <- q + labs(x = "End Position", y = get_easiness_label(easiness))
  q
}

#effect sizes
# effect sizes
line_Q1_eff_in <- get_effect_sizes(wjd_inphrase_ll %>% filter(MLA_main_type == "line", notespersec_quartile == "1"))
lick_Q1_eff_in <- get_effect_sizes(wjd_inphrase_ll %>% filter(MLA_main_type == "lick", notespersec_quartile == "1"))
line_Q2_eff_in <- get_effect_sizes(wjd_inphrase_ll %>% filter(MLA_main_type == "line", notespersec_quartile == "2"))
lick_Q2_eff_in <- get_effect_sizes(wjd_inphrase_ll %>% filter(MLA_main_type == "lick", notespersec_quartile == "2"))
line_Q3_eff_in <- get_effect_sizes(wjd_inphrase_ll %>% filter(MLA_main_type == "line", notespersec_quartile == "3"))
lick_Q3_eff_in <- get_effect_sizes(wjd_inphrase_ll %>% filter(MLA_main_type == "lick", notespersec_quartile == "3"))
line_Q4_eff_in <- get_effect_sizes(wjd_inphrase_ll %>% filter(MLA_main_type == "line", notespersec_quartile == "4"))
lick_Q4_eff_in <- get_effect_sizes(wjd_inphrase_ll %>% filter(MLA_main_type == "lick", notespersec_quartile == "4"))

density.means <- wjd_inphrase_ll %>%
  group_by(notespersec_quartile) %>%
  summarize(
    mean_dense = mean(notespersec),
    sd_dense = sd(notespersec),
    min_dense = min(notespersec),
    max_dense = max(notespersec)
  )

library("Hmisc")
TempoCorr <- wjd_inphrase_ll %>%
  dplyr::select(avgtempo, notespersec)
TempoCorr.rcorr <- rcorr(as.matrix(TempoCorr))
TempoCorr.coeff <- TempoCorr.rcorr$r %>% round(2)
TempoCorr.p <- TempoCorr.rcorr$P %>% round(3)