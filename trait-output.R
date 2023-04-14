# load packages ----------------------------------------------------------------

library(tidyverse)
library(plotly)

# function traits --------------------------------------------------------------

trait_output <- function(data, value){

## arguments:
  #data = the dataset you want to extract values from
  #value = the participant ID value corresponding to the particular person you want to get trait scores for

  #creating hexaco norming data
  hexaco_norm <- data.frame(matrix(ncol = 0, nrow = 1126))

  set.seed(1998)

  hexaco_norm$hh <- rtruncnorm(1126, 1, 5, 3.23, .66)
  hexaco_norm$em <- rtruncnorm(1126, 1, 5, 3.36, .7)
  hexaco_norm$ex <- rtruncnorm(1126, 1, 5, 3.51, .62)
  hexaco_norm$ag <- rtruncnorm(1126, 1, 5, 3.10, .63)
  hexaco_norm$cn <- rtruncnorm(1126, 1, 5, 3.47, .61)
  hexaco_norm$op <- rtruncnorm(1126, 1, 5, 3.49, .67)

  #select person
  data <- data %>%
    filter(ID == value)

  #get HEXACO scores
  hexaco_norm$op_score <- rep(data$openness, times = 1126)
  hexaco_norm$ag_score <- rep(data$agreeableness, times = 1126)
  hexaco_norm$ex_score <- rep(data$extraversion, times = 1126)
  hexaco_norm$cn_score <- rep(data$conscientiousness, times = 1126)
  hexaco_norm$hh_score <- rep(data$honestyhumility, times = 1126)
  hexaco_norm$em_score <- rep(data$emotionality, times = 1126)

  #get scores as percentiles
  hexaco_norm$op_qscore <- ecdf(hexaco_norm$op)(hexaco_norm$op_score)
  hexaco_norm$ag_qscore <- ecdf(hexaco_norm$ag)(hexaco_norm$ag_score)
  hexaco_norm$ex_qscore <- ecdf(hexaco_norm$ex)(hexaco_norm$ex_score)
  hexaco_norm$cn_qscore <- ecdf(hexaco_norm$cn)(hexaco_norm$cn_score)
  hexaco_norm$hh_qscore <- ecdf(hexaco_norm$hh)(hexaco_norm$hh_score)
  hexaco_norm$em_qscore <- ecdf(hexaco_norm$em)(hexaco_norm$em_score)

  hexaco_norm <- hexaco_norm %>%
    mutate(op_qscore = round(op_qscore*100),
           ag_qscore = round(ag_qscore*100),
           ex_qscore = round(ex_qscore*100),
           cn_qscore = round(cn_qscore*100),
           hh_qscore = round(hh_qscore*100),
           em_qscore = round(em_qscore*100))

  #generate plots
  #agreeableness
  agplot <- ggplot(hexaco_norm, aes(x = ag))+
    geom_density(color = "steelblue4", fill = "steelblue1", linewidth = 0.9, bw = 0.3, alpha = 0.7)+
    geom_segment(aes(x = quantile(ag, probs = 0.5), y = 0, xend = quantile(ag, probs = 0.5), yend = 0.58, text = "Average"), color = "steelblue4", linetype = "dashed", linewidth = 0.4, alpha = 0.5)+
    geom_segment(aes(x = quantile(ag, probs = 0.01), y = 0.65, xend = quantile(ag, probs = 0.99), yend = 0.65), linewidth = 1, color = "steelblue3")+
    geom_point(aes(x = quantile(ag, probs = 0.99), y = 0.65, text = "99th percentile"), size = 1.5, color = "steelblue3")+
    geom_point(aes(x = quantile(ag, probs = 0.01), y = 0.65, text = "1st percentile"), size = 1.5, color = "steelblue3")+
    geom_point(aes(x = quantile(ag, probs = 0.95), y = 0.65, text = "95th percentile"), size = 2, color = "steelblue3")+
    geom_point(aes(x = quantile(ag, probs = 0.05), y = 0.65, text = "5th percentile"), size = 2, color = "steelblue3")+
    geom_point(aes(x = quantile(ag, probs = 0.80), y = 0.65, text = "80th percentile"), size = 2.5, color = "steelblue3")+
    geom_point(aes(x = quantile(ag, probs = 0.20), y = 0.65, text = "20th percentile"), size = 2.5, color = "steelblue3")+
    geom_point(aes(x = quantile(ag, probs = 0.5), y = 0.65, text = "Average"), size = 4, color = "steelblue3")+
    geom_segment(aes(x = ag_score, y = 0, xend = ag_score, yend = 0.65), color = "black", linewidth = 1)+
    geom_point(aes(x = ag_score, y = 0.65, text = ag_qscore), color = "black", size = 4.5)+
    geom_text(aes(x = ag_score, y = 0.68, label = "Your Score"))+
    theme_void()+
    scale_y_continuous(
      limits = c(0, 0.75),
      expand = c(0, 0))+
    labs(title = "Agreeableness")

  #extraversion
  explot <- ggplot(hexaco_norm, aes(x = ex))+
    geom_density(color = "forestgreen", fill = "palegreen1", linewidth = 0.9, bw = 0.3, alpha = 0.7)+
    geom_segment(aes(x = quantile(ex, probs = 0.5), y = 0, xend = quantile(ex, probs = 0.5), yend = 0.58, text = "Average"), color = "forestgreen", linetype = "dashed", linewidth = 0.4, alpha = 0.5)+
    geom_segment(aes(x = quantile(ex, probs = 0.01), y = 0.65, xend = quantile(ex, probs = 0.99), yend = 0.65), linewidth = 1, color = "palegreen3")+
    geom_point(aes(x = quantile(ex, probs = 0.99), y = 0.65, text = "99th percentile"), size = 1.5, color = "palegreen3")+
    geom_point(aes(x = quantile(ex, probs = 0.01), y = 0.65, text = "1st percentile"), size = 1.5, color = "palegreen3")+
    geom_point(aes(x = quantile(ex, probs = 0.95), y = 0.65, text = "95th percentile"), size = 2, color = "palegreen3")+
    geom_point(aes(x = quantile(ex, probs = 0.05), y = 0.65, text = "5th percentile"), size = 2, color = "palegreen3")+
    geom_point(aes(x = quantile(ex, probs = 0.80), y = 0.65, text = "80th percentile"), size = 2.5, color = "palegreen3")+
    geom_point(aes(x = quantile(ex, probs = 0.20), y = 0.65, text = "20th percentile"), size = 2.5, color = "palegreen3")+
    geom_point(aes(x = quantile(ex, probs = 0.5), y = 0.65, text = "Average"), size = 4, color = "palegreen3")+
    geom_segment(aes(x = ex_score, y = 0, xend = ex_score, yend = 0.65), color = "black", linewidth = 1)+
    geom_point(aes(x = ex_score, y = 0.65, text = ex_qscore), color = "black", size = 4.5)+
    geom_text(aes(x = ex_score, y = 0.68, label = "Your Score"))+
    theme_void()+
    scale_y_continuous(
      limits = c(0, 0.75),
      expand = c(0, 0))+
    labs(title = "Extraversion")

  #honesty-humility
  hhplot <- ggplot(hexaco_norm, aes(x = hh))+
    geom_density(color = "goldenrod4", fill = "gold1", linewidth = 0.9, bw = 0.3, alpha = 0.7)+
    geom_segment(aes(x = quantile(hh, probs = 0.5), y = 0, xend = quantile(hh, probs = 0.5), yend = 0.54, text = "Average"), color = "goldenrod4", linetype = "dashed", linewidth = 0.4, alpha = 0.5)+
    geom_segment(aes(x = quantile(hh, probs = 0.01), y = 0.6, xend = quantile(hh, probs = 0.99), yend = 0.6), linewidth = 1, color = "goldenrod3")+
    geom_point(aes(x = quantile(hh, probs = 0.99), y = 0.6, text = "99th percentile"), size = 1.5, color = "goldenrod3")+
    geom_point(aes(x = quantile(hh, probs = 0.01), y = 0.6, text = "1st percentile"), size = 1.5, color = "goldenrod3")+
    geom_point(aes(x = quantile(hh, probs = 0.95), y = 0.6, text = "95th percentile"), size = 2, color = "goldenrod3")+
    geom_point(aes(x = quantile(hh, probs = 0.05), y = 0.6, text = "5th percentile"), size = 2, color = "goldenrod3")+
    geom_point(aes(x = quantile(hh, probs = 0.80), y = 0.6, text = "80th percentile"), size = 2.5, color = "goldenrod3")+
    geom_point(aes(x = quantile(hh, probs = 0.20), y = 0.6, text = "20th percentile"), size = 2.5, color = "goldenrod3")+
    geom_point(aes(x = quantile(hh, probs = 0.5), y = 0.6, text = "Average"), size = 4, color = "goldenrod3")+
    geom_segment(aes(x = hh_score, y = 0, xend = hh_score, yend = 0.6), color = "black", linewidth = 1)+
    geom_point(aes(x = hh_score, y = 0.6, text = hh_qscore), color = "black", size = 4.5)+
    geom_text(aes(x = hh_score, y = 0.63, label = "Your Score"))+
    theme_void()+
    scale_y_continuous(
      limits = c(0, 0.7),
      expand = c(0, 0))+
    labs(title = "Honesty-Humility")

  #conscientiousness
  cnplot <- ggplot(hexaco_norm, aes(x = cn))+
    geom_density(color = "orange4", fill = "orange1", linewidth = 0.9, bw = 0.3, alpha = 0.7)+
    geom_segment(aes(x = quantile(cn, probs = 0.5), y = 0, xend = quantile(cn, probs = 0.5), yend = 0.58, text = "Average"), color = "orange4", linetype = "dashed", linewidth = 0.4, alpha = 0.5)+
    geom_segment(aes(x = quantile(cn, probs = 0.01), y = 0.65, xend = quantile(cn, probs = 0.99), yend = 0.65), linewidth = 1, color = "orange3")+
    geom_point(aes(x = quantile(cn, probs = 0.99), y = 0.65, text = "99th percentile"), size = 1.5, color = "orange3")+
    geom_point(aes(x = quantile(cn, probs = 0.01), y = 0.65, text = "1st percentile"), size = 1.5, color = "orange3")+
    geom_point(aes(x = quantile(cn, probs = 0.95), y = 0.65, text = "95th percentile"), size = 2, color = "orange3")+
    geom_point(aes(x = quantile(cn, probs = 0.05), y = 0.65, text = "5th percentile"), size = 2, color = "orange3")+
    geom_point(aes(x = quantile(cn, probs = 0.80), y = 0.65, text = "80th percentile"), size = 2.5, color = "orange3")+
    geom_point(aes(x = quantile(cn, probs = 0.20), y = 0.65, text = "20th percentile"), size = 2.5, color = "orange3")+
    geom_point(aes(x = quantile(cn, probs = 0.5), y = 0.65, text = "Average"), size = 4, color = "orange3")+
    geom_segment(aes(x = cn_score, y = 0, xend = cn_score, yend = 0.65), color = "black", linewidth = 1)+
    geom_point(aes(x = cn_score, y = 0.65, text = cn_qscore), color = "black", size = 4.5)+
    geom_text(aes(x = cn_score, y = 0.68, label = "Your Score"))+
    theme_void()+
    scale_y_continuous(
      limits = c(0, 0.75),
      expand = c(0, 0))+
    labs(title = "Conscientiousness")

  #emotionality
  emplot <- ggplot(hexaco_norm, aes(x = em))+
    geom_density(color = "firebrick4", fill = "firebrick1", linewidth = 0.9, bw = 0.3, alpha = 0.7)+
    geom_segment(aes(x = quantile(em, probs = 0.5), y = 0, xend = quantile(em, probs = 0.5), yend = 0.53, text = "Average"), color = "firebrick", linetype = "dashed", linewidth = 0.4, alpha = 0.5)+
    geom_segment(aes(x = quantile(em, probs = 0.01), y = 0.6, xend = quantile(em, probs = 0.99), yend = 0.6), linewidth = 1, color = "firebrick")+
    geom_point(aes(x = quantile(em, probs = 0.99), y = 0.6, text = "99th percentile"), size = 1.5, color = "firebrick")+
    geom_point(aes(x = quantile(em, probs = 0.01), y = 0.6, text = "1st percentile"), size = 1.5, color = "firebrick")+
    geom_point(aes(x = quantile(em, probs = 0.95), y = 0.6, text = "95th percentile"), size = 2, color = "firebrick")+
    geom_point(aes(x = quantile(em, probs = 0.05), y = 0.6, text = "5th percentile"), size = 2, color = "firebrick")+
    geom_point(aes(x = quantile(em, probs = 0.80), y = 0.6, text = "80th percentile"), size = 2.5, color = "firebrick")+
    geom_point(aes(x = quantile(em, probs = 0.20), y = 0.6, text = "20th percentile"), size = 2.5, color = "firebrick")+
    geom_point(aes(x = quantile(em, probs = 0.5), y = 0.6, text = "Average"), size = 4, color = "firebrick")+
    geom_segment(aes(x = em_score, y = 0, xend = em_score, yend = 0.6), color = "black", linewidth = 1)+
    geom_point(aes(x = em_score, y = 0.6, text = em_qscore), color = "black", size = 4.5)+
    geom_text(aes(x = em_score, y = 0.63, label = "Your Score"))+
    theme_void()+
    scale_y_continuous(
      limits = c(0, 0.7),
      expand = c(0, 0))+
    labs(title = "Emotionality")

  #openness
  opplot <- ggplot(hexaco_norm, aes(x = op))+
    geom_density(color = "purple4", fill = "purple1", linewidth = 0.9, bw = 0.3, alpha = 0.7)+
    geom_segment(aes(x = quantile(op, probs = 0.5), y = 0, xend = quantile(op, probs = 0.5), yend = 0.55, text = "Average"), color = "purple4", linetype = "dashed", linewidth = 0.4, alpha = 0.5)+
    geom_segment(aes(x = quantile(op, probs = 0.01), y = 0.6, xend = quantile(op, probs = 0.99), yend = 0.6), linewidth = 1, color = "purple3")+
    geom_point(aes(x = quantile(op, probs = 0.99), y = 0.6, text = "99th percentile"), size = 1.5, color = "purple3")+
    geom_point(aes(x = quantile(op, probs = 0.01), y = 0.6, text = "1st percentile"), size = 1.5, color = "purple3")+
    geom_point(aes(x = quantile(op, probs = 0.95), y = 0.6, text = "95th percentile"), size = 2, color = "purple3")+
    geom_point(aes(x = quantile(op, probs = 0.05), y = 0.6, text = "5th percentile"), size = 2, color = "purple3")+
    geom_point(aes(x = quantile(op, probs = 0.80), y = 0.6, text = "80th percentile"), size = 2.5, color = "purple3")+
    geom_point(aes(x = quantile(op, probs = 0.20), y = 0.6, text = "20th percentile"), size = 2.5, color = "purple3")+
    geom_point(aes(x = quantile(op, probs = 0.5), y = 0.6, text = "Average"), size = 4, color = "purple3")+
    geom_segment(aes(x = op_score, y = 0, xend = op_score, yend = 0.6), color = "black", linewidth = 1)+
    geom_point(aes(x = op_score, y = 0.6, text = op_qscore), color = "black", size = 4.5)+
    geom_text(aes(x = op_score, y = 0.63, label = "Your Score"))+
    theme_void()+
    scale_y_continuous(
      limits = c(0, 0.7),
      expand = c(0, 0))+
    labs(title = "Openness to Experience")
}