suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(showtext))
showtext_auto()
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")

## Experiments config
K <- 2
sigma <- 0.5

## Parameters
T <- 4000
gamma <- 0.5

## Aesthetics
theme_font <- theme(axis.title.x = element_text(size = 45, family="Times New Roman"),
                    axis.title.y = element_text(size = 45, family="Times New Roman"),
                    axis.text.x = element_text(size = 45, family="Times New Roman"),
                    axis.text.y = element_text(size = 45, family="Times New Roman"),
                    legend.text = element_text(size = 45, family="Times New Roman"),
                    legend.title = element_text(size = 45,family="Times New Roman"),
                    strip.text = element_text(size = 45,family="Times New Roman"))

## Collect results
regret <- c()
for(seed in 1:30){
  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_gamma%.2f.txt", K, sigma, T, 3, seed, gamma)
  res1 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = 3, seed = seed, t = 1:T)

  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_gamma%.2f.txt", K, sigma, T, 2, seed, 0.1)
  res2 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = 2, seed = seed, t = 1:T)
  
  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_gamma%.2f.txt", K, sigma, T, T, seed, gamma)
  res3 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = T, seed = seed, t = 1:T)

  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_lasso.txt", K, sigma, T, 3, seed, gamma)
  res4 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = "LASSO bandit", seed = seed, t = 1:T)
 
  regret <- rbind(regret, rbind(res1, res2, res3, res4))
}

regret <- regret %>% filter(M != 5) %>% group_by(M, t) %>%
  summarise(regret = mean(cum_regret), lo = regret - sqrt(var(cum_regret)) * 1.96 / sqrt(30), hi = regret + sqrt(var(cum_regret)) *1.96 /sqrt(30)) %>%
  mutate(M = factor(M, levels = c(3, 2, T, "LASSO bandit"), labels = c("LBGL (M=3)", "LBGL (M=2)", "LBGL (M=T)", "LASSO Bandit")))

regret <- regret %>% filter(t %in% seq(100, T, by = 100))

pp <- ggplot(regret, aes(x=t, y = regret, group = M, col = M , fill = M)) +
  ##   geom_line() + 
  geom_point(alpha = 0.7) + 
  geom_smooth(aes(ymin = lo, ymax = hi), stat = "identity", alpha =0.5) +
  theme_bw() + 
  xlab("t") +
  ylab("Cumulative regret (t)") +
  ylim(c(0,2000)) +
  theme_font +
  theme(legend.position = c(0.2,0.8),
        legend.background = element_blank()) +
  scale_color_manual(values = c("#1c84c6", "#1ab394", "#f8ac59", "#ed5565")) +
  scale_fill_manual(values = c("#1c84c6", "#1ab394", "#f8ac59", "#ed5565"))

out_file <- sprintf("../figs/K%d_sigma%.2f_T%d_gamma%.2f.pdf", K, sigma, T, gamma)
ggsave(out_file, pp, width = 16, height = 10)

