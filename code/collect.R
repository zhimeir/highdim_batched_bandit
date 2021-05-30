suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(showtext))
showtext_auto()
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")

## Experiments config
K <- 2
sigma <- 0.5

## Parameters
T <- 6000
gamma <- 0.5

## Aesthetics
theme_font <- theme(axis.title.x = element_text(size = 20, family="Times New Roman"),
                    axis.title.y = element_text(size = 20, family="Times New Roman"),
                    axis.text.x = element_text(size = 20, family="Times New Roman"),
                    axis.text.y = element_text(size = 20, family="Times New Roman"),
                    legend.text = element_text(size = 20, family="Times New Roman"),
                    legend.title = element_text(size = 20,family="Times New Roman"),
                    strip.text = element_text(size = 20,family="Times New Roman"))

## Collect results
regret <- c()
for(seed in 1:30){
  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_gamma%.2f.txt", K, sigma, T, 3, seed, gamma)
  res1 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = 3, seed = seed, t = 1:T)

  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_gamma%.2f.txt", K, sigma, T, T, seed, gamma)
  res2 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = T, seed = seed, t = 1:T)

  ##   res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_lasso.txt", K, sigma, T, 3, seed, gamma)
  ##   res3 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
  ##     select(cum_regret) %>%
  ##     mutate(M = "LASSO bandit", seed = seed, t = 1:T)
  regret <- rbind(regret, rbind(res1, res2))
}

regret <- regret %>% group_by(M, t) %>%
  summarise(cum_regret = mean(cum_regret), lo = quantile(cum_regret, 0.025), hi = quantile(cum_regret, 0.975)) %>%
  mutate(M = factor(M, levels = c(3, T, "LASSO bandit"), labels = c("Batched (M=3)", "Online", "LASSO Bandit")))

pp <- ggplot(regret, aes(x=t, y = cum_regret, group = M, col = M , fill = M)) +
  ##   geom_line() + 
  ##   geom_point(size = 0.1) + 
  geom_smooth(aes(ymin = lo, ymax = hi), stat = "identity") +
  theme_bw() + 
  xlab("t") +
  ylab("Cumulative regret (t)") +
  theme_font +
  theme(legend.position = c(0.2,0.8),
        legend.background = element_blank()) +
  scale_color_manual(values = c("#1c84c6", "#1ab394", "#f8ac59")) +
  scale_fill_manual(values = c("#1c84c6", "#1ab394", "#f8ac59"))

out_file <- sprintf("../figs/K%d_sigma%.2f_T%d_gamma%.2f.pdf", K, sigma, T, gamma)
ggsave(out_file, pp, width = 16, height = 10)

