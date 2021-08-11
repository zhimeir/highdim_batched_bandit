suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(showtext))
showtext_auto()
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")

## Experiments config
K <- 2
sigma <- 0.5

## Parameters
T <- 6000
gamma <- 0.1

## Aesthetics
theme_font <- theme(axis.title.x = element_text(size = 45, family="Times New Roman"),
                    axis.title.y = element_text(size = 45, family="Times New Roman"),
                    axis.text.x = element_text(size = 45, family="Times New Roman"),
                    axis.text.y = element_text(size = 45, family="Times New Roman"),
                    legend.text = element_text(size = 45, family="Times New Roman"),
                    legend.title = element_text(size = 45,family="Times New Roman"),
                    strip.text = element_text(size = 45,family="Times New Roman"))

## Collect results
ind <- c(1,4,5,7,8,9)
regret <- c()
for(seed in ind){
  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_gamma%.2f.txt", K, sigma, T, 3, seed, 0.1)
  res1 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = 3, seed = seed, t = 1:T)

  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_gamma%.2f.txt", K, sigma, T, 2, seed, 0.1)
  res2 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = 2, seed = seed, t = 1:T)
  
  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_ols.txt", K, sigma, T, 3, seed, 0.1)
  res3 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = "OLS bandit", seed = seed, t = 1:T)
  ## 
  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_lasso.txt", K, sigma, T, 3, seed, 0.1)
  res4 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = "LASSO bandit", seed = seed, t = 1:T)
  
  res_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_gamma%.2f.txt", K, sigma, T, T, seed, 0.5)
  res5 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = T, seed = seed, t = 1:T)
 
  regret <- rbind(regret, rbind(res1, res2, res3, res4, res5))
}

regret <- regret %>% filter(M != 5) %>% group_by(M, t) %>%
  summarise(regret = mean(cum_regret), lo = regret - sqrt(var(cum_regret)) * 1.96 / sqrt(30), hi = regret + sqrt(var(cum_regret)) *1.96 /sqrt(30)) %>%
  mutate(Method = factor(M, levels = c(3, 2, T, "LASSO bandit", "OLS bandit"), labels = c("LBGL (M=3)", "LBGL (M=2)", "LBGL (M=T)", "Lasso Bandit", "OLS Bandit")))

regret <- regret %>% filter(t %in% seq(100, T, by = 100))

pp <- ggplot(regret, aes(x=t, y = regret, group = Method, col = Method , fill = Method)) +
  ##   geom_line() + 
  geom_point(alpha = 0.7) + 
  geom_smooth(aes(ymin = lo, ymax = hi), stat = "identity", alpha =0.5) +
  theme_bw() + 
  xlab("t") +
  ylab("Cumulative regret (t)") +
  ##   ylim(c(0,2000)) +
  theme_font +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_blank()) +
  scale_color_manual(values = c("#1c84c6", "#1ab394", "#f8ac59", "#ed5565", "brown")) +
  scale_fill_manual(values = c("#1c84c6", "#1ab394", "#f8ac59", "#ed5565", "brown"))

out_file <- sprintf("../figs/K%d_sigma%.2f_T%d_gamma%.2f.pdf", K, sigma, T, gamma)
ggsave(out_file, pp, width = 16, height = 10)

