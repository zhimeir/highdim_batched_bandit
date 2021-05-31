suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(showtext))
showtext_auto()
font_add("Times New Roman","/System/Library/Fonts/Supplemental/Times New Roman.ttf")


## Parameters
gamma <- 0.01
T <- 5528
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
for(seed in 1:10){
  res_file <- sprintf("../results/warfarin_M%d_seed%d_gamma%.2f.txt",  3, seed, gamma)
  res1 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = 3, seed = seed, t = 1:T)

  ##   res_file <- sprintf("../results/warfarin_M%d_seed%d_gamma%.2f.txt",  9, seed, gamma)
  ##   res2 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
  ##     select(cum_regret) %>%
  ##     mutate(M = 9, seed = seed, t = 1:T)

  res_file <- sprintf("../results/warfarin_oracle_seed%d.txt", seed)
  res3 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = "oracle-linear", seed = seed, t = 1:T)

  res_file <- sprintf("../results/warfarin_logistic_seed%d.txt", seed)
  res4 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = "oracle-logistic", seed = seed, t = 1:T)

  res_file <- sprintf("../results/warfarin_ols_seed%d.txt", seed)
  res5 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = "ols", seed = seed, t = 1:T)
  
  res_file <- sprintf("../results/warfarin_doctor_seed%d.txt", seed)
  res6 <- read_delim(res_file, delim = " ", col_types = cols()) %>%
    select(cum_regret) %>%
    mutate(M = "doctor", seed = seed, t = 1:T)
  regret <- rbind(regret, rbind(res1, res3, res4, res5, res6))
}

regret <- regret %>% group_by(M, t) %>%
  summarise(regret = mean(cum_regret), sd = sqrt(var(cum_regret) / 10) * 1.96 ,
            lo = regret - sd, hi = regret + sd) %>%
  mutate(M = factor(M, levels = c(3, 9, "oracle-linear", "oracle-logistic", "lasso", "doctor", "ols"), 
                    labels = c("LBGL (M=3)", "LBGL (M=logT)", "Oracle-linear", "Oracle-logistic", "Lasso Bandits", "Doctor", "OLS Bandit"))) %>%
  filter(M != "Online")
 
ind <- seq(300, 5400, by= 100)
regret <- regret %>% filter(t %in% ind)

pp <- ggplot(regret, aes(x=t, y = regret, group = M, col = M , fill = M)) +
  ##   geom_line() + 
  geom_point(alpha = 0.2) + 
  geom_smooth(aes(ymin = lo, ymax = hi), stat = "identity") +
  theme_bw() + 
  xlab("t") +
  ylab("Fraction of incorrect pulls (t)") +
  ylim(c(0.1,1)) +
  theme_font +
  theme(legend.position = c(0.8,0.8),
        legend.background = element_blank()) +
  scale_color_manual(values = c("#1c84c6", "#1ab394", "#f8ac59", "#ed5565", "black", "gray")) +
  scale_fill_manual(values = c("#1c84c6", "#1ab394", "#f8ac59", "#ed5565",  "black", "gray"))

out_file <- sprintf("../figs/warfarin.pdf")
ggsave(out_file, pp, width = 10, height = 6)

