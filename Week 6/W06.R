library(dplyr)
library(tidyr)
library(ggplot2)

X <- read.csv("session_ab_test.csv")

X <- X %>% mutate(group = ifelse(group == "control", "Version.1", "Version.2"))

ggplot(X, aes(y = group, x = session_duration_min, fill = group)) + 
  geom_violin(alpha=.25, show.legend = FALSE) + 
  geom_jitter(alpha = .5, stroke=0, size = .75, height = .05, show.legend = FALSE) + 
  geom_boxplot(alpha = 0, show.legend = FALSE, width = .2, outlier.shape = NA) + 
  theme_minimal() + 
  xlab("Session duration (min)")

# ggsave("01.png", width = 1800, height = 800, units = "px")

X <- X %>% mutate(session_duration_min = log10(session_duration_min))

ggplot(X, aes(y = group, x = session_duration_min, fill = group)) + 
  geom_violin(alpha=.25, show.legend = FALSE) + 
  geom_jitter(alpha = .5, stroke=0, size = .75, height = .05, show.legend = FALSE) + 
  geom_boxplot(alpha = 0, show.legend = FALSE, width = .2, outlier.shape = NA) + 
  theme_minimal() + 
  xlab("log10(Session duration (min))")

# ggsave("02.png", width = 1800, height = 800, units = "px")

x1 <- X$session_duration_min[X$group == "Version.1"]
x2 <- X$session_duration_min[X$group == "Version.2"]

t.test(x1, x2, conf.level = 0.99)

# Alternatively, you can use the formula notation:
# t.test(session_duration_min ~ group, data = X)

# ==================================

X <- read.csv("fintech_model_comparison.csv")

ggplot(X, aes(x = model, y = profit_usd)) + 
  geom_violin(alpha = 0) + 
  geom_jitter(alpha = .5, stroke=0, size = .75, width = .05) + 
  geom_boxplot(aes(fill = model), alpha = 0.5, 
               show.legend = FALSE, width = .2, 
               outlier.shape = NA) +
  theme_minimal() + 
  xlab("") + ylab("Profit (USD)")

# ggsave("03.png", width = 1800, height = 800, units = "px")

myaov <- aov(profit_usd ~ model, data = X)
summary(myaov)

par(mfrow = c(2,2))
plot(myaov, pch = 16, cex = .5, las = 1)
par(mfrow = c(1,1))


ggplot(data.frame(x=myaov$residuals), aes(sample = x)) + 
  geom_qq() + geom_qq_line(col = "red") +
  theme_minimal() + 
  xlab("Normal quantiles") + ylab("Residuals")


X %>%
  mutate(residual = myaov$residuals) %>%
  ggplot(aes(x = model, y = residual)) + 
  geom_violin(aes(fill = model), alpha = .25, show.legend = FALSE) + 
  geom_jitter(width = .05, alpha = .1) +
  theme_minimal()

