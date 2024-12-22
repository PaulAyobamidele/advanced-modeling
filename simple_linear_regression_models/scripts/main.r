library(tidyverse)
x <- rnorm(50)
head(x)
y <- 10 + 3 * x
plot(x, y, pch = 19)
error <- rnorm(50)
y <- 10 + 3 * x + error

plot(x, y, pch = 19)

mdl <- lm(y ~ x)
mdl
head(fitted(mdl))


summary(mdl)
coef(mdl)


# check for 10

coef(mdl)["(Intercept)"] + coef(mdl)["x"] * 10

# 38.00241


xvals <- seq(from = -3, to = 3, by = 0.1)
xvals


new <- tibble(x = xvals)
new


new$fit <- predict(mdl, new)
new


mydf <- tibble(x = x, y = y)
mdl <- lm(y ~ x, data = mydf)


library(broom)
tidy(mdl)
glance(mdl)
augment(mdl)


mydf %>% ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal()



null_mdl <- lm(y ~ 1, data = mydf)
coef(null_mdl)


mean(y)
