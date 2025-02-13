library(openxlsx)
library(ggplot2)

# Read in hurricane classification in North America Data
hurricanes <- read.xlsx(paste0("https://userpage.fu-berlin.de/soga/data",
                               "/raw-data/hurricanes.xlsx"))

str(hurricanes)

# Create bar chart by the number and type of hurricane
ggplot(hurricanes, aes(x = Year, fill = factor(Type))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(
    name = "Type of Hurricane",
    labels = c("tropical-only", "baroclinic influences",
               "baroclinic initiation")
  ) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Change the type column
# 0 applies to tropical, 1 applies to non-tropical hurricanes
hurricanes$Type_new <- ifelse(test = hurricanes$Type == 0, yes = 0, no = 1)
table(hurricanes$Type_new)

# Apply logistic regression model
log_reg_model <- glm(Type_new ~ FirstLat, data = hurricanes,
                     family = "binomial")
summary(log_reg_model)

predict(log_reg_model, newdata = list(FirstLat = c(10, 23.5, 30)),
        type = "response")

# Create graph to display logistic regression modelling
lats <- seq(min(hurricanes$FirstLat), max(hurricanes$FirstLat), 0.1)

probs <- predict(log_reg_model,
  newdata = data.frame(FirstLat = lats),
  type = "response",
  se.fit = TRUE
)

pm <- probs$fit
pu <- probs$fit + probs$se.fit * 1.96 # 95% confidence interval
pl <- probs$fit - probs$se.fit * 1.96 # 95% confidence interval

plot(hurricanes$FirstLat,
  hurricanes$Type_new,
  pch = 16,
  cex = 1,
  ylab = "Probability",
  xlab = "Formation Latitude /(N/)"
)

grid()
lines(lats, pm, lwd = 2)
abline(h = 0.1, lty = 2)