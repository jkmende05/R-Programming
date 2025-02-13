# Libraries to be used
library(e1071)
library(kernlab)

# Get random points to plot that are clearly separable
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))

x[y == 1, ] <- x[y == 1, ] + 3 / 2
df <- data.frame(x = x, y = as.factor(y))

ggplot(data = df, aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 6) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  theme(legend.position = "none") +
  theme_minimal(base_size = 24)

# Apply support vector machine and plot
svmfit <- svm(y ~ ., data = df, kernel = "linear", scale = FALSE)
plot(svmfit, df)

kernfit <- ksvm(x, y, type = "C-svc", kernel = "vanilladot")
plot(kernfit, data = x)

# Get random points that are not fully separable by linear boundary
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
df <- data.frame(x = x, y = as.factor(y))

ggplot(data = df, aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 6) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  theme(legend.position = "none") +
  theme_minimal(base_size = 24)

# Get svm and plot
svmfit <- svm(y ~ ., data = df, kernel = "linear", cost = 10)
plot(svmfit, df)

# Apply non-linear boundary to random points
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2.5
x[101:150, ] <- x[101:150, ] - 2.5
y <- c(rep(1, 150), rep(2, 50))
df <- data.frame(x = x, y = as.factor(y))

ggplot(data = df, aes(x = x.2, y = x.1, color = y, shape = y)) +
  geom_point(size = 6) +
  scale_color_manual(values = c("#000000", "#FF0000")) +
  theme(legend.position = "none") +
  theme_minimal(base_size = 24)

set.seed(123)

training <- base::sample(200, 100, replace = FALSE)
svmfit <- svm(y ~ ., data = df[training, ], kernel = "radial",
              gamma = 1, cost = 1)
plot(svmfit, df)