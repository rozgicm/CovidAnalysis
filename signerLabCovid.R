library(ggplot2)
library(plyr)
library(see)

growth = function(q, k) {
  q^k
}

cases = function(q, k) {
  sum(q^(0:k))
}


d = expand.grid(q = c(2.5, 1.5, 0.625), k = seq(0, 6, 0.1))
d$days = d$k * 5
d = ddply(d, .(q, k, days), function(df) {
  data.frame(growth = growth(df$q, df$k))
})

ggplot(d, aes(x = days, y = growth, color = as.factor(q), group = q)) +
  geom_line() +
  geom_point(data = d[d$k %in% 0:6,]) +
  labs(x = "Days", y = "Increase") +
  scale_color_material_d(name = "Number of persons infected by one person in 5 days") +
  theme_lucid() +
  theme(legend.position = "top")

ggplot(d[d$k %in% 0:6,], aes(growth, as.factor(q), fill = factor(days, levels = seq(30, 0, -5), labels = seq(30, 0, -5)))) +
  geom_bar(stat = "identity", orientation = "y") +
  labs(x = "Total number of infected persons", y = "Number of persons infected by one person") +
  scale_x_continuous(breaks = seq(0, 800, 50)) +
  scale_fill_material_d(name = "Days") +
  theme_lucid()


e = ddply(d[d$k %in% 0:6,], .(q), function(df) {
  data.frame(cases = cumsum(df$growth), k = df$k, days = df$days, growth = df$growth)
})

ggplot(e, aes(x = days, y = cases, color = as.factor(q), group = q)) +
  geom_line() +
  geom_point() +
  labs(x = "Days", y = "Total number of infected persons") +
  scale_color_material_d(name = "Number of persons infected by one person") +
  theme_lucid() +
  theme(legend.position = "top")
# zu eckig


f = expand.grid(q = seq(0.05, 3, 0.01), k = 0:6)
f$days = f$k * 5
f = ddply(f, .(q, k, days), function(df) data.frame(cases = cases(df$q, df$k)))

ggplot(f, aes(q, cases, color = as.factor(days))) +
  geom_point() +
  labs(x = "Number of persons infected by one person", y = "Total number of infected persons") +
  scale_x_continuous(breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 2000, 50)) +
  scale_color_material_d(name = "Days") +
  theme_lucid()


g = expand.grid(q = seq(0.1, 3, 0.1), k = 6)
g$days = g$k * 5
g = ddply(g, .(q, k, days), function(df) data.frame(cases = cases(df$q, df$k)))

ggplot(g, aes(cases, as.factor(q))) +
  geom_bar(stat = "identity", orientation = "y") +
  labs(y = "Number of persons infected by one person", x = "Total number of infected persons") +
  scale_x_continuous(breaks = seq(0, 2000, 50)) +
  theme_lucid()


h = expand.grid(q = seq(0.1, 3, 0.1), k = 0:6)
h$days = h$k * 5
h = ddply(h, .(q, k, days), function(df) data.frame(growth = growth(df$q, df$k)))

ggplot(h, aes(growth, as.factor(q), fill = factor(days, levels = seq(30, 0, -5), labels = seq(30, 0, -5)))) +
  geom_bar(stat = "identity", orientation = "y") +
  labs(y = "Number of persons infected by one person", x = "Total number of infected persons") +
  scale_x_continuous(breaks = seq(0, 2000, 50)) +
  scale_fill_material_d(name = "Days") +
  theme_lucid()
