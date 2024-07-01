#########################################################
####### Ordinal and interval scale classification #######
################### Simulation trials ###################
#########################################################
############### G. Binotto and R. Delgado ###############
#########################################################


### Libraries
library(ggplot2)
library(reshape2)
library(latex2exp)
#library(RColorBrewer)
library(viridis)


### Sources
source("SimulationsFunc.R")


### Trials

### r=3
## Trial 1: n1 varies.
## Case A: l2<1
Lmax <- 2
l2 <- 0.4
sim1 <- simulation(n.col = c(20, 37, 15), L = c(1, l2))
sim2 <- simulation(n.col = c(40, 37, 15), L = c(1, l2))
sim3 <- simulation(n.col = c(60, 37, 15), L = c(1, l2))
sim4 <- simulation(n.col = c(80, 37, 15), L = c(1, l2))
sim5 <- simulation(n.col = c(100, 37, 15), L = c(1, l2))
sim6 <- simulation(n.col = c(120, 37, 15), L = c(1, l2))

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 0.8) +
    ylim(200, 10^3) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_3$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_1=20$"), TeX("$n_1=40$"), TeX("$n_1=60$"), TeX("$n_1=80$"), TeX("$n_1=100$"), TeX("$n_1=120$")),
                        values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]


## Case B: l2>1
Lmax <- 5
l2 <- 3.7
sim1 <- simulation(n.col = c(5, 37, 15), L = c(1, l2))
sim2 <- simulation(n.col = c(10, 37, 15), L = c(1, l2))
sim3 <- simulation(n.col = c(15, 37, 15), L = c(1, l2))
sim4 <- simulation(n.col = c(20, 37, 15), L = c(1, l2))
sim5 <- simulation(n.col = c(25, 37, 15), L = c(1, l2))
sim6 <- simulation(n.col = c(30, 37, 15), L = c(1, l2))

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 3.5) +
    ylim(300, 2*10^3) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_3$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_1=5$"), TeX("$n_1=10$"), TeX("$n_1=15$"), TeX("$n_1=20$"), TeX("$n_1=25$"), TeX("$n_1=30$")),
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
#                        values = c("#8c273c", "#d62828", "#f77f00", "#335c67", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]



## Trial 2: n2 varies.
## Case A: l2<1
Lmax <- 2
l2 <- 0.4
sim1 <- simulation(n.col = c(20, 7, 15), L = c(1, l2))
sim2 <- simulation(n.col = c(20, 17, 15), L = c(1, l2))
sim3 <- simulation(n.col = c(20, 27, 15), L = c(1, l2))
sim4 <- simulation(n.col = c(20, 37, 15), L = c(1, l2))
sim5 <- simulation(n.col = c(20, 47, 15), L = c(1, l2))
sim6 <- simulation(n.col = c(20, 57, 15), L = c(1, l2))

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 1) +
    ylim(0, 600) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_3$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_2=7$"), TeX("$n_2=17$"), TeX("$n_2=27$"), TeX("$n_2=37$"), TeX("$n_2=47$"), TeX("$n_2=57$")),
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
#                        values = c("#8c273c", "#d62828", "#f77f00", "#335c67", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]


## Case B: l2>1
Lmax <- 5
l2 <- 3.7
sim1 <- simulation(n.col = c(20, 7, 15), L = c(1, l2))
sim2 <- simulation(n.col = c(20, 17, 15), L = c(1, l2))
sim3 <- simulation(n.col = c(20, 27, 15), L = c(1, l2))
sim4 <- simulation(n.col = c(20, 37, 15), L = c(1, l2))
sim5 <- simulation(n.col = c(20, 47, 15), L = c(1, l2))
sim6 <- simulation(n.col = c(20, 57, 15), L = c(1, l2))

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 4.5) +
    ylim(500, 2*10^3) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_3$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_2=7$"), TeX("$n_2=17$"), TeX("$n_2=27$"), TeX("$n_2=37$"), TeX("$n_2=47$"), TeX("$n_2=57$")),
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
#                       values = c("#8c273c", "#d62828", "#f77f00", "#335c67", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]



## Trial 3: n3 varies.
## Case A: l2<1
Lmax <- 2
l2 <- 0.4
# Good: l2 = 0.4, 0.5, 1
sim1 <- simulation(n.col = c(20, 37, 5), L = c(1, l2))
sim2 <- simulation(n.col = c(20, 37, 15), L = c(1, l2))
sim3 <- simulation(n.col = c(20, 37, 25), L = c(1, l2))
sim4 <- simulation(n.col = c(20, 37, 35), L = c(1, l2))
sim5 <- simulation(n.col = c(20, 37, 45), L = c(1, l2))
sim6 <- simulation(n.col = c(20, 37, 55), L = c(1, l2))

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 2) +
    ylim(0, 1.5*10^3) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_3$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_3=5$"), TeX("$n_3=15$"), TeX("$n_3=25$"), TeX("$n_3=35$"), TeX("$n_3=45$"), TeX("$n_3=55$")),
#                       values = c("#88d4ab", "#67b99a", "#469d89", "#248277", "#036666", "#073b3a")) +
#                       values = c("#264653", "#287271", "#2a9d8f", "#faab36", "#f78104", "#fd5901")) +
#                       values = c("#003049", "#6b2c39", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")) +
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
#                        values = c("#8c273c", "#335c67", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
#                       values = brewer.pal(9, "Greens")[c(3, 5:9)]) +
# no #                  values = c("#83c5be", "#42999b", "#006d77", "#006466", "#0b525b", "#1b3a4b")) +
# no #                  values = c("#50bb6c", "#069e2d", "#058e3f", "#04773b", "#036016", "#03440c")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]


## Case B: l2>1
Lmax <- 10
l2 <- 3.7
# Good: l2 = 3.7, 5.3
sim1 <- simulation(n.col = c(20, 37, 15), L = c(1, l2))
sim2 <- simulation(n.col = c(20, 37, 35), L = c(1, l2))
sim3 <- simulation(n.col = c(20, 37, 55), L = c(1, l2))
sim4 <- simulation(n.col = c(20, 37, 75), L = c(1, l2))
sim5 <- simulation(n.col = c(20, 37, 95), L = c(1, l2))
sim6 <- simulation(n.col = c(20, 37, 115), L = c(1, l2))


rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 7) +
    ylim(0, 4*10^3) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_3$"), y = TeX("$TC_{max}^{int}$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_3=15$"), TeX("$n_3=35$"), TeX("$n_3=55$"), TeX("$n_3=75$"), TeX("$n_3=95$"), TeX("$n_3=115$")),
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]



## Trial 4: l2 varies.
Lmax <- 10
n <- c(20, 37, 15)
l2 <- seq(0.1, Lmax, by = 0.1)

df <- simulation.L(n.col = n, l2 = l2)
plot(df$l2, df$xmin, type = 'l')

# A
ggplot(df, aes(l2, xmin, colour = min.TCmax)) + geom_line(size = 1.5) +
    xlim(0, 10) +
    ylim(0, 1.5) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$\u2113_2$"), y = TeX("$x_{min}$"), color = TeX("$TC_{max}^{int}(x_{min})$")) +
    scale_color_gradient(low = "#fcbf49", high = "#d62828") +
    theme(legend.position = c(0.818, 0.32), legend.text.align = 0)

# B
ggplot(df, aes(l2, xmin, colour = min.TCmax)) + geom_line(size = 1.5) +
    xlim(0, 10) +
    ylim(0, 1.5) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$\u2113_2$"), y = TeX("$x_{min}$"), color = TeX("$TC_{max}^{int}(x_{min})$")) +
    scale_color_gradient(low = "#77bca8", high = "#233545") +
    theme(legend.position = c(0.818, 0.32), legend.text.align = 0)
    


### r=4
## Trial 5: n1 varies.
Lmax <- 10
n1 <- 20
n2 <- 37
n3 <- 15
n4 <- 28
l <- c(1, 0.4, 3.7)
sim1 <- simulation(n.col = c(20, n2, n3, n4), L = l)
sim2 <- simulation(n.col = c(40, n2, n3, n4), L = l)
sim3 <- simulation(n.col = c(60, n2, n3, n4), L = l)
sim4 <- simulation(n.col = c(80, n2, n3, n4), L = l)
sim5 <- simulation(n.col = c(100, n2, n3, n4), L = l)
sim6 <- simulation(n.col = c(120, n2, n3, n4), L = l)

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 7) +
    ylim(200, 5*10^4) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_4$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_1=20$"), TeX("$n_1=40$"), TeX("$n_1=60$"), TeX("$n_1=80$"), TeX("$n_1=100$"), TeX("$n_1=120$")),
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]


## Trial 6: n2 varies.
Lmax <- 10
n1 <- 20
n2 <- 37
n3 <- 15
n4 <- 28
l <- c(1, 0.4, 3.7)
sim1 <- simulation(n.col = c(n1, 7, n3, n4), L = l)
sim2 <- simulation(n.col = c(n1, 17, n3, n4), L = l)
sim3 <- simulation(n.col = c(n1, 27, n3, n4), L = l)
sim4 <- simulation(n.col = c(n1, 37, n3, n4), L = l)
sim5 <- simulation(n.col = c(n1, 47, n3, n4), L = l)
sim6 <- simulation(n.col = c(n1, 57, n3, n4), L = l)

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 7) +
    ylim(200, 2*10^4) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_4$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_2=7$"), TeX("$n_2=17$"), TeX("$n_2=27$"), TeX("$n_2=37$"), TeX("$n_2=47$"), TeX("$n_2=57$")),
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]


## Trial 7: n3 varies.
Lmax <- 10
n1 <- 20
n2 <- 37
n3 <- 15
n4 <- 28
l <- c(1, 0.4, 3.7)
sim1 <- simulation(n.col = c(n1, n2, 15, n4), L = l)
sim2 <- simulation(n.col = c(n1, n2, 25, n4), L = l)
sim3 <- simulation(n.col = c(n1, n2, 35, n4), L = l)
sim4 <- simulation(n.col = c(n1, n2, 45, n4), L = l)
sim5 <- simulation(n.col = c(n1, n2, 55, n4), L = l)
sim6 <- simulation(n.col = c(n1, n2, 65, n4), L = l)

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 6) +
    ylim(2500, 1.2*10^4) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_4$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_3=15$"), TeX("$n_3=25$"), TeX("$n_3=35$"), TeX("$n_3=45$"), TeX("$n_3=55$"), TeX("$n_3=65$")),
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]


## Trial 8: n4 varies.
Lmax <- 10
n1 <- 20
n2 <- 37
n3 <- 15
n4 <- 28
l <- c(1, 0.4, 3.7)
sim1 <- simulation(n.col = c(n1, n2, n3, 8), L = l)
sim2 <- simulation(n.col = c(n1, n2, n3, 18), L = l)
sim3 <- simulation(n.col = c(n1, n2, n3, 28), L = l)
sim4 <- simulation(n.col = c(n1, n2, n3, 38), L = l)
sim5 <- simulation(n.col = c(n1, n2, n3, 48), L = l)
sim6 <- simulation(n.col = c(n1, n2, n3, 58), L = l)

rightint <- seq(0.01, Lmax, by = 0.01)
df <- data.frame(rightint = rightint,
                 sim1 = sim1$rightint.TCmax,
                 sim2 = sim2$rightint.TCmax,
                 sim3 = sim3$rightint.TCmax,
                 sim4 = sim4$rightint.TCmax,
                 sim5 = sim5$rightint.TCmax,
                 sim6 = sim6$rightint.TCmax)

df <- melt(df ,  id.vars = 'rightint', variable.name = 'simulation')

df.min <- data.frame(xmin = c(sim1$xmin, sim2$xmin, sim3$xmin, sim4$xmin, sim5$xmin, sim6$xmin),
                     min.TCmax = c(sim1$min.TCmax, sim2$min.TCmax, sim3$min.TCmax, sim4$min.TCmax, sim5$min.TCmax, sim6$min.TCmax),
                     simulation = c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6")) 

ggplot(df, aes(rightint,value)) + geom_line(aes(colour = simulation), size = 1.1) +
    geom_point(data = df.min, aes(x = xmin, y = min.TCmax, colour = simulation), size = 2.5) +
    xlim(0, 8) +
    ylim(2500, 2*10^4) +
    theme_grey(base_size = 28) +
    labs(x = TeX("$x=\u2113_4$"), y = TeX("$TC_{max}^{int}(x)$"), color = "Legend") +
    scale_color_manual(labels = c(TeX("$n_4=8$"), TeX("$n_4=18$"), TeX("$n_4=28$"), TeX("$n_4=38$"), TeX("$n_4=48$"), TeX("$n_4=58$")),
                       values = c("#335c67", "#8c273c", "#d62828", "#f77f00", "#fcbf49", "#e6ddac")) +
    theme(legend.position = c(0.88, 0.75), legend.text.align = 0)

df.min
df.min$min.TCmax[2:6]-df.min$min.TCmax[1:5]


## Trial 9: l2 and l3 vary.
n <- c(20, 37, 15, 28)
l2 <- 1:3
l3 <- 4:6
Lmax <- 10
simulation.r4.L(n.col = n, l2 = l2, l3 = l3)

#Lmax <- 3
Lmax <- 10
l2 <- seq(0.1, Lmax, by = 0.1)
l3 <- seq(0.1, Lmax, by = 0.1)
#l2 <- seq(0.01, Lmax, by = 0.01)
#l3 <- seq(0.01, Lmax, by = 0.01)
#Lmax <- 30
#l2 <- 1:30
#l3 <- 1:30

start_time <- Sys.time()
df <- simulation.r4.L(n.col = n, l2 = l2, l3 = l3)
end_time <- Sys.time()

end_time-start_time

ggplot(df, aes(x = l2, y = l3, fill = xmin)) +
    geom_tile() +
    theme_grey(base_size = 28) +
    scale_fill_gradient(low = "#77bca8", high = "#233545") +
    labs(x = TeX("$\u2113_2$"), y = TeX("$\u2113_3$"), fill = TeX("$x_{min}$")) 

ggplot(df, aes(x = l2, y = l3, fill = min.TCmax)) +
    geom_tile() +
#    xlim(0, 1) +
#    ylim(0, 1) +
    theme_grey(base_size = 28) +
    scale_fill_gradient(low = "#77bca8", high = "#233545") +
    labs(x = TeX("$\u2113_2$"), y = TeX("$\u2113_3$"), fill = TeX("$TC_{max}^{int}(x_{min})$"))

# Other colors options
ggplot(df, aes(x = l2, y = l3, fill = xmin)) +
    geom_tile() +
    theme_grey(base_size = 28) +
    scale_fill_viridis(option = "D", direction = -1, end = 1) +
    #scale_fill_viridis(option = "G", direction = -1) +
    labs(x = TeX("$\u2113_2$"), y = TeX("$\u2113_3$"), fill = TeX("$x_{min}$")) 

ggplot(df, aes(x = l2, y = l3, fill = min.TCmax)) +
    geom_tile() +
    #    xlim(0, 1) +
    #    ylim(0, 1) +
    theme_grey(base_size = 28) +
    scale_fill_viridis(option = "D", begin = 0, end = 1, values = c(0, 0.1, 1), direction = -1) +
    labs(x = TeX("$\u2113_2$"), y = TeX("$\u2113_3$"), fill = TeX("$TC_{max}^{int}$"))
