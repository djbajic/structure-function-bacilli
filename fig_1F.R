d <- fread('data/data_supernatants.csv')
s.coef <- fread('data/singles_coefficients_fig_1.csv')
fit <- fread('data/supernatants_dMMfits.csv')

pdf('results/fig1_F.pdf', 4,4)
ggplot(fit[S1!=S2], aes(x = Vj.pred, y = Vj.fit)) +
    geom_smooth(method = "lm", se = FALSE, colour = 'red') +
    geom_point(shape = 1, size=3) + 
    scale_y_log10() + scale_x_log10() +
    annotation_logticks(sides="bl", size=.1) +
    theme_bw() + theme(panel.grid = element_blank()) +
    geom_errorbar(aes(ymin = Vj.fit - 2*se,
                      ymax = Vj.fit + 2*se), width = 0.02, size = .2) +
    geom_errorbarh(aes(xmin = Vj.pred - 2*Vj.pred.se,
                       xmax = Vj.pred + 2*Vj.pred.se), height = 0.02, size = .2) +
    xlab(expression('V'['j,predicted'])) +
    ylab(expression('V'['j,fitted']))
dev.off()

with(fit[S1!=S2], summary(lm(Vj.pred~Vj.fit)))
