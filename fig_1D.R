d <- fread('data/data_supernatants.csv')
s.coef <- fread('data/singles_coefficients_fig_1.csv')
fit <- fread('data/supernatants_dMMfits.csv')

pdf('results/fig1_D.pdf', 11.4,2.2)
ggplot(d[S1==S2], aes(x = xt, y = f.j)) +
    geom_line(data = fit[S1 == S2], aes(x = xt, y = f.fitted)) +
    geom_point(colour = 'red', size = 1) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks=c(0, 6, 12)) +
    scale_y_continuous(breaks=c(0,.5, 1)) + 
    facet_grid(. ~ ordered(S1, unique(S1)))
dev.off()
