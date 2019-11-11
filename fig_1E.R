d <- fread('data/data_supernatants.csv')
s.coef <- fread('data/singles_coefficients_fig_1.csv')
fit <- fread('data/supernatants_dMMfits.csv')

pdf('results/fig1_E.pdf', 10,10)
ggplot(d[S1!=S2], aes(x = xt, y = f.j)) +
    geom_ribbon(data = fit[S1 != S2], inherit.aes = FALSE,
                aes(x = xt, ymin = lower.f.pred,
                    ymax = upper.f.pred), fill = "gray90") +
    geom_line(data = fit[S1 != S2], aes(x = xt, y = f.pred), colour = 'gray') +
    geom_line(data = fit[S1 != S2], aes(x = xt, y = f.fitted)) +
    geom_point(colour = 'red', size = 2) +
    theme_bw() + ylim(0, 1) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks=c(0, 6, 12)) +
    scale_y_continuous(breaks=c(0,.5, 1)) + 
    facet_grid(ordered(S1, unique(S1)) ~ S2)
dev.off()
