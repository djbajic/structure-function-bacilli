source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')
fit <- fread('data/supernatants_dMMfits.csv')

aux <- gr[c.len<2][,c('comp', 'Vj.fit', 'se')]
aux <- unique(aux)

fig1 <- fit[S1==S2][, c('S1', 'Vj.fit', 'se')]
fig1 <- unique(fig1)
names(fig1) <- c('comp', 'Vj.fit.FIG1', 'se.FIG1')

aux <- merge(fig1, aux, by = 'comp')

p <- 'results/fig_S1B.pdf'
pdf(p, 4,4, family = 'CM Sans')
ggplot(aux, aes(x = Vj.fit.FIG1, y = Vj.fit)) +
    geom_errorbar(aes(ymin=Vj.fit-se, ymax=Vj.fit+se), width = 0) +
    geom_errorbarh(aes(xmin=Vj.fit.FIG1-se.FIG1,
                       xmax=Vj.fit.FIG1+se.FIG1), height = 0) +
    geom_point(shape = 1, size = 2) +
    labs(x = 'Replicate 1 (data from Fig. 1)',
         y = 'Replicate 2 (data from Fig. 2)') +
    theme_bw() + ylim(0, 8.5) +
    geom_abline(intercept = 0, slope = 1, colour = 'red') +
    theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
