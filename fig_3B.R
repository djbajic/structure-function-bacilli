source('functions.R')
d <- fread('data/three_sp_consortia.csv')

p <- 'results/fig_3B.pdf'
pdf(p, 5, 4, family = 'CM Sans')
ggplot(subset(d, bg!='-'), aes(as.factor(bg), d.epsilon.ABC,
                                 colour = pair %like% 'P' | bg == 'P')) +
    geom_hline(yintercept = 0, col='gray') +
    geom_errorbar(aes(ymin=d.epsilon.ABC - d.epsilon.ABC.se,
                      ymax=d.epsilon.ABC + d.epsilon.ABC.se),
                  size=.5, width = .3, colour = 'gray') +
    geom_point(, size = 4) +
    scale_colour_manual(values = c('skyblue3', 'chocolate2')) +
    theme_bw() + labs(title = 'Pairs cocultured with one background strain',
                      y =  expression(paste(epsilon, ' = V'[obs] - 'V'[exp])),
                      x = 'Background strain',
                      colour = 'Background \nstrain') +
    theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
