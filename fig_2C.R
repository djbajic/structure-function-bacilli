source('functions.R')
strfun <- fread('data/structure_function_PA.csv')

aux <- strfun[V2 %in% c('M', 'T', 'M-T')]

p <- 'results/fig_2C.pdf'
pdf(p, 4, 4, family = 'CM Sans')
ggplot(aux, aes(x = len.v2, y = Vj.real.V1)) +
geom_segment(data = aux, colour = 'gray30', alpha=.5,
             mapping = aes(x = len.v1, xend = len.v2,
                           y = Vj.1.V1, yend = Vj.1.V2)) +
geom_segment(data = aux, colour = 'red3', alpha=.5,
             mapping = aes(x = len.v1, xend = len.v2,
                           y = Vj.real.V1, yend = Vj.real.V2)) +
geom_errorbar(aes(ymin = Vj.real.V2 - se.real.V2,
                  ymax = Vj.real.V2 + se.real.V2), width = 0.1,
              colour = 'red3', alpha = .5) +
labs(x = 'Number of species', y='Vj') +
theme_bw() + ylim(0, 12) + xlim(0, 2.11) + labs(title = 'Order 2 (including pairwise)') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
