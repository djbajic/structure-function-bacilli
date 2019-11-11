source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')
strfun <- fread('data/structure_function_PA.csv')

p <- 'results/fig_2B.pdf'
pdf(p, 4, 4, family = 'CM Sans')
ggplot(gr, aes(x = c.len, y = Vj.fit)) +
geom_point(data = strfun, colour = 'gray30', alpha=.4, size = 1,
           aes(x = len.v2, y = Vj.real.V2)) +
geom_segment(data = strfun, colour = 'gray30', alpha=.5,
             mapping = aes(x = len.v1, xend = len.v2,
                           y = Vj.real.V1, yend = Vj.real.V2)) +
labs(x = 'Number of species', y='Vj') +
theme_bw() + ylim(0, 40) + xlim(0, 6) + labs(title = 'Observed landscape') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
