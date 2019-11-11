source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')
strfun <- fread('data/structure_function_PA.csv')

p <- 'results/inset_2A.pdf'
pdf(p, 4, 4, family = 'CM Sans')
ggplot(gr[c.len<2], aes(x = c.len, y = Vj.fit)) +
geom_segment(data = strfun[len.v2<2], colour = 'gray30', alpha=.5, 
             mapping = aes(x = len.v1, xend = len.v2,
                           y = Vj.1.V1, yend = Vj.1.V2)) +
labs(x = 'Number of species', y='Vj') +
theme_bw() + ylim(0, 8) + xlim(0, 1) + labs(title = 'Order 1 (only single effects)') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
