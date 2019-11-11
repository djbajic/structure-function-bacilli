source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')
map <- fread('data/stepwise_map_PA.csv')
strfun <- fread('data/structure_function_PA.csv')

p <- 'results/fig_S13A.pdf'
pdf(p, 6, 4)#, family = 'CM Sans')
ggplot(strfun, aes(Vj.2.V2, Vj.real.V2, colour = len.v2)) +
geom_errorbarh(aes(xmin=Vj.2.V2 - Vj.2.V2.se,
                   xmax=Vj.2.V2 + Vj.2.V2.se),
               size=.5, width = 0, colour = 'gray') +
geom_errorbar(aes(ymin=Vj.real.V2 - se.real.V2,
                  ymax=Vj.real.V2 + se.real.V2),
              size=.5, width = 0, colour = 'gray') +
geom_point(size = 3) + xlim(0, 100) + ylim(0, 50) +
theme_bw() + labs(y = 'Measured' ,
                  x = 'Predicted using pairwise model',
                  colour = 'Species in\n community') +
geom_abline(intercept = 0, slope = 1) +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
