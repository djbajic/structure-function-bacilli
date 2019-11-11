source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')
map <- fread('data/stepwise_map_PA.csv')
strfun <- fread('data/structure_function_PA.csv')

p <- 'results/fig_S13B.pdf'
pdf(p, 6, 4)#, family = 'CM Sans')
ggplot(strfun, aes(len.v2, abs(eps.h), colour = V2 %like% 'P')) +
geom_errorbar(aes(ymin=abs(eps.h) - eps.h.se,
                  ymax=abs(eps.h) + eps.h.se),
              size=.5, width = 0, colour = 'gray') +
geom_hline(yintercept = 0, col='gray', linetype = 2) +
geom_point(size = 3) +
scale_colour_manual(values = c('skyblue3', 'chocolate2')) +
theme_bw() + labs(y = 'Deviation from pairwise' ,
                  x = 'Community size',
                  colour = 'P. polymyxa \nin community') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
