source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')
map <- fread('data/stepwise_map_PA.csv')
strfun <- fread('data/structure_function_PA.csv')

p <- 'results/fig_S13C.pdf'
pdf(p, 5, 4, family = 'CM Sans')
ggplot(strfun, aes(len.v2, abs(eps.h3), colour = V2 %like% 'P')) +
    geom_hline(yintercept = 0, col='gray', linetype = 2) +
    geom_errorbar(aes(ymin=abs(eps.h3) - eps.h3.se,
                      ymax=abs(eps.h3) + eps.h3.se),
                  size=.5, width = 0, colour = 'gray') +
    geom_point(size = 3) +
    scale_colour_manual(values = c('skyblue3', 'chocolate2')) +
    theme_bw() + labs(title = 'Communities',
                      y = 'Higher order epistasis' ,
                      x = 'Community size',
                      colour = 'P. polymyxa \nin community') +
    theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
