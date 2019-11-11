source('functions.R')
# .. tests with media suplemented with vitamins

vit <- fread('data/data_Polymyxa_vitamins.csv')
vit <- melt(vit)
vit[, value := ifelse(value<0, 0, value)]
vit <- vit[,.(od.m = mean(value), se = sd(value)/sqrt(2)), by = variable]

p <- 'results/fig_S12.pdf'
pdf(p, 4,4, family = 'CM Sans')
ggplot(vit, aes(variable, od.m)) + 
geom_bar(stat="identity",
         fill="chocolate2", # Use black outlines,
         size=.3) +      # Thinner lines
geom_errorbar(aes(ymin=od.m-se, ymax=od.m+se),
              size=.3,    # Thinner lines
              width=.2) +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
