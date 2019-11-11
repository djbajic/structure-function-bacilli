
source('functions.R')
k <- fread('data/M_langm_data.csv')
fitline <- fread('data/M_langm_fitline.csv')

clrs <- c("#C4961A", "maroon3", "mediumseagreen", "chocolate2", "#4E84C4", "#293352")

pdf('results/C_SN_titration_langmuir.pdf', 6, 4)
ggplot(k, aes(x = cfu.m, y = fit)) +
geom_line(data = fitline, aes(x = cfu.m, y = predicted), linetype = 'dashed', 
          inherit.aes=FALSE) +
geom_hline(aes(yintercept = mean(fit[fit>0])),
           linetype = 'dashed', colour = 'red2') +
geom_ribbon(aes(x = cfu.m,
                ymin = mean(fit[fit>0])-sd(fit[fit>0]),
                ymax = mean(fit[fit>0])+sd(fit[fit>0])),
            fill = 'red3', alpha=0.15, inherit.aes=FALSE) +
geom_point(aes(shape = cond.SN,
               color = cond.SN, alpha = 10*as.numeric(cond.SN.dil)),
           size = 3, stroke=2) +
geom_errorbar(aes(ymin = fit-fit.se, ymax = fit+fit.se,
                  color = cond.SN, alpha = 10*as.numeric(cond.SN.dil)),
              width = 0) +
geom_errorbarh(aes(xmin = cfu.m-cfu.se, xmax = cfu.m+cfu.se,
                   color = cond.SN, alpha = 10*as.numeric(cond.SN.dil))) +
scale_color_manual(values = clrs) +
scale_shape_manual(values = c(0, 4, 1, 2, 5:6)) +
theme_bw() +
scale_y_log10(limits = c(0.05, 60)) + 
scale_x_continuous(labels = scales::scientific) +
theme(panel.grid = element_blank()) +
annotation_logticks(sides="l", size=.1) +
labs(x = 'CFU', y = 'V')
dev.off()
