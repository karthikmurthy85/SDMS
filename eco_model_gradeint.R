ecog <- read.csv('/home/karthik/jupyter_dir/SDMS/return_indices_gradientHarvest.csv')
head(ecog)
ecog$X <- NULL

delB <- NA

ids <- unique(ecog$id)
for (i in ids){
  
  a1 <- subset(ecog, id %in% i)
  aa1 <- a1[, c(2:4, 6:8)]
  az1 <- apply(aa1, 2, scale)
  az2 <- apply(az1, 1, mean)
  az3 <- 1/exp(az2)
  
  a1$IRI0 <-  az3
  
  library(ggplot2)
  ggplot(a1, aes(x = vHamp, y = IRI0, colour = Resl, fill = Resl))+
    geom_point() + geom_line()
  
  moddf <- with( a1, data.frame(y = IRI0, x = (uH), cat = Resl) )
                 
  mod <- (lm(y ~ x + cat, data = moddf))
  delB[i] <- mod$coefficients[3]
  # confint(mod)
  
}

library(ggplot2)
x_lb <- expression(mu~'(force)')
y_lab <- expression(IRI[0])
ggplot(a1, aes(x = uH, y = IRI0, colour = Resl, fill = Resl))+
  geom_point() + geom_line()+
  geom_point(size=2) + geom_line() +
  ggtitle(" trend in resilience index \n with gradient of force ") +
  ylab(y_lab) + xlab(x_lb)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.position = c(.8,.85),
    #legend.spacing.y = unit(1, 'cm'),
    legend.key.height = unit(1, 'cm'),
    legend.text = element_text(color = "black", size = 20),
    axis.text = element_text(size = 14, colour = 'black'),
    axis.title = element_text(size = 16, colour = 'black'),
    plot.title = element_text(size = 20, colour = 'black', hjust =0.5)
  )+
  guides(colour = guide_legend(override.aes = list(size=3)))

library(reshape2)
eco_conf <- melt(quantile(delB*-1, probs = c(0.025, 0.5, 0.975)))


##differential resilience capabilites ##

edf <- read.csv('~/jupyter_dir/SDMS/ecological_model_results.csv')
head(edf)

rest <- t.test(edf$High.resilience, edf$Low.resilience, paired = TRUE)
rest
dff <- data.frame(x = 'resilience', y = rest$estimate, 
                  ymin = rest$conf.int[1], ymax = rest$conf.int[2])

y_lb <- expression(beta~IRI[0])
ggplot(dff, aes(x= x, y = y))+
  geom_point(size=20)+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.2,
                position=position_dodge(.9))+
  ggtitle("differential force") +
  xlab("") + ylab(y_lab) +
  theme_bw()


