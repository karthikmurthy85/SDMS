vgrn_m<- read.csv('/home/karthik/jupyter_dir/SDMS/return_indices_gradRain_vegM.csv')
head(vgrn_m)

vgrn_m$X <- NULL

delB <- NA

ids <- unique(vgrn_m$id)
for (i in ids){
  
  a1 <- subset(vgrn_m, id %in% i)
  aa1 <- a1[, c(2:4, 6:8)]
  az1 <- apply(aa1, 2, scale)
  az2 <- apply(az1, 1, mean)
  az3 <- 1/exp(az2)
  
  a1$IRI0 <-  az3
  
  library(ggplot2)
  # ggplot(a1, aes(x = vHamp, y = IRI0, colour = Resl, fill = Resl))+
  #   geom_point() + geom_line()
  
  moddf <- with( a1, data.frame(y = IRI0, x = (vH), cat = Resl) )
  moddf1 <-  subset(moddf, cat %in% 'low')
  moddf2 <- subset(moddf, cat %in% 'high')
  
  plot(moddf2$x, moddf2$y,ty='o', col='green2', ylim = c(0,4))
  points(moddf1$x, moddf1$y, ty='o', col='red2')
  

  
  # ggplot(moddf, aes(x = x, y = (y), colour = cat, fill = cat))+
  #   geom_point() + geom_line() + ggtitle(i)
  # 
  
  mod <- (lm((y) ~ x + cat, data = moddf))
  delB[i] <- mod$coefficients[3]
  
  print(i)
  
}

delB
summary(delB)



library(ggplot2)
x_lb <- expression(mu~'(rain)')
y_lab <- expression(IRI[0])
ggplot(a1, aes(x = (uH), y = IRI0, colour = Resl, fill = Resl))+
  geom_point() + geom_line()+
  geom_point(size=2) + geom_line() +
  ggtitle("b) trend in resilience index with \n gradient of precipitation ") +
  ylab(y_lab) + xlab(x_lb)+
  theme_bw()+
  theme(
    legend.title = element_blank(),
    legend.position = c(.7,.85),
    #legend.spacing.y = unit(1, 'cm'),
    legend.key.height = unit(1, 'cm'),
    legend.text = element_text(color = "black", size = 20),
    axis.text = element_text(size = 14, colour = 'black'),
    axis.title = element_text(size = 16, colour = 'black'),
    plot.title = element_text(size = 20, colour = 'black', hjust =0.5)
  )+
  guides(colour = guide_legend(override.aes = list(size=3)))



melt(quantile(delB*-1, probs = c(0.025, 0.5, 0.975)))

## Similar precipitation IRI0

vgsm_df <- read.csv('~/jupyter_dir/SDMS/veg_soilmoisture_model_results000.csv')
head(vgsm_df)
main_lb <- expression('a) Histogram'~Delta~IRI[0])
x_lb <-  expression(Delta~IRI[0])
hist(vgsm_df$High.resilience - vgsm_df$Low.resilience, breaks =30, 
     main = main_lb, xlab = x_lb, col='royalblue2')
quantile(vgsm_df$High.resilience - vgsm_df$Low.resilience, probs = c(0.025, 0.5, 0.975))

t.test(vgsm_df$High.resilience, vgsm_df$Low.resilience, paired = TRUE, alternative = "two.sided")


