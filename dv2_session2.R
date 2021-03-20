library(ggplot2)
library(GGally)
library(data.table)
library('hexbin')
df <- fread('http://bit.ly/CEU-R-numbers-set')

# some DEA 
str(df)
summary(df)
hist(df$y)
hist(df$x)
ggpairs(df)
plot(df)

ggplot(df, aes(x,y)) +geom_point()
ggplot(df, aes(x,y)) +geom_point(alpha=.1) # 能更清楚看到点的分布

ggplot(df, aes(x,y)) + geom_hex() # 颜色功越深，频率越高

ggplot(df, aes(factor(x),y)) + geom_boxplot() # factor化，但是需要优化：改为提琴图

ggplot(df, aes(factor(x),y)) + geom_violin()

ggplot(df, aes(factor(x),y)) + geom_violin() +geom_jitter() # 加了噪音，所以点点不会完全重合

ggplot(df, aes(factor(x),y)) + geom_violin() +
  geom_jitter(height =0, width = 0.1) 

ggplot(df, aes(y, fill = factor(x))) + geom_density()
ggplot(df, aes(y, fill = factor(x))) + geom_density(alpha =0.5)

ggplot(df, aes(y)) + geom_density() + facet_wrap(~x)
ggplot(df, aes(y)) + geom_histogram() + facet_wrap(~x)

# heatmap: mean and sd dev of y per x
# library(dplyr)
# df_m <- df %>% mutate(mean_y = mean(y, na.rm = T),
#                       std_y = sd(y),na.rm = T) %>% as.matrix()
# 
# heatmap(df)

# another answer:
df[, list(mean= mean(y), sd= sd(y)), by= x]

# create a long table
?melt
df1 <- melt(df[, list(mean= mean(y), sd= sd(y)), by= x], id = 'x')
ggplot(df1, aes(x, variable, fill = value)) + geom_tile()

# clustering
?hclust

dm <- dist(iris[,1:4])
hc <- hclust(dm)

plot(hc)
rect.hclust(hc, k=3)

# animation
for (i in 2:8) {
  plot(hc)
  rect.hclust(hc, k=i)
  Sys.sleep(1)
}

library(animation)
ani.options(interval = 1) # sleep time = 1s
saveGIF({
  for (i in 2:8) {
    plot(hc)
    rect.hclust(hc, k=i)
  }
})

library(dendextend) #可以多次plot d,还可以加ggplot
d <- as.dendrogram(hc)
d <- color_branches(d, k=3)
plot(d)

ggplot(d)
ggplot(d, horiz=T, labels= F)
ggplot(d, labels= F) + scale_y_reverse(expand=c(.2,1))+ coord_polar(theta = "x")

saveGIF({
  for (i in 2:8) {
    d <- as.dendrogram(hc)
    d <- color_branches(d, k=i)
    print(ggplot(d))
  }
})

## cluster members
clusters <- cutree(hc,3)

library(NbClust)
NbClust(iris[,1:4], method = 'complete', index = 'all')

ggplot(iris, aes(Sepal.Length,Sepal.Width, shape = Species, color = clusters)) + geom_point()

ggplot(iris, aes(Sepal.Length,Sepal.Width, shape = Species, color = factor(clusters))) + 
  geom_point(size = 3) # see overlap

## add linear model by both species and cluster membership
ggplot(iris, aes(Sepal.Length,Sepal.Width, shape = Species, color = factor(clusters))) + 
  geom_point(size = 3) + geom_smooth(method = 'lm')

ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters), 
                 linetype = Species)) + 
  geom_point(size = 3) + geom_smooth(method = 'lm')

##  + extra dimension: time
saveGIF(
  
)

library(gganimate)
ggplot(iris,aes(Sepal.Length, Sepal.Width,
                colors = factor(clusters))) +
         geom_point(size=3)+
         geom_smooth(method = 'lm')+
         transition_states(Species) +
  labs(title = "{closest_state}") # f-string, glue,改变了标题

library(glue)
?paste
?sprintf # Use C-style String Formatting Commands
glue('2+2={2+2}') # {}里面能run程序！
sprintf('2+2= %s ', 2+2) 

# add subtitle
ggplot(iris, aes(Sepal.Length, Sepal.Width,
                 colors = factor(clusters)))+
  geom_point(size = 3) + 
  geom_smooth(method = 'lm') +
  transition_states(Species) +
  labs(title = '{closest_state}',
       subtitle = 'Number of flowers: {nrow(subset(iris, Species == closest_state))}')

# c&p from last week
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
bookings[, price_per_night := price / nnights]
hotels <- merge(
  features,
  bookings[, .(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id')

hotels[, list(
  price_avg =mean(price_per_night),
  rating_avg = mean(rating, na.rm=T), # 有点烦，一遍一遍加na.rm
  star_avg = mean(stars, na.rm=T)
  ), by=city]

# .SD (soft set) 批量处理 iterate
hotels[,lapply(.SD, mean,na.rm=T), by=city,
       .SDcols=c('price_per_night','rating', 'stars','distance')]

which(sapply(hotels, is.numeric)) # keep true values only
names(which(sapply(hotels, is.numeric)))

hotels[,lapply(.SD, mean,na.rm=T), by=city, # 保留numeric的col
       .SDcols=names(which(sapply(hotels, is.numeric)))]

str(hotels)

hotels[, c('price_per_night_huf','price_per_night_usd') := list(
  price_per_night*360,
  price_per_night*1.18 # ugly
)]
# a tidy way
hotels[, `:=`(
  price_per_night_huf=price_per_night*360,
  price_per_night_usd=price_per_night*1.18 
)]

## TODO MIN, MEAN, max, median of price
mystats <- function(x)list(
  min = min(x, na.rm = T),
  avg = mean(x,na.rm = T),
  max= max(x,na.rm = T)
)

mystats(hotels$price_per_night)

hotels[, as.list(unlist(lapply(.SD, mystats))), .SDcols= c('price_per_night','rating','stars'),
       by=city] # list& unlist did a transformation job

library(datasauRus)
str(datasaurus_dozen_wide)

# transform into a line-table format
df <- rbindlist(lapply(1:13, function(i)
  cbind(dataset = i, 
        setnames(datasaurus_dozen_wide[, c((2*i)-1, 2*i)],
                 c('x','y')))))
str(df)

df <- rbindlist(lapply(1:13, function(i) 
  cbind(dataset = sub('_y', '', names(datasaurus_dozen_wide)[2*i]), 
        setnames(datasaurus_dozen_wide[, c((2*i)-1, 2*i)],
                 c('x', 'y')))))

ggplot(df, aes(x,y)) + geom_point()+facet_wrap(~dataset)+geom_smooth(method = 'lm')

#TODO visualize the x and y scatterplots split by data set
ggplot(datasaurus_dozen, aes(x,y))+ geom_point()+facet_wrap(~dataset)

library(gganimate)
ggplot(datasaurus_dozen, aes(x,y))+ geom_point() +
  transition_states(dataset)

subtitle <- function(df, round = 2) {
  paste0(
    'mean(x)=', round(mean(df$x), round), ', sd(x)=', round(sd(df$x), round), '\n',
    'mean(y)=', round(mean(df$y), round), ', sd(y)=', round(sd(df$y), round), '\n',
    'cor(x, y)=', round(cor(df$x, df$y), round)
  )
}

ggplot(datasaurus_dozen, aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = TRUE) +
  transition_states(dataset) +
  labs(title = '{closest_state}',
       subtitle = '{subtitle(subset(datasaurus_dozen, dataset == closest_state))}')

## interactivity
p <- ggplot(mtcars,aes(wt,hp,color=factor(am))) + geom_point()
rm(p)
p <- last_plot() # 昨日重现

library(ggiraph)
?girafe
girafe(ggobj = p)

p <- ggplot(mtcars,aes(wt,hp,color=factor(am), tooltip= rownames(mtcars))) + 
  geom_point_interactive()
girafe(ggobj = p) # 每个点能看到标签

(mtcars$tooltips <- paste(rownames(mtcars), 'with',mtcars$gear, 'gears'))
p <- ggplot(mtcars,aes(wt,hp,color=factor(am), tooltip= tooltips)) + 
  geom_point_interactive()
girafe(ggobj = p) 

# CSS
# JS
p <- ggplot(mtcars,aes(wt,hp,color=factor(am), 
                       tooltip= tooltips, 
                       data_id = factor(gear))) + 
  geom_point_interactive()
girafe(ggobj = p, options = list(opts_hover(css='fill:black;'))) 

# TODO htmlwidgets
library(ggthemes)
p <- ggplot(mtcars,aes(wt,hp,color=factor(am))) + geom_point()
p+ theme_bw()
p+theme_minimal()
p+theme_void()

p+ theme_economist() + scale_color_economist()
p+  theme_excel() + scale_color_excel()
p+  theme_stata() + scale_color_stata()

?theme
theme_custom <- function(){
  theme(
    panel.background = element_rect(
      fill='orange',
      color = 'white'
    ),
    legend.position = 'top'
  )
}

theme_top_legend <- function() theme(legend.position = 'top')

p+theme_custom() # overwrite all plots
p+theme_custom() + theme_bw() + theme_top_legend# 前后顺序不同，会被overwrite的颜色也不同
p+theme_bw()+theme_custom() 

library(ggthemr) # 不用custom了

ggthemr_reset()
p

download.file('http://bit.ly/r-intro-nasa', 'image.jpg',mode = 'wb')
library(jpeg)
img <- readJPEG('image.jpg')
str(img)

dim(img)
h <- dim(img)[1]
w <- dim(img)[2]

img2d <- matrix(img, h*w)
str(img2d)

pca <- prcomp(img2d)
pca
pca$rotation
summary(pca)

image(matrix(pca$x[,1],h))
image(matrix(pca$x[,2],h))

extractColors <- function(x) rgb(x[1],x[2],x[3])






