library(ggplot2)
source('http://bit.ly/CEU-R-shoes')
ls()
plot(students$shoe,students$math)

# EDA 
str(students)

plot(students$y,students$math) # two factors of age

plot(students$x,students$math) # age

library(GGally)
ggpairs(students)

cor(residuals(lm(math ~x, students)), residuals(lm(shoe ~x, students)))

library(psych)
partial.r(students,1:2,3)

plot(residuals(lm(math ~ x, students)), residuals(lm(shoe ~ x, students)))
abline(lm(residuals(lm(math ~ x, students)) ~ residuals(lm(shoe ~ x, students))))

plot(residuals(lm(math ~ x, students)), residuals(lm(shoe ~ x, students)))
abline(lm(residuals(lm(shoe ~ x, students)) ~ residuals(lm(math ~ x, students))))

rm(list = ls())
sessionInfo()

?ls
ls(all = TRUE)
.secret # "A warm hello from the Internet."

## Take aways:
## * don't use `rm(list = ls())` in your scripts ... rather set RStudio to never save/load your session again
## * don't `source` R scripts downloaded from the Internet without first checking the content

readLines('http://bit.ly/CEU-R-shoes')

t <- tempfile()
download.file('https://bit.ly/hun-cities-distance',t, mode = 'wb')
file.info(t)

library(readxl)
cities <- read_excel(t)

cities <- cities[,-1]
cities <- cities[-nrow(cities),]
str(cities)

# MDS
?cmdscale
mds <- cmdscale(as.dist(cities))
mds
plot(mds)
text(mds[,1], mds[,2], names(cities))

mds <- -mds
plot(mds)
text(mds[,1], mds[,2], names(cities))

mds <- as.data.frame(mds)
mds$city <- rownames(mds)

library(ggplot2)     
ggplot(mds, aes(V1,V2,label= city))+
  geom_text()+
  theme_void()

?eurodist

# practice

mds_eu <- cmdscale(eurodist)
mds_eu <- as.data.frame(mds_eu)
mds_eu$city <- rownames(mds_eu)

ggplot(mds_eu, aes(V1,V2,label=city))+
  geom_text()+
  theme_void()

# mtcars
mds_car <- as.data.frame(cmdscale(dist(scale(mtcars))))
mds_car$car <- rownames(mds_car)
str(mds_car)

ggplot(mds_car,aes(V1,V2,label = car)) +
  geom_text()+
  theme_void()

library(ggrepel)
ggplot(mds_car,aes(V1,V2,label = car)) +
  geom_text_repel()+
  theme_void()

##
?UCBAdmissions
plot(UCBAdmissions)

berkeley <- as.data.frame(UCBAdmissions)
str(berkeley)

p <- ggplot(berkeley, aes(Gender, Freq, fill=Admit))+
  geom_col(position ='fill')  # add 100% 

p+ facet_wrap(~Dept) +
  scale_fill_manual(values = c(
    'Admitted'= 'darkgreen',
    "Rejected" = 'red'
  ))

p+facet_wrap(~Dept) +scale_fill_brewer(palette = "Dark2")

## ?iris
ggplot(iris,aes(Sepal.Length, Sepal.Width)) + geom_point() +geom_smooth(method = 'lm')

ggplot(iris,aes(Sepal.Length, Sepal.Width,color =Species)) + 
  geom_point() +
  geom_smooth(method = 'lm')
# a combination
ggplot(iris,aes(Sepal.Length, Sepal.Width)) + 
  geom_point(aes(color =Species)) +
  geom_smooth(aes(color =Species),method = 'lm') +
  geom_smooth(method = 'lm', se = FALSE,color= 'black')

# MDS
mds_iris <- as.data.frame(cmdscale(dist(iris[,-5])))
mds_iris

ggplot(mds_iris, aes(V1,V2,color = iris$Species)) + geom_point()
# compared with variables, it is similar to V1 and V2

ggplot(mds_iris, aes(V1,V2,color = iris$Species)) + geom_point() + 
  geom_smooth(method = 'lm')

## 
library(data.table) # dt[i,j, by =...]
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices') r

# chaining
bookings[price<100 & offer == 0][1:5]

#dt[i,j]
bookings[, .N] # counting rows
bookings[price<100, .N]

bookings[price<100, mean(price)] # calculate means
bookings[price<100, summary(price)] 
bookings[price<100, hist(price)] 

# TODO 
bookings[weekend ==1, mean(price)] # 138.4654
bookings[weekend ==0, mean(price)] # 259.5493

#dt[i,j, by=...]
bookings[,mean(price), by =weekend] # by = groupby

summary(bookings$nnights)
bookings[, .N, by=nnights]

x <- bookings[,list(price=mean(price)),by=list(weekend,nnights, holiday)][order(price)]
x[weekend==0]

bookings[, list(min=min(price), mean = mean(price), max = max(price)), 
                by= list(weekend, nnights, holiday)]

# the avg price of the stars of hotels


features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
# rolling joins, non-eui joins <> LEFT | RIGHT | INNER | OUTER
merge(bookings, features)[, .N]
bookings[,.N]

bookings[!hotel_id %in% unique(features$hotel_id)] # 不在hotel但在booking的
merge(bookings, features)[, mean(price),  by= stars]

bookings$price_per_night <- bookings$price / bookings$nnights
bookings[, price_per_night := price / nnights] # overwrite this column 

hotels <- merge(
  features,
  bookings[, list(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id'
)
str(hotels)

## TODO compute avg price per number of stars
hotels[, mean(price_per_night),  by= stars][order(stars)]
# remove NA
hotels[, mean(price_per_night),  by= stars][order(stars)][!is.na(stars)]
hotels[!is.na(stars), mean(price_per_night),  by= stars][order(stars)]

hotels[, .(stars, bookings, price_per_night)]

?weighted.mean
hotels[!is.na(stars), weighted.mean(price_per_night, bookings),  by= stars][order(stars)]

## TODO list countries above avg rating
countries <- hotels[, .(
  rating = mean(rating, na.rm = TRUE)), # better than filter before ,
  by = country]
# dt[i]
countries[rating > mean(rating, na.rm= TRUE)]

## TODO a bew column to ccategorize into 3 buckets by price (cheap, avg, expensive)
hotels[, pricecat := cut(price_per_night,3)]
hotels[,.N, by= pricecat] # extremely skewed

hotels[, pricecat := cut(price_per_night,c(0,100,250, Inf))]
hotels[,.N, by= pricecat]

hotels[, pricecat := cut(price_per_night,c(0,100,250, Inf), 
                         labels = c('cheap', 'avg', 'expensive'))]
hotels[,.N, by= pricecat][order(pricecat)]

## TODO use a stats approach to categorize the hotels into below avg, avg, above avg
q <- quantile(hotels$price_per_night, probs = seq(0,1,0.33))
q[2]
q[3]

hotels[, pricecat := cut(price_per_night,c(0,q[2],q[3], Inf), 
                         labels = c('cheap', 'avg', 'expensive'))]
hotels[,.N, by= pricecat]

## TODO use a stats approach to categorize the hotels into below avg, avg, above avg by the country
hotels[, pricecat := cut(price_per_night,c(0,q[2],q[3], Inf), 
                         labels = c('cheap', 'avg', 'expensive'))]
hotels[,.N, by= list(country,pricecat)]

hotels[, q1:= quantile(price_per_night, probs=0.33), by= country]
hotels[, q2:= quantile(price_per_night, probs=0.66), by= country]
hotels <- hotels[q1!=q2]
hotels[, pricecat := cut(price_per_night,c(0,q1,q2, Inf), 
                         labels = c('cheap', 'avg', 'expensive')),
       by = country]

# 
str(anscombe)
lapply(1:4,function(i) anscombe[,i])
lapply(1:4,function(i) cor(anscombe[,c(i,i+4)]))
lapply(1:4,function(i) anscombe[,c(i,i+4)])

df <- rbindlist(lapply(1:4,function(i) cbind(dataset = i,anscombe[,c(i,i+4)])))

ggplot(df, aes(x1,y1))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~dataset)
