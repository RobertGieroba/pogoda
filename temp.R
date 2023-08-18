library(keras)
library(ggplot2)

link = 'https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/synop/'
dane = c()
kolumny = c("nazwa", "rok", "miesiac", "dzien", "nos", "fws", "temp",
            "cpw", "wlgs", "ppps", "pppm", "wodz", "wono")
filtr = c(2, 3, 4, 5, 6, 8, 10, 12, 14, 16, 18, 20, 22)
# for (rok in 2001:2018){
#   nazwa = paste0('s_d_t_495_', rok)
#   zrodlo = paste0(link,rok,"/",rok,"_495_s.zip")
#   dane <- c(dane, get_file(nazwa, zrodlo, extract=T))
# }
# for (miesiac in 1:7){
#   nazwa = paste0('s_d_t_', ifelse(miesiac>=10, miesiac, paste0('0', miesiac)), '_2019')
#   zrodlo = paste0(link,"2019/2019_",ifelse(miesiac>=10, miesiac, paste0('0', miesiac)),"_s.zip")
#   dane=c(dane, get_file(nazwa, zrodlo, extract=T))
# }
x = c()
for (sciezka in dane){
  x=rbind(x,read.csv(paste0(sciezka), header = F))
}
x=rbind(read.csv('daneopogodzie/s_d_t_495_1991_1995.csv', header = F),
        read.csv('daneopogodzie/s_d_t_495_1996_2000.csv', header = F),x)
dane = c()
# for (rok in 2001:2018){
#   nazwa = paste0('s_d_t_400_', rok)
#   zrodlo = paste0(link,rok,"/",rok,"_400_s.zip")
#   dane <- c(dane, get_file(nazwa, zrodlo, extract=T))
# }
y=c()
for (sciezka in dane){
  y=rbind(y,read.csv(paste0(sciezka), header = F))
}
y=rbind(read.csv('daneopogodzie/s_d_t_400_1991_1995.csv', header = F),
        read.csv('daneopogodzie/s_d_t_400_1996_2000.csv', header = F),y)
x=x[,filtr[-1]]
y=y[,filtr[-1]]
x <- x[,-1]
y <- y[,-(1:3)]
colnames(x) <- kolumny[-(1:2)]
colnames(y) <- paste0(kolumny[-(1:4)],".zg")
x=cbind(x,y)
x$miesiac <- x$miesiac/12
x$dzien <- x$dzien/31
sr <- apply(x[,-(1:2)], 2, mean)
od <- apply(x[,-(1:2)], 2, sd)
for(i in 3:ncol(x)){
  x[,i] <- (x[,i]-sr[i-2])/od[i-2]
}
ggplot(x, aes(1:nrow(x), temp,)) + geom_line(color='steelblue')
remove(dane, filtr, kolumny, link, sciezka)
x <- as.matrix(x)
okno <- 18
dane <- array(0, dim=c(nrow(x)-okno-1, okno, ncol(x)))
wz <- rep(0, nrow(x)-okno-1)
for (i in 1:(nrow(x)-okno-1)){
  dane[i,,] <- x[i:(i+okno-1),]
  wz[i] <- x[i+okno,5]
}
id_wal <- 1:100#sample.int(dim(x)[1], 0.1*dim(x)[1])
wal <- dane[id_wal,,]
dane <- dane[-id_wal,,]
wz_wal <- wz[id_wal]
wz <- wz[-id_wal]
model <- keras_model_sequential()
model %>% layer_lstm(128, input_shape = c(okno,dim(dane)[3]))
model %>% layer_dense(1, 'linear')
model %>% compile('adam', 'mse')
uczenie <- model %>% fit(dane, wz, epochs = 15, validation_split = 0.15, batch_size = 64#,
                         #callbacks = callback_early_stopping(patience=20, restore_best_weights = T)
                         )
rok_na_zakres <- function(rok){
  ((rok-1991)*365+1):((rok-1991)*365+365)
}
#start <- rok_na_zakres(2018)
start <- 1:dim(dane)[1]
wynik <- model %>% predict(dane[start,,])
ggplot(mapping = aes(1:length(start))) + geom_line(mapping = aes(y=wynik), color='steelblue') +
  geom_line(mapping = aes(y=wz[start]), color='orange')
cor(wynik, wz[start])
cor(dane[start,okno,5], wz[start])
model %>% evaluate(wal,wz_wal)
pred <- model %>% predict(wal)
cor(wz_wal, pred)
ggplot(mapping = aes(1:length(id_wal))) + geom_line(mapping = aes(y=pred), color='steelblue') +
  geom_line(mapping = aes(y=wz_wal), color='orange')
