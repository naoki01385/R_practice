#このセクションでは、標準組み込みデータセットを
#利用していきます。

datasets::airquality

library(datasets)
library(tidyverse)

View(airquality)        

#このデータセットは、1973年5月～9月のニューヨークでの
#大気の状態を示すものです。

#Ozone オゾン(ppb)
#Solar.R  Solar radiation in Langleys in the frequency
#　　　　　band 4000–7700 Angstroms from 0800 to 1200 
#          hours 
#          at Central Park
#Wind　風速(mph)
#Temp　気温(°F)
#Month　月(1-12)
#Day　日(1-31)

head(airquality)

#1
#まずは風速(mph)を(m/s)へ
#続いて、気温を華氏から摂氏へ

#1mph = 0.44704m/s
#1°F =1.8°C+32

#
dt <- airquality %>% 
  mutate(ms_wind = Wind/0.44704) %>% 
  mutate(c_temp = (Temp-32)/1.8) %>% 
  select(-Wind, -Temp)

head(dt)

#2
#気温をX軸、オゾンをY軸にとって散布図を記載


#
ggplot(dt) + geom_point(aes(c_temp, Ozone))


#なんとなく、直線関係がありそう？

#Q3：最小二乗法での回帰直線の式を計算してみよう！

#Step１:
X <- dt$c_temp
Y <- dt$Ozone

X
Y

#Step2: Ozoneの値に欠損値があるため、ここでは
#欠損しているデータを取り除いておこう
dt <- dt %>% filter(!is.na(Ozone))

X <- dt$c_temp
Y <- dt$Ozone

X
Y

#Step3:普通に計算してみる！
a <- {(sum(X*Y))-(mean(Y)*sum(X))}/{sum(X*X)-(mean(X)*sum(X))}
a


b <- mean(Y)-a*mean(X)
b

ggplot(dt) + 
  geom_point(aes(c_temp,Ozone)) +
  geom_abline(slope = a, intercept = b, color = "red")

#Step4:分散、共分散を使って同じことをやってみる
a2 <- cov(X,Y)/var(X)
a2

b2 <- mean(Y)-a*mean(X)
b2

ggplot(dt) + 
  geom_point(aes(c_temp,Ozone)) +
  geom_abline(slope = a2, intercept = b2, color = "blue")

#数式で考えると長丁場でしたが、これで、
#最小二乗法を導出して、
#Rで実行できるようになりました。

#では、楽な方法をお教えしましょう。
#最小二乗法による回帰直線は、
#lmという関数で実はRに実装されています
#また、グラフもいちいち線を書く命令を記載しなくても、
#ggplotでお手軽に描画する方法も実装されています。

#今回、dtというデータのうち、YをOzone、
#Xをc_tempという変数で
#実施しましたが、

#いわゆる普通の統計ソフトの出力：
mod1 <- lm(data = dt, formula = Ozone ~ c_temp)
summary(mod1)

#簡単に回帰直線を引いてみる
ggplot(dt,aes(c_temp, Ozone))+
  geom_point() + 
  geom_smooth(method="lm")

#linear model
#========================================スライドへ
#summary(mod1)の読み方

#=================================================
#modelを描画(plot)する
plot(mod1,which = 1)



