#重回帰分析のRでの利用方法

#非常に簡単です。
#今回は、
#install.packages("carData")
library(carData)
library(tidyverse)

carData::Prestige %>% head()
# education
# Average education of occupational incumbents, 
# years, in 1971.
# 
# income
# Average income of incumbents, 
# dollars, in 1971.
# 
# women
# Percentage of incumbents who are women.
# 
# prestige
# Pineo-Porter prestige score for occupation, 
# from a social survey conducted in the mid-1960s.
# 
# census
# Canadian Census occupational code.
# 
# type
# Type of occupation. 
# A factor with levels (note: out of order): 
#   bc, Blue Collar; 
#   prof, Professional, Managerial, and Technical; 
#   wc, White Collar.

#Prestigeは職業の「威信」を表すスコアだそうです。


#では、これらの変数がincomeにどのような影響を与えるか、
#重回帰分析を行っていきましょう。

dt <- carData::Prestige

#まずデータセットを確認します
View(dt)
summary(dt)

summary(dt$type)

#全ての変数の分布を確認してみましょう
ggdt <- ggplot(dt)

ggdt + geom_histogram(aes(x=education))
#...

#と書いていくのもいいですが、こんな方法もあります
dt2 <- dt %>% select(-census)

install.packages("GGally")
library(GGally)
GGally::ggpairs(dt2)



#では、重回帰分析を行います。
mod1 <- lm(formula = income ~ education + women + prestige + type,
           data =dt)
summary(mod1)

#=======================================================================

#多重共線性を調べてみましょう
mod1 <- lm(formula = income    ~ education + women    + prestige + type, data =dt)

mod2 <- lm(formula = education ~             women    + prestige + type, data = dt)

rsq_education <- summary(mod2)$r.squared
vif_education <- 1/(1-rsq_education)
vif_education

mod3 <- lm(formula = women ~ education +          prestige + type, data = dt)
rsq_education <- summary(mod3)$r.squared
vif_women <- 1/(1-rsq_education)
vif_women

#という風に一つずつ計算する手もありますが、実は、carパッケージに
#自動で計算してくれるものがあるので、
car::vif(mod1)
#で求まります(VIFの値が計算した値と一致していますね？)

#今回、5をカットオフとしてみると、educationかtypeは省いてもよさそうです。
#educationを省いた場合
mod4 <-  lm(formula = income ~ women + prestige + type, data =dt)
summary(mod4)
car::vif(mod4)

#typeを省いた場合
mod5 <- lm(formula = income ~ education + women + prestige, data=dt)
summary(mod5)
car::vif(mod5)

#==============factor型の表記について===================
#補足：
summary(mod1)
#では、typeprofと、typewcに分かれていますが、bcはありません。
#これは、bcを基準として、profクラスになると、（有意差はありませんが)
#＄714給与が上昇するというような意味あいになります。
#因子型のlevelのようにその数字が直接の意味を持たない場合は、
#3クラスの因子型であれば、Y = ~~ + (B_f * X_f) + ~~~という形では「なく」
#  Y = ~~ + (B_f1 * X_f1) + (B_f2 * X_f2) + ~~~~ というように、クラス数ー1個の別々の
# X(Xは該当で1、非該当で0)のモデルとして取り扱われます。

#補足2：
summary(mod1)
#(4 observations deleted due to missingness)
#とありますが、これは、typeにNAがあるためです。
#NAを別の値やこの場合にレベルにしてあげれば含んで分析することも可能で、

dttest <- dt %>% 
  mutate(type = if_else(is.na(type),"Other",as.character(type))) %>% 
  mutate(type = as.factor(type))         

mod_other <- lm(formula = income ~ education + women + prestige + type, 
                data = dttest)
summary(mod_other)
#今回の場合は意味ないですけどね。


#============モデル評価はここで描出したグラフをスライドで
plot(mod4, which=5)


#==============Residual Vs Leverage====================
set.seed(100)
testdt <- tibble(
  x = runif(30,0,10)
) %>% 
  mutate(ax = 2.5*x) %>% 
  mutate(b = 10) %>% 
  mutate(randomness = rnorm(30,0,10)) %>% 
  mutate(y = ax + b + randomness) %>% 
  select(x,y)

ggplot(testdt,aes(x,y)) + 
  geom_point() + 
  scale_x_continuous(limits=c(1,25)) +
  scale_y_continuous(limits=c(1,100))

tmod1 <- lm(y ~ x, testdt)

ggplot(testdt,aes(x,y)) + 
  geom_point() + 
  geom_smooth(method="lm",se=FALSE, fullrange=TRUE) +
  scale_x_continuous(limits=c(1,25)) + 
  scale_y_continuous(limits=c(1,100))


#さて、理想的な回帰直線が引けています。が、
#例えば「変な点」を足してみましょう。
#xが5のときの予測は
 2.5*5 + 10
#(5,22.5-20)というデータを別のコラムy2として足してみます。 
 testdt <- testdt %>% 
   mutate(y2 = y) %>% 
   add_row(x=5, y=22.5, y2=22.5-20)
 
 ggtry1 <- 
 ggplot(testdt) +
   geom_point(aes(x=x,y=y2), color="red", size=2) +
   geom_point(aes(x=x,y=y), color="blue", size=2) + 
   scale_x_continuous(limits=c(1,25)) + 
   scale_y_continuous(limits=c(1,100))

 ggtry1 + geom_smooth(aes(x=x,y=y),
                      method="lm",
                      se=FALSE, 
                      color="blue", fullrange=TRUE) 
 ggtry1 + geom_smooth(aes(x=x,y=y2),
                      method="lm",
                      se=FALSE, 
                      color="red", 
                      linetype="dashed", 
                      fullrange=TRUE) +
   geom_smooth(aes(x=x,y=y),
               method="lm",se=FALSE, 
               color="blue", fullrange=TRUE) 
 
 
 #ほとんど赤い点の影響はありません。
 #ところが、xが25の場合、
 　2.5*25 + 10
#(25,72.5-20)というデータをコラムy3として足してみます。
 testdt <- testdt %>% 
   mutate(y3 = y) %>% 
   add_row(x=25,y=72.5,y2=72.5, y3=72.5-20)


 ggtry2 <- 
   ggplot(testdt) +
   geom_point(aes(x=x,y=y3), color="red", size=2) +
   geom_point(aes(x=x,y=y), color="blue", size=2) + 
   scale_x_continuous(limits=c(1,25)) + 
   scale_y_continuous(limits=c(1,100))
 
 ggtry2 + geom_smooth(aes(x=x,y=y),method="lm",se=FALSE, color="blue") 
 ggtry2 + geom_smooth(aes(x=x,y=y3),method="lm",
                      se=FALSE, color="red", linetype="dashed") +
   geom_smooth(aes(x=x,y=y),method="lm",se=FALSE, color="blue") 
 
 #予測との「ずれ」の大きさは、最初も後も同じです。
 #しかし、最小二乗法による予測では、
 # xの値が全体の平均より大きい場合は同じずれでも、
 # 予測に与える影響が大きく変わってきます。
 
 #これを、leverageが大きい
 #（ある説明変数が説明変数の平均からどれだけずれているか）
 #といいます。
 
 #また、Cook’s Distanceは、その観測データiが持つ影響力
 #というイメージをもってもらえれば、
 
 model_blue <- lm(y~x, testdt)
 model_red <- lm(x~y3, testdt)
 
 plot(model_blue, which=5)
 plot(model_red, which=5) 

#model_redでのCook's Distanceがある程度あって、
#Leverageが高い点をモデルに含むかどうかを検討
#しなければなりません。
 
#あるデータを除外する、しないは
 #そのデータの性質にもよるため、ここでは
#踏み込みません。
 
#以上で、重回帰分析の実施とそのモデル評価に
#ついての基本でした。
 
