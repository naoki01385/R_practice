#================================================================

#Logistic Regression

install.packages("mlbench")

library(mlbench)


data("BreastCancer")

View(BreastCancer)
summary(BreastCancer)

# [,1]	 Id	 Sample code number 
# [,2]	 Cl.thickness	 Clump Thickness
# [,3]	 Cell.size	 Uniformity of Cell Size
# [,4]	 Cell.shape	 Uniformity of Cell Shape
# [,5]	 Marg.adhesion	 Marginal Adhesion
# [,6]	 Epith.c.size	 Single Epithelial Cell Size
# [,7]	 Bare.nuclei	 Bare Nuclei
# [,8]	 Bl.cromatin	 Bland Chromatin
# [,9]	 Normal.nucleoli	 Normal Nucleoli
# [,10]	 Mitoses	 Mitoses
# [,11]	 Class	 Class

#このデータセットの目的変数は、Class
summary(BreastCancer$Class)
#benign -> 良性
#malignant -> 悪性

library(ggplot2)
ggplot(BreastCancer,aes(Cell.size,Class)) + 
  geom_point()
ggplot(BreastCancer,aes(Cell.size, Class)) + 
  geom_jitter()
ggplot(BreastCancer,aes(Cell.size, Class)) + 
  geom_count()

#それでは、Cell.sizeから、その細胞が悪性か良性かを
#benignとmalignantに分類するロジスティック回帰分析
#を行います。

dt <- BreastCancer %>% 
  select(Cell.size, Class)

summary(dt)

#Cell.sizeが因子になっているため、
#数値へ変換しておきます。

dt <- dt %>% 
  mutate(Cell.size = as.numeric(Cell.size))
summary(dt)
#線形モデルではlm()を利用しましたが、[linear model]
#ロジスティック回帰分析はglm()を利用します
#[generalized linear model:一般化線形モデル]

#glm()、でのロジスティック回帰分析の指定方法は
#次のように記載します
# glm(
#   formula = Class ~ Cell.size,
#   family = binomial(link="logit"),
#   data = dt
# )

#formulaは、Y = aX + b => Y ~ X

#familyは、想定する分布が二項分布(binomial)

#linkは、logit : ln{p/(1-p)}
#        ln{p/(1-p)} = aX+b
#  の式から
#        p=σ(aX+b)
#　にたどりついたことを覚えていますか？
#　　　　「線」：aX + b
#　がどのような式を最初に結び付けるのか
#を指定するのがlink=""の部分です。

logistic_model <- 
  glm(formula = Class ~ Cell.size,
      family = binomial(link="logit"),
      data = dt
  )

summary(logistic_model)

#ということで、できました！
# a = 1.489
# b = -4.960

#===================================
#実際のところ、どういう線になっているのか、
#aとbを利用して計算してみましょう

#sigmoid関数を作ります
sigmoid <- function(X,a,b){
  result <- 1/{1+(exp(-{a*X+b}))}
  return(result)
}

a <- 1.489
b <- -4.960

dt <- dt %>% 
  mutate(sigmoid_value = sigmoid(Cell.size,a,b))

dt

summary(dt)

ggplot(dt,aes(x=Cell.size)) +
  geom_jitter(aes(y=Class)) +
  geom_line(aes(y=sigmoid_value))

ggplot(dt,aes(x=Cell.size)) +
  geom_jitter(aes(y=as.numeric(Class))) +
  geom_line(aes(y=sigmoid_value))


ggplot(dt,aes(x=Cell.size)) +
  geom_jitter(aes(y=as.numeric(Class)-1)) +
  geom_line(aes(y=sigmoid_value))

ggplot(dt,aes(x=Cell.size)) +
  geom_jitter(aes(y=as.numeric(Class)-1)) +
  geom_line(aes(y=sigmoid_value))

#ちょっとかくかくしているので、こんな方法もありですね
X <- seq(1,10,by=0.1)
sigdt <- tibble(
  x = X,
  sigX = sigmoid(X,a,b)
)

ggplot(dt,aes(x=Cell.size)) +
  geom_jitter(aes(y=as.numeric(Class)-1)) +
  geom_line(data = sigdt, 
            aes(x=X,y=sigX),
            color="red",size=2) +
  labs(x="細胞のサイズ(cm)",y="細胞の悪性有無(0=良性,1=悪性")

#=====================スライドへ==============================

#====Gradient DescentによるEstimation==========
#それでは、勾配法を利用して、aとbを求めてみましょう。

dt <- BreastCancer %>% 
  select(X = Cell.size, Y = Class) %>% 
  mutate(X = as.numeric(X)) %>% 
  mutate(Y = as.numeric(Y)-1)

dt
#これで、Xは数値、Yは0か1のデータになりました。

#aX+bのa,bをまずは適当に決めます
set.seed(1)
a <- runif(1,0,10)
b <- runif(1,0,10)

a
b

#a,b,X_iから、
#予測したロジスティック回帰の数値を返す関数を
#つくります。

add_prediction <- function(dt,a,b){
  Z <- a*dt$X + b
  sig_Z <- 1/(1+exp(-Z))
  
  dt <- dt %>% mutate(pred = sig_Z)
  
  return(dt)
}

head(dt)
#add_prediction関数にdtと、a,bを与えて実行すると
#新たな、predという（ランダムに決めた、でたらめな）
#予測がそれぞれの列に追加されます。
dt <- add_prediction(dt,a,b)

head(dt)

#対数尤度lnLiを求めます
add_lnLi <- function(dt){
  
  dt <- dt %>% 
    mutate(
      lnLi = (pred^Y)*((1-pred)^(1-Y))
    )
}

dt <- add_lnLi(dt)
head(dt)

sum(dt$lnLi)
#これが、適当に決めたaとbでの対数尤度の値です。
#これを可能な限り大きくするaとbを求めるというのが
#今回のミッションです。

#δl/δaをもとめてみましょう
dl_by_da <- function(dt){
  {dt$X*(dt$Y - dt$pred)} %>% sum()　
  #この式は、連鎖律を利用してもとめたものです
}

dl_by_da(dt)

#負の数値になっています。（右肩下がり）
#この傾きに逆らって登れば（aの値を負方向へ動かせば）、
#ln(L)は増加するはずです。

#具体的には、負の傾きであれば、左方向へ、
#正の傾きであれば、右方向へ

#ここで、傾きの「急さ」に応じて移動距離を決めます
rate <- 0.001

#たとえば傾きが1000であれば、1000*0.001=1
#だけaを増やす傾きが1であれば、1*0.001=0.001だけ
#aを増やすというイメージです
#このように補正するメリットとしては、傾きが急だと、
#大きめにaを動かしても逆側（最大値の向こう側）へ
#行ってしまう心配が少ないことと、
#目的値付近に近づいた場合に慎重にaの値を変化させる
#ことができます

a
a <- a + (dl_by_da(dt))*rate
a

#同じように、
dl_by_db <- function(dt){
  (dt$Y - dt$pred) %>% sum()
}

dl_by_db(dt)

b
b <- b + (dl_by_db(dt))*rate
b

#ここから、どんどんaとbの値を更新していきます。
#あと、対数尤度とa,bの変化を記録しておきましょう
record_lnL <- c()
record_a <- c()
record_b <- c()


for(i in 1:5000){
  dt <- add_prediction(dt,a,b) 
  dt <- add_lnLi(dt)
  
  record_lnL <- c(record_lnL,sum(dt$lnLi))
  record_a <- c(record_a,a)
  record_b <- c(record_b,b)
  
  if(i%%500==0){
    print(str_c("更新",i,"回目：尤度は",sum(dt$lnLi)))
  }
  
  rate <- 0.001

  a <- a + (dl_by_da(dt))*rate
  b <- b + (dl_by_db(dt))*rate
  
}
#どのように対数尤度とa、bが変化したか確認します。

result_table <- tibble(
  trial = c(1:5000),
  lnl　= record_lnL,
  a = record_a,
  b = record_b
)

ggplot(result_table)+geom_line(aes(trial,lnl))
ggplot(result_table)+geom_line(aes(trial,a))
ggplot(result_table)+geom_line(aes(trial,b))

a
b
#このaとbの値を、glm関数でもとめたものと比較してみましょう

logistic_model$coefficients
a
b

#一致しています！

#ということで、このように、
#尤度(その事象が起こる可能性)を最大可するように
#aやbの値（パラメーター）をちょっとずつ傾き
#（gradient)にさからって動かすことで、
#推定を行うことができました。

#これを、gradient ascend法といいます
#（もしくは、-Lを最小化するために、坂道方向に
#下っていく、gradient descendという方法もあります
#が、本質は同じです）

#この手法は、数学的には「美しくない」ですが、
#コンピューターが発達して、無理やり計算を行うことで
#美しい数式に匹敵する精度で
#パラメーターを推定できるため
#かなり有用です。

#ディープラーニングをはじめ、多くの
#「機械学習」では、このgradient descedの
#考え方をベースにモデルのパラメーターが推定されます。

#===================
#deviance
library("mlbench")
library("tidyverse")

data("BreastCancer")
#引き続き、BreastCancerデータで見ていきます。
dt <- BreastCancer %>% 
  select(X = Cell.size, Y = Class)

#とりあえず、dtの入力をXとYにしておきます。
#そして、Yの値を0か1に変換します
dt <- dt %>% 
  mutate(X = as.numeric(X)) %>% 
  mutate(Y = as.numeric(Y)) %>% 
  mutate(Y = Y-1)

head(dt)

model <- glm(formula = Y ~ X,
             family = "binomial"(link = "logit"),
             data = dt)
model$deviance

#ついで予測する気のない定数モデル
model_fix <- glm(formula = Y ~ 1,
                 family = "binomial"(link = "logit"),
                 data=dt)
model_fix$deviance

#最後に頑張りすぎた
#（パラメーターがデータの数だけある)予測モデル
parameters <- as.factor(1:length(dt$X))
model_max <- glm(formula = Y ~ parameters,
                 family="binomial"(link = "logit"),
                 data=dt)
model_max$deviance

#以上で、3つのモデルの逸脱度がわかりました。

#Residual Devianceが作ったモデルー最小
model$deviance - model_max$deviance
#NullDevianceが最大ー最小
model_fix$deviance - model_max$deviance

summary(model)

#一致しています！
#======================スライドへ==============
#線形モデルでは、モデル全体の検定結果が表示されていましたが、
#ロジスティック回帰のモデルのSummaryにはありません
#ここでは、χ^2検定を行います。

model
anova(model,test="Chisq")





#スライドで利用されたグラフーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーーー
library(tidyverse)

tab <- tibble(
  x = c(seq(221,320,1)/10,
        seq(270,295,2)/10,seq(244,269,2)/10),
  y = c(rep(0,50),rep(1,50),
        rep(0,13), rep(1,13))
)

ggplot(tab) + geom_point(aes(x,as.factor(y))) +
  labs(x="ある日の気温", y="熱中症の救急搬送の有無(0=なし、1=あり")

logmod <- glm(data = tab, formula = y~x, family = "binomial")

summary(logmod)

lmmod <- lm(data = tab, formula = y ~ x)

ggplot(tab,aes(x,y)) + geom_point() + geom_smooth(method="lm",se=FALSE) +
  labs(x="ある日の気温", y="熱中症の救急搬送の有無(0=なし、1=あり")

ggplot(tab,aes(x,y)) + geom_point() + 
  geom_smooth(method="glm", method.args = list(family="binomial"),se=FALSE) +
  labs(x="ある日の気温", y="熱中症の救急搬送の有無(0=なし、1=あり")

lntab <- tibble(
  X = seq(1:20000)/10000,
  ln_X = log(seq(1:20000)/10000)
)

ggplot(lntab,aes(X,ln_X)) + geom_line()


sigmoid <- function(x){
  1/(1+exp(-1*x))
}

tabsigmoid <- tibble(
  x = seq(-100,100)/10,
  sig = sigmoid(seq(-100,100)/10)
)

ggplot(tabsigmoid) + geom_line(aes(x,sig)) +
  labs(y = "σ(x)")

set.seed(100)

func_binom_hist <- function(n,p){
  rand_bin <-rbinom(n,1000,p)/1000

 dt <- tibble(x = rand_bin)
 gg <- ggplot(dt,aes(x))+geom_histogram(binwidth=0.001) + labs(x="確率")
 return(gg)
}

func_binom_hist_with_limit <- function(n,p){
  rand_bin <-rbinom(n,1000,p)/1000
  
  dt <- tibble(x = rand_bin) %>% 
    count(x) %>% 
    mutate(sum_n = sum(n)) %>% 
    mutate(dens = n/sum_n)
    
    
  gg <- ggplot(dt)+geom_point(aes(x=x,y=dens),stat="identity") + labs(x="",y="") +
    scale_x_continuous(limits = c(0,1)) +
    theme_classic()
  return(gg)
}


set.seed(100)
func_binom_hist(1000000,1/3)


func_binom_hist_with_limit(10000000,0.01)
func_binom_hist_with_limit(1000000,0.70)


tabdbin <- tibble(
  x = dbinom(1,100,0.1)
)

dbinom(1,100,0.1)



x <- seq(0, 1, length=100)
rand_bin <- rbinom()

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)


