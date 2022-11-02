#GLM
library(tidyverse)
a <- 20
b <- 1


glmdt <- tibble(x = runif(100,0,10)) %>% 
  mutate(ax = a*x) %>% 
  mutate(b = b) %>% 
  mutate(senkei = ax+b) %>%
  mutate(y = exp(senkei)) %>%  #link=log (log^-1 = e)
  mutate(error = rgamma(100, shape=0.01, rate=1)) %>% 
  mutate(y = y + error) %>% 
  select(x,y)

#この点は、link関数がlog、誤差がgamma分布に従う点のあつまり

ggplot(glmdt)+geom_point(aes(x=x, y=y))

#普通に線形モデルと仮定してみると
mod_lm <- lm(formula = y ~ x, glmdt)
#　　↑
#　　同じ
#　　↓
mod_lm <- glm(formula = y ~ x, 
              family = gaussian(link="identity"), 
              data = glmdt)

ggplot(glmdt) + 
  geom_point(aes(x,y)) + 
  geom_smooth(aes(x,y), method="lm", se=FALSE)

#あてはまっている？
plot(mod_lm, which=1)
#残差にパターンあり、
plot(mod_lm, which=2)
#はずれている
plot(mod_lm, which=3)
plot(mod_lm, which=5)


#さて、いくつかモデルを作ってみましょう

mod_gau_identity <- glm(formula = y ~ x, 
                        family=gaussian(link="identity"),
                        data =glmdt)
mod_gau_log <- glm(formula = y ~ x, 
                   family=gaussian(link="log"), 
                   data =glmdt)
mod_gam_identity <- glm(formula = y ~ x, 
                        family=Gamma(link="identity"), 
                        data =glmdt)
mod_gam_log <- glm(formula = y ~ x, 
                   family=Gamma(link="log"), 
                   data =glmdt)

#モデルから予測してみましょう
glmdt <- glmdt %>% 
  mutate(y1 = predict.glm(mod_gau_identity, 
                          type="response", 
                          glmdt)) %>% 
  mutate(y2 = predict.glm(mod_gau_log, 
                          type="response", 
                          glmdt)) %>% 
  mutate(y3 = predict.glm(mod_gam_identity, 
                          type="response", 
                          glmdt)) %>% 
  mutate(y4 = predict.glm(mod_gam_log, 
                          type="response", 
                          glmdt))

ggplot(glmdt) + 
  geom_point(aes(x=x, y=y)) +
  geom_line(aes(x=x, y=y1),
            color="orange", 
            linetype="dashed") +
  geom_line(aes(x=x, y=y2),
            color="red", 
            linetype="solid") +
  geom_line(aes(x=x, y=y3),
            color="green", 
            linetype="dashed") +
  geom_line(aes(x=x, y=y4),
            color="blue", 
            linetype="solid") 

#identity => Y = aX + b
#log =>  log(Y) = aX + b <=> Y = e^(aX+b)

#実際にはこの点の集まりは、
#ガンマ分布のerror項とlink関数をlogと指定しています。

mod_gau_identity$deviance
mod_gau_log$deviance
mod_gam_identity$deviance
mod_gam_log$deviance

summary(mod_gam_log)
#このモデル評価はロジスティック回帰分析のときと一緒です。
#ただし、係数の解釈はリンク関数で変わってきます。
#このモデルの場合は、xが1増えると、e^0.08分、
#yの値が増加します
#が、これはxの値によって増加幅は変わるので
#注意が必要です。

exp(0.08 * 4) - exp(0.08 * 3)
exp(0.08 * 11) - exp(0.08 * 10)

#非常に簡単なさわりですが、GLMについて説明を行いました
#familyやリンク関数の指定は、分析対象のデータがどのように
#発生しているかの前提知識が必要であるといわれています。

#例えば、健康診断のデータのうち、人の体重はガンマ分布
#に従うことが知られているので、体重を予測する場合に
#重回帰分析（正規分布）を利用するよりも、
#GLMで、確率分布をガンマ分布、そして体重がマイナス
#になることはないので、リンク関数をidentity(直線)でなく
#0から+∞までとなる
#  log(y)　= ax + b
#     <=>   y = e^(ax+b)
# でモデル化するほうがより自然かもしれません。

#このように、「重回帰分析」だから、データが正規分布
#していないといけない！だから、変数を対数変換しよう！
#とか、なんか直線っぽくないけど、マイナスが切片に
#なってておかしいけど、とりあえず有意差でたから
#これでいこう！みたいに不自由なモデリングではなく
#ある程度、前提となる知識をいかしたオーダーメードの
#モデリングができることがGLMの強みだと考えます。

#以上で、統計セクションは終了です。
#もし、このコースの統計セクションを聞いて
#モデリングや機械学習等に興味をもってくださった
#方がいれば、ぜひ一緒に勉強していきましょう



#=======================スライドのグラフ

tibble(x = seq(-10,10,0.1)) %>% 
  mutate(y = {1/(1+exp(-x))}) %>% 
  ggplot() + geom_line(aes(x,y)) +
  labs(x = "aX + b", y ="p")


tibble(x = seq(0,3,0.1)) %>% 
  mutate(y = exp(x)) %>% 
  ggplot() + geom_line(aes(x,y)) +
  labs(x = "aX + b", y = "y")

tibble(x = seq(0.1,3,0.1)) %>% 
  mutate(y = 1/x) %>% 
  ggplot() + geom_line(aes(x,y)) + 
  labs(x = "aX + b", y="y")
