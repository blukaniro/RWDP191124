## 根重密度推定例, 根研50, 2019/11/24 田島亮介
## ライブラリの読み込み
library(rstan)
library(inline) 
library(Rcpp)

## Stanモデル
Model<- "
data {
    int<lower=0> NT;
    int<lower=0> ND;
    real<lower=0> D[ND];
    int<lower=0> RN[NT];
    real<lower=0> S[NT, ND];
}

parameters {
    real<lower=0> b0;
    real<lower=0> b1;
    real<lower=0> b2[NT];
    real<lower=0> sgm;
}

model {
    for (nd in 1:ND){
        for (nt in 1:NT){
            S[nt, nd] ~ normal((b0+b1*RN[nt]-b2[nt]*D[nd]), sgm);
        }
    }
}
    "

## データの読み込み，整形
d <-read.table("data.txt", header=T)
S <- as.matrix(d[,3:5])
NT <- nrow(S)
ND <- ncol(S)
D <- c(8.5, 15, 17)
RN <- d$RN
data_for_stan <- list(NT=NT, ND=ND, D=D, RN=RN, S=S)

## MCMCの実行
set.seed(123)
fit <- stan(
    model_code = Model,
    data = data_for_stan
    )

## 検証
Real<-c(d$CoreA,d$CoreB,d$CoreC)
A<-summary(fit)$summary["b0", "mean"]+summary(fit)$summary["b1", "mean"]*RN-(summary(fit)$summary[3:23, "mean"])*D[1]
B<-summary(fit)$summary["b0", "mean"]+summary(fit)$summary["b1", "mean"]*RN-(summary(fit)$summary[3:23, "mean"])*D[2]
C<-summary(fit)$summary["b0", "mean"]+summary(fit)$summary["b1", "mean"]*RN-(summary(fit)$summary[3:23, "mean"])*D[3]
Predicted<-c(A,B,C)

cor (Real, Predicted) # 相関係数
plot (Real, Predicted) # 図示
