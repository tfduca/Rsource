#多重対応分析
#組み込みのデータセット"HairEyeColor"を用いる
#方針は以下のサイトのD.3.2「多重クロス表を多重対応分析」(2016/9/9閲覧)
#http://monge.tec.fukuoka-u.ac.jp/r_analysis/0r_analysis.html


library(MASS)
x <- data.frame(HairEyeColor)
#データ形式をスタックに変換
x.stak <- data.frame(lapply(x, function(i) rep(i, x[,"Freq"]))[-4])
#x.stakの各成分をfactor型に変換
x.stak.f <- data.frame(apply(x.stak, 2, factor))

#分析
y.stak.f <- mca(x.stak.f, nf = 2)

