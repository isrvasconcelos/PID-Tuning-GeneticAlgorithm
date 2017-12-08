K1 <- 1
K2  <- 1
G1 <- tf(-K1,1)
G2 <- tf(c(0,2), c(1,2))

numg3 <- -0.125*c(1, 0.435)
deng3 <- pracma::polymul(c(1, 1.23), c(1, 0.226, 0.0169))
G3 <- tf(numg3, deng3)

H1 <- tf(c(-K2, 0), c(0, 1))

G4 <- series(G2, G3)
G5 <- feedback(G4, H1)
Ge <- series(G1, G5)
T1 <- feedback(Ge, 1)
stepplot(T1, t = seq(0,40,0.1))
