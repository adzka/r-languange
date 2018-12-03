
air <- read.csv(file.choose(), header=TRUE)

air

model <-lm(kubik ~ waktu, data=air)
summary(model)

plot(kubik ~ waktu, data=air)
abline(model, col = "red", lwd = 1)

predict(model, data.frame(waktu = 50))

motor <- read.csv(file.choose(), header=TRUE)

motor

poly_model <- lm(torsi ~ poly(rpm,degree=2), data = motor)
poly_model

x <- with(motor, seq(min(rpm), max(rpm), length.out=30000))
y <- predict(poly_model, newdata = data.frame(rpm = x))

plot(torsi ~ rpm, data = motor)
lines(x, y, col = "red")
