# Nonlinear regression
data(pressure)
str(pressure)
summary(pressure)
pressure$temperature = pressure$temperature + 273.15
pressure$pressure = pressure$pressure * .1333
summary(pressure)
pres = pressure$pressure
temp = pressure$temperature
rm(pressure)
ls()
par(mfrow=c(1,4)) 
plot(pres ~ temp, main="Vapor Pressure\nof Mercury", xlab="Temperature (degrees Kelvin)", ylab="Pressure (kPascals)", log="y")

