[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEacfar1** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFEacfar1

Published in: Statistics of Financial Markets

Description: 'Plots the autocorrelation function of an AR(1) (autoregressive) process.'

Keywords: acf, autocorrelation, autoregressive, discrete, graphical representation, linear, plot, process, simulation, stationary, stochastic, stochastic-process, time-series

See also: SFEacfar2, SFEacfma1, SFEacfma2, SFEpacfar2, SFEpacfma2, SFEfgnacf

Author: Joanna Tomanek

Author[Matlab]: Christian M. Hafner 

Submitted: Fri, June 13 2014 by Felix Jung

Input: 
- a : alpha value
- lag : lag value

Example: 
- 1: a=0.9, lag=30
- 2: a=-0.9, lag=30
```

![Picture1](SFEacfar1-1_m.png)

![Picture2](SFEacfar1-2_m.png)

![Picture3](SFEacfar11.png)

![Picture4](SFEacfar12.png)

### R Code
```r


# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# parameter settings
lag = "30"  # lag value
a   = "0.9"  # value of alpha_1

# Input alpha_1
message = "      give alpha"
default = a
a       = winDialogString(message, default)
a       = type.convert(a, na.strings = "NA", as.is = FALSE, dec = ".")

# Input lag value
message = "      give lag"
default = lag
lag     = winDialogString(message, default)
lag     = type.convert(lag, na.strings = "NA", as.is = FALSE, dec = ".")

# Plot
plot(ARMAacf(ar = a, ma = numeric(0), lag.max = lag, pacf = FALSE), type = "h", 
    xlab = "lag", ylab = "acf")
title("Sample autocorrelation function (acf)")
```

automatically created on 2018-05-28

### MATLAB Code
```matlab

clear
clc
close all

% user inputs parameters
disp('Please input lag value lag, value of alpha1 a as: [30, 0.9]') ;
disp(' ') ;
para = input('[lag, a] = ');
while length(para) < 2
    disp('Not enough input arguments. Please input in 1*2 vector form like [30, 0.9] or [30 0.9]');
    para = input('[lag, a] = ');
end
lag = para(1);
a   = para(2);

% main computation
randn('state', 0)              % Start from a known state.
x = randn(10000, 1);           % 10000 Gaussian deviates ~ N(0, 1).
y = filter(1, [1 -a], x);      % Create an AR(1) process.
autocorr(y, lag, [], 2);       % Plot
```

automatically created on 2018-05-28