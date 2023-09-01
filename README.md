# TeslaVolatilityForecast

Forecasting the volatility of Tesla stocksForecasting the volatility of Tesla stocks
Aug 2022 - Nov 2022Aug 2022 - Nov 2022
Our aim is volatility forecasting, which is the crux to a multitude of finance applications. We
will compare EGARCH, HAR-RV, HAR-RAV, a simple average forecast combination, and
Granger-Ramanathan forecast combination models to the GARCH model at a 1, 5 and 20 step
horizon under QLIKE loss and MSE by using 5 minute intra-daily data of Tesla Inc. stock to
construct Realised Volatility and Realised Absolute Value as predictors. We found that a
Granger-Ramanathan combination forecast beats out both the GARCH(1,1) and a benchmark
simple average combination forecast, under both the MSE and QLike loss functions when
tested against them via a Diebold-Mariano test for all horizons considered. As for the single
models, HAR-RAV conclusively beats out the benchmark GARCH(1, 1) model at the 5 and
20 step horizons under both MSE and QLIKE loss.
