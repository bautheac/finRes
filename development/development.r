options(usethis.full_name = "Olivier Bauthéac")
usethis::use_gpl3_license()
usethis::use_readme_md()


test <- readRDS(file = "O:/Dropbox/Code/R/Packages/storethat/development/data - 2018-09-22.rds")

plot_performance(object = test$data[[7L]], ticker = "IVV US Equity")

test$data[[7L]]@tickers
?plot_performance


