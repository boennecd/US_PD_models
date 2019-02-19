#####
# get data
temp <- tempfile()
download.file("http://www.maria-vassalou.com/data/defaultdataset.zip", temp)
dat <- unz(temp, "defaultdataset.txt")
dat_read <- read.table(dat)
colnames(dat_read) <- c(
  "year", "month", "permno", "prop_default", "log_assets", "asset_vol")
saveRDS(dat_read, file.path("tmp", "dtd-04-article.RDS"))
unlink(temp)

#####
# Look at mean and std of data from other paper
dat_read <- subset(dat_read, year >= 1980L)
nrow(dat_read)
range(dat_read$prop_default)

with(subset(dat_read, prop_default > 0 & prop_default < 1), 
     range(prop_default))
eps <- 1e-15
dat_read$prop_default <- pmin(pmax(dat_read$prop_default, eps), 1 - eps)
max(dat_read$year)

dat_read$dtd <- -qnorm(dat_read$prop_default)
quantile(dat_read$dtd, seq(0, 1, .1))
mean(dat_read$dtd)
sd(dat_read$dtd)

#####
# compare with ours
our_dat <- readRDS(file.path("data", "final.RDS"))
make_ym_inv <- our_dat$ym_funcs$make_ym_inv
our_dat <- our_dat$data[, c("tstart", "dtd")]
our_dat$year <- as.integer(format(make_ym_inv(our_dat$tstart), "%Y"))

our_dat$dtd <- pmin(pmax(our_dat$dtd, qnorm(eps)), qnorm(1 - eps))
our_dat <- subset(our_dat, year >= 1980L & year <= max(dat_read$year))

quantile(our_dat$dtd, seq(0, 1, .1))
mean(our_dat$dtd)
sd(our_dat$dtd)
