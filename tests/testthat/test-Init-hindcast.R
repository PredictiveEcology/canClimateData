## Init() used `projected_yrs` for hindcasts, which is only defined inside .inputObjects(), so
## projectedType = "hindcast" failed with "object 'projected_yrs' not found". This runs the real
## Init(), so it fails on the code before the fix.

test_that("Init() with projectedType = 'hindcast' samples one layer per projected year", {
  for (useCache in c(FALSE, TRUE)) {
    path <- withr::local_tempdir()
    withr::with_options(list(reproducible.useCache = useCache,
                             reproducible.useNewDigestAlgorithm = 2,
                             reproducible.cachePath = file.path(path, "cache")), {
      sim <- runInit(path = path, projectedType = "hindcast",
                     params = list(projectedClimateYears = 2011:2015))
    })
    projMDC <- sim$projectedClimateRasters$MDC
    lbl <- paste("projected MDC, useCache =", useCache)
    expect_identical(names(projMDC), paste0("year", 2011:2015), label = lbl)
    expect_identical(fileLayerNames(projMDC), paste0("year", 2011:2015),
                     label = paste("file behind", lbl))
  }
})
