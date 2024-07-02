test_that("Sims", {
    for (m in c("SimYes", "SimNo", "SimGaussian", "SimHenson", "SimHensonRT")) {
          expect_equal(do.call(paste0("opiSetup_for_", m), args = list()), list(err = NULL), label = m)
    }

    a <- opiSetup_for_Octopus900()
    expect_named(a, "err", label = "Octopus900")
    expect_gt(nchar(a$err), 10, label = "Octopus900")
})