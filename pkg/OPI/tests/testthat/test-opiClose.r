test_that("Sims", {
    for (m in c("SimYes", "SimNo", "SimGaussian", "SimHenson", "SimHensonRT"))
        expect_equal(do.call(paste0("opiClose_for_", m), args = list()), list(err = NULL), label = m)
})

test_that("Machines", {
    for (m in c("ImoVifa", "PhoneHMD", "PicoVR")) {
        a <- do.call(paste0("opiClose_for_", m), args = list())
        expect_named(a, "err", label = m)
        expect_gt(nchar(a$err), 5, label = m)
    }

    assign("socket", file(nullfile()), envir = .opi_env$O900)
    expect_equal(opiClose_for_Octopus900(), list(err = NULL), label = "Octopus900")

    #assign("socket", file(nullfile(), raw = TRUE), envir = .opi_env$Compass)
    #expect_warning(a <- opiClose_for_Compass())
    #expect_names(a, "err")
    #expect_names(a, "fixations")
})