test_that("Sims", {
    for (m in c("SimYes", "SimNo", "SimGaussian", "SimHenson", "SimHensonRT")) {
        a <- do.call(paste0("opiQueryDevice_for_", m), args = list())
        expect_named(a, c("isSim", "machine"), label = m)
        expect_equal(a$isSim, TRUE, label = m)
        expect_equal(a$machine, m, label = m)
    }
})

test_that("Machines", {
    a <- opiQueryDevice_for_Compass()
    expect_gt(length(a), 1)

    expect_equal(opiQueryDevice_for_Octopus900(), list(isSim = FALSE))

    for (m in c("ImoVifa", "PhoneHMD", "PicoVR")) {
        a <- do.call(paste0("opiQueryDevice_for_", m), args = list())
        expect_named(a, "err", label = m)
        expect_equal(length(a), 1, label = m)
    }

    #expect_error(a <- opiQueryDevice_for_Octopus900(), label = "Octopus900")
    #expect_error(a <- opiQueryDevice_for_PhoneHMD(), label = "PhoneHMD")
    #expect_error(a <- opiQueryDevice_for_PicoVR(), label = "PicoVR")
})