test_that("Sims", {
    for (m in c("SimYes", "SimNo", "SimGaussian", "SimHenson")) {
        a <- do.call(paste0("opiInitialise_for_", m), args = list())
        expect_equal(a, list(err = NULL), label = m)
    }

    expect_warning(a <- opiInitialise_for_SimGaussian(sd = -1))
    expect_named(a, "err")
    expect_gt(nchar(a$err), 5)

    expect_warning(a <- opiInitialise_for_SimHenson(type = "Z"))
    expect_named(a, "err")
    expect_gt(nchar(a$err), 5)

    expect_warning(a <- opiInitialise_for_SimHenson(cap = -10))

    expect_warning(a <- opiInitialise_for_SimHenson(type = "X", A = NA))
    expect_named(a, "err")
    expect_gt(nchar(a$err), 5)

    expect_warning(a <- opiInitialise_for_SimHenson(type = "X", B = NA))
    expect_named(a, "err")
    expect_gt(nchar(a$err), 5)

    expect_warning(a <- opiInitialise_for_SimHensonRT(type = "Z", rtData = data.frame(Dist = 1:100, Rt = 1:100, Person = 1:100)))
    expect_named(a, "err")
    expect_gt(nchar(a$err), 5)

    expect_warning(a <- opiInitialise_for_SimHensonRT(cap = -10, rtData = data.frame(Dist = 1:100, Rt = 1:100, Person = 1:100)))

    expect_warning(a <- opiInitialise_for_SimHensonRT(type = "X", A = NA, rtData = data.frame(Dist = 1:100, Rt = 1:100, Person = 1:100)))
    expect_named(a, "err")
    expect_gt(nchar(a$err), 5)

    expect_warning(a <- opiInitialise_for_SimHensonRT(type = "X", B = NA, rtData = data.frame(Dist = 1:100, Rt = 1:100, Person = 1:100)))
    expect_named(a, "err")
    expect_gt(nchar(a$err), 5)
})

#test_that("Machines", {
#    expect_error(a <- opiInitialise_for_Octopus900(), label = "Octopus900")
#    expect_error(a <- opiInitialise_for_Compass(), label = "Compass")
#    expect_error(a <- opiInitialise_for_ImoVifa(), label = "ImoVifa")
#    expect_error(a <- opiInitialise_for_PhoneHMD(), label = "PhoneHMD")
#    expect_error(a <- opiInitialise_for_PicoVR(), label = "PicoVR")
#})