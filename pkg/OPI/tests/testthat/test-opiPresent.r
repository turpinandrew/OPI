test_that("Sims", {

    for (m in c("SimYes", "SimNo", "SimGaussian", "SimHenson", "SimHensonRT")) {
        chooseOPI(m)

        if (m == "SimHensonRT")
            args <- list(rtData = data.frame(Dist = 1:100, Rt = 1:100, Person = 1:100))
        else
            args <- list()
        do.call("opiInitialise", args = args)

        a <- do.call("opiPresent", args = list(stim = list(x = 0, y = 0, level = 10)))
        expect_named(a, c("err", "seen", "time"), label = m)
        expect_equal(a$err, NULL, label = m)

        if (m == "SimYes") {
            expect_equal(a$seen, TRUE, label = m)
            expect_equal(a$time, NA, label = m)
        }

        if (m == "SimNo") {
            expect_equal(a$seen, FALSE, label = m)
            expect_equal(a$time, NA, label = m)
        }

        if (m == "SimHensonRT")
            expect_true(!is.na(a$time), label = m)
    }
})