context("unconventional plots (related t, association, interaction, panelled logistic)")
set.seed(1212)
data(exercise_data)
data("relationship_satisfaction")
d = exercise_data
k = d
deleteme = which(k$rewards == "no rewards")
k = k[-(deleteme[1:2]), ]

test_that("unconventional plots", {
  vdiffr::expect_doppelganger("related T",
                              flexplot(weight.loss ~ rewards, data = k, related = T))
  vdiffr::expect_doppelganger("association plot",
                              flexplot(gender ~ rewards, data = d, jitter = c(.05, 0)))
  vdiffr::expect_doppelganger("interaction plot",
                              flexplot(
                                weight.loss ~ therapy.type + gender,
                                data = d,
                                alpha = .4
                              ))
  vdiffr::expect_doppelganger(
    "interaction plot panel",
    flexplot(
      weight.loss ~ therapy.type | gender,
      data = d,
      sample = 50
    )
  )
  vdiffr::expect_doppelganger(
    "panelled logistic with sampling",
    flexplot(
      gender ~ weight.loss |
        therapy.type,
      data = d,
      sample = 50,
      method = "logistic"
    )
  )
})