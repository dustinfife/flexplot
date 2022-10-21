context("plot_modifiers")

test_that("modify_smooth works", {
  #p = mediate_plot(weight.loss~motivation + therapy.type, data=exercise_data)
  set.seed(12312)
  flexplot(y~x, data=small) %>% modify_smooth(color="black", method="lm") %>% 
    vdiffr::expect_doppelganger(title = "modify_smooth", fig = .)
  flexplot(y~x, data=small) %>% modify_smooth(method="lm") %>% 
    vdiffr::expect_doppelganger(title = "modify_smooth no color", fig = .)
  flexplot(y~x + a, data=small) %>% modify_smooth(color="black") %>% expect_error()
  flexplot(y~x + a, data=small) %>% modify_smooth(color=c("red", "black")) %>%
    vdiffr::expect_doppelganger(title = "modify_smooth with group aes", fig = .)
})
# maybe make this a separate package?

