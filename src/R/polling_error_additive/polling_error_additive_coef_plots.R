library(tidyverse)
library(cmdstanr)

fit <- readRDS("data/us_input/polling_error/polling_error_abs_cov_mat.RDS")



###############################################################################
## Base effects
eta <- fit$summary("eta", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(t = 1:n()) %>%
  left_join(df %>% distinct(t, year)) %>%
  dplyr::select(-variable, t) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`) %>%
  arrange(q50) %>%
  mutate(year = factor(year, levels = year))

gamma <- fit$summary("gamma", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(r = 1:n()) %>%
  left_join(df %>% distinct(r, Region)) %>%
  dplyr::select(-variable, r) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`) %>%
  arrange(q50) %>%
  mutate(Region = factor(Region, levels = Region))

delta <- fit$summary("delta", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(d = 1:n()) %>%
  left_join(df %>% distinct(d, Division)) %>%
  dplyr::select(-variable, d) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`) %>%
  arrange(q50) %>%
  mutate(Division = factor(Division, levels = Division))

beta <- fit$summary("beta", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(s = 1:n()) %>%
  left_join(df %>% distinct(s, State, state) %>% arrange(s)) %>%
  dplyr::select(-variable, s) %>%
  mutate(state = factor(state, levels = states_2020_ordered)) %>%
  arrange(state) %>%
  mutate(State = factor(State, levels = State)) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`)

alpha <- fit$draws("alpha") %>% as.matrix() %>%
  as_tibble()

sigma <- fit$summary("sigma", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(i = 1:n()) %>%
  left_join(data.frame(i = 1:6,
                       kind = c("State",
                                "Region",
                                "Division",
                                "Year",
                                "Region-Year",
                                "Division-Year"))) %>%
  dplyr::select(-variable, -i) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`) %>%
  arrange(q50) %>%
  mutate(kind = factor(kind, levels = kind))

plt1 <- ggplot(data = alpha, aes(x = 100 * (inv.logit(V1) - 0.5 ))) +
  geom_histogram(bins = 100) +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = "National component (%)") +
  geom_vline(aes(xintercept = 0), linetype = 2, size = 0.6)
plt2 <- ggplot(data = gamma, aes(x = Region, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "Regional polling error component (%)") +
  coord_flip() +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.6)

plt3 <- ggplot(data = delta, aes(x = Division, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "Division polling error component (%)") +
  coord_flip() +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.6)

plt4 <- ggplot(data = beta, aes(x = State, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "State polling error component (%)") +
  coord_flip() +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.6)

plt5 <- ggplot(data = eta, aes(x = year, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "Yearly polling error component (%)") +
  coord_flip() +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.6)

plt6 <- ggplot(data = sigma, aes(x = kind, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "Variance component (%)") +
  coord_flip()

lay <- rbind(c(1, 1, 1, 1, 2, 2, 2, 2),
             c(1, 1, 1, 1, 2, 2, 2, 2),
             c(3, 3, 3, 3, 2, 2, 2, 2),
             c(3, 3, 3, 3, 2, 2, 2, 2),
             c(4, 4, 4, 4, 2, 2, 2, 2),
             c(4, 4, 4, 4, 2, 2, 2, 2),
             c(5, 5, 5, 5, 6, 6, 6, 6),
             c(5, 5, 5, 5, 6, 6, 6, 6)
)
saveRDS(list(plt1 = plt1,
             plt2 = plt2,
             plt3 = plt3,
             plt4 = plt4,
             plt5 = plt5,
             plt6 = plt6,
             lay = lay
), "data/model_output/polling_error/plt_base_effects_v5.Rds")
## graph
plt <- grid.arrange(plt1, plt4, plt2, plt3, plt5, plt6, layout_matrix = lay,
                    bottom = textGrob(
                      "Positive polling errors overestimate Democratic support
               Median, 50%, 80%",
                      gp = gpar(fontface = 3, fontsize = 9),
                      hjust = 1,
                      x = 1
                    ))
## save
ggsave("plots/input/polling_error/v5_base.jpeg",
       plt,
       width = 10, height = 10)






###############################################################################
## Interactions
gamma_eta <- fit$summary("gamma_eta", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(rt = 1:n()) %>%
  left_join(df %>% distinct(rt, Region, year) %>% arrange(rt)) %>%
  dplyr::select(-variable, rt) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`)
plt10 <- ggplot(data = gamma_eta,
                aes(x = Region, y = q50, color = as.factor(year))) +
  geom_point(position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0,
                position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0,
                position = position_dodge(0.6)) +
  theme_light() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  labs(y = "Region-year interaction polling error component (%)",
       color = "Election") +
  coord_flip() +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.6)


delta_eta <- fit$summary("delta_eta", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(dt = 1:n()) %>%
  left_join(df %>% distinct(dt, Division, year) %>% arrange(dt)) %>%
  dplyr::select(-variable, -dt) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`)
plt11 <- ggplot(data = delta_eta,
                aes(x = Division, y = q50, color = as.factor(year))) +
  geom_point(position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0,
                position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0,
                position = position_dodge(0.6)) +
  theme_light() +
  theme(axis.title.y = element_blank(),
        legend.box = "horizontal",
        legend.position = "bottom",
        legend.title = element_blank()) +
  labs(y = "Division-year interaction polling error component (%)",
       color = "Election") +
  coord_flip() +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.6) +
  guides(color = guide_legend(nrow = 1))

lay <- rbind(c(1,1),
             c(2,2),
             c(2,2)
)
saveRDS(list(plt10 = plt10,
             plt11 = plt11,
             lay = lay
), "data/model_output/polling_error/plt_interaction_effects_v5.Rds")

plt <- grid.arrange(plt10, plt11, layout_matrix = lay,
                    bottom = textGrob(
                      "Positive polling errors overestimate Democratic support
               Median, 50%, 80%",
                      gp = gpar(fontface = 3, fontsize = 9),
                      hjust = 1,
                      x = 1
                    ))
ggsave("plots/input/polling_error/v5_interactions.jpeg", plt,
       width = 10, height = 10)


###############################################################################
## State - Year
xi_sigma <- fit$summary("xi_sigma", prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5), c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(s = 1:n()) %>%
  left_join(df %>% distinct(s, State, state) %>% arrange(s)) %>%
  dplyr::select(-variable, s) %>%
  mutate(state = factor(state, levels = states_2020_ordered)) %>%
  arrange(state) %>%
  mutate(State = factor(State, levels = State)) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`)

plt21 <- ggplot(data = xi_sigma, aes(x = State, y = q50)) +
  geom_point() +
  geom_errorbar(aes(ymin = q25, ymax = q75), size = 0.75, width = 0) +
  geom_errorbar(aes(ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "State error scales (%)") +
  coord_flip()

lay <- rbind(c(1, 2))
plt_joint_state <- grid.arrange(plt4, plt21, layout_matrix = lay)
ggsave("plots/input/polling_error/v5_joint_state.jpeg", plt_joint_state,
       width = 10, height = 10)


## xi
xi <- fit$summary("xi",
                  prob_gt_0 = ~ quantile(100 * (inv.logit(.) - 0.5),
                                         c(0.1, 0.25, 0.5, 0.75, 0.9))) %>%
  mutate(state = rep(state_abb, 6),
         State = rep(state_abb_full, 6),
         year = sort(rep(seq(2000, 2020, 4), 50))) %>%
  dplyr::select(-variable) %>%
  mutate(state = factor(state, levels = states_2020_ordered)) %>%
  arrange(state) %>%
  mutate(State = factor(State, levels = unique(State))) %>%
  rename(q10 = `10%`,
         q25 = `25%`,
         q50 = `50%`,
         q75 = `75%`,
         q90 = `90%`) %>%
  left_join(results_2020 %>%
              dplyr::select(state_po, finalTwoPartyVSDemocratic) %>%
              rename(state = state_po))
## Split by percentage 2020
plt_xi1 <- ggplot(data = xi %>%
                    filter(finalTwoPartyVSDemocratic > 58), aes(x = State, y = q50, color = as.factor(year))) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                size = 0.75, width = 0,
                position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = q10, ymax = q90),
                size = 0.5, width = 0,
                position = position_dodge(width = 0.6)) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "State year polling error component (%)",
       color = "Election",
       caption = "Positive polling errors overestimate Democratic support
               Median, 50%, 80%") +
  coord_flip()
plt_xi2 <- ggplot(data = xi %>%
                    filter(finalTwoPartyVSDemocratic < 58,
                           finalTwoPartyVSDemocratic > 50), aes(x = State, y = q50, color = as.factor(year))) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                size = 0.75, width = 0,
                position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = q10, ymax = q90),
                size = 0.5, width = 0,
                position = position_dodge(width = 0.6)) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "State year polling error component (%)",
       color = "Election",
       caption = "Positive polling errors overestimate Democratic support
               Median, 50%, 80%") +
  coord_flip()
plt_xi3 <- ggplot(data = xi %>%
                    filter(finalTwoPartyVSDemocratic < 50,
                           finalTwoPartyVSDemocratic > 40), aes(x = State, y = q50, color = as.factor(year))) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                size = 0.75, width = 0,
                position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = q10, ymax = q90),
                size = 0.5, width = 0,
                position = position_dodge(width = 0.6)) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "State year polling error component (%)",
       color = "Election",
       caption = "Positive polling errors overestimate Democratic support
               Median, 50%, 80%") +
  coord_flip()
plt_xi4 <- ggplot(data = xi %>%
                    filter(finalTwoPartyVSDemocratic < 40), aes(x = State, y = q50, color = as.factor(year))) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = q25, ymax = q75),
                size = 0.75, width = 0,
                position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = q10, ymax = q90),
                size = 0.5, width = 0,
                position = position_dodge(width = 0.6)) +
  theme_light() +
  theme(axis.title.y = element_blank()) +
  labs(y = "State year polling error component (%)",
       color = "Election",
       caption = "Positive polling errors overestimate Democratic support
               Median, 50%, 80%") +
  coord_flip()
# save
ggsave("plots/input/polling_error/v5_xi1.jpeg", plt_xi1,
       width = 10, height = 10)
ggsave("plots/input/polling_error/v5_xi2.jpeg", plt_xi2,
       width = 10, height = 10)
ggsave("plots/input/polling_error/v5_xi3.jpeg", plt_xi3,
       width = 10, height = 10)
ggsave("plots/input/polling_error/v5_xi4.jpeg", plt_xi4,
       width = 10, height = 10)
