library(nflverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(gridExtra)
pbp <- load_pbp(seasons=TRUE)
weekly <- calculate_player_stats_def(pbp, weekly=TRUE) %>% 
  filter(position_group!="SPEC") %>%
  mutate(season_type = ifelse(season <= 2020, ifelse(week <= 16, "REG", "POST"),
                              ifelse(week <= 17, "REG", "POST")))
player_stats <- load_player_stats(seasons = TRUE, stat_type = "offense") %>%
  mutate(team = recent_team) %>% full_join(weekly, by=c("player_id", "player_display_name",
                                                        "position", "position_group",
                                                        "team", "season","week",
                                                        "season_type"), multiple="all") %>%
  filter(season_type == "REG" & position_group!="SPEC") %>%
  group_by(player_id, player_display_name, position, position_group, team, season) %>% 
  summarise(games = n(), across(completions:passing_2pt_conversions, sum), pacr_per_game = mean(pacr), 
            dakota = mean(dakota), across(carries:receiving_2pt_conversions, sum), across(racr:wopr, mean),
            special_teams_tds = sum(special_teams_tds), across(def_tackles:def_penalty_yards, sum))
contracts <- load_contracts() %>% 
  mutate(expiration = year_signed + years - 1,
         temp_team = ifelse(team=="49ers", "SF", ifelse(team=="Bears", "CHI",
                             ifelse(team=="Bengals", "CIN",
                                    ifelse(team=="Bills", "BUF",
                                           ifelse(team=="Broncos", "DEN",
                                                  ifelse(team=="Browns", "CLE",
                                                         ifelse(team=="Buccaneers", "TB",
                                                                ifelse(team=="Cardinals", "ARI",
                                                                       ifelse(team=="Chargers", "LAC",
                                                                              ifelse(team=="Chiefs", "KC",
                                                                                     ifelse(team=="Colts", "IND",
                                                                                            ifelse(team=="Commanders", "WAS",
                                                                                                   ifelse(team=="Cowboys", "DAL",
                                                                                                          ifelse(team=="Dolphins", "MIA",
                                                                                                                 ifelse(team=="Eagles", "PHI",
                                                                                                                        ifelse(team=="Falcons", "ATL",
                                                                                                                               ifelse(team=="Giants", "NYG",
                                                                                                                                      ifelse(team=="Jaguars", "JAX",
                                                                                                                                             ifelse(team=="Jets", "NYJ",
                                                                                                                                                    ifelse(team=="Lions", "DET",
                                                                                                                                                           ifelse(team=="Packers", "GB",
                                                                                                                                                                  ifelse(team=="Panthers", "CAR",
                                                                                                                                                                         ifelse(team=="Patriots", "NE",
                                                                                                                                                                                ifelse(team=="Raiders", "LV",
                                                                                                                                                                                       ifelse(team=="Rams", "LA",
                                                                                                                                                                                              ifelse(team=="Ravens", "BAL",
                                                                                                                                                                                                     ifelse(team=="Saints", "NO",
                                                                                                                                                                                                            ifelse(team=="Seahawks", "SEA",
                                                                                                                                                                                                                   ifelse(team=="Steelers", "PIT",
                                                                                                                                                                                                                          ifelse(team=="Texans", "HOU",
                                                                                                                                                                                                                                 ifelse(team=="Titans", "TEN",
                                                                                                                                                                                                                                        ifelse(team=="Vikings", "MIN", team)))))))))))))))))))))))))))))))),
         merge_team = sub("[^[:alnum:]].*", "", temp_team), merge_name = 
           ifelse(player=="Michael Carter II" & position=="CB", "Michael Carter II",
                  clean_player_names(player))) %>% filter(year_signed >= 2013) %>% 
  select(-c(is_active, player_page, date_of_birth, height, weight)) %>%
  group_by(player, position, year_signed) %>% mutate(max_value = max(value)) %>%
  ungroup() %>% filter(value == max_value)
player_stats <- player_stats %>% mutate(merge_name = ifelse(player_display_name=="Michael Carter" & position=="CB",
                                                            "Michael Carter II", 
                                                            clean_player_names(player_display_name)),
                                        merge_team = team) %>%
  inner_join(contracts, by=c("merge_name", "merge_team"), multiple="all") %>%
  mutate(name = merge_name, team = merge_team, pos = position.y) %>%
  filter(season >= year_signed & season <= expiration) %>%
  group_by(name, team, pos, year_signed, expiration, value, apy, guaranteed, 
           apy_cap_pct, inflated_value, inflated_apy, inflated_guaranteed, college,
           draft_year, draft_round, draft_overall, draft_team) %>%
  summarize(across(games:def_penalty_yards, \(x) mean(x, na.rm=TRUE)),.groups="drop") %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(experience = ifelse(year_signed==draft_year, "Rookie", "Veteran"))

thm <- theme_minimal() + theme(text = element_text(size = 14))

afc <- ggplot(teams_colors_logos %>%
                filter((team_abbr!="LAR" & team_abbr!="OAK" & team_abbr!="SD" &
                          team_abbr!="STL") & team_conf=="AFC"), aes(x=0, y=team_abbr)) +
  geom_text(aes(label=paste0(team_name)), hjust=0) + thm + 
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + theme(plot.title = element_text(face = "bold"),
                                                    plot.title.position = "plot",
                                                    axis.title.x = element_blank(),
                                                    axis.title.y = element_blank(),
                                                    axis.text.y = element_nfl_logo(size = 0.75),
                                                    axis.text.x = element_blank())
nfc <- ggplot(teams_colors_logos %>%
                filter((team_abbr!="LAR" & team_abbr!="OAK" & team_abbr!="SD" &
                          team_abbr!="STL") & team_conf=="NFC"), aes(x=0, y=team_abbr)) +
  geom_text(aes(label=paste0(team_name)), hjust=0) + thm + 
  theme(panel.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + theme(plot.title = element_text(face = "bold"),
                                                    plot.title.position = "plot",
                                                    axis.title.x = element_blank(),
                                                    axis.title.y = element_blank(),
                                                    axis.text.y = element_nfl_logo(size = 0.75),
                                                    axis.text.x = element_blank())
grid.arrange(afc, nfc, ncol=2)

off <- ggplot(player_stats %>% filter(pos=="QB" | pos== "RB" | pos=="WR" | pos=="TE")) +
  geom_density(aes(x=inflated_apy, color=experience), lwd=1) + facet_wrap(~ pos) +
  scale_color_manual(values = c(Rookie = "#D50A0A", Veteran = "#013369")) + thm

def <- ggplot(player_stats %>% filter(pos=="ED" | pos== "IDL" | pos=="LB" | pos=="CB" | pos=="S")) +
  geom_density(aes(x=inflated_apy, color=experience), lwd=1) + facet_wrap(~ pos) +
  scale_color_manual(values = c(Rookie = "#D50A0A", Veteran = "#013369")) + thm

grid.arrange(off, def, ncol=2)

QB <- player_stats %>% filter(pos=="QB" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy,
         games:dakota, rushing_epa) %>% mutate(qb_epa = passing_epa + rushing_epa)
qb_epa_mod <- lm(QB$qb_epa ~ QB$inflated_apy)
qb.epop <- resid(qb_epa_mod)
QB <- cbind(QB, qb.epop) %>% group_by(team) %>% summarise(qb.epop = sum(qb.epop))
ggplot(QB, aes(x=reorder(team, -qb.epop), y=qb.epop)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran Quarterback Expected Points Over Projected", x="Team",
       y="Expected Points Over Projected (EPOP)")

RB <- player_stats %>% filter(pos=="RB" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy, games, carries:rushing_epa, receiving_epa) %>%
  mutate(rb_epa = rushing_epa + receiving_epa)
rb_epa_mod <- lm(RB$rb_epa ~ RB$inflated_apy)
rb.epop <- resid(rb_epa_mod)
RB <- cbind(RB, rb.epop) %>% group_by(team) %>% summarise(rb.epop = sum(rb.epop))
ggplot(RB, aes(x=reorder(team, -rb.epop), y=rb.epop)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran Runningback Expected Points Over Projected", x="Team",
       y="Expected Points Over Projected (EPOP)")

WR <- player_stats %>% filter(pos=="WR" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy, games, receptions:wopr)
wr_epa_mod <- lm(WR$receiving_epa ~ WR$inflated_apy)
wr.epop <- resid(wr_epa_mod)
WR <- cbind(WR, wr.epop) %>% group_by(team) %>% summarise(wr.epop = sum(wr.epop))
ggplot(WR, aes(x=reorder(team, -wr.epop), y=wr.epop)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran Wide Receiver Expected Points Over Projected", x="Team",
       y="Expected Points Over Projected (EPOP)")

TE <- player_stats %>% filter(pos=="TE" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy, games, receptions:wopr)
te_epa_mod <- lm(TE$receiving_epa ~ TE$inflated_apy)
te.epop <- resid(te_epa_mod)
TE <- cbind(TE, te.epop) %>% group_by(team) %>% summarise(te.epop = sum(te.epop))
ggplot(TE, aes(x=reorder(team, -te.epop), y=te.epop)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran Tight End Expected Points Over Projected", x="Team",
       y="Expected Points Over Projected (EPOP)")
  

ED <- player_stats %>% filter(pos=="ED" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_tackles_for_loss + def_sacks + def_qb_hits - (def_penalty_yards/10))
ed_impact_mod <- lm(ED$impact ~ ED$inflated_apy)
ed.ipoe <- resid(ed_impact_mod)
ED <- cbind(ED, ed.ipoe) %>% group_by(team) %>% summarise(ed.ipoe = sum(ed.ipoe))
ggplot(ED, aes(x=reorder(team, -ed.ipoe), y=ed.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran EDGE Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

IDL <- player_stats %>% filter(pos=="IDL" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_tackles_for_loss + def_sacks + def_qb_hits - (def_penalty_yards/10))
idl_impact_mod <- lm(IDL$impact ~ IDL$inflated_apy)
idl.ipoe <- resid(idl_impact_mod)
IDL <- cbind(IDL, idl.ipoe) %>% group_by(team) %>% summarise(idl.ipoe = sum(idl.ipoe))
ggplot(IDL, aes(x=reorder(team, -idl.ipoe), y=idl.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran Interior Defensive Line Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

LB <- player_stats %>% filter(pos=="LB" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_tackles_for_loss + def_sacks + def_qb_hits + def_pass_defended - (def_penalty_yards/10))
lb_impact_mod <- lm(LB$impact ~ LB$inflated_apy)
lb.ipoe <- resid(lb_impact_mod)
LB <- cbind(LB, lb.ipoe) %>% group_by(team) %>% summarise(lb.ipoe = sum(lb.ipoe))
ggplot(LB, aes(x=reorder(team, -lb.ipoe), y=lb.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran Linebacker Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

CB <- player_stats %>% filter(pos=="CB" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_pass_defended + def_interceptions - (def_penalty_yards/10))
cb_impact_mod <- lm(CB$impact ~ CB$inflated_apy)
cb.ipoe <- resid(cb_impact_mod)
CB <- cbind(CB, cb.ipoe) %>% group_by(team) %>% summarise(cb.ipoe = sum(cb.ipoe))
ggplot(CB, aes(x=reorder(team, -cb.ipoe), y=cb.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran Cornerback Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

S <- player_stats %>% filter(pos=="S" & experience=="Veteran") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_pass_defended + def_interceptions + (def_tackles_solo/10) - (def_penalty_yards/10))
s_impact_mod <- lm(S$impact ~ S$inflated_apy)
s.ipoe <- resid(s_impact_mod)
S <- cbind(S, s.ipoe) %>% group_by(team) %>% summarise(s.ipoe = sum(s.ipoe))
ggplot(S, aes(x=reorder(team, -s.ipoe), y=s.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) + scale_color_nfl(type="primary") +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Veteran Safety Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

QB_rookie <- player_stats %>% filter(pos=="QB" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy,
         games:dakota, rushing_epa) %>% mutate(qb_epa = passing_epa + rushing_epa)
r_qb_epa_mod <- lm(QB_rookie$qb_epa ~ QB_rookie$inflated_apy)
r_qb.epop <- resid(r_qb_epa_mod)
QB_rookie <- cbind(QB_rookie, r_qb.epop) %>% group_by(team) %>% summarise(r_qb.epop = sum(r_qb.epop))
ggplot(QB_rookie, aes(x=reorder(team, -r_qb.epop), y=r_qb.epop)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie Quarterback Expected Points Over Projected", x="Team",
       y="Expected Points Over Projected (EPOP)")

RB_rookie <- player_stats %>% filter(pos=="RB" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy, games, carries:rushing_epa, receiving_epa) %>%
  mutate(rb_epa = rushing_epa + receiving_epa)
r_rb_epa_mod <- lm(RB_rookie$rb_epa ~ RB_rookie$inflated_apy)
r_rb.epop <- resid(r_rb_epa_mod)
RB_rookie <- cbind(RB_rookie, r_rb.epop) %>% group_by(team) %>% summarise(r_rb.epop = sum(r_rb.epop))
ggplot(RB_rookie, aes(x=reorder(team, -r_rb.epop), y=r_rb.epop)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie Runningback Expected Points Over Projected", x="Team",
       y="Expected Points Over Projected (EPOP)")

WR_rookie <- player_stats %>% filter(pos=="WR" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy, games, receptions:wopr)
r_wr_epa_mod <- lm(WR_rookie$receiving_epa ~ WR_rookie$inflated_apy)
r_wr.epop <- resid(r_wr_epa_mod)
WR_rookie <- cbind(WR_rookie, r_wr.epop) %>% group_by(team) %>% summarise(r_wr.epop = sum(r_wr.epop))
ggplot(WR_rookie, aes(x=reorder(team, -r_wr.epop), y=r_wr.epop)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie Tight End Expected Points Over Projected", x="Team",
       y="Expected Points Over Projected (EPOP)")

TE_rookie <- player_stats %>% filter(pos=="TE" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy, games, receptions:wopr)
r_te_epa_mod <- lm(TE_rookie$receiving_epa ~ TE_rookie$inflated_apy)
r_te.epop <- resid(r_te_epa_mod)
TE_rookie <- cbind(TE_rookie, r_te.epop) %>% group_by(team) %>% summarise(r_te.epop = sum(r_te.epop))
ggplot(TE_rookie, aes(x=reorder(team, -r_te.epop), y=r_te.epop)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie Tight End Expected Points Over Projected", x="Team",
       y="Expected Points Over Projected (EPOP)")

ED_rookie <- player_stats %>% filter(pos=="ED" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_tackles_for_loss + def_sacks + def_qb_hits - (def_penalty_yards/10))
r_ed_impact_mod <- lm(ED_rookie$impact ~ ED_rookie$inflated_apy)
r_ed.ipoe <- resid(r_ed_impact_mod)
ED_rookie <- cbind(ED_rookie, r_ed.ipoe) %>% group_by(team) %>% summarise(r_ed.ipoe = sum(r_ed.ipoe))
ggplot(ED_rookie, aes(x=reorder(team, -r_ed.ipoe), y=r_ed.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie EDGE Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

IDL_rookie <- player_stats %>% filter(pos=="IDL" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(def_stops = def_tackles_for_loss + def_sacks + def_qb_hits - (def_penalty_yards/10))
r_idl_stops_mod <- lm(IDL_rookie$def_stops ~ IDL_rookie$inflated_apy)
r_idl.ipoe <- resid(r_idl_stops_mod)
IDL_rookie <- cbind(IDL_rookie, r_idl.ipoe) %>% group_by(team) %>% summarise(r_idl.ipoe = sum(r_idl.ipoe))
ggplot(IDL_rookie, aes(x=reorder(team, -r_idl.ipoe), y=r_idl.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie Interior Defensive Line Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

LB_rookie <- player_stats %>% filter(pos=="LB" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_tackles_for_loss + def_sacks + def_qb_hits + def_pass_defended - (def_penalty_yards/10))
r_lb_impact_mod <- lm(LB_rookie$impact ~ LB_rookie$inflated_apy)
r_lb.ipoe <- resid(r_lb_impact_mod)
LB_rookie <- cbind(LB_rookie, r_lb.ipoe) %>% group_by(team) %>% summarise(r_lb.ipoe = sum(r_lb.ipoe))
ggplot(LB_rookie, aes(x=reorder(team, -r_lb.ipoe), y=r_lb.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie Linebacker Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

CB_rookie <- player_stats %>% filter(pos=="CB" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_pass_defended + def_interceptions - (def_penalty_yards/10))
r_cb_impact_mod <- lm(CB_rookie$impact ~ CB_rookie$inflated_apy)
r_cb.ipoe <- resid(r_cb_impact_mod)
CB_rookie <- cbind(CB_rookie, r_cb.ipoe) %>% group_by(team) %>% summarise(r_cb.ipoe = sum(r_cb.ipoe))
ggplot(CB_rookie, aes(x=reorder(team, -r_cb.ipoe), y=r_cb.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie Cornerback Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

S_rookie <- player_stats %>% filter(pos=="S" & experience=="Rookie") %>%
  select(name:expiration, inflated_value, inflated_apy, games, def_tackles:def_penalty_yards) %>%
  mutate(impact = def_pass_defended + def_interceptions + (def_tackles_solo/10) - (def_penalty_yards/10))
r_s_impact_mod <- lm(S_rookie$impact ~ S_rookie$inflated_apy)
r_s.ipoe <- resid(r_s_impact_mod)
S_rookie <- cbind(S_rookie, r_s.ipoe) %>% group_by(team) %>% summarise(r_s.ipoe = sum(r_s.ipoe))
ggplot(S_rookie, aes(x=reorder(team, -r_s.ipoe), y=r_s.ipoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Rookie Safety Impact Plays Over Expected", x="Team",
       y="Impact Plays Over Expected (IPOE)")

vet_team_ranks <- QB %>% full_join(RB, by="team", multiple="all") %>%
  full_join(WR, by="team", multiple="all") %>% full_join(TE, by="team", multiple="all") %>%
  full_join(ED, by="team", multiple="all") %>% full_join(IDL, by="team", multiple="all") %>%
  full_join(LB, by="team", multiple="all") %>% full_join(CB, by="team", multiple="all") %>%
  full_join(S, by="team", multiple="all")

rookie_team_ranks <- QB_rookie %>% full_join(RB_rookie, by="team", multiple="all") %>%
  full_join(WR_rookie, by="team", multiple="all") %>%
  full_join(TE_rookie, by="team", multiple="all") %>%
  full_join(ED_rookie, by="team", multiple="all") %>%
  full_join(IDL_rookie, by="team", multiple="all") %>%
  full_join(LB_rookie, by="team", multiple="all") %>% 
  full_join(CB_rookie, by="team", multiple="all") %>%
  full_join(S_rookie, by="team", multiple="all")

simple_vet_ranks <- vet_team_ranks %>% group_by(team) %>% mutate(tpoe = sum(qb.epop:s.ipoe))
ggplot(simple_vet_ranks, aes(x=reorder(team, -tpoe), y=tpoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Team Production Over Expected: Veteran Contracts",
       subtitle="No Positional Value Adjustment", x="Team", 
       y="Team Production Over Expected (TPOE)")

simple_rookie_ranks <- rookie_team_ranks %>% group_by(team) %>% mutate(tpoe = sum(r_qb.epop:r_s.ipoe))
ggplot(simple_rookie_ranks, aes(x=reorder(team, -tpoe), y=tpoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Team Production Over Expected: Rookie Contracts",
       subtitle="No Positional Value Adjustment", x="Team",
       y="Team Production Over Expected (TPOE)")

value_vet_ranks <- vet_team_ranks %>% group_by(team) %>%
  mutate(tpoe = qb.epop*2.94 + rb.epop*0.89 + wr.epop*1.46 + te.epop*0.85 + ed.ipoe*1.63 +
           idl.ipoe*1.37 + lb.ipoe*1.13 + cb.ipoe*1.23 + s.ipoe*1.04)
ggplot(value_vet_ranks, aes(x=reorder(team, -tpoe), y=tpoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Team Production Over Expected: Veteran Contracts",
       subtitle="Adjusted For Positional Value", x="Team",
       y="Team Production Over Expected (TPOE)")

value_rookie_ranks <- rookie_team_ranks %>% group_by(team) %>%
  mutate(tpoe = r_qb.epop*2.94 + r_rb.epop*0.89 + r_wr.epop*1.46 + r_te.epop*0.85 +
           r_ed.ipoe*1.63 + r_idl.ipoe*1.37 + r_lb.ipoe*1.13 + r_cb.ipoe*1.23 + r_s.ipoe*1.04)
ggplot(value_rookie_ranks, aes(x=reorder(team, -tpoe), y=tpoe)) +
  geom_col(aes(color=team, fill=team), width=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
  scale_color_nfl(type="primary") +
  scale_fill_nfl(alpha=0.4) + thm + theme(plot.title = element_text(face = "bold"),
                                          plot.title.position = "plot",
                                          axis.text.x = element_blank()) +
  labs(title="Team Production Over Expected: Rookie Contracts",
       subtitle="Adjusted For Positional Value", x="Team",
       y="Team Production Over Expected (TPOE)")
