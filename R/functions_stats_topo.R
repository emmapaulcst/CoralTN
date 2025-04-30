stats_realm <- bga %>% 
  ungroup() %>% 
  select(Realm, S:zQn) %>% 
  group_by(Realm) %>% 
  summarise(min_S = round(min(S), 2), max_S = round(max(S), 2), mean_S = round(mean(S), 2), median_S = round(median(S), 2), 
            min_C = round(min(C), 2), max_C = round(max(C), 2), mean_C = round(mean(C), 2), median_C = round(median(C), 2), 
            min_zN = round(min(zN), 2), max_zN = round(max(zN), 2), mean_zN = round(mean(zN), 2), median_zN = round(median(zN), 2), 
            min_zQ = round(min(zQn), 2), max_zQ = round(max(zQn), 2), mean_zQ = round(mean(zQn), 2), median_zQ = round(median(zQn), 2))
