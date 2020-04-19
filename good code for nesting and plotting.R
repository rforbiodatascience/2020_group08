by_cyl <- mtcars %>%  
  group_by(cyl, gear) %>%  
  nest() %>%  
  rename(cyl_data = data)
by_cyl

by_gear <- mtcars %>% 
  group_by(gear) %>%  
  nest() %>%  
  rename(gear_data = data)

by_gear
         
mtcars_nest <- left_join(by_cyl, by_gear, by = "gear")
mtcars_nest

mtcars_nest <- mtcars_nest %>% 
  mutate(
    map(cyl_data, ~ ggplot(., aes(x = wt, y = mpg)) + 
          geom_point() +
          geom_smooth(se = TRUE, color = 'blue')
    )
  ) %>% 
  rename(plot_cyl = `map(...)`)


mtcars_nest 

mtcars_nest <- mtcars_nest %>% 
  mutate(
    map(gear_data, ~ ggplot(., aes(x = wt, y = mpg)) + 
          geom_point() +
          geom_smooth(se = TRUE, color = 'red')
    )
  ) %>% 
  rename(plot_gear = `map(...)`)


mtcars_nest <- mtcars_nest %>% 
  mutate( 
    plot_cyl_gear = map2(cyl_data, gear_data,
                         ~ ggplot(.x, aes(x = wt, y = mpg)) +
                           geom_smooth(se = TRUE, color = 'blue') +
                           geom_smooth(data = .y, se = TRUE, color = 'red')
    ) )

mtcars_nest$plot_cyl_gear[1]
?rowwise


my_graph <- function(data, groupvar){
  
  data %>%
    group_by(vs, {{groupvar}}, am) %>%
    summarise(n=n()) %>%
    mutate(sum = sum(n)) %>%
    rowwise %>%
    mutate(xx = list(broom::tidy(prop.test(n, sum, conf.level=0.95)))) %>%
    unnest(xx) %>%
    ggplot() +
    geom_bar(aes(x=factor(vs), y=estimate, fill={{groupvar}}), stat = "identity") +
    geom_errorbar(aes(x=factor(vs), ymin=conf.low, ymax = conf.high), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(vars({{groupvar}}))+
    theme(legend.position = "none") 
  
}

my_graph(data = mtcars, groupvar = am)


mtcars %>%
  group_by(vs, {{am}}, am) %>%
  summarise(n=n()) 

par()
?par





