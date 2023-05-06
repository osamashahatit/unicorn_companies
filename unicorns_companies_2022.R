# Inserting Libreries

library(tidyverse)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(plotly)

# Importing df 

unicorn_companies <- 
  as_tibble(
    read_csv("unicorn_companies_2022.csv"))

# Cleaning col names to snake_case

unicorn_companies <- 
  unicorn_companies %>% 
  rename_with(~ tolower(gsub(' ', '_', .x, fixed = TRUE))) %>% 
  rename('investors' = 'select_inverstors', 'valuation' ='valuation_($b)')

# Check for duplicates based on 'company' col

sum(duplicated(unicorn_companies$company))

unicorn_companies %>% 
  filter(duplicated(company))

unicorn_companies <- distinct(unicorn_companies, company, .keep_all = TRUE)

# Remove outliers in 'founded_year' col

unicorn_companies <- 
  unicorn_companies %>% 
  filter(founded_year >= 1990)

unicorn_companies %>%
  distinct(founded_year, .keep_all = TRUE) %>%
  arrange(founded_year) %>%
  na.omit() %>%
  pull(founded_year)

# Clean 'valuation' col by removing $ and changing data type

unicorn_companies$valuation <- 
  gsub('\\$', '', unicorn_companies$valuation)

unicorn_companies$valuation <- 
  as.numeric(unicorn_companies$valuation)

# Clean 'date_joined' col data type

unicorn_companies$date_joined <- 
  as.Date(
    unicorn_companies$date_joined, '%m/%d/%Y')

# Clean 'industry' col categorical values

unicorn_companies <- unicorn_companies %>% 
  mutate(
    industry = if_else(
      industry %in% c(
        'Vision Plus Capital, GSR Ventures, ZhenFund',
        'Vertex Ventures SE Asia, Global Founders Capital, Visa Ventures',
        'Tiger Global Management, Tiger Brokers, DCM Ventures',
        'Temasek, Guggenheim Investments, Qatar Investment Authority',
        'SingTel Innov8, Alpha JWC Ventures, Golden Gate Ventures',
        'Sequoia Capital, Thoma Bravo, Softbank',
        'Sequoia Capital China, Shunwei Capital Partners, Qualgro',
        'Sequoia Capital China, ING, Alibaba Entrepreneurs Fund',
        'Mundi Ventures, Doqling Capital Partners, Activant Capital',
        'Kuang-Chi',
        'Jungle Ventures, Accel, Venture Highway',
        'Hopu Investment Management, Boyu Capital, DC Thomson Ventures',
        'B Capital Group, Monk\'s Hill Ventures, Dynamic Parcel Distribution',
        'Andreessen Horowitz, DST Global, IDG Capital',
        '500 Global, Rakuten Ventures, Golden Gate Ventures',
        'Dragonfly Captial, Qiming Venture Partners, DST Global'
      ), 
      'Ventures and Funds', 
      industry
    ),
    industry = recode(
      industry, 
      'Finttech' = 'Fintech', 
      'Artificial intelligence' = 'Artificial Intelligence'
    )
  )

# Clean 'investors' col byeplacing 'None' with NA

nrow(unicorn_companies[unicorn_companies$investors == 'None', ])

unicorn_companies$investors <- gsub('None', NA, unicorn_companies$investors)

# Clean 'founded_year' and drop all rows with 'founded_year' is None

  # Replacing 'None' with 'NA'

unicorn_companies$founded_year <- gsub('None', NA, unicorn_companies$founded_year)

  # Data type to date

unicorn_companies$founded_year <- 
  as.Date(
    format(
      paste0('01/01/', unicorn_companies$founded_year)), 
    '%m/%d/%Y')

# Clean 'total_raised' col

  # Extract the last character of the 'total_raised' col into a new col 'total_raised_ext'

unicorn_companies$total_raised_ext <- 
  substr(
    unicorn_companies$total_raised, 
    nchar(unicorn_companies$total_raised), 
    nchar(unicorn_companies$total_raised))

  # Remove unwanted char from the "total_raised" col

unicorn_companies$total_raised <- 
  gsub('\\$', '', unicorn_companies$total_raised)
unicorn_companies$total_raised <- 
  gsub('B', '', unicorn_companies$total_raised)
unicorn_companies$total_raised <- 
  gsub('M', '', unicorn_companies$total_raised)
unicorn_companies$total_raised <- 
  gsub('None', 'NA', unicorn_companies$total_raised)
unicorn_companies$total_raised <- 
  gsub('K', '', unicorn_companies$total_raised)

  # Convert the 'total_raised' col to numeric data type

unicorn_companies$total_raised <- 
  as.numeric(unicorn_companies$total_raised)

  # Multiply the 'total_raised' col by the factor in the 'total_raised_ext' col

for (i in 1:nrow(unicorn_companies)) {
  if (unicorn_companies$total_raised_ext[i] == 'B') {
    unicorn_companies$total_raised[i] <- unicorn_companies$total_raised[i] * 1000000000
  } else if (unicorn_companies$total_raised_ext[i] == 'M') {
    unicorn_companies$total_raised[i] <- unicorn_companies$total_raised[i] * 1000000
  } else if (unicorn_companies$total_raised_ext[i] == 'K') {
    unicorn_companies$total_raised[i] <- unicorn_companies$total_raised[i] * 1000
  }
}

remove(i)

  # Drop the 'total_raised_ext' col from the data frame

unicorn_companies$total_raised_ext <- NULL

# Check 'financial_stage', 'investors_count', 'deal_terms' and 'portfolio_exits' for missing values

nrow(unicorn_companies[unicorn_companies$financial_stage == 'None', ])

nrow(unicorn_companies[unicorn_companies$investors_count == 'None', ])

nrow(unicorn_companies[unicorn_companies$deal_terms == 'None', ])

nrow(unicorn_companies[unicorn_companies$portfolio_exits == 'None', ])

# Drop 'financial_stage' and 'portfolio_exits' for high count of missing values

unicorn_companies$financial_stage <- NULL

unicorn_companies$portfolio_exits <- NULL

# Replacing 'None' with 'NA' in 'investors_count' and 'deal_terms'

unicorn_companies$investors_count <- gsub('None', NA, unicorn_companies$investors_count)

unicorn_companies$deal_terms <- gsub('None', NA, unicorn_companies$deal_terms)

# Clean the data type for other cols

unicorn_companies$country <- 
  as.factor(unicorn_companies$country)

unicorn_companies$city <- 
  as.factor(unicorn_companies$city)

unicorn_companies$industry <- 
  as.factor(unicorn_companies$industry)

unicorn_companies$investors_count <- 
  as.integer(unicorn_companies$investors_count)

unicorn_companies$deal_terms <- 
  as.integer(unicorn_companies$deal_terms)

# Total Valuation of Unicorns in Top 10 Countries ($B)

unicorn_companies %>%
  group_by(country) %>%
  summarize(valuation_sum = sum(valuation)) %>%
  arrange(desc(valuation_sum)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, -valuation_sum),
             y = valuation_sum,
             fill = valuation_sum)) +
  geom_bar(aes(x = reorder(country, -valuation_sum),
               y = valuation_sum,
               fill = valuation_sum),
           stat = "identity") +
  scale_fill_gradient(low = 'cadetblue2',
                      high = 'dodgerblue4') +
  labs(x = 'Country',
       y = 'Total valuation of unicorns ($B)',
       title = 'Valuation of unicorns in top 10 countries ($B)') +
  geom_text(aes(label = paste0('$ ', round(valuation_sum), ' B')),
            position = position_dodge(width=0.9),
            vjust=-0.5,
            size = 3) +
  theme_grey() +
  theme(plot.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, size = 9),
        legend.position = 'none',
        panel.background = element_rect(fill = 'grey90'))

# Unicorns world map

map_data <- map_data("world") %>%
  mutate(region = gsub('USA', 'United States', region)) %>%
  mutate(region = gsub('UK', 'United Kingdom', region)) %>%
  left_join(
    unicorn_companies %>%
      group_by(country) %>%
      tally() %>% 
      arrange(desc(n)),
    by = c("region" = "country"))

ggplotly(
  ggplot(map_data, aes(x = long, y = lat, group = group)) + 
    geom_polygon(
      aes(
        fill = cut(n, c(0, 10, 50, 100, 500, Inf), 
                   labels = c("1-24", "25-49", "50-99", "100-499", "500+")), 
        text = paste(
          "<b>", region, "</b><br>", 
          "Unicorns = ", n, "<br>"
        )
      ),
      color = 'white'
    ) + 
    scale_fill_manual(
      values = c(
        "1-24" = '#ffa600',
        "25-49" = '#ff6361',
        "50-99" = '#bc5090',
        "100-499" = '#58508d',
        "500+" = '#003f5c'
      ),
      na.value = "grey90"
    ) +
    theme_void() +
    labs(x = 'Industry',
         y = 'Total valuation ($B)', 
         title = 'Unicorns world map by number of companies',
         fill = "Number of Unicorns") +
    theme(
      plot.title = element_text(face = "bold"), 
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ),
  tooltip = 'text'
) 

# Valuation by Industry (%)

industry_n <- 
  unicorn_companies %>%
  group_by(industry) %>% 
  summarise(valuation_industry = sum(valuation))

industry_n <- 
  industry_n %>% 
  mutate(
    percentage_valuation = round(
      industry_n$valuation_industry / sum(industry_n$valuation_industry)*100)
  )

industry_n$industry <- 
  reorder(industry_n$industry, industry_n$valuation_industry)

industry_n %>% 
  ggplot(
    aes(
      x="", 
      y = valuation_industry, 
      fill= industry)
  ) +
  geom_bar(
    width = 1, 
    stat="identity", 
    color = 'white'
  ) +
  geom_text(
    aes(
      x= 1.4, 
      label = paste0(percentage_valuation, ' %')
    ), 
    position = position_stack(vjust = 0.5), 
    size = 3
  ) +
  theme_classic() +
  coord_polar(
    "y", 
    start=0
  ) +
  scale_fill_discrete(
    limits=rev(levels(factor(industry_n$industry))), 
    name = 'Industry'
  ) + 
  theme(
    plot.title = element_text(face = "bold"),
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank()
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    fill = NULL, 
    title = 'Unicorns market share by industry'
  )

# Valuation by industry ($)

unicorn_companies %>%
  group_by(industry) %>%
  summarize(valuation_sum = sum(valuation)) %>%
  arrange(desc(valuation_sum)) %>%
  ggplot(
    aes(
      x = fct_reorder(industry, valuation_sum), 
      y = valuation_sum, 
      fill = valuation_sum
    )
  ) +
  geom_bar(
    stat = "identity"
  ) + 
  coord_flip() +
  scale_fill_gradient(
    low = 'cadetblue2', 
    high = 'dodgerblue4'
  ) +
  labs(
    x = 'Industry',
    y = 'Total valuation ($B)', 
    title = 'Unicorns market valuation by industry'
  ) + 
  geom_text(
    aes(
      label = paste0('$', round(valuation_sum), 'B')
    ), 
    vjust = 0.3, 
    hjust = -0.2, 
    size = 3
  ) + 
  theme_gray() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = 'none'
  )

# Correlation between total raised and valuation

  ggplotly(unicorn_companies %>% 
             ggplot(
               aes(
                 x = valuation,
                 y = total_raised / 1000000000, 
                 color = total_raised / 1000000000, 
                 size = valuation, 
                 label = company,
                 alpha = 0.7
               )
             ) + 
             geom_point(
               aes(
                 text = paste(
                   "<b>", company, "</b><br><br>", 
                   "Industry:", industry, "<br>", 
                   "Total raised = $", scales::comma(round(total_raised / 1000000000, 2)), "B<br>", 
                   "Valuation = $", scales::comma(round(valuation, 1)), "B"
                 )
               )
               ,alpha = 0.7
             ) + 
             scale_color_gradient(
               low = 'cadetblue2', 
               high = 'dodgerblue4'
             ) +
             scale_size_binned(
               range = c(0.1, 5)
             ) + 
             labs(
               title = 'Correlation between total raised and valuation', 
               color = 'Total raised ($B)',
               x = 'Total raised ($B)', 
               y = 'Valuation ($B)'
             ) + 
             theme_gray() +
             theme(plot.title = element_text(face = "bold")), 
           tooltip = 'text')

# Relationship between founded year and number of Unicorns

founded_joined <- 
  as_tibble(
    data.frame(
      date_joined = as.integer(format(as.Date(unicorn_companies$date_joined), "%Y")), 
      founded_year = as.integer(format(as.Date(unicorn_companies$founded_year), "%Y"))))

founded_joined$unicorn_time <- 
  founded_joined$date_joined - founded_joined$founded_year 

founded_n <- founded_joined %>% 
  group_by(founded_year) %>% 
  filter(founded_year >= 1985) %>% 
  tally()

joined_n <-  founded_joined %>% 
  group_by(date_joined) %>% 
  tally()

ggplotly(
  ggplot() + 
    geom_area(
      data = founded_n, 
      aes(
        x = founded_year, 
        y = n), 
      color = 'cadetblue3', 
      fill = 'lightcoral', 
      alpha = 0.6
      ) + 
    geom_point(
      data = founded_n, 
      aes(
        x = founded_year, 
        y = n, 
        text = paste(
          "<b>Founded year: ",founded_year, "</b><br>", 
          "Number:", n, "<br>")), 
      color = 'dodgerblue4', 
      size = 0.9) + 
    geom_area(
      data = joined_n, 
      aes(
        x = date_joined, 
        y = n), 
      color = 'firebrick', 
      fill = 'dodgerblue4', 
      alpha = 0.3
      ) + 
    geom_point(
      data = joined_n, 
      aes(
        x = date_joined, 
        y = n, 
        text = paste(
          "<b>Year joined: ",date_joined, "</b><br>", 
          "Number:", n, "<br>")), 
      color = 'firebrick4', 
      size = 1
      ) + 
    scale_x_continuous(
      breaks = seq(1900, 2030, by = 10)) + 
    labs(title = 'Companies by year founded vs year joined unicorn club', 
         x = 'Years', 
         y = 'Number of unicorns') + 
    theme_grey() + 
    theme(plot.title = element_text(face = "bold")), 
  tooltip = 'text')

# Years took to become a Unicorn

founded_joined <- founded_joined %>% 
  filter(unicorn_time > 0 & unicorn_time < 25)

ggplot(founded_joined, aes(x = unicorn_time, fill = after_stat(count))) + 
  geom_histogram(binwidth = 1, color = 'white') + 
  scale_x_continuous(breaks = seq(0, 40, by = 2)) +
  scale_y_continuous(breaks = seq(0, 200, by = 10)) +
  scale_fill_gradient(low = "cadetblue2", high = "dodgerblue4", name = '') +
  labs(title = 'Time to reach unicorn status',
       x = 'Number of years',
       y = 'Number of unicorns') +
  theme_grey() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = 'None')

# Another way to create world map

companies_geo <- 
  left_join(
    unicorn_companies %>%
      group_by(country) %>%
      tally() %>%
      arrange(desc(n)) %>%
      slice_head(n = 10),
    as.character(unique(unicorn_companies$country)) %>% geocode_OSM(.),
    by = c('country' = 'query')
  )

ggplot() +
  geom_polygon(
    map_data('world'),
    mapping = aes(x = long, y = lat, group=group),
    color = 'grey55',
    fill = 'ghostwhite'
  ) +
  geom_point(
    data = companies_geo,
    mapping = aes(x = lon, y = lat, size = n, color = n),
    inherit.aes = FALSE
  ) +
  scale_size_binned(
    range = c(1,10),
    nice.breaks = 'TRUE',
    label = scales::comma
  ) +
  scale_color_gradient(
    low = 'skyblue',
    high = 'navyblue'
  ) +
  guides(
    color = guide_legend(),
    size = guide_legend()
  ) +
  labs(
    title = 'Total number of unicorns in top 10 countries',
    size = '',
    color = ''
  ) +
  geom_text(
    data = companies_geo,
    label = paste0(companies_geo$country, ":", as.character(format(companies_geo$n, big.mark=","))),
    mapping = aes(x = lon, y = lat),
    hjust = 0.5,
    vjust = -1.5,
    fontface = 'bold',
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_void()


