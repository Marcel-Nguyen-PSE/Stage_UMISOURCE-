gdp_cap <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.KD",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    gdp_cap = NY.GDP.PCAP.KD
  )

df_africa <- df_africa %>%
  left_join(
    gdp_cap,
    by = c("country_code", "year")
  )

gdp_growth <- WDI(
  country = "all",
  indicator = "NY.GDP.MKTP.KD.ZG",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    gdp_growth = NY.GDP.MKTP.KD.ZG
  )

df_africa <- df_africa %>%
  left_join(
    gdp_growth,
    by = c("country_code", "year")
  )

internet <- WDI(
  country = "all",
  indicator = "IT.NET.USER.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    internet = IT.NET.USER.ZS
  )

df_africa <- df_africa %>%
  left_join(
    internet,
    by = c("country_code", "year")
  )

electricity <- WDI(
  country = "all",
  indicator = "EG.ELC.ACCS.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    electricity = EG.ELC.ACCS.ZS
  )

df_africa <- df_africa %>%
  left_join(
    electricity,
    by = c("country_code", "year")
  )

political_stab <- WDI(
  country = "all",
  indicator = "PV.EST",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    political_stab = PV.EST
  )

df_africa <- df_africa %>%
  left_join(
    political_stab,
    by = c("country_code", "year")
  )

tertiary_enrol <- WDI(
  country = "all",
  indicator = "SE.TER.ENRR",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    tertiary_enrol = SE.TER.ENRR
  )

df_africa <- df_africa %>%
  left_join(
    tertiary_enrol,
    by = c("country_code", "year")
  )

education_exp <- WDI(
  country = "all",
  indicator = "SE.XPD.TOTL.GD.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    education_exp = SE.XPD.TOTL.GD.ZS
  )

df_africa <- df_africa %>%
  left_join(
    education_exp,
    by = c("country_code", "year")
  )

rd_exp <- WDI(
  country = "all",
  indicator = "GB.XPD.RSDV.GD.ZS",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    rd_exp = GB.XPD.RSDV.GD.ZS
  )

df_africa <- df_africa %>%
  left_join(
    rd_exp,
    by = c("country_code", "year")
  )

researchers <- WDI(
  country = "all",
  indicator = "SP.POP.SCIE.RD.P6",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    researchers = SP.POP.SCIE.RD.P6
  )

df_africa <- df_africa %>%
  left_join(
    researchers,
    by = c("country_code", "year")
  )

patents <- WDI(
  country = "all",
  indicator = "IP.PAT.RESD",
  start = min(df_africa$year, na.rm = TRUE),
  end = max(df_africa$year, na.rm = TRUE),
  extra = TRUE
) %>%
  transmute(
    country_code = iso3c,
    year = as.numeric(year),
    patents = IP.PAT.RESD
  )

df_africa <- df_africa %>%
  left_join(
    patents,
    by = c("country_code", "year")
  )


