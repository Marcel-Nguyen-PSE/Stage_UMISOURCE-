library(tidyverse)
library(readxl)
library(rio)
library(xtable)
library(here)
library(gtsummary)
library(glue)
library(scales)
library(patchwork)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(car)
library(haven)
library(fixest) 
library(sf)
library(did)
library(rdrobust)
library(TwoWayFEWeights)
library(Synth)
library(fredr)
library(plm)
library(openalexR)
library(purrr)
library(np)

options(
  openalex.mailto = "Marcel.Nguyen@ens.psl.eu"
)

qs_2014 <- read_csv('Data/qs_rankings_2014_complete.csv')
qs_2015 <- read_csv('Data/qs_rankings_2015_complete.csv')
qs_2016 <- read_csv('Data/qs_rankings_2016_complete.csv')
qs_2017 <- read_csv('Data/qs_rankings_2017.csv')
qs_2018 <- read_csv('Data/qs_rankings_2018_complete.csv')
qs_2019 <- read_csv('Data/qs_rankings_2019.csv')
qs_2020 <- read_csv('Data/qs_rankings_2020.csv') %>% mutate(year = 2020)
qs_2021 <- read_csv('Data/qs_rankings_2021.csv') %>% mutate(year = 2021)
qs_2022 <- read_csv('Data/qs_rankings_2022.csv') %>% mutate(year = 2022)
qs_2023 <- read_csv('Data/qs_rankings_2023.csv') %>% mutate(year = 2023)
qs_2024 <- read_csv('Data/qs_rankings_2024.csv') %>% mutate(year = 2024)
qs_2025 <- read_csv('Data/qs_rankings_2025.csv') %>% mutate(year = 2025)

qs_list <- list(
  qs_2014,
  qs_2015,
  qs_2016,
  qs_2017,
  qs_2018,
  qs_2019,
  qs_2020,
  qs_2021,
  qs_2022,
  qs_2023,
  qs_2024,
  qs_2025
)

qs_list_clean <- lapply(qs_list, function(df) {
  df %>%
    # convert all columns to character first
    mutate(across(everything(), as.character))
})

qs_panel <- bind_rows(qs_list_clean) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  arrange(university_name, year) %>%
  mutate(
    across(
      where(is.character),
      ~ case_when(
        . %in% c("null", "NULL", "n/a", "N/A", "NA", "", " ") ~ NA_character_,
        TRUE ~ .
      )
    )
  )

tw_panel <- read_csv('Data/THE World University Rankings 2016-2026.csv') %>%
  rename(name = 'Name',
        rank = 'Rank',
        scores_overall = 'Overall Score',
        scores_teaching = 'Teaching',
        scores_research = 'Research Quality',
        location = 'Country') 

tw_panel_2011 <- read_csv('Data/2011_2015_rankings.csv') %>%
  select(-rank_order, -...22, -subjects_offered, -closed, -aliases, -unaccredited, -scores_international_outlook_rank, -scores_research_rank, -scores_citations_rank, -scores_overall_rank, -scores_teaching_rank, -scores_industry_income_rank) 

qs_panel_citations <- qs_panel %>%
  select(year, `Citations per Faculty`, university_name) %>%
  rename(name = 'university_name')

tw_panel_merge <- bind_rows(
  tw_panel_2011 %>% mutate(across(everything(), as.character)),
  tw_panel %>% mutate(across(everything(), as.character))
) %>%
  mutate(
    year = as.integer(Year)
  ) %>%
  distinct(name, year, .keep_all = TRUE) %>%
  select(-Year) %>%
  arrange(name, year)

tw_panel_merge <- tw_panel_merge %>%
  left_join(
    qs_panel_citations %>%
      select(
        name,
        year,
        `Citations per Faculty`
      ) %>%
      distinct(name, year, .keep_all = TRUE),
    by = c("name", "year")
  ) %>%
  rename(
    citations_per_faculty = `Citations per Faculty`
  ) %>%
  mutate(
    citations_per_faculty =
      as.numeric(citations_per_faculty)
  )

university_names <- tw_panel_merge %>%
  distinct(name) %>%
  pull(name)

manual_matches <- tribble(

  ~original_name, ~search_name,

  "University of Fortaleza (UNIFOR)", "Universidade de Fortaleza",

  "University of Georgia (USA)", "University of Georgia",

  "University of Hai'l", "University of Ha'il",

  "University of Health Sciences, Turkey", "University of Health Sciences",

  "University of La Rochelle", "La Rochelle University",

  "University of Lampung", "Universitas Lampung",

  "University of Las Américas (UDLA)", "Universidad de Las Americas",

  "University of Macerata (UNIMC)", "University of Macerata",

  "University of Manouba", "University of Manouba",

  "University of Marrakech Cadi Ayyad", "Cadi Ayyad University",

  "University of Mascara", "University of Mascara",

  "University of Medellín", "University of Medellin",

  "University of Medicine and Dentistry of New Jersey", "Rutgers Biomedical and Health Sciences",

  "University of Milan-Bicocca", "University of Milano-Bicocca",

  "University of New Brunswick UNB", "University of New Brunswick",

  "University of New Mexico (Main campus)", "University of New Mexico",

  "University of Nice Sophia Antipolis", "Universite Nice Sophia Antipolis",

  "University of Nigeria Nsukka", "University of Nigeria",

  "University of Northern British Columbia (UNBC)", "University of Northern British Columbia",

  "University of Passo Fundo (UPF)", "Universidade de Passo Fundo",

  "University of Pittsburgh-Pittsburgh campus", "University of Pittsburgh",

  "University of Pécs", "University of Pecs",

  "University of Rome III", "Roma Tre University",

  "University of Santiago, Chile (USACH)", "University of Santiago Chile",

  "University of Science and Technology of Oran Mohamed-Boudiaf", "University of Oran",

  "University of South Carolina-Columbia", "University of South Carolina",

  "University of South Florida (Tampa)", "University of South Florida",

  "University of Thi-Qar", "Thi Qar University",

  "University of Tunis El Manar", "Tunis El Manar University",

  "University of Virginia (Main campus)", "University of Virginia",

  "University of the Andes, Colombia", "Universidad de los Andes",

  "University of the Andes, Venezuela", "Universidad de Los Andes Venezuela",

  "University of the North, Colombia", "Universidad del Norte",

  "University of the Peloponnese", "University of Peloponnese",

  "Université Bourgogne Franche-Comté (UBFC)", "Universite Bourgogne Franche Comte",

  "Université Cheikh Anta Diop de Dakar (UCAD)", "Cheikh Anta Diop University",

  "Université Chouaïb Doukkali", "Chouaib Doukkali University",

  "Université Djillali Liabès de Sidi Bel-Abbès", "Djillali Liabes University",

  "Université Mohammed Premier", "Mohammed First University",

  "Université Mouloud Mammeri de Tizi-Ouzou", "Mouloud Mammeri University",

  "Université de Skikda", "University of Skikda",

  "V.I. Vernadsky Crimean Federal University (Vernadsky CFU)", "Vernadsky Crimean Federal University",

  "V.N. Karazin Kharkiv National University", "Karazin Kharkiv National University",

  "Vels Institute of Science, Technology and Advanced Studies (VISTAS)", "VELS University",

  "Veltech University", "Vel Tech University",

  "Vignan’s Foundation for Science, Technology and Research", "Vignan University",

  "Warsaw University of Life Sciences – SGGW", "Warsaw University of Life Sciences",

  "Xi’an Jiaotong-Liverpool University", "Xian Jiaotong Liverpool University",

  "Xuzhou Medical University", "Xuzhou Medical University",

  "Yonsei University (Seoul campus)", "Yonsei University",

  "Yuri Gagarin State Technical University of Saratov (SSTU)", "Yuri Gagarin State Technical University",

  "École Nationale des Ponts et Chaussées - Institut Polytechnique de Paris", "Ecole des Ponts ParisTech",

  "École Nationale des Travaux Publics de l'État (ENTPE)", "ENTPE",

  "Adana Alparslan Türkeş Science and Technology University", "Adana Alparslan Turkes Science and Technology University",

  "Ahvaz Jundishapur University of Medical Sciences (AJUMS)", "Ahvaz Jundishapur University of Medical Sciences",

  "Allameh Tabataba ’i University", "Allameh Tabataba'i University",

  "Amity University - Kolkata", "Amity University Kolkata",

  "Amity University, Noida", "Amity University",
   "Anglia Ruskin University (ARU)", "Anglia Ruskin University",

  "Antonio Nariño University", "Antonio Narino University",

  "Arizona State University (Tempe)", "Arizona State University",

  "Asia University, Taiwan", "Asia University",

  "Autonomous University of Guerrero (UAGro)", "Universidad Autonoma de Guerrero",

  "Autonomous University of San Luis Potosi", "Autonomous University of San Luis Potosi",

  "Aydın Adnan Menderes University", "Aydin Adnan Menderes University",

  "Azerbaijan Technical University (AzTU)", "Azerbaijan Technical University",

  "B. S. Abdur Rahman Crescent Institute of Science and Technology", "B S Abdur Rahman Crescent Institute of Science and Technology",

  "Babeş-Bolyai University", "Babes Bolyai University",

  "Balochistan University of Information Technology, Engineering and Management Sciences (BUITEMS)", "Balochistan University of Information Technology Engineering and Management Sciences",

  "Bandung Institute of Technology (ITB)", "Bandung Institute of Technology",

  "Bangladesh Agricultural University (BAU)", "Bangladesh Agricultural University",

  "Belgorod State National Research University", "Belgorod National Research University",

  "Bezmiâlem Vakif University", "Bezmialem Vakif University",

  "Blaise Pascal University", "Blaise Pascal University",

  "Blida 1 University", "Blida 1 University",

  "Botswana International University of Science and Technology (BIUST)", "Botswana International University of Science and Technology",

  "CEU Universities", "Central European University",

  "Campus Bio-Medico University of Rome", "Campus Bio Medico University of Rome",

  "Cankaya University", "Cankaya University",

  "Centro Federal de Educação Tecnológica Celso Suckow da Fonseca (CEFET-RJ)", "CEFET RJ",

  "China Medical University, Taiwan", "China Medical University",

  "Christ University Bengaluru India", "Christ University",

  "Colorado State University, Fort Collins", "Colorado State University",

  "Comenius University in Bratislava", "Comenius University Bratislava",

  "Czech University of Life Sciences Prague (CZU)", "Czech University of Life Sciences Prague",

  "Daegu Gyeongbuk Institute of Science and Technology (DGIST)", "DGIST",

  "Daffodil International University (DIU)", "Daffodil International University",

  "Datta Meghe Institute of Higher Education & Research (Deemed to be University)", "Datta Meghe Institute of Higher Education and Research",

  "Delta State University, Abraka", "Delta State University Abraka",

  "Dr B.R. Ambedkar National Institute of Technology Jalandhar", "Dr BR Ambedkar National Institute of Technology Jalandhar",

  "Dr D. Y. Patil Vidyapeeth, Pune", "Dr D Y Patil Vidyapeeth",

  "ESPCI ParisTech", "ESPCI Paris",

  "Egypt-Japan University of Science and Technology (E-JUST)", "Egypt Japan University of Science and Technology",

  "Empress Catherine II Saint Petersburg Mining University", "Saint Petersburg Mining University",

  "European University of Madrid", "European University of Madrid",

  "Federal Institute of Education, Science and Technology of São Paulo", "Federal Institute of Sao Paulo",

  "Federal University of Agriculture, Abeokuta", "Federal University of Agriculture Abeokuta",

  "Federal University of Paraí­ba (UFPB)", "Federal University of Paraiba",

  "Federal University of Rio Grande do Norte (UFRN)", "Federal University of Rio Grande do Norte",

  "Federal University of Vales do Jequitinhonha e Mucuri", "Federal University of Jequitinhonha and Mucuri Valleys",

  "Federal University of the State of Rio de Janeiro (UNIRIO)", "Federal University of the State of Rio de Janeiro",

  "Ferhat Abbas Sétif University 1", "Ferhat Abbas Setif University 1",

  "Firat University", "Firat University",

  "G.B. Pant University of Agriculture & Technology, Pantnagar", "GB Pant University of Agriculture and Technology",

  "GMR Institute of Technology", "GMR Institute of Technology",

  "George Emil Palade University of Medicine, Pharmacy, Science, and Technology of Targu Mures", "George Emil Palade University of Medicine Pharmacy Science and Technology",

  "Georgia Health Sciences University", "Augusta University",

  "Gheorghe Asachi Technical University of Iaşi", "Gheorghe Asachi Technical University of Iasi",
  "Gwangju Institute of Science and Technology (GIST)", "Gwangju Institute of Science and Technology",

  "HSE University", "HSE University",

  "Hamadan University of Medical Sciences (UMSHA)", "Hamadan University of Medical Sciences",

  "Hangzhou Dianzi University (HDU)", "Hangzhou Dianzi University",

  "Hatay Mustafa Kemal University", "Hatay Mustafa Kemal University",

  "Hazara University Mansehra", "Hazara University",

  "Humboldt University of Berlin", "Humboldt University Berlin",

  "I. Horbachevsky Ternopil National Medical University", "Ternopil National Medical University",

  "IFHE Hyderabad", "ICFAI Foundation for Higher Education",

  "Ibn Khaldoun University", "Ibn Khaldoun University",

  "Ibn Zohr University", "Ibn Zohr University",

  "Ilmenau University of Technology", "Technische Universitat Ilmenau",

  "Institute for Advanced Studies in Basic Sciences (IASBS)", "Institute for Advanced Studies in Basic Sciences",

  "Islamic Azad University, Najafabad Branch (IAUN)", "Islamic Azad University Najafabad Branch",

  "Islamic Science University of Malaysia (USIM)", "Islamic Science University of Malaysia",

  "Isra University (Jordan)", "Isra University",

  "Istanbul Okan University", "Istanbul Okan University",

  "Iuliu Haţieganu University of Medicine and Pharmacy Cluj-Napoca", "Iuliu Hatieganu University of Medicine and Pharmacy",

  "Jagannath University, Bangladesh", "Jagannath University",

  "Jamia Hamdard University", "Jamia Hamdard",

  "Jan Kochanowski University in Kielce", "Jan Kochanowski University",

  "Jawaharlal Nehru Technological University Anantapur (JNTUA)", "Jawaharlal Nehru Technological University Anantapur",

  "Jaypee Institute of Information Technology, Noida", "Jaypee Institute of Information Technology",

  "Jiangxi Normal University (JXNU)", "Jiangxi Normal University",

  "Jinan University (China)", "Jinan University",

  "K.N. Toosi University of Technology", "KN Toosi University of Technology",

  "KLE Academy of Higher Education and Research", "KLE Academy of Higher Education and Research",

  "KMITL – King Mongkut’s Institute of Technology Ladkrabang", "King Mongkut's Institute of Technology Ladkrabang",

  "Karamanoğlu Mehmet Bey University", "Karamanoglu Mehmetbey University",

  "Karnatak University Dharwad", "Karnatak University",

  "Kashan University of Medical Sciences and Health Services", "Kashan University of Medical Sciences",

  "Kazan National Research Technological University", "Kazan National Research Technological University",

  "Khalifa University of Science, Technology and Research", "Khalifa University",

  "Khulna University of Engineering and Technology (KUET)", "Khulna University of Engineering and Technology",

  "Kirikkale University", "Kirikkale University",

  "Korea Advanced Institute of Science and Technology (KAIST)", "KAIST",

  "Kyungpook National University (KNU)", "Kyungpook National University",

  "L.N. Gumilyov Eurasian National University", "L N Gumilyov Eurasian National University",

  "Lahore College for Women University (LCWU)", "Lahore College for Women University",

  "Larbi Tebessi University", "Larbi Tebessi University",

  "Lille 1 University – Science and Technology", "Lille 1 University",

  "Lille 2 University – Health and Law", "Lille 2 University",

  "Lincoln University (New Zealand)", "Lincoln University",

  "Lingnan University Hong Kong", "Lingnan University",

  "Lusophone University of Humanities and Technologies", "Lusophone University",

  "M S Ramaiah University of Applied Sciences (RUAS)", "MS Ramaiah University of Applied Sciences",

  "MNS University of Agriculture, Multan", "MNS University of Agriculture",

  "Maharishi Markandeshwar University (MMU)", "Maharishi Markandeshwar University",

  "Management & Science University (MSU)", "Management and Science University",

  "Maria Curie-Skłodowska University (UMCS)", "Maria Curie Sklodowska University",

  "Medical University of Bialystok", "Medical University of Bialystok",

  "Medical University of Innsbruck", "Medical University of Innsbruck",

  "Mehran University of Engineering and Technology, Jamshoro, Pakistan", "Mehran University of Engineering and Technology",

  "Mendeleev University of Chemical Technology", "Dmitry Mendeleev University of Chemical Technology",

  "Military Institute of Science and Technology (MIST)", "Military Institute of Science and Technology",

  "Mizzou - University of Missouri", "University of Missouri",

  "Mohammed V University of Rabat", "Mohammed V University",

  "Mohammed VI Polytechnic University", "Mohammed VI Polytechnic University",

  "Moscow Institute of Physics and Technology (MIPT)", "Moscow Institute of Physics and Technology",

  "Moscow Technical University of Communications and Informatics", "Moscow Technical University of Communications and Informatics",

  "Moulay Ismail University", "Moulay Ismail University",

  "Mugla Sitki Kocman University", "Mugla Sitki Kocman University",

  "M’Hamed Bougara University of Boumerdès", "Mhamed Bougara University of Boumerdes",

  "NOVA University of Lisbon", "NOVA University Lisbon",

  "Nanjing University of Science and Technology (NJUST)", "Nanjing University of Science and Technology",

  "Nanyang Technological University, Singapore", "Nanyang Technological University",

  "National Institute of Applied Sciences of Lyon (INSA Lyon)", "INSA Lyon",

  "National Institute of Applied Sciences, Strasbourg (INSA Strasbourg)", "INSA Strasbourg",

  "National Research Saratov State University", "Saratov State University",

  "National Research University of Electronic Technology (MIET)", "National Research University of Electronic Technology",

  "National Taiwan University (NTU)", "National Taiwan University",

  "National University of Comahue (UNCo)", "National University of Comahue",

  "National University of Distance Education (UNED)", "National University of Distance Education",

  "National University of Medical Sciences (NUMS)", "National University of Medical Sciences",

  "National University of Rosario (UNR)", "National University of Rosario",

  "National University of Science and Technology (MISiS)", "MISIS University",

  "National University of the South", "National University of the South",

  "National Veterinary School of Alfort (EnvA)", "National Veterinary School of Alfort",

  "National Yang Ming Chiao Tung University (NYCU Taiwan)", "National Yang Ming Chiao Tung University",

  "New Mexico State University (Main campus)", "New Mexico State University",

  "Nicolaus Copernicus University in Toruń", "Nicolaus Copernicus University",

  "Nitte (Deemed to be University)", "Nitte University",

  "Noakhali Science and Technology University (NSTU)", "Noakhali Science and Technology University",

  "Northeastern University, China", "Northeastern University China",

  "Northeastern University, US", "Northeastern University",

  "Northwest University (China)", "Northwest University",

  "O.P. Jindal Global University", "OP Jindal Global University",

  "October University for Modern Sciences and Arts (MSA)", "Modern Sciences and Arts University",

  "Odesa I.I. Mechnikov National University", "Odesa Mechnikov National University",

  "Ohio State University (Main campus)", "Ohio State University",

  "Ohio University (Main campus)", "Ohio University",

  "Ondokuz Mayis University", "Ondokuz Mayis University",

  "Otto von Guericke University of Magdeburg", "Otto von Guericke University Magdeburg",

  "Ovidius University of Constanța", "Ovidius University of Constanta",

  "Ozyegin University", "Ozyeegin University",

  "PMAS Arid Agriculture University Rawalpindi", "PMAS Arid Agriculture University",

  "Pace University-New York", "Pace University",

  "Paris-Sorbonne University – Paris 4", "Paris Sorbonne University",

  "Pavlov First Saint Petersburg State Medical University", "Pavlov University",

  "Pavol Jozef Šafárik University in Košice", "Pavol Jozef Safarik University",
  "Penn State (Main campus)", "Pennsylvania State University",

  "Platov South-Russian State Polytechnic University (NPI)", "Platov South Russian State Polytechnic University",

  "Pohang University of Science and Technology (POSTECH)", "POSTECH",

  "Pontifical Catholic University of Rio Grande do Sul (PUCRS)", "Pontifical Catholic University of Rio Grande do Sul",

  "Poznan University of Technology", "Poznan University of Technology",

  "Prince Sultan University (PSU)", "Prince Sultan University",

  "Qom University of Medical Sciences", "Qom University of Medical Sciences",

  "R V College of Engineering", "RV College of Engineering",

  "Regional Autonomous University of the Andes (UNIANDES)", "Regional Autonomous University of Los Andes",

  "Reshetnev Siberian State University of Science and Technology", "Reshetnev Siberian State University",

  "Russian University of Transport (MIIT)", "Russian University of Transport",

  "Rutgers University – New Brunswick", "Rutgers University New Brunswick",

  "Rutgers University–New Brunswick", "Rutgers University New Brunswick",

  "SRUC (Scotland’s Rural College)", "Scotlands Rural College",

  "Samara State Aerospace University", "Samara National Research University",

  "Sant’Anna School of Advanced Studies – Pisa", "SantAnna School of Advanced Studies",

  "Shanghai University of Finance and Economics (SUFE)", "Shanghai University of Finance and Economics",

  "Shivaji University, Kolhapur", "Shivaji University",

  "Soochow University, China", "Soochow University",

  "Soochow University, Taiwan", "Soochow University Taiwan",

  "South Valley University (Egypt)", "South Valley University",

  "Southern University of Science and Technology (SUSTech)", "Southern University of Science and Technology",

  "St Petersburg Electrotechnical University (LETI)", "Saint Petersburg Electrotechnical University",

  "State Islamic University of Sunan Gunung Djati Bandung", "Sunan Gunung Djati State Islamic University",

  "State University of Southwestern Bahia (UESB)", "State University of Southwestern Bahia",

  "Sungkyunkwan University (SKKU)", "Sungkyunkwan University",

  "Tashkent Institute of Irrigation and Agricultural Mechanisation", "Tashkent Institute of Irrigation and Agricultural Mechanization Engineers",

  "The British University in Egypt", "British University in Egypt",

  "The Catholic University of America", "Catholic University of America",

  "The Catholic University of Korea (CUK)", "Catholic University of Korea",

  "The Chinese University of Hong Kong", "Chinese University of Hong Kong",

  "The Education University of Hong Kong", "Education University of Hong Kong",

  "The Hashemite University", "Hashemite University",

  "The Hong Kong Polytechnic University", "Hong Kong Polytechnic University",

  "The Islamia University of Bahawalpur", "Islamia University of Bahawalpur",

  "The Jikei University School of Medicine", "Jikei University School of Medicine",

  "The John Paul II Catholic University of Lublin", "John Paul II Catholic University of Lublin",

  "The University of Aizu", "University of Aizu",

  "The University of Alabama", "University of Alabama",

  "The University of Chicago", "University of Chicago",

  "The University of Danang", "University of Da Nang",

  "The University of Electro-Communications", "University of Electro Communications",

  "The University of Haripur", "University of Haripur",

  "The University of Jordan", "University of Jordan",

  "The University of Tennessee-Knoxville", "University of Tennessee Knoxville",

  "The University of Tulsa", "University of Tulsa",

  "Tokyo Medical and Dental University (TMDU)", "Tokyo Medical and Dental University",

  "Tomsk State University of Control Systems and Radioelectronics", "Tomsk State University of Control Systems and Radioelectronics",

  "Toyohashi University of Technology (TUT)", "Toyohashi University of Technology",

  "UEES, Espiritu Santo University", "Universidad de Especialidades Espiritu Santo",

  "UEH University", "University of Economics Ho Chi Minh City",

  "UNIR – International University of La Rioja", "International University of La Rioja",

  "USAMV Cluj-Napoca", "University of Agricultural Sciences and Veterinary Medicine Cluj Napoca",

  "Ulsan National Institute of Science and Technology (UNIST)", "UNIST",

  "Unisinos University", "University of Vale do Rio dos Sinos",

  "Universidad Andrés Bello (UNAB)", "Universidad Andres Bello",

  "Universidad Autónoma de Nuevo León (UANL)", "Autonomous University of Nuevo Leon",

  "Universidad Autónoma del Estado de Mexico (UAEMex)", "Autonomous University of the State of Mexico",

  "Universidad Católica San Antonio de Murcia (UCAM)", "Catholic University of Murcia",

  "Universidad Industrial de Santander (UIS)", "Industrial University of Santander",

  "Universidad Panamericana (UP)", "Universidad Panamericana",

  "Universidad Pedagogica y Tecnologica de Colombia - UPTC", "Pedagogical and Technological University of Colombia",

  "Universidad Peruana de Ciencias Aplicadas (UPC)", "Peruvian University of Applied Sciences",

  "Universidad Pontificia Bolivariana (UPB)", "Pontifical Bolivarian University",

  "Universidade Estadual de Feira de Santana (UEFS)", "State University of Feira de Santana",

  "Universidade Estadual do Oeste do Paraná (Unioeste)", "Western Parana State University",

  "Universidade Federal Rural do Semi-Arido", "Federal Rural University of the Semi Arid Region",

  "Universidade Federal de Ciências da Saúde de Porto Alegre (UFCSPA)", "Federal University of Health Sciences of Porto Alegre",

  "Universidade Federal de Ouro Preto (UFOP)", "Federal University of Ouro Preto",

  "Universidade Federal do ABC (UFABC)", "Federal University of ABC",

  "Universidade Federal do Ceará (UFC)", "Federal University of Ceara",

  "Universidade Federal do Maranhão (UFMA)", "Federal University of Maranhao",

  "Universidade Federal do Paraná (UFPR)", "Federal University of Parana",

  "Universidade Tecnológica Federal do Paraná (UTFPR)", "Federal University of Technology Parana",

  "Universitas Muhammadiyah Surakarta", "Muhammadiyah University of Surakarta",

  "Universitat Autònoma de Barcelona (UAB)", "Autonomous University of Barcelona",

  "Universiti Malaysia Pahang Al-Sultan Abdullah (UMPSA)", "Universiti Malaysia Pahang Al Sultan Abdullah",

  "Universiti Malaysia Sabah (UMS)", "Universiti Malaysia Sabah",

  "Universiti Malaysia Sarawak (UNIMAS)", "Universiti Malaysia Sarawak",

  "Universiti Malaysia Terengganu (UMT)", "Universiti Malaysia Terengganu",

  "Universiti Tenaga Nasional (UNITEN)", "Universiti Tenaga Nasional",

  "Universiti Tun Hussein Onn Malaysia (UTHM)", "Universiti Tun Hussein Onn Malaysia",

  "University Cadi Ayyad of Marrakech", "Cadi Ayyad University",

  "University of Abou Bekr Belkaïd Tlemcen", "Abou Bekr Belkaid University of Tlemcen",

  "University of Cadiz", "University of Cadiz",

  "University of Camerino (Unicam)", "University of Camerino",

  "University of Cergy-Pontoise", "CY Cergy Paris Universite",

  "University of Cincinnati – Uptown", "University of Cincinnati",

  "University of Côte d’Azur", "Universite Cote dAzur",

  "University of Economics in Prague", "Prague University of Economics and Business"

)

university_names <- ifelse(
  university_names %in% manual_matches$original_name,
  manual_matches$search_name[match(university_names, manual_matches$original_name)],
  university_names
)

fetch_inst_id_safe <- function(u) {
  message("Fetching: ", u)
  inst <- tryCatch(
    oa_fetch(
      entity = "institutions",
      search = u,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  tibble(
    name = u,
    inst_id = if (
      is.null(inst) || nrow(inst) == 0
    ) NA_character_ else inst$id[1]
  )
}

if (file.exists("inst_ids_progress.rds")) {
  inst_ids <- readRDS("inst_ids_progress.rds")
} else {
  inst_ids <- tibble(
    name = university_names,
    inst_id = NA_character_
  )
}

remaining_names <- inst_ids %>%
  filter(is.na(inst_id)) %>%
  pull(name) 

for (u in remaining_names) {
  result <- fetch_inst_id_safe(u)
  inst_ids <- inst_ids %>%
    filter(name != u) %>%
    bind_rows(result)
  saveRDS(inst_ids, "inst_ids_progress.rds")

}

length(remaining_names)

tw_panel_merge <- tw_panel_merge %>%
  mutate(name = ifelse(
    name %in% manual_matches$original_name,
    manual_matches$search_name[match(name, manual_matches$original_name)],
    name
  )) %>%
  left_join(inst_ids %>% distinct(name, .keep_all = TRUE), by = "name")


fetch_pubs <- function(inst_id, year, retries = 3) {
  for (i in seq_len(retries)) {
    Sys.sleep(0.5)
    works <- tryCatch(
      oa_fetch(
        entity           = "works",
        institutions.id  = inst_id,
        publication_year = year,
        count_only       = TRUE,
        verbose          = FALSE
      ),
      error = function(e) NULL
    )
    if (!is.null(works)) break
    Sys.sleep(5 * i)
  }
  n <- if (is.null(works)) NA_integer_ else as.integer(works$count[1])
  tibble(inst_id = inst_id, year = year, n_publications = n)
}

# Resume from checkpoint if exists
pubs <- if (file.exists("pubs_progress.rds")) readRDS("pubs_progress.rds") else tibble(inst_id = character(), year = integer(), n_publications = integer())

# Build the full list of (inst_id, year) pairs to fetch
to_fetch <- tw_panel_merge %>%
  distinct(inst_id, year) %>%
  filter(!is.na(inst_id)) %>%
  anti_join(pubs, by = c("inst_id", "year"))  # skip already fetched

message(nrow(to_fetch), " requests remaining")

for (i in seq_len(nrow(to_fetch))) {
  result <- fetch_pubs(to_fetch$inst_id[i], to_fetch$year[i])
  pubs   <- bind_rows(pubs, result)
  saveRDS(pubs, "pubs_progress.rds")
}

tw_panel_merge <- tw_panel_merge %>%
  left_join(
    pubs,
    by = c("inst_id", "year")
  )

write_csv(tw_panel_merge, "tw_panel_merge.csv")
write_csv(inst_ids, "inst_ids.csv")
write_csv(pubs, "pubs.csv")

fetch_citations <- function(inst_id, year) {
  message("Fetching: ", inst_id, " | ", year)
  works <- tryCatch(
    oa_fetch(
      entity           = "works",
      institutions.id  = inst_id,
      publication_year = as.integer(year),
      per_page         = 200,
      verbose          = FALSE
    ),
    error = function(e) NULL
  )
  n <- if (is.null(works) || nrow(works) == 0) NA_integer_ else sum(works$cited_by_count, na.rm = TRUE)
  tibble(inst_id = inst_id, year = as.integer(year), n_citations = n)
}

citations <- if (file.exists("citations_progress.rds")) {
  readRDS("citations_progress.rds")
} else {
  tibble(
    inst_id     = character(),
    year        = integer(),
    n_citations = integer()
  )
}


to_fetch <- tw_panel_merge %>%
  distinct(inst_id, year) %>%
  filter(!is.na(inst_id)) %>%
  anti_join(citations, by = c("inst_id", "year"))

message(nrow(to_fetch), " requests remaining")

for (i in seq_len(nrow(to_fetch))) {
  result <- fetch_citations(
    to_fetch$inst_id[[i]],
    to_fetch$year[[i]]
  )
  citations <- bind_rows(citations, result)
  saveRDS(citations, "citations_progress.rds")
}

write_csv(citations, "Data/citations.csv")

################################################################################################################

tw_panel_merge <- read_csv('Data/tw_panel_merge.csv') %>%
   mutate(inst_id = gsub("https://openalex.org/", "", inst_id))

african_countries <- c(

  "Algeria", "Angola", "Benin", "Botswana",

  "Burkina Faso", "Burundi", "Cameroon",

  "Cape Verde", "Central African Republic",

  "Chad", "Comoros", "Congo",

  "Democratic Republic of the Congo",

  "Djibouti", "Egypt", "Equatorial Guinea",

  "Eritrea", "Eswatini", "Ethiopia",

  "Gabon", "Gambia", "Ghana",

  "Guinea", "Guinea-Bissau", "Ivory Coast",

  "Kenya", "Lesotho", "Liberia",

  "Libya", "Madagascar", "Malawi",

  "Mali", "Mauritania", "Mauritius",

  "Morocco", "Mozambique", "Namibia",

  "Niger", "Nigeria", "Rwanda",

  "Senegal", "Seychelles", "Sierra Leone",

  "Somalia", "South Africa", "South Sudan",

  "Sudan", "Tanzania", "Togo",

  "Tunisia", "Uganda", "Zambia",

  "Zimbabwe"

)

global_north <- c(

  "United States", "Canada", "United Kingdom", "France", "Germany",

  "Italy", "Spain", "Netherlands", "Belgium", "Sweden", "Norway",

  "Denmark", "Finland", "Switzerland", "Austria", "Australia",

  "New Zealand", "Japan", "South Korea", "Singapore",

  "Ireland", "Portugal", "Greece", "Poland", "Czech Republic",

  "Hungary", "Slovakia", "Slovenia", "Estonia", "Latvia", "Lithuania", "Saudi Arabia",

  "Israel", "Russian Federation", "Ukraine", "Taiwan", "Hong Kong", "Romania", "United Arab Emirates", "Croatia", "Iceland", "Luxembourg", "Macao", "Qatar"

)

global_south <- c(

  "Algeria", "Angola", "Argentina", "Bangladesh", "Brazil",

  "Chile", "China", "Colombia", "Egypt", "Ethiopia",

  "Ghana", "India", "Indonesia", "Iran", "Iraq",

  "Kenya", "Malaysia", "Mexico", "Morocco", "Nigeria",

  "Pakistan", "Peru", "Philippines", "South Africa", "Thailand",

  "Tunisia", "Turkey", "Uganda", "Vietnam", "Zimbabwe", "Mozambique", "Kazakhstan", "Georgia", "Sri Lanka", "Paraguay", "Jordan", "Syria", "Lebanon", "Palestine", "Tanzania", "Brunei Darussalam", "Azerbaijan", "Botswana", "Namibia", "Cuba", "Kuwait", "Belarus", "Costa Rica", "Cyprus", "Uzbekistan", "Fiji", "Oman", "Bulgaria", "Uzbekistan", "Montenegro",
  "Ecuador", "Puerto Rico", "Venezuela", "Mauritius", "Kosovo", "Rwanda", "Nepal", "Jamaica", "Mongolia", "Zambia", "Bosnia and Herzegovina", "Bulgaria", "Serbia", "Czechia", "Armenia", "Northern Macedonia", "Bahrain"

)
view(global_south)

panel_kernel <- tw_panel_merge %>%
  arrange(inst_id, year) %>%
  group_by(inst_id) %>%
  mutate(
    research_t  = log1p(n_publications),
    research_t1 = dplyr::lead(log1p(n_publications))
  ) %>%
  ungroup() %>%
  filter(
    !is.na(research_t),
    !is.na(research_t1)
  ) %>%
  mutate(
    north_south = case_when(
      location %in% global_north ~ "North",
      location %in% global_south ~ "South",
      TRUE ~ NA_character_
    )
  )

south_data <- panel_kernel %>%
  filter(north_south == "South")

north_data <- panel_kernel %>%
  filter(north_south == "North")

kernel_model <- npreg(
  research_t1 ~ research_t,
  data = panel_kernel
)

kernel_model_south <- npreg(
  research_t1 ~ research_t,
  data = south_data
)

kernel_model_north <- npreg(
  research_t1 ~ research_t,
  data = north_data
)

grid <- data.frame(
  research_t = seq(
    min(panel_kernel$research_t),
    max(panel_kernel$research_t),
    length.out = 300
  )
)

grid$research_t1_hat <- predict(kernel_model, newdata = grid)

ggplot(grid, aes(x = research_t, y = research_t1_hat)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    x = "Research stock at t: log(1 + publications)",
    y = "Research stock at t+1: log(1 + publications)",
    title = "Kernel-estimated research transition function"
  ) +
  theme_minimal()

grid_south <- data.frame(
  research_t = seq(
    min(south_data$research_t),
    max(south_data$research_t),
    length.out = 300
  )
)

grid_south$research_t1_hat <- predict(kernel_model_south, newdata = grid_south)

ggplot(grid_south, aes(x = research_t, y = research_t1_hat)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    x = "Research stock at t: log(1 + publications)",
    y = "Research stock at t+1: log(1 + publications)",
    title = "Kernel-estimated research transition function"
  ) +
  theme_minimal()

grid_north <- data.frame(
  research_t = seq(
    min(north_data$research_t),
    max(north_data$research_t),
    length.out = 300
  )
)

grid_north$research_t1_hat <- predict(kernel_model_north, newdata = grid_north)

ggplot(grid_north, aes(x = research_t, y = research_t1_hat)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    x = "Research stock at t: log(1 + publications)",
    y = "Research stock at t+1: log(1 + publications)",
    title = "Kernel-estimated research transition function"
  ) +
  theme_minimal()

is_na_north <- panel_kernel %>%
  filter(is.na(north_south)) %>%
  pull(location) 

