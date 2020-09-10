#' Simulated Dataset About Experiences with the Paranormal
#'
#' A dataset containing combat attributes of almost 812 fighters in the final
#' Avengers battle. This dataset illustrates several types of problems one might
#' encounter for univariate variables, including bimodality, zero-inflated data,
#' outliers, mixed up labels, etc.
#'
#' @format A data frame with 1000 rows and 7 variables: \describe{
#'   \item{conviction}{Degree of conviction they have about the existence of the
#'   paranormal} \item{fear}{How much they fear being kidnapped by aliens}
#'   \item{time}{How much time they spend a year researching the paranormal}
#'   \item{kidnapped}{Whether they've been kidnapped by aliens}
#'   \item{experiences.type}{What type of experiences they have had with the
#'   paranormal? Can be "eerie feeling," "presence watching", "saw UFO", "saw ghost", or "heard voice"} 
#'   \item{income}{How much money they make}
#'   \item{age}{Age of respondent}
#'   \item{gender}{Gender of respondent}
#'   \item{political}{Political Ideology of respondent}}
"paranormal"

#' Simulated Statistics on the Final Avengers Battle
#'
#' A dataset containing combat attributes of almost 812
#' fighters in the final Avengers battle
#'
#' @format A data frame with 812 rows and 13 variables:
#' \describe{
#'   \item{iq}{Intelligence}
#'   \item{agility}{weighted scores on an obstacle course}
#'   \item{speed}{Speed in running the 40 meter dash}
#'   \item{strength}{Pounds lifted in a benchpress}
#'   \item{damage.resistance}{Amount of skin deflection (in mm) when slapped with a frozen fish}
#'   \item{flexibility}{Number of inches past their toes they can reach}
#'   \item{willpower}{Length of time they wait at a DMV for a driver's license}
#'   \item{ptsd}{Score on a Likert-scale PTSD questionnaire}
#'   \item{north_south}{whether the individual was assigned to fight on the north or south battlefield}
#'   \item{died}{Whether the person died at the final battle}
#'   \item{kills}{Number of enemies killed in the final battle}
#'   \item{injuries}{Number of injuries sustained. Anything more than 5 is marked as a 5}
#'   \item{minutes.fighting}{Number of minutes spent fighting before dying or giving up}
#'   \item{shots.taken}{Number of shots (with a gun) or punches thrown at an enemy}
#'   \item{superpower}{Whether the individual has a superpower}   
#' }
"avengers"



#' Simulated Dataset About People's Attractiveness
#'
#' A dataset containing 207 observations, containing the following information
#'
#' @format A data frame with 207 rows and 5 variables: \describe{
#'   \item{stats_skills}{performance on a statistics know-how test}
#'   \item{group}{Treatment group (control, watching hitch, receiving stats training)}
#'   \item{gender}{Gender of person being rated}
#'   \item{exercise}{Self-reported average number of minutes exercised per week}
#'   \item{attractiveness_1}{Attractiveness score of person being rated at baseline}
#'   \item{attractiveness_2}{Attractiveness score of person being rated at follow up}
#'   \item{age}{Age of respondent}
#'   \item{gender}{Gender of respondent}
#'   \item{political}{Political Ideology of respondent}
#'}
"attractiveness"

#' Alcohol use among youth
#'
#' A dataset containing alcohol use from 82 youth over the course of three years
#' @importFrom utils packageVersion
#'
#' @format A data frame with 246 rows and 9 variables:
#' \describe{
#'   \item{ID}{ID of participant}
#'   \item{AGE}{AGE of participant}
#'   \item{COA}{Are they a child of an alcoholic? 1=yes}
#'   \item{MALE}{Gender (1=male)}
#'   \item{AGE_14}{Number of years since age 14}
#'   \item{ALCUSE}{Average number of alcoholic drinks a week }
#'   \item{PEER}{Average number of alcoholic drinks a week among their peers}
#'   \item{CPEER}{Median-centered amount their peers drink}
#'   \item{CCOA}{Child of alcoholic, centered}
#' }
"alcuse"

#' Job performance dataset
#'
#' These simulated data show relationships between various predictors of job performance with missing data
#'
#' @format A data frame with 187 rows and 4 variables:
#' \describe{
#'   \item{job.performance}{supervisor ratings of job performance}
#'   \item{iq}{intelligence}
#'   \item{conscientiousness}{score on a conscientious scale}
#'   \item{interview}{Rating on an interview scale}
#' }
"job_data"

#' Sales of books on Amazon
#'
#' A data-scraped dataset of book sales from about 200K authors
#'
#' @format A data frame with 197,197 rows and 22 variables:
#' \describe{
#' \item{Sold.by}{Publisher of book}
#' \item{Title}{Deidentified book title}
#' \item{Author}{Deidentified book author}
#' \item{Kindle.eBooks.Sales.Rank}{Sales rank}
#' \item{Sale.Price}{Price of book}
#' \item{Total.Reviews}{Number of reviews}
#' \item{Average.Rating}{average of reviews}
#' \item{Preorder}{Available for pre-order?}
#' \item{Date.Published}{Date published}
#' \item{Categories}{Fiction category}
#' \item{DRM}{Digital Rights Management}
#' \item{KU}{Kindle Unlimited}
#' \item{Has.ISBN}{Does the book have an ISBN?}
#' \item{Price.Was.Set.By.Publisher}{Was the price set by the publisher?}
#' \item{Consolidated.Digital.List.Price}{Dunno}
#' \item{Daily.Units.Sold}{Estimated number of books sold a day}
#' \item{Daily.Gross.Sales}{Estimated daily sales}
#' \item{Daily.Author.Revenue}{Daily author revenue (estimated)}
#' \item{Publisher}{Publisher type}
#' \item{Year}{Year published}
#' \item{Month}{Month Published}
#' \item{age}{Age of book}
#' }
"authors"

#' Dataset examines what affects blood pressure.
#'
#' This dataset looks at how various lifestyle choices affect blood pressure. 
#'
#' @format A data frame with xx rows and 4 variables:
#' \describe{
#'   \item{BP}{Blood pressure level}
#'   \item{Exercise}{description}
#'   \item{Vegetarian}{Whether the subject is vegetarian or not.}
#'   \item{Stress}{Level of stress}
#' }
"blood_pressure"


#' Dataset seeks to examine what can affect birthweight. 
#'
#' This dataset examines how various variables, especially related to parents, can affect the birthweight of a baby.
#'
#' @format A data frame with xx rows and 16 variables:
#' \describe{
#'   \item{id}{observation id}
#' 	 \item{headcirumference}{Circumference of baby's head.}
#'   \item{length}{description}
#'   \item{Birthweight}{Weight at time of baby's head.}
#'   \item{Gestation}{Length of time baby was gestated.}
#'   \item{smoker}{is the mother a smoker (1=yes)}
#'   \item{motherage}{Age of mother.}
#'   \item{mnocig}{number of cigarettes smoked by mother}
#'   \item{mheight}{mother's height}
#'   \item{mppwt}{mother pre pregnancy weight}
#'   \item{fage}{father's age}
#'   \item{fedyrs}{father's years of education}
#'   \item{fnocig}{father number of cigarettes}
#'   \item{fheight}{father height}
#'   \item{lowbwt}{binary indicator of whether it's considered low weight or not}
#'   \item{mage35}{mother's age, centered on 35}
#'   \item{LowBirthWeight}{Same as above, but labeled}
#' }
"birthweight"

#' Dataset examines what affects cholesterol. 
#'
#' This dataset looks at how margarine effect cholesterol
#'
#' @format A data frame with xx rows and 4 variables:
#' \describe{
#'   \item{ID}{description}
#'   \item{Margarine}{description}
#'   \item{Week}{description}
#'   \item{Cholesterol}{description}
#' }
"cholesterol"


#' Criminal dataset
#'
#' This is a simulated dataset that looks at how various variables affect convictions and rape instances
#'
#' @format A data frame with xx rows and 6 variables:
#' \describe{
#'   \item{ses}{Socioeconomic status of the subject}
#'   \item{empathy}{description}
#'   \item{depression}{Whether the subject is depressed}
#'   \item{rape}{description}
#'   \item{convictions}{How many times the subject has been convicted of a crime}
#'   \item{aggression}{How aggressive the subject is}
#' }
"criminal_data"


#' Diet dataset
#'
#' This dataset seeks to examine how effective certain diets can be for losing weight.
#'
#' @format A data frame with xx rows and 6 variables:
#' \describe{
#'   \item{Person}{description}
#'   \item{gender}{The subject's gender}
#'   \item{Age}{Age of the subject}
#'   \item{Height}{Height of the subject}
#'   \item{pre.weight}{Weight of the subject before the diet}
#'   \item{weight6weeks}{Weight of the subject after 6 weeks}
#'   \item{Diet}{Type of diet the subject went on}
#' }
"diet"


#' Drug use and SES
#'
#' This dataset looks at a possible relationship between drug usage and socioeconomic status.  
#'
#' @format A data frame with xx rows and 3 variables:
#' \describe{
#'   \item{drug.use}{description}
#'   \item{SES}{Socioeconomic status of the subject}
#'   \item{therapy}{Whether subjects attends therapy for drug use}
#' }
"drug_use"


#' Exercise dataset
#'
#' Simulated data that sees how weight loss varies as a function of various predictors, and treatment condition
#'
#' @format A data frame with xx rows and 13 variables:
#' \describe{
#'   \item{therapy.type}{description}
#'   \item{health}{description}
#'   \item{motivation}{description}
#'   \item{weight.loss}{description}
#'   \item{gender}{description}
#'   \item{satisfaction}{description}
#'   \item{rewards}{description}
#'   \item{income}{description}
#'   \item{muscle.gain}{description}
#'   \item{muscle.gain.missing}{description}
#'   \item{six.mo.weight}{description}
#' }
"exercise_data"



#' Graduate income
#'
#' This dataset examines whether schools and academic performance can lead to higher income in professions. Simulated data
#'
#' @format A data frame with xx rows and 4 variables:
#' \describe{
#'   \item{Grad.School}{Whether the subject attended grad school}
#'   \item{Profession}{Profession of the subject}
#'   \item{GPA}{GPA of the subject}
#'   \item{Income}{The current income of the subject}
#'   \item{Years}{Years since they graduated}
#' }
"graduate_income"

#' Math Achievement Dataset
#'
#' Examines the relationship between various variables and math achievement. Great for HLM. 
#'
#' @format A data frame with xx rows and 6 variables:
#' \describe{
#'   \item{School}{Which school the subject attends}
#'   \item{Minority}{description}
#'   \item{Sex}{Gender of the subject}
#'   \item{SES}{Socioeconomic status of the subject}
#'   \item{MathAch}{description}
#'   \item{MEANSES}{description}
#' }
"math"

#' NSDUH Dataset
#'
#' This is the 2014 National Survey of Drug Use and Health (NSDUH) dataset
#'
#' @format A data frame with xx rows and 27 variables:
#' \describe{
#'   \item{cig.rec}{smoked cigarettes recently?}
#'   \item{alc.rec}{used alcohol recently?}
#'   \item{coc.rec}{used cocain recently?}
#'   \item{her.rec}{used heroin recently?}
#'   \item{alc.freq}{frequency of alcohol use}
#'   \item{cocain.freq}{frequency of cocain use}
#'   \item{heroin.freq}{frequency of heroin use}
#'   \item{cig.freq}{frequency of cigarette use}
#'   \item{inpatient}{have they received inpatient treatment?}
#'   \item{outpatient}{have they received outpatient treatment?}
#'   \item{moves.past5}{moved in the past 5 years?}
#'   \item{attend.rel.serv}{do you attend religious services?}
#'   \item{rel.inf.dec}{how influential is religion on your decisions?}
#'   \item{distress}{mental distress score}
#'   \item{whodas.impairment}{mental impairment score}
#'   \item{MI}{probability of mental illness}
#'   \item{major.dep}{ever had major depression}
#'   \item{dep.pas.yr}{depression in the past year}
#'   \item{age}{age}
#'   \item{military}{been in military}
#'   \item{health.rating}{self reported health rating}
#'   \item{sex}{sex}
#'   \item{education}{educational attainment}
#'   \item{race}{race}
#'   \item{county}{type of county they live in (small metro, large metro, etc)}
#'   \item{income}{how much bank they make}
#' }
"nsduh"

#' Plant Growth
#'
#' Measures of seedling growth depending on various characteristics. NOT simulated data. 
#'
#' @format A data frame with xx rows and 5 variables:
#' \describe{
#'   \item{Location}{Location stored (greenhouse vs inside)}
#'   \item{Soil.Type}{Type of soil used}
#'   \item{Method}{Method of planting (soil blocks versus newspaper)}
#'   \item{Diameter}{Diameter of trunk}
#'   \item{Leaves}{Number of leaves}
#' }
"plant_growth"

#' Relationship satisfaction dataset
#'
#' Data about various factors affecting relationship satisfaction. Simulated data. 
#'
#' @format A data frame with xx rows and 7 variables:
#' \describe{
#'   \item{conscientiousness}{description}
#'   \item{honesty}{description}
#'   \item{communication}{description}
#'   \item{interests}{description}
#'   \item{satisfaction}{description}
#'   \item{separated}{description}
#'   \item{gender}{description}
#' }
"relationship_satisfaction"

#' Secret agent dataset
#'
#' Simulated data about various characteristics of secret agents
#'
#' @format A data frame with xx rows and 7 variables:
#' \describe{
#'   \item{muscle}{bicep circumference}
#'   \item{height}{height in inches}
#'   \item{tortured}{have they been tortured on a mission?}
#'   \item{missions}{number of missions completed}
#'   \item{Course.Score}{score on an obstacle course}
#'   \item{Branch}{Branch of service (CIA versus MI6)}
#'   \item{Salary}{Annual salary}
#' }
"secret_agent"

#' Tablesaw injuries
#'
#' Simulated data about the probability of getting injured by a tablesaw
#'
#' @format A data frame with xx rows and 5 variables:
#' \describe{
#'   \item{safety}{description}
#'   \item{attention}{description}
#'   \item{gender}{description}
#'   \item{probs}{description}
#'   \item{injury}{description}
#' }
"tablesaw.injury"
