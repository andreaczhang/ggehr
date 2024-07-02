# antibiotics who----

#' WHO antibiotics list
#'
#' This dataset contains information on antibiotics from WHO. More details to follow.
#'
#' @format
#' \describe{
#' \item{Antibiotic}{Name of the antibiotic}
#' \item{Class}{Antibiotic class}
#' \item{ATC code}{ATC code}
#' \item{Category}{Access, watch or reserve}
#' \item{Listed on EML/EMLc 2023}{Whether listed}
#' \item{color_code}{Color code according to category}
#' }
#' @examples
#' head(ab_who)
"ab_who"


# antibiotics ahus ----
#' AHUS antibiotics list
#'
#' This dataset contains information on antibiotics from AHUS. More details to follow.
#'
#' @format
#' \describe{
#' \item{Antibiotic}{Name of the antibiotic}
#' \item{Category}{Access, watch or reserve}
#' \item{Norsk_AHUS}{The Norwegian name, in AHUS hospital system}
#' \item{color_code}{Color code according to category}
#' }
#' @examples
#' head(ab_ahus)
"ab_ahus"


# _______ ----
# demographics ----

#' Demo data: demographics
#'
#' This dataset contains 10 samples of randomly generated data to for demonstration purpose.
#'
#' @format
#' \describe{
#' \item{ID}{Unique identifier}
#' \item{sex}{Sex of the patient}
#' \item{age}{Age}
#' \item{dept}{Department of admission: gastrosurgery, orthopaedics, gastro medicine, neurology}
#' \item{t0}{Admission time (in hours). This is relative to the data extraction time.}
#' \item{los}{Length of stay (in hours)}
#' \item{adm_type}{Type of admission: emergency or elective}
#' }
#' @examples
#' head(demographics)
"demographics"


# ab_prescription ----

#' Demo data: antibiotics prescription
#'
#' This dataset contains 10 samples of randomly generated data to for demonstration purpose.
#'
#' @format
#' \describe{
#' \item{ID}{Unique identifier}
#' \item{time}{Record time (in hours)}
#' \item{ab_name}{Name of the antibiotics}
#' \item{firsttime}{Whether it is the first time this antibiotics is prescribed}
#' \item{abform_filled}{Whether the form for antibiotics is filled}
#' \item{purpose}{Purpose for the antibiotics prescription}
#' \item{source}{Type of infection: community acquire, hospital acquired or profylactic uses}
#' }
#' @examples
#' head(ab_prescription)
"ab_prescription"




# ab_use ----

#' Demo data: antibiotics use
#'
#' This dataset contains 10 samples of randomly generated data to for demonstration purpose.
#'
#' @format
#' \describe{
#' \item{ID}{Unique identifier}
#' \item{time}{Record time (in hours)}
#' \item{use}{Dose of antibiotics}
#' \item{unit}{Unit of the drug}
#' \item{ab_name}{Name of the antibiotics}
#' \item{ab_method}{Method of drug use}
#' \item{ab_dose}{Additional information of the drug use}
#' }
#' @examples
#' head(ab_use)
"ab_use"



# catheters ----
#' Demo data: catheters
#'
#' This dataset contains 10 samples of randomly generated data to for demonstration purpose.
#'
#' @format
#' \describe{
#' \item{ID}{Unique identifier}
#' \item{time}{Record time (in hours)}
#' \item{catheter}{Name of the catheter}
#' \item{use}{How long the catheter has been used}
#' \item{unit}{Minutes}
#' }
#' @examples
#' head(catheters)
"catheters"



# location ----
#' Demo data: location
#'
#' This dataset contains 10 samples of randomly generated data to for demonstration purpose.
#'
#' @format
#' \describe{
#' \item{ID}{Unique identifier}
#' \item{postid}{ID for the stay of the major ward}
#' \item{subpostid}{ID for the stay of the associated ward: only sop (central operation room), intensive (intensive care unit) and postop (post operative unit)}
#' \item{post}{Name of the ward}
#' \item{time}{time}
#' \item{time_stay_minutes}{Minutes}
#' }
#' @examples
#' head(location)
"location"




