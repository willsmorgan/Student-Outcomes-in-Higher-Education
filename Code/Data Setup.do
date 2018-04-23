clear

* defining a full_data subset for goofing around
cd "R:\Data\ASU Core\Student Data\Peoplesoft Data"
import delimited using "full_data.csv", clear

keep strm session_code acad_plan emplid unt_taken passing_m cip_code course_load course took_course descr mmap_course transfer crse_grade_off residency ethnicity gender acad_level ssr_component subject location age_at_class_start catalog_nbr school_year core_course ncore_taken_bot nth_term term incoming_gpa complete corbetter mastery prev_term_gpa cgrade_wdrw cgrade modal class_nbr first_gen pell_eligibility netcost_wloan part_time_unof took_instructor college instructor_id part_time_offi
order emplid strm class_nbr
sort emplid strm class_nbr

******** necessary drops for cleaning ******************************************
drop if inlist(acad_level, "Graduate", "Law", "Post Bacc Graduate", "Post-Bacc Graduate")
drop if course_load > 24
drop if gender == "U"
drop if subject == "ASU"
drop if unt_taken > 4   /* get rid of internships */
drop if session_code == "DYN"

drop if inlist(crse_grade_off, "-", "EN", "H", "I", "LC", "NR", "P", "X", "XE")
drop if inlist(crse_grade_off, "Y", "Z")
destring crse_grade_off, force gen(cg_temp)
drop if cg_temp != .
drop cg_temp

drop if ssr_component != "LEC"
drop if nth_term > 16
drop if residency == " "
drop if incoming_gpa > 4.33
drop if missing(college)

drop if inlist(location, "AWC", "CALHC", "CHANDGILBT", "EAC", "GLBACAD")
drop if inlist(location, "GLENDALE", "MESATEMPE", "OTHERAZ", "OTHERUS")
drop if inlist(location, "OUTSIDEUS", "PHOENIX", "SCOTTSDALE", "TUCSON")

********** things to regenerate ************************************************

* Make sure missing values are filled in with something
replace pell_eligibility = "U" if pell_eligibility == ""

* recode winter to fall term
replace term = 1 if term == 4
decode term, gen(str_term)
drop term
ren str_term term

* generate course level (based on catalog_nbr)
gen course_level = "lower division"
replace course_level = "upper division" if catalog_nbr >= 300 & catalog_nbr <= 499
replace course_level = "graduate" if catalog_nbr > 499

* change cip_code to just be first couple digits
replace cip_code = "00.00" if cip_code == "-"
gen temp = substr(cip_code, 1, 2)
gen acad_field = subinstr(temp, ".", "", .)
drop if acad_field == "00"
replace acad_field = "01" if acad_field == "1"
replace acad_field = "03" if acad_field == "3"
replace acad_field = "04" if acad_field == "4"
replace acad_field = "05" if acad_field == "5"
replace acad_field = "09" if acad_field == "9"
drop temp

**** variables not needed for model ****
tostring class_nbr, replace
tostring strm, replace
tostring emplid, replace
gen id = emplid + "_" + strm + "_" + class_nbr
drop emplid class_nbr

cap drop acad_plan catalog_nbr cip_code subject catalog_nbr descr instructor_id ssr_component unt_taken

ren (crse_grade_off location modal) (grade campus modality)
ren part_time_unof part_time 

order *, alpha

foreach v of varlist * {
	di "`v'"
	drop if missing(`v')
}

compress

export delimited using "D:\W. Morgan\Projects\Student-Outcomes-in-Higher-Education\Data\ps_data.csv", q replace

sample 10
export delimited using "D:\W. Morgan\Projects\Student-Outcomes-in-Higher-Education\Data\ps_subset.csv", q replace
