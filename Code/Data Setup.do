* defining a full_data subset for nn goofing around
cd "R:\Data\ASU Core\Student Data\Peoplesoft Data"
use "full_data.dta", clear

keep strm session_code acad_plan emplid unt_taken passing_m cip_code total_units course took_course descr mmap transfer crse_grade_off residency ethnic_group gender_cd acad_level ssr_component subject location age_at_class_start catalog_nbr school_year core ncore_taken nth_term term incoming_gpa complete corbetter mastery prev_gpa cgrade_we cgrade_wz modal class_nbr first_gen pell_eligibility nc_wloan part_time_unof took_instructor college instructor_id
order emplid strm class_nbr
sort emplid strm class_nbr

******** necessary drops for cleaning ******************************************
drop if inlist(acad_level, "Graduate", "Law", "Post Bacc Graduate", "Post-Bacc Graduate")
drop if total_units > 24
drop if gender_cd == "U"
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
* Get rid of residency in favor of an indicator
gen in_state = 0
replace in_state = 1 if residency == "RES"
drop residency

* Extra dependent variable for getting an A
gen aorbetter = 0
replace aorbetter = 1 if crse_grade_off == "A-" | crse_grade_off == "A" | crse_grade_off == "A+"

*cgrade_we needs to be 0 when incoming_gpa == .
cap drop cgrade_we cgrade_wz
gen cgrade = . 
replace cgrade = 4 if crse_grade_off == "A"
replace cgrade = 4.33 if crse_grade_off == "A+"
replace cgrade = 3.67 if crse_grade_off == "A-"
replace cgrade = 3 if crse_grade_off == "B"
replace cgrade = 3.33 if crse_grade_off == "B+"
replace cgrade = 2.67 if crse_grade_off == "B-"
replace cgrade = 2 if crse_grade_off == "C"
replace cgrade = 2.33 if crse_grade_off == "C+"
replace cgrade = 1.67 if crse_grade_off == "C-"
replace cgrade = 1 if crse_grade_off == "D"
replace cgrade = 0 if crse_grade_off == "E"

gen cgrade_we = cgrade 
replace cgrade_we = prev_gpa - 2 if crse_grade_off == "W"
replace cgrade_we = 0 if cgrade_we <0 | missing(cgrade_we)

gen cgrade_wz = cgrade
replace cgrade_wz = 0 if crse_grade_off == "W"
drop cgrade

*passing_m needs to be zero in the case of a withdrawal
replace passing_m = 0 if crse_grade_off == "W"

*ncore_taken/mmap_taken should be defined as number of core/mmap courses taken before current session
cap drop ncore_taken
sort emplid acad_plan strm session_code
by emplid acad_plan: gen nc = sum(core) 
by emplid acad_plan: gen ncore_taken = nc[_n-1]
by emplid acad_plan: replace ncore_taken = 0 if _n==1
bysort emplid acad_plan strm session_code: replace ncore_taken = ncore_taken[1]
drop nc

cap drop mmap_taken
sort emplid acad_plan strm session_code
by emplid acad_plan: gen mm = sum(mmap) 
by emplid acad_plan: gen mmap_taken = mm[_n-1]
by emplid acad_plan: replace mmap_taken = 0 if _n==1
bysort emplid acad_plan strm session_code: replace mmap_taken = mmap_taken[1]
drop mm

ren (ncore_taken mmap_taken) (ncore_taken_bot mmap_taken_bot)

* Proportion of student's courses in a term that are part of major map/core
gen temp = 1

bysort emplid strm: egen tot_course = total(temp)
bysort emplid strm: egen tot_mmap = total(mmap)
bysort emplid strm: egen tot_core = total(core)

bysort emplid strm: gen mmap_prop = tot_mmap / tot_course
bysort emplid strm: gen core_prop = tot_core / tot_course

drop tot_course tot_mmap tot_core

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
drop emplid strm class_nbr

cap drop acad_plan catalog_nbr cip_code subject catalog_nbr descr incoming_gpa instructor_id ssr_component unt_taken

ren (core crse_grade_off ethnic_group gender_cd location mastery mmap modal) (core_course grade ethnicity gender campus borbetter mmap_course modality)
ren (nc_wloan part_time_unof prev_gpa total_units) (netcost_wloan full_time prev_term_gpa course_load)

order *, alpha

foreach v of varlist * {
	di "`v'"
	drop if missing(`v')
}

compress

export delimited using "D:\W. Morgan\Projects\Efficacy v1.01\Data\ps_data.csv", q replace

sample 10
export delimited using "D:\W. Morgan\Projects\Efficacy v1.01\Data\ps_subset.csv", q replace
