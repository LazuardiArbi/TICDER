(defglobal
    ?*frame* = 0;
    ?*ViewMaleAccount* = nil;
    ?*ViewFemaleAccount* = nil;
    ?*SearchMaleInterest* = nil;
    ?*SearchFemaleInterest* = nil;
    ?*CountMaleAccount* = 0;
    ?*CountFemaleAccount* = 0;
    ?*CountMaleSearch* = 0;
    ?*CountFemaleSearch* = 0;
    )

(deftemplate MaleData
    (slot name)
    (slot age)
    (slot hobby)
    (slot interest)
    (slot height)
    (slot income)
    )

(deftemplate FemaleData
    (slot name)
    (slot age)
    (slot hobby)
    (slot interest)
    (slot height)
    )

(deftemplate MaleResult
    (slot name)
    (slot hobby)
    (slot height)
    (slot income)
    (slot rate)
    )

(deftemplate FemaleResult
    (slot name)
    (slot hobby)
    (slot height)
    (slot rate)    
    )

(deftemplate User
	(slot name)
    (slot gender)
    (slot relationship)
    (slot age)
    (slot height)
    (slot hobby)
    (slot income_preference)
    )

(defquery GetUserData
    ?numUser <- (User (name ?input_name) (gender ?input_gender) (relationship ?input_relationship) (age ?input_age) (height ?input_height) (hobby ?input_hobby) (income_preference ?input_income))
    )

(defquery GetMaleResult
    ?numMale <- (MaleResult (name ?nM) (hobby ?hM) (height ?heM) (income ?iM) (rate ?rM))
    )

(defquery GetFemaleResult
	?numFemale <- (FemaleResult (name ?nF) (hobby ?hF) (height ?heF) (rate ?rF))
	)

(deffacts MaleFacts
    (MaleData (name "Calvin") (age 20) (hobby "Game") (interest "Female") (height 170) (income 600))
    (MaleData (name "Vincent") (age 18) (hobby "Sport") (interest "Female") (height 165) (income 450))
    (MaleData (name "Bambang") (age 33) (hobby "Music") (interest "Female") (height 175) (income 200))
    (MaleData (name "David") (age 69) (hobby "Sport") (interest "Male") (height 159) (income 1000))
    (MaleData (name "Cristian") (age 27) (hobby "Game") (interest "Female") (height 192) (income 50))
    )

(reset)

(deffacts FemaleFacts
    (FemaleData (name "Cindy") (age 18) (hobby "Music") (interest "Male") (height 150))
    (FemaleData (name "Jennifer") (age 50) (hobby "Game") (interest "Female") (height 190))
    (FemaleData (name "Tukiyem") (age 99) (hobby "Sport") (interest "Male") (height 167))
    (FemaleData (name "Elis") (age 32) (hobby "Music") (interest "Male") (height 170))
    (FemaleData (name "Yolanda") (age 28) (hobby "Game") (interest "Male") (height 164))
    )

(reset)

(retract 0)

(defrule VIEW_MALE
    ?*ViewMaleAccount* <- (viewMaleAccount)
    (MaleData (name ?n) (age ?a) (hobby ?hob) (interest ?in) (height ?he) (income ?i))
    =>
    (++ ?*CountMaleAccount*)
    (printout t ?*CountMaleAccount*".     " ?n "     	" ?a "     	" ?hob "     	" ?in"     	"?he"cm""			"?i"$ USD" crlf)
	)

(defrule VIEW_FEMALE
    ?*ViewFemaleAccount* <- (viewFemaleAccount)
    (FemaleData (name ?n) (age ?a) (hobby ?hob) (interest ?in) (height ?he))
    =>
    (++ ?*CountFemaleAccount*)
    (printout t ?*CountFemaleAccount*".     "?n "     	" ?a "     	" ?hob "     	" ?in"     	"?he"cm"crlf)
	)

(defrule SEARCH_MALE_RELATIONSHIP
	?*SearchMaleInterest* <- (searchMaleInterest ?gender ?age ?height ?hobby ?income)
	(MaleData (name ?n) (age ?a&:(< (abs(- ?age ?a)) 40)) (hobby ?hob) (interest ?gender) (height ?he&:(< (abs(- ?height ?he)) 40)) (income ?i))
    =>
	(bind ?rate_value 100)
    (if (neq ?hob ?hobby) then
        (bind ?total1 (- ?rate_value 25))
        )
    (bind ?total2 (- ?total1 (abs (- ?age ?a))))
    (bind ?total3 (- ?total2 (abs (- ?height ?he))))
    (bind ?total4 (- ?total3 (abs (- ?income ?i))))
    (if (> ?total4 0) then
        (++ ?*CountMaleSearch*)
		(assert (MaleResult (name ?n) (hobby ?hob) (height ?he) (income ?i) (rate ?total4)))
        )
	)

(defrule SEARCH_MALE_RELATIONSHIP_NOTFOUND
	?*SearchMaleInterest* <- (searchMaleInterest ?gender ?age ?height ?hobby ?income)
	(not (MaleData (name ?n) (age ?a&:(< (abs(- ?age ?a)) 40)) (hobby ?hob) (interest ?gender) (height ?he&:(< (abs(- ?height ?he)) 40)) (income ?i)))
    =>
    (printout t "Data Not Found")    
    )

(defrule SEARCH_FEMALE_RELATIONSHIP_FEMALE
    ?*SearchFemaleInterest* <- (searchFemaleInterest ?gender ?age ?height ?hobby)
    (FemaleData (name ?n) (age ?a&:(< (abs(- ?age ?a)) 40)) (hobby ?hob) (interest ?gender) (height ?he&:(< (abs(- ?height ?he)) 40)))
    =>
    (bind ?rate_value 100)
    (if (neq ?hob ?hobby) then
        (bind ?total1 (- ?rate_value 25))
        )
    (bind ?total2 (- ?total1 (abs (- ?age ?a))))
    (bind ?total3 (- ?total2 (abs (- ?height ?he))))
    (if (> ?total3 0) then
        (++ ?*CountFemaleSearch*)
        (assert (FemaleResult (name ?n) (hobby ?hob) (height ?he) (rate ?total3)))
        )
    )

(defrule SEARCH_FEMALE_RELATIONSHIP_NOTFOUND
	?*SearchFemaleInterest* <- (searchFemaleInterest ?gender ?age ?height ?hobby)
	(not (FemaleData (name ?n) (age ?a&:(< (abs(- ?age ?a)) 40)) (hobby ?hob) (interest ?gender) (height ?he&:(< (abs(- ?height ?he)) 40))))
    =>
    (printout t "Data Not Found")    
    )

(defrule DELETE_MALE_FACTS
    ?num1 <- (Delete_Index_male ?number)
    ?num <- (MaleData)
    =>
    (++ ?*CountMaleAccount*)
    (if (eq ?number ?*CountMaleAccount*) then
        (retract ?num)
        (retract ?num1)
        (-- ?*CountMaleAccount*)
        )
    )

(defrule DELETE_FEMALE_FACTS
    ?num1 <- (Delete_Index_female ?number)
    ?num <- (FemaleData)
    =>
    (++ ?*CountFemaleAccount*)
    (if (eq ?number ?*CountFemaleAccount*) then
        (retract ?num)
		(retract ?num1) 
        (-- ?*CountFemaleAccount*)
        )
    )

(defrule MODIFY_MALE_FACTS
    ?num1 <- (Modify_Index_Male ?number ?name ?age ?hobby ?relationship ?height ?income)
    ?num <- (MaleData)     
    =>
    (++ ?*CountMaleAccount*)
    (if (eq ?number ?*CountMaleAccount*) then
        (retract ?num1)
        (modify ?num (name ?name) (age ?age) (hobby ?hobby) (interest ?relationship) (height ?height) (income ?income))
        )    
    )

(defrule MODIFY_FEMALE_FACTS
	?num1 <- (Modify_Index_Female ?number ?name ?age ?hobby ?relationship ?height)
	?num <- (FemaleData)	 
    =>
    (++ ?*CountFemaleAccount*)
    (if (eq ?number ?*CountFemaleAccount*) then
        (retract ?num1)
        (modify ?num (name ?name) (age ?age) (hobby ?hobby) (interest ?relationship) (height ?height))
        )    
    )

(deffunction createGUIFrame ()
    (bind ?*frame* (new Template))
    )

(deffunction numeric(?text)
    (try 
        (++ ?text)
     catch
        (return FALSE))
    
    (return TRUE)
    )

(deffunction clsScreen ()
    "comment"
    (for (bind ?i 0) (< ?i 100) (++ ?i)
        (printout t crlf)   
    )
	)

(deffunction submenu_1_male()
	(bind ?*CountMaleAccount* 0)
    (printout t "List of Male Member" crlf)
    (assert (viewMaleAccount))
    (run)
    (retract ?*ViewMaleAccount*)
          
    (printout t "Press ENTER to continue...")
    (readline)
    )

(deffunction submenu_1_female()
	(bind ?*CountFemaleAccount* 0)
    (printout t "List of Female Member" crlf)
    (assert (viewFemaleAccount))
    (run)
    (retract ?*ViewFemaleAccount*)
    
    (printout t "Press ENTER to continue...")
    (readline)    
    )

(deffunction submenu_2_male ()
    (bind ?input_name "")
    (bind ?input_age 0)
    (bind ?input_hobby "")
    (bind ?input_relationship "")
    (bind ?input_height 0)
    (bind ?input_income -1)
    
    
    (while (or (< (str-length ?input_name ) 3 ) (> (str-length ?input_name ) 20 ))
        (printout t "Input the name [3 - 20 characters length]: ")
        (bind ?input_name (readline))    
    )
    
    (while (or (< ?input_age 16) (> ?input_age 80))
        (printout t "Input "?input_name"'s age [16 - 80](years): ")
        (bind ?input_age (read))    
    )
    
    (while (and  (neq ?input_hobby "Sport") (neq ?input_hobby "Music") (neq ?input_hobby "Game"))
        (printout t "Input "?input_name"'s hobby [ Sport | Music | Game ](CASE-SENSITIVE): ")
        (bind ?input_hobby (readline))
    )
    
    (while (and  (neq ?input_relationship "Male") (neq ?input_relationship "Female"))
        (printout t "Input "?input_name"'s relationship preference [ Male | Female ](CASE-SENSITIVE): ")
        (bind ?input_relationship (readline))
    )
    
    (while (or (< ?input_height 100) (> ?input_height 240))
        (printout t "Input "?input_name"'s height [100 - 240](cm): ")
        (bind ?input_height (read))    
    )
    
    (while (or (< ?input_income 0) (> ?input_income 10000))
        (printout t "Input "?input_name"'s income [0 - 10000](USD): ")
        (bind ?input_income (read))    
    )
    
    (assert (MaleData (name ?input_name) (age ?input_age) (hobby ?input_hobby) (interest ?input_relationship) (height ?input_height) (income ?input_income)))
    (printout t "Successfuly added!" crlf)
    
    (++ ?*CountMaleAccount*)
    (printout t "Press ENTER to continue...")            
    (readline)
	)

(deffunction submenu_2_female ()
    (bind ?input_name "")
    (bind ?input_age 0)
    (bind ?input_hobby "")
    (bind ?input_relationship "")
    (bind ?input_height 0)
    (bind ?input_income 0)
    
    (while (or (< (str-length ?input_name ) 3 ) (> (str-length ?input_name ) 20 ))
        (printout t "Input the name [3 - 20 characters length]: ")
        (bind ?input_name (readline))    
    )
    
    (while (or (< ?input_age 16) (> ?input_age 80))
        (printout t "Input "?input_name"'s age [16 - 80](years): ")
        (bind ?input_age (read))    
    )
    
    (while (and  (neq ?input_hobby "Sport") (neq ?input_hobby "Music") (neq ?input_hobby "Game"))
        (printout t "Input "?input_name"'s hobby [ Sport | Music | Game ](CASE-SENSITIVE): ")
        (bind ?input_hobby (readline))
    )
    
    (while (and  (neq ?input_relationship "Male") (neq ?input_relationship "Female"))
        (printout t "Input "?input_name"'s relationship preference [ Male | Female ](CASE-SENSITIVE): ")
        (bind ?input_relationship (readline))
    )
    
    (while (or (< ?input_height 100) (> ?input_height 240))
        (printout t "Input "?input_name"'s height [100 - 240](cm): ")
        (bind ?input_height (read))    
    )
    
    (assert (FemaleData (name ?input_name) (age ?input_age) (hobby ?input_hobby) (interest ?input_relationship) (height ?input_height)))
    (printout t "Successfuly added!" crlf)
    
    (++ ?*CountFemaleAccount*)
    (printout t "Press ENTER to continue...")            
    (readline)
	)

(deffunction submenu_3_male ()
    (bind ?choose -1)
    (bind ?input_name "")
    (bind ?input_age 0)
    (bind ?input_hobby "")
    (bind ?input_relationship "")
    (bind ?input_height 0)
    (bind ?input_income -1)
    (bind ?*CountMaleAccount* 0)
    
    (printout t "List of Male Member" crlf)
    (assert (viewMaleAccount))
    (run)
    (retract ?*ViewMaleAccount*)
    
    (while (or (< ?choose 1) (> ?choose ?*CountMaleAccount*))
    	(printout t "Which member to be updated[1.."?*CountMaleAccount*" | 0 back to main menu]: ")
    	(bind ?choose (read))
        (if (eq ?choose 0) then
			(break)
        )
	)
    
    (if (neq ?choose 0) then
    	(while (or (< (str-length ?input_name ) 3 ) (> (str-length ?input_name ) 20 ))
        	(printout t "Input the name [3 - 20 characters length]: ")
        	(bind ?input_name (readline))    
    	)
    
    	(while (or (< ?input_age 16) (> ?input_age 80))
        	(printout t "Input "?input_name"'s age [16 - 80](years): ")
        	(bind ?input_age (read))    
    	)
    
    	(while (and  (neq ?input_hobby "Sport") (neq ?input_hobby "Music") (neq ?input_hobby "Game"))
        	(printout t "Input "?input_name"'s hobby [ Sport | Music | Game ](CASE-SENSITIVE): ")
        	(bind ?input_hobby (readline))
    	)
    
    	(while (and  (neq ?input_relationship "Male") (neq ?input_relationship "Female"))
        	(printout t "Input "?input_name"'s relationship preference [ Male | Female ](CASE-SENSITIVE): ")
        	(bind ?input_relationship (readline))
    	)
    
    	(while (or (< ?input_height 100) (> ?input_height 240))
        	(printout t "Input "?input_name"'s height [100 - 240](cm): ")
        	(bind ?input_height (read))    
    	)
    
    	(while (or (< ?input_income 0) (> ?input_income 10000))
        	(printout t "Input "?input_name"'s income [0 - 10000](USD): ")
        	(bind ?input_income (read))    
    	)

        (bind ?*CountMaleAccount* 0)
        (assert (Modify_Index_Male ?choose ?input_name ?input_age ?input_hobby ?input_relationship ?input_height ?input_income))
        (run)
    	(printout t "Male member with id :" ?choose" => successfully updated!"crlf)
    	(printout t "Press ENTER to continue...")
    	(readline)
	)
	)
    
(deffunction submenu_3_female ()
    (bind ?choose -1)
    (bind ?input_name "")
    (bind ?input_age 0)
    (bind ?input_hobby "")
    (bind ?input_relationship "")
    (bind ?input_height 0)
    (bind ?*CountFemaleAccount* 0)
    
    (printout t "List of Female Member" crlf)
    (assert (viewFemaleAccount))
    (run)
    (retract ?*ViewFemaleAccount*)
	
    
    (while (or (< ?choose 1) (> ?choose ?*CountFemaleAccount*))
    	(printout t "Which member to be updated[1.."?*CountFemaleAccount*" | 0 back to main menu]: ")
    	(bind ?choose (read))
		(if (eq ?choose 0) then
			(break)
        )
	)
    
    (if (neq ?choose 0) then
    	(while (or (< (str-length ?input_name ) 3 ) (> (str-length ?input_name ) 20 ))
        	(printout t "Input the name [3 - 20 characters length]: ")
        	(bind ?input_name (readline))    
    	)
    
    	(while (or (< ?input_age 16) (> ?input_age 80))
        	(printout t "Input "?input_name"'s age [16 - 80](years): ")
        	(bind ?input_age (read))    
    	)
    
    	(while (and  (neq ?input_hobby "Sport") (neq ?input_hobby "Music") (neq ?input_hobby "Game"))
        	(printout t "Input "?input_name"'s hobby [ Sport | Music | Game ](CASE-SENSITIVE): ")
        	(bind ?input_hobby (readline))
    	)
    
    	(while (and  (neq ?input_relationship "Male") (neq ?input_relationship "Female"))
        	(printout t "Input "?input_name"'s relationship preference [ Male | Female ](CASE-SENSITIVE): ")
        	(bind ?input_relationship (readline))
    	)
    
    	(while (or (< ?input_height 100) (> ?input_height 240))
        	(printout t "Input "?input_name"'s height [100 - 240](cm): ")
        	(bind ?input_height (read))    
    	)
    
        (bind ?*CountFemaleAccount* 0)
    	(assert (Modify_Index_Female ?choose ?input_name ?input_age ?input_hobby ?input_relationship ?input_height))
        (run)
    	(printout t "Female member with id :" ?choose" => successfully updated!" crlf)
    	(printout t "Press ENTER to continue...")
    	(readline)
	)
	)

(deffunction submenu_4_male()
    (bind ?choose -1)
    (bind ?*CountMaleAccount* 0)
   
    (printout t "List of Male Member" crlf)
    (assert (viewMaleAccount))
    (run)
    (retract ?*ViewMaleAccount*)
	
    (while (or (< ?choose 1) (> ?choose ?*CountMaleAccount*))
        (printout t "Which member to be updated[1.."?*CountMaleAccount*" | 0 back to main menu]: ")
   		(bind ?choose (read))
        (if (eq ?choose 0) then
            (break)
            )
        )
    (if (neq ?choose 0) then
        (bind ?*CountMaleAccount* 0)
        (assert (Delete_Index_male ?choose))
        (run)
    	(printout t "Successfuly deleted!" crlf)
    	(printout t "Press ENTER to continue...")
    	(readline)
        )
	)

(deffunction submenu_4_female()
    (bind ?choose -1)
    (bind ?*CountFemaleAccount* 0)
    
    (printout t "List of Female Member" crlf)
    (assert (viewFemaleAccount))
    (run)
    (retract ?*ViewFemaleAccount*)
    
	(while (or (< ?choose 1) (> ?choose ?*CountFemaleAccount*))
    	(printout t "Which member to be updated[1.."?*CountFemaleAccount*" | 0 back to main menu]: ")
    	(bind ?choose (read))
		(if (eq ?choose 0) then
            (break)
            )
        )
    
    (if (neq ?choose 0) then
		(bind ?*CountFemaleAccount* 0)
        (assert(Delete_Index_female ?choose))
        (run)
    	(printout t "Successfuly deleted!" crlf)
    	(printout t "Press ENTER to continue...")
    	(readline)
        )
	)

(deffunction menu_5()
    (bind ?input_name "")
    (bind ?input_gender "")
    (bind ?input_age 0)
    (bind ?input_hobby "")
    (bind ?input_relationship "")
    (bind ?input_height 0)
    (bind ?input_income -1)
    
    (while (or (< (str-length ?input_name ) 3 ) (> (str-length ?input_name ) 20 ))
        (printout t "Input your name [3 - 20 characters length]: ")
        (bind ?input_name (readline))    
    )
    
    (while (and  (neq ?input_gender "Male") (neq ?input_gender "Female"))
        (printout t "Input your gender [ Male | Female ] (CASE-SENSITIVE): ")
    	(bind ?input_gender (readline))
	)
    
	(while (and  (neq ?input_relationship "Male") (neq ?input_relationship "Female"))
        (printout t "Input your relationship preference [ Male | Female ](CASE-SENSITIVE): ")
        (bind ?input_relationship (readline))
    )
    (while (or (< ?input_age 16) (> ?input_age 80))
        (printout t "Input your age [16 - 80](years): ")
        (bind ?input_age (read))    
    )
    
    (while (or (< ?input_height 100) (> ?input_height 240))
        (printout t "Input your height [100 - 240](cm): ")
        (bind ?input_height (read))    
    )
    
    (while (and  (neq ?input_hobby "Sport") (neq ?input_hobby "Music") (neq ?input_hobby "Game"))
        (printout t "Input your hobby [ Sport | Music | Game ](CASE-SENSITIVE): ")
        (bind ?input_hobby (readline))
    )
    
    (if (eq ?input_relationship "Male") then
    	(while (or (< ?input_income 0) (> ?input_income 10000))
        		(printout t "Input your preferred income [0 - 10000]($USD): ")
        		(bind ?input_income (read))    
    	)
        (assert (User (name ?input_name) (gender ?input_gender) (relationship ?input_relationship) (age ?input_age) (height ?input_height) (hobby ?input_hobby) (income_preference ?input_income)))
    	(bind ?*CountMaleSearch* 0)
    	(assert (searchMaleInterest ?input_relationship ?input_age ?input_height ?input_hobby ?input_income))
    	(run)
        (retract ?*SearchMaleInterest*)

		(readline)
		)
    
    (if (eq ?input_relationship "Female") then
        (assert (User (name ?input_name) (gender ?input_gender) (relationship ?input_relationship) (age ?input_age) (height ?input_height) (hobby ?input_hobby) (income_preference 0)))
    	(bind ?*CountFemaleSearch* 0)
    	(assert (searchFemaleInterest ?input_relationship ?input_age ?input_height ?input_hobby))
    	(run)
        (retract ?*SearchFemaleInterest*)  
  
    	(readline)        
		)
    (facts)
	)

(deffunction menu ()
    (printout t "==========" crlf)
    (printout t "| TICDER |" crlf)
    (printout t "==========" crlf)
    (printout t " 1. View Members" crlf)
    (printout t " 2. Add a New Member" crlf)
    (printout t " 3. Update Member's Profile" crlf)
    (printout t " 4. Delete Member" crlf)
    (printout t " 5. Search Match" crlf)
    (printout t " 6. Exit" crlf)
	)

(deffunction submenu_umum()
    (printout t "1. Male" crlf)
    (printout t "2. Female" crlf)
    )

(deffunction menu_1 ()
    (printout t "List of our member to be viewed"crlf)
    (printout t "-------------------------------"crlf)    
	)

(deffunction menu_2 ()
    (printout t "Types of members to be added"crlf)
    (printout t "----------------------------"crlf)
	)

(deffunction menu_3 ()
    (printout t "List of our members to be updated"crlf)
    (printout t "---------------------------------"crlf)
	)

(deffunction menu_4 ()
    (printout t "List of our members to be deleted"crlf)
    (printout t "---------------------------------"crlf)
	)

(deffunction main ()
    (bind ?ch -1)
    
    (while (neq ?ch 6)
        (clsScreen)
        (facts)
        (menu)
        (printout t ">> Input [1-6]: ")
        (bind ?ch (read))
        
        (if (eq ?ch 1) then
            (bind ?ch1 -1)
            (clsScreen)
            (menu_1)
            (submenu_umum)
   
            (while (or (< ?ch1 1) (> ?ch1 2))
            	(printout t ">> Choose [1..2 | 0 back to main menu]: ")
            	(bind ?ch1 (read))
                (if (eq ?ch1 0) then 
                    (break)
                    )    
			)
            
            (if (eq ?ch1 1) then
                ;Panggil rule submenu_1_male
				(submenu_1_male)
            elif (eq ?ch1 2) then
                ;panggil rule submenu_1_female
				(submenu_1_female)
            )
        elif (eq ?ch 2) then
            (bind ?ch2 -1)
            (clsScreen)
            (menu_2)
            (submenu_umum)
   
			(while (or (< ?ch2 1) (> ?ch2 2))
            	(printout t ">> Choose [1..2 | 0 back to main menu]: ")
            	(bind ?ch2 (read))
                (if (eq ?ch2 0) then 
                    (break)
                    )    
				)

            (if (eq ?ch2 1) then
                ;Panggil function submenu_2_male
                (submenu_2_male)
            elif (eq ?ch2 2) then
                ;panggil function submenu_2_female
                (submenu_2_female)
            )
        elif (eq ?ch 3) then
            (bind ?ch3 -1)
            (clsScreen)
            (menu_3)
            (submenu_umum)
            
            (while (or (< ?ch3 1) (> ?ch3 2))
            	(printout t ">> Choose [1..2 | 0 back to main menu]: ")
            	(bind ?ch3 (read))
                (if (eq ?ch3 0) then 
                    (break)
                    )    
			)
            
            (if (eq ?ch3 1) then
                ;panggil function dari submenu_3_male
                (submenu_3_male)
             elif (eq ?ch3 2) then
                ;panggil function dari submenu_3_female
                 (submenu_3_female)          
			)
		elif (eq ?ch 4) then
			(bind ?ch4 -1)
            (clsScreen)
            (menu_4)
            (submenu_umum)
            
            (while (or (< ?ch4 1) (> ?ch4 2))
            	(printout t ">> Choose [1..2 | 0 back to main menu]: ")
            	(bind ?ch4 (read))
                (if (eq ?ch4 0) then 
                    (break)
                    )    
			)
            
            (if (eq ?ch4 1) then
                ;panggil function dari submenu_4_male
                (submenu_4_male)
             elif (eq ?ch4 2) then
                ;panggil function dari submenu_4_female
                 (submenu_4_female)
			) 
		elif(eq ?ch 5) then
            (menu_5)
		)
    )
	)

(main)