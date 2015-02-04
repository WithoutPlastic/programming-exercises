(displayln "Load database ...")

(add-assert! address ((Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(add-assert! job ((Bitdiddle Ben) (computer wizard)))
(add-assert! salary ((Bitdiddle Ben) 60000))
(add-assert! supervisor ((Bitdiddle Ben) (Warbucks Oliver)))

(add-assert! address ((Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(add-assert! job ((Hacker Alyssa P) (computer programmer)))
(add-assert! salary ((Hacker Alyssa P) 40000))
(add-assert! supervisor ((Hacker Alyssa P) (Bitdiddle Ben)))

(add-assert! address ((Fect Cy D) (Cambridge (Ames Street) 3)))
(add-assert! job ((Fect Cy D) (computer programmer)))
(add-assert! salary ((Fect Cy D) 35000))
(add-assert! supervisor ((Fect Cy D) (Bitdiddle Ben)))

(add-assert! address ((Tweakit Lem E) (Boston (Bay State Road) 22)))
(add-assert! job ((Tweakit Lem E) (computer technician)))
(add-assert! salary ((Tweakit Lem E) 25000))
(add-assert! supervisor ((Tweakit Lem E) (Bitdiddle Ben)))

(add-assert! address ((Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(add-assert! job ((Reasoner Louis) (computer programmer trainee)))
(add-assert! salary ((Reasoner Louis) 30000))
(add-assert! supervisor ((Reasoner Louis) (Hacker Alyssa P)))

(add-assert! address ((Warbucks Oliver) (Swellesley (Top Heap Road))))
(add-assert! job ((Warbucks Oliver) (administration big wheel)))
(add-assert! salary ((Warbucks Oliver) 150000))

(add-assert! address ((Scrooge Eben) (Weston (Shady Lane) 10)))
(add-assert! job ((Scrooge Eben) (accounting chief accountant)))
(add-assert! salary ((Scrooge Eben) 75000))
(add-assert! supervisor ((Scrooge Eben) (Warbucks Oliver)))

(add-assert! address ((Cratchet Robert) (Allston (N Harvard Street) 16)))
(add-assert! job ((Cratchet Robert) (accounting scrivener)))
(add-assert! salary ((Cratchet Robert) 18000))
(add-assert! supervisor ((Cratchet Robert) (Scrooge Eben)))

(add-assert! address ((Aull DeWitt) (Slumerville (Onion Square) 5)))
(add-assert! job ((Aull DeWitt) (administration secretary)))
(add-assert! salary ((Aull DeWitt) 25000))
(add-assert! supervisor ((Aull DeWitt) (Warbucks Oliver)))

(add-assert! can-do-job ((computer wizard) (computer programmer)))
(add-assert! can-do-job ((computer wizard) (computer technician)))
(add-assert! can-do-job ((computer programmer) (computer programmer trainee)))
(add-assert! can-do-job ((administration secretary) (administration big wheel)))

(add-rule! same (?x ?x) (always-true))

(add-rule! lives-near
           (?person-a ?person-b)
           (and (assert address (?person-a (?town . ?rest-a)))
                (assert address (?person-b (?town . ?rest-b)))
                (not (rule same (?person-a ?person-b)))))

(add-rule! outranked-by
           (?staff ?boss)
           (or (assert supervisor (?staff ?boss))
               (and (assert supervisor (?staff ?middle-manager))
                    (rule outranked-by (?middle-manager ?boss)))))

(add-rule! wheel
           (?person)
           (and (assert supervisor (?middle-manager ?person))
                (assert supervisor (?x ?middle-manager))))
