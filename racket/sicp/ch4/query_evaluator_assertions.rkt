(displayln "Load database ...")

(add-assertion! address ((Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(add-assertion! job ((Bitdiddle Ben) (computer wizard)))
(add-assertion! salary ((Bitdiddle Ben) 60000))
(add-assertion! supervisor ((Bitdiddle Ben) (Warbucks Oliver)))

(add-assertion! address ((Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(add-assertion! job ((Hacker Alyssa P) (computer programmer)))
(add-assertion! salary ((Hacker Alyssa P) 40000))
(add-assertion! supervisor ((Hacker Alyssa P) (Bitdiddle Ben)))

(add-assertion! address ((Fect Cy D) (Cambridge (Ames Street) 3)))
(add-assertion! job ((Fect Cy D) (computer programmer)))
(add-assertion! salary ((Fect Cy D) 35000))
(add-assertion! supervisor ((Fect Cy D) (Bitdiddle Ben)))

(add-assertion! address ((Tweakit Lem E) (Boston (Bay State Road) 22)))
(add-assertion! job ((Tweakit Lem E) (computer technician)))
(add-assertion! salary ((Tweakit Lem E) 25000))
(add-assertion! supervisor ((Tweakit Lem E) (Bitdiddle Ben)))

(add-assertion! address ((Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(add-assertion! job ((Reasoner Louis) (computer programmer trainee)))
(add-assertion! salary ((Reasoner Louis) 30000))
(add-assertion! supervisor ((Reasoner Louis) (Hacker Alyssa P)))

(add-assertion! address ((Warbucks Oliver) (Swellesley (Top Heap Road))))
(add-assertion! job ((Warbucks Oliver) (administration big wheel)))
(add-assertion! salary ((Warbucks Oliver) 150000))

(add-assertion! address ((Scrooge Eben) (Weston (Shady Lane) 10)))
(add-assertion! job ((Scrooge Eben) (accounting chief accountant)))
(add-assertion! salary ((Scrooge Eben) 75000))
(add-assertion! supervisor ((Scrooge Eben) (Warbucks Oliver)))

(add-assertion! address ((Cratchet Robert) (Allston (N Harvard Street) 16)))
(add-assertion! job ((Cratchet Robert) (accounting scrivener)))
(add-assertion! salary ((Cratchet Robert) 18000))
(add-assertion! supervisor ((Cratchet Robert) (Scrooge Eben)))

(add-assertion! address ((Aull DeWitt) (Slumerville (Onion Square) 5)))
(add-assertion! job ((Aull DeWitt) (administration secretary)))
(add-assertion! salary ((Aull DeWitt) 25000))
(add-assertion! supervisor ((Aull DeWitt) (Warbucks Oliver)))

(add-assertion! can-do-job ((computer wizard) (computer programmer)))
(add-assertion! can-do-job ((computer wizard) (computer technician)))
(add-assertion! can-do-job ((computer programmer) (computer programmer trainee)))
(add-assertion! can-do-job ((administration secretary) (administration big wheel)))

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
