#lang racket
(define geheimText  "HGINEHWTEIBLUNARWPCLDWCVDDYUVNCETJIBLTFTQEVIIFFJEEHRNREQGIVNZYTLRZPUVNYIJKEMHBJKNQNKMHVOZCGXBLSCINJIMREZEKYGKIRXLLEGPVEDDWELNJIMKUMHSLHHKGCIDWTKDDVSYEHREVRFMQWEKHFJBDVHVSEYOBEKXJDAAIOUSNROVNRGIVIMHJVSBLPVNRXFAUMKGIATWJKZDXEFRSSCVNVYOUEQFBIIGVHFLCRFJGDWDYMDMEVBKMUQESWJVKZINDTHLSXOKHFEERLBRRRMFBADQNKERQJKGNPEVNDQLRMLIVEDRMOXTDMOCIDHERBDMERSGEUVIMIXLNCISJALIHVWZPUXELIMFDDMEVNRGIZFEISZMJPFZNDRTTHHJGVEQKSVIEXFJMHXXZLCINNEGISJCGEVKNHGIKDHIGVLRIOIIEJFVRRGIRUSRVIHHRBLFHREZEGSFYIBLHCATFFUIDAFCLDRWVRRGICIMKFEALIOUERGIZFEISLNCOBYNTREUARLBKMHXJYRDQTZNFIOUIDPPIEKIZXESEO;WGRQPWVPYPWSEJIFFSCRIQUSDOXYMIKKVXXSZTIAISZSSKRCÜZYKILRZPVYIGLHNGVQUSVXRVOVKFYOQIIZIVXSEBIEEHRTH;HZRNUPYFSDOXRUBVSFRUBUOIFFSCRIOQHRYXRZNFMIFMPVXKYQWTNDHDÜQBJIAZPVOHRDPVXÜLEGBXAQJQVKKRVTBBÄRXREQYGYRD")

; Gibt die Verschiebung einer x langen Buchstabenkombination zurück.
(define (getDisplacementFromCombination alphabetString stringLength startValue compStringLength [counter 0] [buffer ""])
  (cond
    [(equal? stringLength ( + counter compStringLength)) 0]
    [(equal? startValue (substring alphabetString 0 compStringLength)) (+ compStringLength counter)]
    [else
     (getDisplacementFromCombination (substring alphabetString 1 (string-length alphabetString)) stringLength startValue compStringLength (add1 counter) buffer)
    ]
  )
)

; Gibt die Verschiebungen aller x langen Kombinationen zurück. Wenn es keine gibt wird 0 zurückgegeben.
(define (getAllDisplecements alphabetString alphabetStringBuffer compStringLength [displacements '()])
  (cond
    [(< (string-length alphabetString) (* 2 compStringLength)) displacements]
    [else
     (getAllDisplecements (substring alphabetString 1 (string-length alphabetString)) alphabetStringBuffer compStringLength (cons (list (substring alphabetString 0 compStringLength)
                                                                                                                       
          (getDisplacementFromCombination (substring alphabetString compStringLength (string-length alphabetString))
                                          
              (string-length (substring alphabetString compStringLength (string-length alphabetString))) (substring alphabetString 0 compStringLength) compStringLength))

                                                                                                                 displacements))
    ]
  )
)

; Gestaltet die Liste mit den Verschiebungen übersictlicher
(define (sortBigToSmall list)
  (define (getLargestNumber list listLength [buffer 0] [counter 0])
    (cond
      [(equal? listLength counter) buffer]
      [else
       (cond
         [(> (first (rest (first list))) buffer) (getLargestNumber (rest list) listLength (first (rest (first list))) (add1 counter))]
         [else
          (getLargestNumber (rest list) listLength buffer (add1 counter))
         ]
       )
      ]
    )
  )

  (define (removeItem list elm)
    (cond
      [(empty? list) '()]
      [(equal? (first (rest (first list))) elm) (removeItem (rest list) elm)]
      [else
       (cons (first list) (removeItem (rest list) elm))
      ]
    )
  )
  (cond
    [(empty? list) '()]
    [else
     (define largestNumber (getLargestNumber list (length list)))
     (cons largestNumber (sortBigToSmall (removeItem list largestNumber)))
    ]
  )
)

; Entferne nullen
(define (removeZero list)
  (cond
    [(empty? list) '()]
    [(equal? (first list) 0) (removeZero (rest list))]
    [else (cons (first list) (removeZero (rest list)))]
  )
)

; Algorithmus für die Berechnung aller Primfaktoren
(define (calculate-prime-numbers-of-n n)
  (cond
    [(equal? n 1) '()]
    [else
     (define (inner n [t 2])
       (cond
         [(<= (* t t) n)
          (cond
            [(equal? (modulo n t) 0) (cons t (inner (/ n t) t))]
            [else
             (inner n (add1 t))
            ]
          )
         ]
         [else (cons n '())]
       )
     )
     (inner n)
    ]
  )
)

; Fügt alle Primfaktorzerlegungen in einer Liste zusammen.
(define (getAllPrimaFactors list)
  (cond
    [(empty? list) '()]
    [else (cons (calculate-prime-numbers-of-n (first list)) (getAllPrimaFactors (rest list)))]
  )
)

;Führt die oberen Funktionen für einen erflogreichen Kasiski-Test aus.
(define (doKasiskiTest list [numberOfCharsToCompare 3])
  (define displacementList (getAllDisplecements geheimText geheimText numberOfCharsToCompare))
  (define finalDisplacementList (removeZero (sortBigToSmall displacementList)))
  (getAllPrimaFactors finalDisplacementList)
)

(displayln "Führe den Kasiski-Test aus:")
(displayln "Folgend wird die Primfaktor-Verteilung ausgegeben:")
(define prim3 (doKasiskiTest geheimText 3))
(displayln prim3)

;https://de.m.wikibooks.org/wiki/Algorithmensammlung:_Zahlentheorie:_Primfaktorisierung

; Ab hier wird probiert den größten gemeinsamen Teiler zu finden.
; !!!Wichtig funktioniert nicht immer!!! Wenn es zu viele zufällige Dopplungen gibt, kommt ein faches Ergenbis heraus!!!

; Entfernt eine Liste aus einer 2D-Liste, wobei nach dem Ersten Element iner der inneren Liste geschaut wird.
(define (rmSubListFrom2DList list elm)
  (define (inner list elm)
    (cond
      [(empty? list) '()]
      [(equal? (first list) elm) (inner (rest list) elm)]
      [else (cons (first list) (inner (rest list) elm))]
    )
  )
  (cond
    [(empty? list) '()]
    [else (cons (inner (first list) elm) (rmSubListFrom2DList (rest list) elm))]
  )
)

; Zählt, wie oft eine Zahl in der 2D-Primfaktorzerlegten Liste vorkommt.
(define (countNumberIn2DPrimeDistributionArray listBuffer greatestNumber)
  (define (inner listBufferTwo gNumber)
    (cond
      [(empty? listBufferTwo) 0]
      [(equal? (first listBufferTwo) gNumber) (+ 1 (inner (rest listBufferTwo) gNumber))]
      [else (inner (rest listBufferTwo) gNumber)]
    )
  )
  (cond
    [(empty? listBuffer) 0]
    [else
     (+ (inner (first listBuffer) greatestNumber) (countNumberIn2DPrimeDistributionArray (rest listBuffer) greatestNumber))
    ]
  )
)

; Erstellt aus der Liste mit den Primfaktoren eine Liste mit den einzelnen Häufigkeiten dieser.
(define (getFrequncyDistribution listBuffer)
  (cond
    [(empty? listBuffer) '()]
    [(equal? (first listBuffer) '()) (getFrequncyDistribution (rest listBuffer))]
    [else (cons (list (first (sort (first listBuffer) >)) (countNumberIn2DPrimeDistributionArray listBuffer (first (sort (first listBuffer) >))))
                (getFrequncyDistribution (rmSubListFrom2DList listBuffer (first (sort (first listBuffer) >)))))
    ]
  )
)

; Gibt die größte Zahl in einer 2D-Liste aus, welche immer an zweiter Stelle steht.
(define (get2DListBiggest listBuffer [lastValue '(0 0)])
  (cond
    [(empty? listBuffer) lastValue]
    [(> (first (rest (first listBuffer))) (first (rest lastValue))) (get2DListBiggest (rest listBuffer) (first listBuffer))]
    [else (get2DListBiggest (rest listBuffer) lastValue)]
  )
)

; Zähle, wie oft eine Zahl in einer 2D Zahlenliste auftaucht.
(define (getAnzIntIn2DArr l elm [listBuffer '()])
  (define (inner li elm [anz 0])
    (cond
      [(empty? li) anz]
      [(equal? (first li) elm) (inner (rest li) elm (add1 anz))]
      [else (inner (rest li) elm anz)]
    )
  )
  (cond
    [(empty? l) listBuffer]
    [else (getAnzIntIn2DArr (rest l) elm (cons (inner (first l) elm) listBuffer))]
  )
)

; Zähle, wie oft ein Element in einer Liste vorkommt.
(define (getAnzInList li elm)
  (cond
    [(empty? li) 0]
    [(equal? (first li) elm) (add1 (getAnzInList (rest li) elm))]
    [else (getAnzInList (rest li) elm)]
  )
)

; Entferne ein Element aus einer Liste.
(define (removeItem li elm)
  (cond
    [(empty? li) '()]
    [(equal? (first li) elm) (removeItem (rest li) elm)]
    [else (cons (first li) (removeItem (rest li) elm))]
  )
)

; Versuche den größten gemeinsamen Teiler herauszufinden.
(define (tryGetGreatestCommonDivider listBufferOne)
  (cond
    [(equal? (modulo (first (get2DListBiggest (getFrequncyDistribution listBufferOne))) 2) 0)
     (define bufferList (getAnzIntIn2DArr listBufferOne (first (get2DListBiggest (getFrequncyDistribution listBufferOne)))))
     (define (getNumberOfElmsInSubLists listBufferTwo elm)
       (cond
         [(empty? listBufferTwo) '()]
         [else (cons (getAnzInList (first listBufferTwo) elm) (getNumberOfElmsInSubLists (rest listBufferTwo) elm))]
       )
     )
     (define numberOfElmsInSubLists (getNumberOfElmsInSubLists listBufferOne (first (get2DListBiggest (getFrequncyDistribution listBufferOne)))))
     (define (countSimilarItems listBufferThree)
       (cond
         [(empty? listBufferThree) '()]
         [else
          (cons (list (first listBufferThree) (getAnzInList listBufferThree (first listBufferThree))) (countSimilarItems (removeItem listBufferThree (first listBufferThree))))
          ]
       )
     )
     (* (first (get2DListBiggest (countSimilarItems numberOfElmsInSubLists))) (first (get2DListBiggest (getFrequncyDistribution listBufferOne))))
    ]
    [else
     (first (get2DListBiggest (getFrequncyDistribution listBufferOne)))
    ]
  )
)

; Versuche nun den ggt herauszubekommen.
(displayln "Eventuell ist der ggt:")
(displayln (tryGetGreatestCommonDivider prim3))