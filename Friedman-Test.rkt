#lang racket
#|
MIT License

Copyright (c) 2023 Jan-Ole Giebel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#

; Diese Datei beinhaltet den zweiten Code für Jan-Ole Giebels PL im Fach Informatik zur Durchführung des Friedman-Tests.
(define geheimText  "AZBBDPHMAKDDNOXGPQFGWNXMRWGQKAMNYSLOWHBDNMGDNABDDBWQAQLBDQYEADHQWVDDNTBDCMGDNOXGPHNLAZLSAVNMZNKZCBXHJMGLWBKNOMGMWOXMKALDSWAHJOXGPLBDNMBRAEBQXZBMCMGCQMGFAUBSPMEMWKALKATLXQDTJLDNIUXMIQMDEVXQHIWTJODZBNXDVCKTAKDFQBZDJWLRAEXHPMKLWKADJJXHIHPDEBXMOKAHBNWHAOEDEKADBZTFAVTFAVHROMPNDQGFAPMCEMKDEAXVEZAZXMGEWPKQWMWDNOXKWLXMZQXVEZSTCMGNOAXEELXKYILSNWUQEVZDJHNQQMVJGWFLAVPHNUBSAQGDNTTCQVZAWVTMAVZTPOXMKALDSMBSAZFZYPXMHMMYPMGCHQVGXMBLZZBSPMGRYPBEBVTFAVHROMPNDQGFAPMCEMKDEAXVEZUQEVZDJSTEBMXTJLUZJIGDJVTBDTXMEVZQWLNMZEHLEBDNIUMHDZSTNCXBGVTVEMBLIMKFIQMCAZXHOMGAWPG")

;Code zum erstellen eines Alphabetes.
; Characters in Scheme-Symbols umwandeln. (Aus der gegebenen Datei StringFunktionen.rkt.)
(define (char->symbol aChar)
  (define aString (string aChar))
  (string->symbol aString)
  )

; Eigene implementierung der range funktion.
(define (myRange start end)
  (cond
    [(equal? start end) '()]
    [else
     (cons start (myRange (+ start 1) end))
    ]
  )
)

; Falls man ein verschobenes Alphabet erstellen möchte, wird hier der Überlauf erkannt. (Wird hauptsächlich für ein anderes Programm benötigt.)
(define (check_alphabet_overflow  num)
      (+ (modulo (- num 65) 26) 65))

; Die Liste mit den ASCII-Zahlen wird hier in eine Character-List umwandeln.
(define (asciiIntToCharList numList)
  (cond
    [(empty? numList) '()]
    [else
     (cons (char->symbol (integer->char (check_alphabet_overflow (first numList)))) (asciiIntToCharList (rest numList)))
    ]
  )
)

; Die Aplhabet-Liste erstellen.
(define (createAlphabetList displacement)
  (asciiIntToCharList (myRange (+ displacement 65) (+ displacement 91)))
)

; Aus der Liste einen String machen.
(define (createAlphabetString displacement)
  (define (inner alphabetList)
    (cond
      [(empty? alphabetList) ""]
      [else (string-append (symbol->string (first alphabetList)) (inner (rest alphabetList)))]
    )
  )
  (inner (createAlphabetList displacement))
)
;Ende des Codes zum erstellen eines Alphabetes.

;Funktion zum zählen, wie oft ein Element in einem String vorkommt.
(define (calculateNumberOfElmInString stringBuffer elm [counter 0])
  (cond
    [(equal? stringBuffer "") counter]
    [(equal? (substring stringBuffer 0 1) elm) (calculateNumberOfElmInString (substring stringBuffer 1 (string-length stringBuffer)) elm (add1 counter))]
    [else  (calculateNumberOfElmInString (substring stringBuffer 1 (string-length stringBuffer)) elm counter)]
  )
)

; Funktion, um ein Element aus einem String zu entfernen.
(define (removeCharFromString stringBuffer toRemove [newString ""])
  (cond
    [(equal? stringBuffer "") newString]
    [(equal? (substring stringBuffer 0 1) toRemove) (removeCharFromString (substring stringBuffer 1 (string-length stringBuffer)) toRemove newString)]
    [else
     (removeCharFromString (substring stringBuffer 1 (string-length stringBuffer)) toRemove (string-append newString (substring stringBuffer 0 1)))
    ]
  )
)

; Funktion zum finden der Anzahl an möglichen gleichen Buchstaben.
(define (calculateNumberOfEqualPairs stringBuffer alphabetBuffer [index 0])
  (cond
    [(equal? alphabetBuffer "") index]
    [else
     (define n (calculateNumberOfElmInString stringBuffer (substring alphabetBuffer 0 1)))
     (calculateNumberOfEqualPairs (removeCharFromString stringBuffer (substring alphabetBuffer 0 1)) (substring alphabetBuffer 1 (string-length alphabetBuffer)) (+ index (expt n 2)))
    ]
  )
)

; Funktion zur Berechung des Koinzidenzindexes oder auch Kappa ganannt.
(define (calculateCoincidenceIndex stringBuffer alphabetBuffer)
  (define stringBufferLength (string-length stringBuffer))
  (/ (calculateNumberOfEqualPairs stringBuffer alphabetBuffer) (* stringBufferLength (- stringBufferLength 1)))
)

; Funktion zur eigentlichen Durchführung des Friedman testes.
(define (doFriedmanTest stringBuffer coincidenceIndex coincidenceIndexRandom coincidenceIndexGerman)
  (define n (string-length stringBuffer))
  ;(/ (* n (- coincidenceIndexGerman coincidenceIndexRandom)) (- (* coincidenceIndex (- n 1)) (+ (* coincidenceIndexRandom n) coincidenceIndexGerman))) (Andere Formel)
  (/ (- coincidenceIndexGerman coincidenceIndexRandom) (- coincidenceIndex coincidenceIndexRandom))
)

; Ein Alphabet erstellen.
(define alphabet (createAlphabetString 0))

; Den Koinzidenzindex des verschlüsselten Textes berechnen.
(define coIndex (calculateCoincidenceIndex geheimText alphabet))

; Den eigentlichen Friedman-Test, mit den Koinzidenz-Werten: 0.0385 -> für einen zufälligen Text
; und: 0.0762 -> für einen deutschen Text, ausführen.
(doFriedmanTest geheimText coIndex 0.0385 0.0762)