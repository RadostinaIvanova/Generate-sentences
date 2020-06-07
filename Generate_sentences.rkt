#lang racket
(define (listOfDef list)
  (filter (lambda (x) (equal? (cadr x) "О")) list))

(define (listOfSub list)
  (filter (lambda (x) (equal? (cadr x) "П")) list))

(define (listOfAdd list)
  (filter (lambda (x) (equal? (cadr x) "Д")) list))

(define (listOfVerb list)
  (filter (lambda (x) (equal? (cadr x) "С")) list))

(define (filterList filtBy list) (filter (lambda (x) (equal? filtBy (third x))) list))

(define (filterSub filtBy subList) (filterList filtBy subList))
(define (filterVerb filtBy verbList)
  (let ((fl (filterList filtBy verbList)))
    (if (null? fl) (filter (lambda (x) (not(equal? (third x) "мн"))) verbList) fl)))
(define (filterDef filtBy list) list)

(define (randomEl sentList)
  (let* ((len (length sentList))
         (randN (random len)))
     (car (drop sentList randN))))

(define (randomPart someList sentence f)
 (let* ((filtBy (third (car sentence)))
       (filteredList (f filtBy someList)))
       (append sentence (list (randomEl filteredList)))))




(define (wordsList listOfAll)
  (let*  ((deff (listOfDef listOfAll))
          (subjects (listOfSub listOfAll))
          (verbs (listOfVerb listOfAll))
          (adds (listOfAdd listOfAll))
          (genTwoSubj (random 2))
          (sentenceWithDeff (randomEl deff))
          (secDef (randomEl deff))
          (sentenceWithSubject (randomPart subjects (list sentenceWithDeff) filterSub))
          (sentenceWithAnd (append sentenceWithSubject (list (list "и" '()))))
          (subject2 (randomPart subjects (list secDef) filterSub))
          (sentenceWithVerb2Sub (append  sentenceWithAnd subject2 (list (randomEl (filterList "мн" verbs)))))
          (sentenceWithVerb (randomPart verbs sentenceWithSubject filterVerb))
          (sentenceWithAdd (append (if (= genTwoSubj 0) sentenceWithVerb
                                                        sentenceWithVerb2Sub)
                                       (list (randomEl adds)))))
          sentenceWithAdd))

(define (genRandomSent listOfAll)
  (let ((listOfWords (wordsList listOfAll)))
    (string-append (string-titlecase (caar listOfWords)) " " (string-join (map car (cdr listOfWords)) " ") ".")))

(define words (file->list "c:\\Users\\Radi\\Downloads\\Newfolder\\aa.txt"))

(define (getRandomSentence path) (call-with-output-file path #:exists 'append
  (lambda (out) (write (genRandomSent words) out) (newline out))))

