(ns clojure-430-hw05.core)

(defn hello [n]
  (println "Hello, " n))

;; Worker thread to do a pi approximation N times
(defn pi-worker [numSamples]
  (let [odd-numbers (filter odd? (iterate inc 1))]
    (* 4.0 
       (apply + (map / (cycle [1 -1]) (take numSamples odd-numbers))))))

;; Run this for Question 1
;; It will print out the approximation for each worker
;; I couldn't figure out how to total up all of the approximations
;; from my array of futures.  I spent ~2 hours searching the official
;; doc and on stackoverflow questions and couldn't figure out how to
;; do this frustratingly simple operation.
(defn pi-approx [nThreads nSamples]
  (def futures (repeat (future (pi-worker (/ nSamples nThreads)))))
  (def workers (take nThreads futures))
  (def total 0)
  (for [worker workers]
    (println "Pi approx: " @worker)))
