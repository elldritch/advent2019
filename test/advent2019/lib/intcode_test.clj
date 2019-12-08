(ns advent2019.lib.intcode-test
  (:require [clojure.test :refer [deftest is testing]]
            [advent2019.lib.intcode :refer [run-program
                                            resume-program
                                            resume-program-with-input]]))

(defn expect [property input program expected]
  (is (= (property
          (if (nil? input)
            (run-program program)
            (resume-program-with-input (run-program program) input)))
         expected)))

(defn expect-halted-state [program expected]
  (let [result (run-program program)]
    (is (and (= (:status result) :halted)
             (= (:state result) expected)))))

(defn expect-output-for-input [program input output]
  (let [needs-input (run-program program)
        has-output (resume-program-with-input needs-input input)
        halted (resume-program has-output)]
    (is (and (= (:status needs-input) :needs-input)
             (= (:status has-output) :has-output)
             (= (:output has-output) output)
             (= (:status halted) :halted)))))

(deftest intcode
  (testing "addition and multiplication instructions"
    (expect-halted-state [1 9 10 3 2 3 11 0 99 30 40 50]
                         [3500 9 10 70 2 3 11 0 99 30 40 50])
    (expect-halted-state [1 0 0 0 99]
                         [2 0 0 0 99])
    (expect-halted-state [2 3 0 3 99]
                         [2 3 0 6 99])
    (expect-halted-state [2 4 4 5 99 0]
                         [2 4 4 5 99 9801])
    (expect-halted-state [1 1 1 4 99 5 6 0 99]
                         [30 1 1 4 2 5 6 0 99]))
  (testing "parameter modes"
    (expect-halted-state [1002 4 3 4 33]
                         [1002 4 3 4 99])
    (expect-halted-state [1101 100 -1 4 0]
                         [1101 100 -1 4 99]))
  (testing "outputs and comparison logic"
    (let [eq-8-position [3 9 8 9 10 9 4 9 99 -1 8]]
      (expect-output-for-input eq-8-position 7 0)
      (expect-output-for-input eq-8-position 8 1)
      (expect-output-for-input eq-8-position 9 0))
    (let [lt-8-position [3 9 7 9 10 9 4 9 99 -1 8]]
      (expect-output-for-input lt-8-position 7 1)
      (expect-output-for-input lt-8-position 8 0)
      (expect-output-for-input lt-8-position 9 0))
    (let [eq-8-immediate [3 3 1108 -1 8 3 4 3 99]]
      (expect-output-for-input eq-8-immediate 7 0)
      (expect-output-for-input eq-8-immediate 8 1)
      (expect-output-for-input eq-8-immediate 9 0))
    (let [lt-8-immediate [3 3 1107 -1 8 3 4 3 99]]
      (expect-output-for-input lt-8-immediate 7 1)
      (expect-output-for-input lt-8-immediate 8 0)
      (expect-output-for-input lt-8-immediate 9 0)))
  (testing "jumps"
    (let [eq-0-position [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]]
      (expect-output-for-input eq-0-position 0 0)
      (expect-output-for-input eq-0-position 1 1)
      (expect-output-for-input eq-0-position 42 1))
    (let [eq-0-immediate [3 3 1105 -1 9 1101 0 0 12 4 12 99 1]]
      (expect-output-for-input eq-0-immediate 0 0)
      (expect-output-for-input eq-0-immediate 1 1)
      (expect-output-for-input eq-0-immediate 42 1)))
  (testing "large program"
    (let [cmp-8 [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]]
      (expect-output-for-input cmp-8 6 999)
      (expect-output-for-input cmp-8 7 999)
      (expect-output-for-input cmp-8 8 1000)
      (expect-output-for-input cmp-8 9 1001)
      (expect-output-for-input cmp-8 10 1001))))
