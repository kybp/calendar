(defpackage :calendar
  (:use :cl)
  (:export :calendar))
(in-package :calendar)

(defconstant +months+ '(:january :february :march :april :may :june :july
                        :august :september :october :november :december))

(defun month-name (index)
  (if (keywordp index)
      index
      (nth (1- index) +months+)))

(defun month-index (name)
  (if (numberp name)
      name
      (1+ (position name +months+))))

;; yuck
(defun day-of-week (day month year calendar)
  (let* ((m (month-index month))
         (result (real-day-of-week day m year calendar)))
    (if (or (and (eq calendar :julian)
                 (or (and (= m 4) (zerop (mod     year  28)))
                     (and (= m 2) (zerop (mod (1- year) 28)))))
            (and (eq calendar :gregorian)
                 (or (and (= m 9) (zerop (mod     year  400)))
                     (and (= m 2) (zerop (mod (1- year) 400))))))
        (mod (1+ result) 7)
        result)))

(defun real-day-of-week (day month year calendar)
  (let ((y (if (member month '(1 2)) (1- year) year))
        (m (1+ (mod (- month 3) 12))))
    (floor (ecase calendar
             (:gregorian (mod (+ day
                                 (- (* 2.6 m) 0.2)
                                 (* 5 (mod y 4))
                                 (* 4 (mod y 100))
                                 (* 6 (mod y 400)))
                              7))
             (:julian    (mod (+ day
                                 (- (* 2.6 m) 2.2)
                                 (* 5 (mod y 4))
                                 (* 3 (mod y 7)))
                              7))))))

(defun leap-year-p (calendar year)
  (ecase calendar
    (:gregorian (or (and (zerop (mod year 4))
                         (not (zerop (mod year 100))))
                    (zerop (mod year 400))))
    (:julian (zerop (mod year 4)))))

(defun days-in-month (month year calendar)
  (ecase (month-name month)
    (:january   31)
    (:february  (if (leap-year-p calendar year) 29 28))
    (:march     31)
    (:april     30)
    (:may       31)
    (:june      30)
    (:july      31)
    (:august    31)
    (:september 30)
    (:october   31)
    (:november  30)
    (:december  31)))

(defun months-per-row (screen-width)
  ;; 2 columns for each week, with 1 space separating weekdays
  (let ((month-width (+ (* 2 7) 6))
        (sep-width   2))
    (when (< screen-width month-width)
      (error "Screen is too narrow for a calendar."))
    (loop for calendar-width from month-width by (+ month-width sep-width)
       while (< calendar-width screen-width)
       for months-per-row from 1
       finally (return (min 12 months-per-row)))))

(defun sublists (list sublist-length &optional (pad nil padp))
  (let* ((result (loop while list collect
                      (loop while list
                         repeat sublist-length
                         collect (pop list))))
         (short (- sublist-length (length (first (last result))))))
    (when (and padp (not (zerop short)))
      (nconc (first (last result))
             (loop repeat short collect pad)))
    result))

(defun build-month (month year)
  (let ((calendar (if (< year 1752) :julian :gregorian)))
    (sublists
     (nconc (loop repeat (day-of-week 1 month year calendar) collect "  ")
            (loop for day from 1 to (days-in-month month year calendar)
               collecting (format nil "~2d" day)))
     7 "  ")))

(defun build-year (year)
  (loop for month in +months+ collecting (build-month month year)))

(defun print-weekdays (n-times)
  (let ((weekdays "Su Mo Tu We Th Fr Sa"))
    (loop repeat (1- n-times) do (format t "~a  " weekdays)
       finally (write-line weekdays))))

(defun print-month-names (month-names)
  ;; 20 = 7 days * 2 + 6 for 1 space between them
  (format t "~{~v:@<~:(~a~)~>~^  ~}~%"
          (loop for s in (mapcar #'string month-names)
             nconcing (list 20 s))))

(defun print-row-header (month-names)
  (print-month-names month-names)
  (print-weekdays    (length month-names)))

(defun print-month-days (months)
  (loop while (some #'identity months) do
       (format t "~{~{~2d~^ ~}~^  ~}~%"
               (loop for i below (length months)
                  collect (or (pop (nth i months))
                              (loop repeat 7 collect "  "))))))

(defun get-month ()
  (nth-value 4 (decode-universal-time (get-universal-time))))

(defun get-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defun print-year (year screen-width)
  (let* ((months-per-row (months-per-row screen-width))
         (calendar-width (+ (* months-per-row 20) (* (1- months-per-row) 2)))
         (month-names    (sublists +months+ months-per-row))
         (month-days     (sublists (build-year year) months-per-row)))
    (format t "~v:@<~a~>~2%" calendar-width year)
    (loop for name-row in month-names
       and days in month-days do
         (print-row-header name-row)
         (print-month-days days))))

(defun print-month (month year)
  (let ((month-name (month-name month)))
    (format t "~v:@<~:(~a~) ~a~>~%" 20 month-name year)
    (print-weekdays 1)
    (print-month-days (list (build-month month year)))))

(defun calendar (&key (screen-width 80)
                   (month (get-month) month-supplied-p)
                   (year  (get-year)  year-supplied-p))
  (if (and year-supplied-p (not month-supplied-p))
      (print-year year screen-width)
      (if (< screen-width 20)
          (error "Screen is too narrow for a calendar.")
          (print-month month year))))
