(defpackage :calendar
  (:use :cl)
  (:export :calendar))
(in-package :calendar)

(defconstant +months+ '(january february march april may june july
                        august september october november december))

(defun jan-1-day (year)
  ;; Later this will use Julian if the year is <= 1750
  (mod (+ 1
          (* 5 (mod (1- year) 4))
          (* 4 (mod (1- year) 100))
          (* 6 (mod (1- year) 400)))
       7))

(defun days-in-month (month year)
  (let ((leap-year-p (or (and (zerop (mod year 4))
                              (not (zerop (mod year 100))))
                         (zerop (mod year 400)))))
    (case month
      (january   31)
      (february  (if leap-year-p 29 28))
      (march     31)
      (april     30)
      (may       31)
      (june      30)
      (july      31)
      (august    31)
      (september 30)
      (october   31)
      (november  30)
      (december  31))))

;; this is ugly but it works and I'm tired
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
  (let* ((result (loop with list = (copy-list list)
                    while list collect
                      (loop while list
                         repeat sublist-length
                         collect (pop list))))
         (short (- sublist-length (length (first (last result))))))
    (when (and padp (not (zerop short)))
      (nconc (first (last result))
             (loop repeat short collect pad)))
    result))

(defun build-year (year)
  (loop for month in +months+
     for days-in-month = (days-in-month month year)
     and start = (jan-1-day year) then (mod (+ start days-in-month) 7)
     collecting
       (sublists (append (loop repeat start collect "  ")
                         (loop for day from 1 to days-in-month
                            collecting (format nil "~2d" day)))
                 7 "  ")))

(defun print-weekdays (month-names)
  (format t "~{~:[~;Su Mo Tu We Th Fr Sa~^  ~]~}~%" month-names))

(defun print-month-names (month-names)
  ;; 20 = 7 days * 2 + 6 for 1 space between them
  (format t "~{~20:@<~:(~a~)~>~^  ~}~%" (mapcar #'string month-names)))

(defun print-row-header (month-names)
  (print-month-names month-names)
  (print-weekdays    month-names))

(defun print-month-days (months)
  (loop while (some #'identity months) do
       (format t "~{~{~2d~^ ~}~^  ~}~%"
               (loop for i below (length months)
                  collect (or (pop (nth i months))
                              (loop repeat 7 collect "  "))))))

(defun calendar (year &optional (screen-width 80))
  (let* ((months-per-row (months-per-row screen-width))
         (calendar-width (+ (* months-per-row 20) (* (1- months-per-row) 2)))
         (month-names    (sublists +months+ months-per-row))
         (month-days     (sublists (build-year year) months-per-row)))
    (format t "~v:@<~a~>~2%" calendar-width year)
    (loop for name-row in month-names
       and days in month-days do
         (print-row-header name-row)
         (print-month-days days))))