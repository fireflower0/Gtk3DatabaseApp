(ql:quickload '(:cl-dbi)  :silent t)
(ql:quickload '(:cl-cffi-gtk) :silent t)

(defpackage :cl-gtk3db-app
  (:use :cl
        :cl-dbi
        :gtk
        :gdk
        :gdk-pixbuf
        :gobject
        :glib
        :gio
        :pango
        :cairo))

(in-package :cl-gtk3db-app)

(defun db-connect (db-name usr-name db-pass)
  (connect :postgres
           :database-name db-name
           :username usr-name
           :password db-pass))

(defun get-types (data)
  (let ((type-list '()))
    (dolist (n data)
      (cond ((numberp n)
             (setf type-list (append type-list (list "guint"))))
            ((stringp n)
             (setf type-list (append type-list (list "gchararray"))))))
    type-list))

(defun set-head (view head-list)
  (let ((renderer (gtk-cell-renderer-text-new))
        (len (length head-list))
        column)
    (dotimes (n len)
      (setf column (gtk-tree-view-column-new-with-attributes (symbol-name (nth n head-list)) renderer "text" n))
      (gtk-tree-view-append-column view column))))

(defun set-data (model data-list)
  (let ((iter (gtk-list-store-append model))
        (len (length data-list)))
    (dotimes (n len)
      (gtk-list-store-set-value model iter n (nth n data-list)))))

;; Query
(defun exe-query (con sql)
  (let* ((query  (prepare con sql))
         (result (execute query))
         head-list data-list
         (flg 0)
         model view len)
    (loop for row = (fetch result)
          while row
          do (progn
               (setf head-list '())
               (setf data-list '())
               (setf len (length row))
               ;; Get list head and data
               (dotimes (n len)
                 (if (symbolp (nth n row))
                     (setf head-list (append head-list (list (nth n row))))
                     (setf data-list (append data-list (list (nth n row))))))
               (if (= flg 0)
                   ;; Create Header and insert first data.
                   (progn
                     (setf model (make-instance 'gtk-list-store :column-types (get-types data-list)))
                     (setf view  (make-instance 'gtk-tree-view :model model))
                     (set-head view head-list)
                     (set-data model data-list)
                     (setf flg 1))
                   ;; Insert data
                   (set-data model data-list))))
    view))

(defun main ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Database Viewer"
                                 :type :toplevel
                                 :border-width 12))
          (db-name-label   (make-instance 'gtk-label :label "DB Name:"))
          (user-name-label (make-instance 'gtk-label :label "User Name:"))
          (password-label  (make-instance 'gtk-label :label "Password:"))
          (db-name-entry   (make-instance 'gtk-entry))
          (user-name-entry (make-instance 'gtk-entry))
          (password-entry  (make-instance 'gtk-entry))
          (connect-button  (make-instance 'gtk-button :label "Connection"))
          (sql-label       (make-instance 'gtk-label  :label "SQL:"))
          (sql-entry       (make-instance 'gtk-entry))
          (query-button    (make-instance 'gtk-button :label "Query"))
          (view            (make-instance 'gtk-tree-view))
          (table (make-instance 'gtk-table :n-rows 7 :n-columns 4))
          (connection nil))
      ;; Define the signal handlers
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect connect-button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (setf connection (db-connect (gtk-entry-text db-name-entry)
                                                       (gtk-entry-text user-name-entry)
                                                       (gtk-entry-text password-entry)))
                          (setf view (exe-query connection "SELECT relname as TABLE_NAME FROM pg_stat_user_tables"))
                          (gtk-table-attach table view 0 7 3 4)
                          (gtk-widget-show-all window)))
      (g-signal-connect query-button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (if (null connection)
                              (format t "No connection.")
                              (progn
                                (setf view (exe-query connection (gtk-entry-text sql-entry)))
                                (gtk-table-attach table view 0 7 3 4)
                                (gtk-widget-show-all window)))))
      ;; Put the widgets into the table
      ;;                Row
      ;;     0   1   2   3   4   5   6   7
      ;;   0 +---+---+---+---+---+---+---+
      ;;     |   |   |   |   |   |   |   |
      ;;   1 +---+---+---+---+---+---+---+
      ;;     |   |   |   |   |   |   |   |
      ;; C 2 +---+---+---+---+---+---+---+
      ;; o   |   |   |   |   |   |   |   |
      ;; l 3 +---+---+---+---+---+---+---+
      ;;     |   |   |   |   |   |   |   |
      ;;   4 +---+---+---+---+---+---+---+
      ;;                                      R1 R2 C1 C2
      (gtk-table-attach table db-name-label    0  1  0  1)
      (gtk-table-attach table user-name-label  2  3  0  1)
      (gtk-table-attach table password-label   4  5  0  1)
      (gtk-table-attach table db-name-entry    1  2  0  1)
      (gtk-table-attach table user-name-entry  3  4  0  1)
      (gtk-table-attach table password-entry   5  6  0  1)
      (gtk-table-attach table connect-button   6  7  0  1)
      (gtk-table-attach table sql-label        0  1  2  3)
      (gtk-table-attach table sql-entry        1  6  2  3)
      (gtk-table-attach table query-button     6  7  2  3)
      (gtk-table-attach table view             0  7  3  4)
      ;; Put the table into the window
      (gtk-container-add window table)
      ;; Show the window
      (gtk-widget-show-all window))))

;; Execution!
(main)
