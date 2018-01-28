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

(defun example-simple-tree-view ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Database Viewer"
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 300
                                 :default-height 200))
          (view (exe-query (db-connect "test" "nobody" "1234") "SELECT * FROM hello")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window view)
      (gtk-widget-show-all window))))

(example-simple-tree-view)
