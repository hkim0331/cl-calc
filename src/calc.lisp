(in-package :cl-user)
(defpackage calc
  (:use :cl :hunchentoot :cl-who))
(in-package :calc)

(defvar *value* 0)
(defvar *display* 0)
(defvar *stack* nil)
(defvar *operands* 0)
(defvar *http*)

(setf (html-mode) :html5)

;; FIXME: no effect.
(defun static-files ()
  (push (create-static-file-dispatcher-and-handler
         "/calc.css" "static/calc.css") *dispatch-table*))

(defun start-server (&optional (port 8080))
  ;; (setf *http* (make-instance 'easy-acceptor :port port
  ;;                             :document-root #p "static/"))
  (static-files)
  (setf *http* (make-instance 'easy-acceptor :port port))
  (start *http*))

(defun stop-server ()
  (stop *http*))

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html
      :lang "ja"
      (:head
       (:meta
        :charset "utf-8")
       (:meta
        :http-equiv "X-UA-Compatible"
        :content "IE=edge")
       (:meta
        :name "viewport"
        :content "width=device-width, initial-scale=1.0")
       (:link
        :rel "stylesheet"
        :href "/calc.css")
       (:link
        :rel "stylesheet"
        :href "//netdna.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
       (:title ,title))
      (:body
       (:div :class "container"
        ,@body
        (:hr)
        (:span "programmed by hkimura."))))))

;; FIXME, buttons. もっと抽象化のレベルを上げよう。
(defmacro digit-button (value)
  `(htm (:form :action "/digit" :method "post"
               (:input :class "btn btn-primary"
                       :type "submit" :name "name" :value ,value))))

(define-easy-handler (digit :uri "/digit") (name)
  (setf *value* (+ (* 10 *value*) (parse-integer name)))
  (setf *display* *value*)
  (redirect "/index"))

(defmacro push-button ()
    `(htm (:form :action "/push" :method "post"
               (:input :class "btn btn-danger"
                       :type "submit" :value "push"))))

(define-easy-handler (c-push :uri "/push") ()
  (push *value* *stack*)
  (setf *display* *value*)
  (setf *value* 0)
  (incf *operands*)
  (redirect "/index"))

(defmacro clear-button ()
  `(htm (:form :action "/clear" :method "post"
               (:input :class "btn btn-warning"
                       :type "submit" :value "C"))))

(defmacro reset-button ()
  `(htm (:form :action "/reset" :method "post"
               (:input :class "btn btn-warning"
                       :type "submit" :value "R"))))

(define-easy-handler (clear :uri "/clear") ()
  (setf *value* 0)
  (setf *display* *value*)
  (redirect "/index"))

(defmacro op-button (value)
    `(htm (:form :action "/op" :method "post"
               (:input :class "btn btn-info"
                       :type "submit" :name "name" :value ,value))))

(defmacro sign-button ()
  `(htm (:form :action "/sign" :method "post"
               (:input :class "btn btn-info"
                       :type "submit" :name "name" :value "+/-"))))

(define-easy-handler (sign :uri "/sign") ()
  (setf *value* (* -1 *value*)
        *display* (* -1 *display*))
  (redirect "/index"))

(define-easy-handler (op :uri "/op") (name)
  (if (> *operands* 1)
      (let* ((arg2 (pop *stack*))
             (arg1 (pop *stack*))
             (value (cond
                      ((string= name "+") (+ arg1 arg2))
                      ((string= name "-") (- arg1 arg2))
                      ((string= name "*") (* arg1 arg2))
                      ((string= name "/") (/ arg1 arg2))
                      (t (error "error")))))
        (push value *stack*)
        (setf *display* value)
        (setf *value* 0)
        (decf *operands*))
      (setf *display* "forget push?"))
  (redirect "/index"))

(define-easy-handler (c-reset :uri "/reset") ()
  (setf *stack* nil)
  (setf *value* 0)
  (setf *display* 0)
  (setf *operands* 0)
  (redirect "/index"))

(define-easy-handler (calc :uri "/index") ()
    (standard-page
        (:title "calc")
      (:h1 "reverse polish calculator")
      (:input :type "text" :id "calc" :value (str *display*))
      (:br)
      (:br)
      (:table
       (:tr (dolist (i '(0 1 2 3 4 5 6 7 8 9))
              (htm (:td (digit-button i))))
            (:td (push-button)))
       (:tr (dolist (i '(:+ :- :* :/))
              (htm (:td (op-button i)))))
       (:tr (:td (clear-button)) (:td (reset-button))))
      (:table
       (:tr (:td (sign-button))))
      (:hr)
      (:ul
       (:li "C ... 入力中の数をクリアする。")
       (:li "R ... 計算を初期化する。"))))

(defun main ()
  (start-server)
  (loop (sleep 60)))
