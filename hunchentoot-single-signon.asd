(asdf:defsystem hunchentoot-single-signon
  :name "hunchentoot-single-signon"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Implementation of SPNEGO authentication for Hunchentoot"
  :depends-on (:hunchentoot
               :cl-gss
               :split-sequence
               :cl-base64)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "spnego")))))
