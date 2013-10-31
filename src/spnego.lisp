(in-package :hunchentoot-single-signon)

(defun spnego-auth (body-handler-fn &optional failed-auth-fn)
  "Performs SPNEGO authentication. This function is intended to be
called from a Hunchentoot handler, and the implementation assumes that
no further Hunchentoot-related processing will take place after this
call.

BODY-HANDLER-FN is a function that will be called after successful
authentication. It is called with an instance of CL-GSS:NAME
representing the identify of the user.

If given, FAILED-AUTH-FN is a function that will be called when
authentication fails. Note that before this function is called, the
return code has already been set to the correct value and should not
be changed. The purpose of this function is to provide a more useful
error message, and also possibly display a normal login and password
form to provide some alternate login method."
  (labels ((failed-auth ()
             (setf (hunchentoot:return-code*) hunchentoot:+http-authorization-required+)
             (if failed-auth-fn
                 (funcall failed-auth-fn)
                 "Authentication needed")))
    (let ((auth (hunchentoot:header-in* :authorization)))
      (if auth
          ;; The client supplied the 'Authorization' header
          (let ((parts (split-sequence:split-sequence #\Space auth)))
            (unless (and (= (length parts) 2)
                         (string= (string-downcase (car parts)) "negotiate"))
              (error "Can't parse result of 'Authorization' header"))
            (let ((buf (cl-base64:base64-string-to-usb8-array (cadr parts)))
                  (context (hunchentoot:session-value 'spnego-context)))
              (multiple-value-bind (continue-needed context-ret name buffer)
                  (cl-gss:accept-sec buf :context context)
                (unless context
                  (setf (hunchentoot:session-value 'spnego-context) context-ret))
                (when buffer
                  (setf (hunchentoot:header-out "WWW-Authenticate")
                        (format nil "Negotiate ~a"
                                (cl-base64:usb8-array-to-base64-string buffer))))
                (cond (continue-needed
                       (failed-auth))
                      (t
                       (setf (hunchentoot:session-value 'spnego-context) nil)
                       (funcall body-handler-fn name))))))
          ;; ELSE The client did not supply the 'Authorization' header
          (progn
            (setf (hunchentoot:header-out "WWW-Authenticate") "Negotiate")
            (failed-auth))))))
