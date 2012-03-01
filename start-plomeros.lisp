(asdf:load-system :plomeros)

(in-package :plomeros)

(setf *plomeros-channels* '("#foobar123"))
(setf *plomeros-connect-args* '(:nickname "Plomeros"
                                :server "my.irc.server"))

(plomeros::start-plomeros)
