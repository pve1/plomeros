(asdf:load-system :plomeros)

(in-package :plomeros)

(setf *plomeros-channels* '("#foobar123"))
(setf *plomeros-connect-args* '(:nickname "Plomeros"
                                :server "my.irc.server"))

(set-property :daily-set-db "/path/to/my/dailyset.db")
(pushnew 'plomeros-daily-set-hook *plomeros-hooks*)

(plomeros::start-plomeros)
