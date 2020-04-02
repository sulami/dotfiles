{:user {:pedantic? :ranges

        :release-tasks [["vcs" "assert-committed"]
                        ["change" "version" "leiningen.release/bump-version" "release"]
                        ["vcs" "commit"]
                        ["vcs" "tag"]
                        ["deploy" "clojars"]]

        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"
                   ;; For inf-clojure, start a socket REPL (not nREPL) on the server.
                   "-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]

        :deploy-repositories [["releases" {:url "https://repo.clojars.org"
                                           :creds :gpg}]]
        :signing {:gpg-key "0xA1418106"}}

 :repl {:dependencies [[com.bhauman/rebel-readline "RELEASE"]
                       [org.clojure/core.async "RELEASE"]
                       [org.clojure/core.logic "RELEASE"]
                       [org.clojure/test.check "RELEASE"]
                       [criterium "RELEASE"]]
        :aliases {"rebl" ["run" "-m" "rebel-readline.main"]
                  "retl" ["trampoline" "run" "-m" "rebel-readline.main"]}}

 :pretty {:plugins [[io.aviso/pretty "RELEASE"]]
          :dependencies [[io.aviso/pretty "RELEASE" :exclusions [org.clojure/clojure]]]
          :middleware [io.aviso.lein-pretty/inject]}

 :emacs {:plugins [[cider/cider-nrepl "RELEASE"]]
         :dependencies [[cider/cider-nrepl "RELEASE"]]
         :middleware [cider-nrepl.plugin/middleware]}

 :sulami {:dependencies [[org.clojars.sulami/prelude "RELEASE"]]}}
