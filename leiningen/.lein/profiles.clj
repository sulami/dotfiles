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
                       [com.gfredericks/user.clj "RELEASE"]
                       [org.clojure/core.async "RELEASE"]
                       [org.clojure/core.logic "RELEASE"]
                       [org.clojure/test.check "RELEASE"]
                       [com.clojure-goes-fast/clj-java-decompiler "RELEASE"]
                       [com.clojure-goes-fast/clj-memory-meter "RELEASE"]
                       [com.clojure-goes-fast/clj-async-profiler "RELEASE"]
                       [com.clojure-goes-fast/jvm-alloc-rate-meter "RELEASE"]
                       [com.clojure-goes-fast/jvm-hiccup-meter "RELEASE"]
                       [io.aviso/pretty "RELEASE"]
                       [mvxcvi/puget "RELEASE"]
                       [com.hypirion/clj-xchart "RELEASE"]
                       [org.clojure/clojure "RELEASE"]
                       [generateme/fastmath "RELEASE"]
                       [clj-commons/pomegranate "RELEASE"]]
        :aliases {"rebl" ["run" "-m" "rebel-readline.main"]
                  "retl" ["trampoline" "run" "-m" "rebel-readline.main"]}}

 :pretty {:plugins [[io.aviso/pretty "RELEASE"]]
          :dependencies [[io.aviso/pretty "RELEASE" :exclusions [org.clojure/clojure]]]
          :middleware [io.aviso.lein-pretty/inject]}

 :bench {:dependencies [[criterium "RELEASE"]]}

 :emacs {:plugins [[cider/cider-nrepl "RELEASE"]]
         :dependencies [[cider/cider-nrepl "RELEASE"]]
         :middleware [cider-nrepl.plugin/middleware]}

 :sulami {:dependencies [[org.clojars.sulami/prelude "RELEASE"]]}}
