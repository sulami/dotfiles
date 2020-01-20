{:user {:pedantic? :ranges

        :release-tasks [["vcs" "assert-committed"]
                        ["change" "version" "leiningen.release/bump-version" "release"]
                        ["vcs" "commit"]
                        ["vcs" "tag"]
                        ["deploy" "clojars"]]

        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]

        :deploy-repositories [["releases" {:url "https://repo.clojars.org"
                                           :creds :gpg}]]
        :signing {:gpg-key "0xA1418106"}}

 :repl {:dependencies [[com.bhauman/rebel-readline "0.1.4"]]
        :aliases {"rebl" ["run" "-m" "rebel-readline.main"]
                  "retl" ["trampoline" "run" "-m" "rebel-readline.main"]}}

 :pretty {:plugins [[io.aviso/pretty "0.1.37"]]
          :dependencies [[io.aviso/pretty "0.1.37" :exclusions [org.clojure/clojure]]]
          :middleware [io.aviso.lein-pretty/inject]}

 :emacs {:plugins [[cider/cider-nrepl "0.22.0"]]
         :dependencies [[cider/cider-nrepl "0.22.0"]]
         :middleware [cider-nrepl.plugin/middleware]}

 :sulami {:dependencies [[org.clojars.sulami/prelude "0.2.0"]]}}
