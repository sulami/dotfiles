{:user {:plugins [[cider/cider-nrepl "0.21.1"]
                  [io.aviso/pretty "0.1.37"]]
        :dependencies [[org.clojars.sulami/prelude "0.2.0"]
                       [org.clojure/test.check "0.9.0"]
                       [cider/cider-nrepl "0.21.1"]
                       [io.aviso/pretty "0.1.37"]
                       [com.bhauman/rebel-readline "0.1.4"]]
        :middleware [cider-nrepl.plugin/middleware
                     io.aviso.lein-pretty/inject]

        :aliases {"rebl" ["run" "-m" "rebel-readline.main"]
                  "retl" ["trampoline" "run" "-m" "rebel-readline.main"]}

        :release-tasks [["vcs" "assert-committed"]
                        ["change" "version" "leiningen.release/bump-version" "release"]
                        ["vcs" "commit"]
                        ["vcs" "tag"]
                        ["deploy" "clojars"]]
        :deploy-repositories [["releases" {:url "https://repo.clojars.org"
                                           :creds :gpg}]]
        :signing {:gpg-key "0xA1418106"}}}
