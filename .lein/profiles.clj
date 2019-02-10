{:user {:plugins [[io.aviso/pretty "0.1.37"]
                  [cider/cider-nrepl "0.20.0"]
                  [lein-kibit "0.1.6"]]
        :dependencies [[org.clojure/test.check "0.9.0"]
                       [cider/cider-nrepl "0.20.0"]
                       [io.aviso/pretty "0.1.37"]
                       [com.bhauman/rebel-readline "0.1.4"]
                       [com.bhauman/figwheel-main "0.1.9"]
                       [criterium "0.4.4"]]
        :middleware [cider-nrepl.plugin/middleware
                     io.aviso.lein-pretty/inject]

        :aliases {"rebl" ["run" "-m" "rebel-readline.main"]
                  "retl" ["trampoline" "run" "-m" "rebel-readline.main"]}

        :release-tasks [["vcs" "assert-committed"]
                        ["change" "version" "leiningen.release/bump-version" "release"]
                        ["vcs" "commit"]
                        ["vcs" "tag"]
                        ["deploy"]
        :deploy-repositories [["releases" {:url "https://repo.clojars.org"
                                           :creds :gpg}]]
        :signing {:gpg-key "0xA1418106"}}}
