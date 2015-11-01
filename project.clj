(defproject elc5370 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [net.mikera/core.matrix "0.42.0"]
                 [net.mikera/vectorz     "0.51.0"]
                 [net.mikera/vectorz-clj "0.36.0"]
                 [clatrix "0.5.0"]
                 [incanter/incanter-charts "1.5.6"]
                 [incanter/incanter-core   "1.5.6"]]
  :resource-paths ["lib/commons-math3-3.5.jar"]
  :main elc5370.optimization)
