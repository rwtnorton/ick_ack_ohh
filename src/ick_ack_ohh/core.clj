(ns ick-ack-ohh.core)

(defn -main [& args]
  (doseq [s ["Ick!" "Ack!!" "Ohh..."]]
    (println s)))
