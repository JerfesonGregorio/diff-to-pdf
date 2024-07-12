(ns diff_to_pdf.core
  (:require [clj-http.client :as client])
  (:require [cheshire.core :as json]))

(defn diff-check
  [texto1 texto2]
  (let [response (client/post "https://api.diffchecker.com/public/text?output_type=html&email=pro.jerfeson@gmail.com"
                              {:headers {"Content-Type" "application/json"}
                               :body    (json/generate-string {"left"       texto1
                                                               "right"      texto2
                                                               "diff_level" "word"})})]
    (if (= 200 (:status response))
      (:body response))))

(defn read-file [file-name]
  (slurp file-name))

(def txt1 (read-file "src/obrigacao.txt"))

(def txt2 (read-file "src/modelo.txt"))

;; Exemplo de uso:

#_(spit "target/saida.json" (diff-check txt1 txt2))

(spit "./index.html" (diff-check txt2 txt1))

#_(spit "target/saida_html-json.json" (diff-check txt1 txt2))
