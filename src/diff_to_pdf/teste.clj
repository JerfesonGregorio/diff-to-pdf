(ns diff-to-pdf.teste
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumula.diff :as d]
            [clj-pdf.core :as pdf]))

(defn read-file-model [file-name]
  (slurp file-name))

(defn read-file-obligation [file-name]
  (slurp (io/reader file-name :encoding "ISO-8859-1")))     ; Leitura de arquivo em encoding para que o clj-pdf reconheça os símbolos estranhos

(def modelo (read-file-model "src/modelo.txt"))
(def obrigacao (read-file-obligation "src/obrigacao.txt"))

(def diffs (d/diff modelo obrigacao ::d/cleanup ::d/cleanup-semantic))


;----------------------------------------------------------------------------------------------------

(defn operation-background-color [operation]
  (case operation
    :plumula.diff/delete [255 230 230 :red]                 ; vermelho claro para delete (background)
    :plumula.diff/insert [230 255 230 :green]               ; verde claro para insert (background)
    [255 255 255]))                                         ; branco como padrão (background)

(defn operation-text-color [operation]
  (case operation
    :plumula.diff/delete [255 0 0]                          ; vermelho para delete (cor da fonte)
    :plumula.diff/insert [0 128 0]                          ; verde para insert (cor da fonte)
    [0 0 0]))                                               ; preto como padrão (cor da fonte)


(defn count-newlines [s]
  (count (re-seq #"(?m)^\|[a-zA-Z0-9]\d{3}\|" s)))

(defn count-inserted-lines [diffs]
  (reduce + 0 (for [text diffs]
                (count-newlines text))))

(defn count-deleted-lines [diffs]
  (reduce + 0 (for [text diffs]
                (count-newlines text))))

(defn calculate-line-difference [insert delete]
  (- (count-inserted-lines insert) (count-deleted-lines delete)))

(defn diff-insert [diffs]
  (let [combined-chunks (for [{:keys [plumula.diff/operation plumula.diff/text]} diffs]
                          (case operation
                            :plumula.diff/equal [:chunk text]
                            :plumula.diff/insert [:chunk {:background (operation-background-color operation)
                                                          :color      (operation-text-color operation)} text]
                            :plumula.diff/delete nil))]
    (concat [[:paragraph combined-chunks]])))               ; Retorna o trecho original e o que foi inserido (diff verde)


(def abs (atom 0))


(defn incrementar-atom []
  (swap! abs inc)
  nil)

(reset! abs 0)

@abs

(defn diff-delete [diffs]
  (let [combined-chunks (for [{:keys [plumula.diff/operation plumula.diff/text]} diffs]
                          (case operation
                            :plumula.diff/equal [:chunk text]
                            :plumula.diff/delete [:chunk {:background (operation-background-color operation)
                                                          :color      (operation-text-color operation)} text]
                            :plumula.diff/insert (do [:chunk {:background [0 0 0]} ] (incrementar-atom)) ))] ; Local em que a lógica de novas linhas será utilizada
    (concat [[:paragraph combined-chunks]])))               ; Retorna o trecho original e o que foi deletado (diff vermelho)

(defn format! [arg]
  (mapcat identity (into [] arg)))                          ; Transforma a lazy-seq em um vetor e reduz um nível de aninhamento para que a lib clj-pdf possa reconhecê-lo como um parágrafo


(defn diff-to-pdf [model obligation title file-name]
  (pdf/pdf [
            {:title       title
             :orientation :landscape
             :size        :a1}                              ; Configurações da folha do arquivo pdf
            [:table {:header ["Modelo" "Obrigação"]}        ; Definição de colunas: essa configuração é obrigatória, pois é ela que define as duas colunas da tabela para os diffs
             [[:cell (format! (diff-delete model))]]
             [[:cell (format! (diff-insert obligation))]]]]
           file-name))




(diff-to-pdf diffs diffs "sped-fical" "diff_sped-fical.pdf")






(def lazy-seq-example
  (lazy-seq (range 10)))

(def result1 (doall lazy-seq-example))                      ; Consumindo a sequência pela primeira vez
(def result2 (doall lazy-seq-example))






















(comment
  (type (into [] (diff-insert diffs)))
  (diff-delete diffs)
  (into [] (diff-delete diffs))
  (count-newlines "\n|c222|")

  (defn count-pattern [s]
    (count (re-seq #"(?m)^\|[a-zA-Z0-9]\d{3}\|" s)))

  (def input-string "

")

  (count-pattern input-string)

  (count-deleted-lines insert)
  (count-inserted-lines insert)
  (calculate-line-difference m o)
  )