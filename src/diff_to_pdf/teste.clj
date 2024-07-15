(ns diff-to-pdf.teste
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumula.diff :as d]
            [clj-pdf.core :as pdf]))

(defn read-file-model [file-name]
  (slurp file-name))

(defn read-file-obligation [file-name]
  (slurp (io/reader file-name :encoding "ISO-8859-1"))) ; Leitura de arquivo em encoding para que o clj-pdf reconheça os símbolos estranhos

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


(defn diff-insert [diffs]
  (let [combined-chunks (for [{:keys [plumula.diff/operation plumula.diff/text]} diffs]
                          (case operation
                            :plumula.diff/equal [:chunk text]
                            :plumula.diff/insert [:chunk {:background (operation-background-color operation)
                                                          :color      (operation-text-color operation)} text]
                            :plumula.diff/delete nil))]
    (concat [[:paragraph combined-chunks]])))               ; Retorna o trecho original e o que foi inserido (diff verde)




(defn count-newlines [s]
    (re-matches #"\n\|[a-zA-Z0-9]\d{3}\|" s)) ; Padrão a identificar para inserção de linhas em branco




(defn diff-delete [diffs]
  (let [combined-chunks (for [{:keys [plumula.diff/operation plumula.diff/text]} diffs]
                          (case operation
                            :plumula.diff/equal [:chunk text]
                            :plumula.diff/delete [:chunk {:background (operation-background-color operation)
                                                          :color      (operation-text-color operation)} text]
                            :plumula.diff/insert [:chunk {:background [0 0 0]} (apply str (repeat (count-newlines text) "\n"))]))] ; Local em que a lógica de novas linhas será utilizada
    (concat [[:paragraph combined-chunks]])))               ; Retorna o trecho original e o que foi deletado (diff vermelho)

(defn format! [arg]
  (mapcat identity (into [] arg))) ; Transforma a lazy-seq em um vetor e reduz um nível de aninhamento para que a lib clj-pdf possa reconhecê-lo como um parágrafo


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



(comment
  (type (into [] (diff-insert diffs)))
  (diff-delete diffs)
  (into [] (diff-delete diffs))
  (count-newlines "\n|c222|")
  )