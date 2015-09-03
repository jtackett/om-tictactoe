(ns ^:figwheel-always om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.data :as data]
            [clojure.string :as string]
            [re-com.core :refer [button
                                 info-button
                                 input-text
                                 input-textarea
                                 datepicker-dropdown
                                 v-box
                                 hyperlink
                                 h-box
                                 gap
                                 throbber
                                 checkbox
                                 input-time
                                 title
                                 label
                                 box
                                 scroller
                                 progress-bar
                                 md-icon-button]]))

(enable-console-print!)

;; -- Utils -------------------------------------------------------------------

(defn make-new-board
  "A method to make a new board for the beginning of games"
  []
  (mapv #(identity {:position %})
        (range 1 10)))

(defn split-board-rows
  "A simple helepr method to split the board into rows.
  This method could be inline, but it is more readable if
  I give it a good name in larger functions"
  [board]
  (partition 3 board))

;; ----------------------------------------------------------------------------

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:game-board (make-new-board)
                          :whos-turn "X"
                          :nav-panel :current-game
                          :old-games []
                          :undo-position nil
                          :ai? false
                          :x-wins 0
                          :o-wins 0}))

;;; ====================================================================================

;; Check Winner
;;; ====================================================================================

;; Board checking
;; =================================================================================
(defn- check-horizontal [board]
  (let [winner (for [x [0 3 6]]
                 (let [f (get board x)]
                   (if (and (= f (get board (+ x 1)))
                            (= f (get board (+ x 2))))
                     f "")))]
    (remove empty? winner)))

(defn- check-vertical [board]
  (let [winner (for [x [0 1 2]]
                 (let [f (get board x)]
                   (if (and (= f (get board (+ x 3)))
                            (= f (get board (+ x 6))))
                     f "")))]
    (remove empty? winner)))

(defn- check-diaganal [board]
  (let [center (get board 4)]
    (cond
     (and (= center (get board 0)) (= center (get board 8)))
     [center]

     (and (= center (get board 2)) (= center (get board 6)))
     [center]

     :default nil)))

(defn check-winner
  "A set of methods which require the board as a vector of
  player symbols or nils"
  [board]
  (first (concat (check-horizontal board)
                 (check-vertical board)
                 (check-diaganal board))))

(defn board-full?
  "Checks to see if the board is full given a vector of maps
  which is the standard representation for the board in this application"
  [board]
  (if (some nil? (mapv :symbol board))
    false
    (do (js/alert "The Game is a DRAW!!! Please lower your weapons")
      true)))

;; And this is as far as I made it...
(defn next-ai-move [board]
  (let [rows (split-board-rows board)]

    ))

(defn check-board
  "Check to see if the board is full, or if there is winner
  Returns the old board for things to continue or a fresh board"
  [data board]
  (let [formatted-board (mapv :symbol board)]
    ;; Checks if board is full
    (if (board-full? board)
      (om/transact! data :game-board
                    #(make-new-board))
      ;; Check for a winner
      (if-let [winner (check-winner formatted-board)]
        (do
          (js/alert (str "The WINNER is....." winner "!!!"))
          (om/transact! data :game-board
                        #(make-new-board)))
        board))))

;;; ====================================================================================
(defn update-board
  "Handles the logic for completing a move and switching players"
  [board-position players-turn game-board]
  (mapv #(if (= board-position (:position %))
           (assoc % :symbol players-turn)
           %)
        game-board))

(defn move
  "The function which executes a move"
  [{:keys [whos-turn] :as data} position]

  ;; Complete move
  (let [new-board (:game-board
                   @(om/transact! data :game-board
                                  #(update-board position whos-turn %)))]

    ;; Check for winner
    (check-board data new-board)

    ;; Switch player turn
    (om/transact! data :whos-turn
                  #(if (= "X" %)
                     "O"
                     "X"))))

(defn current-game [data owner]
  (reify
    om/IRender
    (render [this]
            (dom/div #js {:id "current-game"}
                     (dom/h2 nil (str "Whoever is " (:whos-turn data) "'s You're up!!!"))
                     (dom/table
                      nil
                      (apply dom/tbody nil
                             (for [r (split-board-rows (:game-board data))]
                               (apply dom/tr nil
                                      (for [c r]
                                        (dom/td nil
                                                (if-let [s (:symbol c)]
                                                  (dom/button #js {:onClick #(js/alert "You can't go there")
                                                                   :style #js {:background-color "#373737"
                                                                               :color "#FFF"
                                                                               :height "100px"
                                                                               :width "100px"}}
                                                              s)

                                                  (dom/button #js {:onClick #(move data (:position c))
                                                                   :style #js {:height "100px"
                                                                               :width "100px"}}
                                                              "Open"))))))))))))


(om/root current-game app-state
         {:target (. js/document (getElementById "tictactoe"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

