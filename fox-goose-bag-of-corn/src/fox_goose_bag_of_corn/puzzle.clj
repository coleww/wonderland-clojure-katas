(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])


(defn river-crossing-plan []

  [[[:fox :goose :corn :you] [:boat] []]
   [[:fox :corn] [:boat :you :goose] []]
   [[:fox :corn] [:boat] [:you :goose]]
   [[:fox :corn] [:boat :you] [:goose]]
   [[:fox :corn :you] [:boat] [:goose]]
   [[:fox] [:boat :corn :you] [:goose]]
   [[:fox] [:boat] [:goose :corn :you]]
   [[:fox] [:boat :you :goose] [:corn]]
   [[:fox :you :goose] [:boat] [:corn]]
   [[:goose] [:boat :fox :you] [:corn]]
   [[:goose] [:boat] [:corn :fox :you]]
   [[:goose] [:boat :you] [:corn :fox]]
   [[:goose :you] [:boat] [:corn :fox]]
   [[] [:boat :goose :you] [:corn :fox]]
   [[] [:boat] [:fox :goose :corn :you]]]
  )
