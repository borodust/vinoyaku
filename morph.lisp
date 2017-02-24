(in-package :vinoyaku)


(define-constant +kanji-kana-table+
    (plist-hash-table
     '(#\ア #\あ #\イ #\い #\ウ #\う #\エ #\え #\オ #\お
       #\カ #\か #\キ #\き #\ク #\く #\ケ #\け #\コ #\こ
       #\サ #\さ #\シ #\し #\ス #\す #\セ #\せ #\ソ #\そ
       #\タ #\た #\チ #\ち #\ツ #\つ #\テ #\て #\ト #\と
       #\ナ #\な #\ニ #\に #\ヌ #\ぬ #\ネ #\ね #\ノ #\の
       #\ハ #\は #\ヒ #\ひ #\フ #\ふ #\ヘ #\へ #\ホ #\ほ
       #\マ #\ま #\ミ #\み #\ム #\む #\メ #\め #\モ #\も
       #\ヤ #\や #\ユ #\ゆ #\ヨ #\よ
       #\ラ #\ら #\リ #\り #\ル #\る #\レ #\れ #\ロ #\ろ
       #\ワ #\わ #\ヲ #\を
       #\ン #\ん
       #\ャ #\ゃ #\ュ #\ゅ #\ョ #\ょ
       #\ッ #\っ

       #\ガ #\が #\ギ #\ぎ #\グ #\ぐ #\ゲ #\げ #\ゴ #\ご
       #\ザ #\ざ #\ジ #\じ #\ズ #\ず #\ゼ #\ぜ #\ゾ #\ぞ
       #\ダ #\だ #\ヂ #\ぢ #\ヅ #\づ #\デ #\で #\ド #\ど
       #\バ #\ば #\ビ #\び #\ブ #\ぶ #\ベ #\べ #\ボ #\ぼ
       #\パ #\ぱ #\ピ #\ぴ #\プ #\ぷ #\ペ #\ぺ #\ポ #\ぽ)
     :test #'equal)
  :test #'equalp)


(defun katakana->hiragana (text)
  (loop with result = (make-array (length text) :element-type (array-element-type text))
     for i from 0
     for ch across text
     do (setf (aref result i) (gethash ch +kanji-kana-table+ ch))
     finally (return result)))


(defun extract-syllabograms (surface feature-string)
  (log:trace "~A: ~A" surface feature-string)
  (loop for i from 0 below 7
     for start-idx = (position #\, feature-string) then
       (position #\, feature-string :start (1+ start-idx))
     while start-idx
     finally (return
               (if start-idx
                   (let ((end-idx (or (position #\, feature-string :start (1+ start-idx))
                                      (length feature-string))))
                     (katakana->hiragana
                      (make-array (- end-idx start-idx 1)
                                  :element-type (array-element-type feature-string)
                                  :displaced-to feature-string
                                  :displaced-index-offset (1+ start-idx))))
                   surface))))


(defun syllabograms (text)
  (with-output-to-string (result)
    (let* ((model (%mecab:model-new2 ""))
           (lattice (%mecab:model-new-lattice model))
           (tagger (%mecab::model-new-tagger model)))
      (cffi:with-foreign-string (foreign-string text)
        (%mecab:lattice-set-sentence lattice foreign-string)
        (%mecab:parse-lattice tagger lattice)
        (let ((head (%mecab:lattice-get-bos-node lattice)))
          (loop for node-ptr = (autowrap:ptr head) then (c-ref node-ptr %mecab:node-t :next)
             until (cffi:null-pointer-p node-ptr)
             do (c-let ((node %mecab:node-t :from node-ptr))
                  (switch ((node :stat) :test #'=)
                    (%mecab:+bos-node+)
                    (%mecab:+eos-node+)
                    (t (let ((syllabograms (extract-syllabograms
                                            (cffi:foreign-string-to-lisp (node :surface * &)
                                                                         :count (node :length))
                                            (node :feature))))
                             (write-sequence syllabograms result))))))))
      (%mecab:destroy tagger)
      (%mecab:lattice-destroy lattice)
      (%mecab:model-destroy model))))
