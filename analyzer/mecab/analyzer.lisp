(cl:defpackage :vinoyaku.analyzer.mecab
  (:use :cl :vinoyaku.api))
(cl:in-package :vinoyaku.analyzer.mecab)


(alexandria:define-constant +kanji-kana-table+
    (alexandria:plist-hash-table
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


(defclass morph ()
  ((source-form :initarg :source-form :reader source-form)
   (part-of-speech :initarg :part-of-speech :reader part-of-speech)
   (original-form :initarg :original-form :reader original-form)
   (pronunciation :initarg :pronunciation :reader pronunciation)))


(defmethod print-object ((this morph) stream)
  (format stream "#<morph ~A, ~A, ~A, ~A>"
          (source-form this)
          (pronunciation this)
          (part-of-speech this)
          (original-form this)))


(defun select-part-of-speech (part-of-speech)
  (bodge-util:switch (part-of-speech :test #'equal)
    ("助詞" :particle)
    ("名詞" :noun)
    ("代名詞" :pronoun)
    ("形容詞" :adjective)
    ("冠詞" :article)
    ("数詞" :numeral)
    ("動詞" :verb)
    ("代動詞" :aux-verb)
    ("助動詞" :modal-verb)
    ("副詞" :adverb)
    ("記号" :sign)
    ("前置詞" :preposition)
    ("接続詞" :conjunction)
    ("間投詞" :interjection)
    ("感動詞" :interjection)
    ("関係詞" :relative)
    ("疑問詞" :interrogative)
    ("限定詞" :determinative)
    (t part-of-speech)))


(defun make-morph (morph-node)
  (flet ((%normalize (string)
           (let ((trimmed (string-trim '(#\Space #\Newline #\Tab) string)))
             (unless (equalp "*" trimmed)
               trimmed))))
    (claw:c-val ((morph-node %mecab:node-t))
      (let* ((surface (cffi:foreign-string-to-lisp
                       (morph-node :surface) :count (morph-node :length)))
             (feature-string (cffi:foreign-string-to-lisp (morph-node :feature))))
        (destructuring-bind (&optional part-of-speech
                               classification-1 classification-2 classification-3
                               usage-type conjugation original-form
                               pronunciation-1 pronunciation-2)
            (mapcar #'%normalize (bodge-util:split-sequence #\, feature-string))
          (declare (ignorable part-of-speech classification-1 classification-2 classification-3
                              usage-type conjugation original-form pronunciation-1 pronunciation-2))
          (make-instance 'morph :source-form surface
                                :part-of-speech (select-part-of-speech part-of-speech)
                                :pronunciation (when pronunciation-1
                                                 (katakana->hiragana pronunciation-1))
                                :original-form original-form))))))


(register-analyzer 'mecab-analyzer)
(defclass mecab-analyzer ()
  ((context :initform nil)
   (model :initform nil)
   (lattice :initform nil)
   (tagger :initform nil)))


(defmethod init-analyzer ((this mecab-analyzer))
  (with-slots (context model lattice tagger) this
    (let* ((root-path (asdf:system-relative-pathname :vinoyaku/analyzer/mecab
                                                     "analyzer/mecab/"))
           (args (format nil "-d ~A -r ~A"
                         (merge-pathnames "ipadic/" root-path)
                         (merge-pathnames "mecabrc" root-path))))
      (setf model (%mecab:model-new2 args)
            lattice (%mecab:model-new-lattice model)
            tagger (%mecab::model-new-tagger model)))))


(defmethod discard-analyzer ((this mecab-analyzer))
  (with-slots (model lattice tagger) this
    (%mecab:destroy tagger)
    (%mecab:lattice-destroy lattice)
    (%mecab:model-destroy model)))


(defmethod analyze ((this mecab-analyzer) (text string))
  (with-slots (model lattice tagger) this
    (cffi:with-foreign-string (foreign-string text)
      (%mecab:lattice-set-sentence lattice foreign-string)
      (%mecab:parse-lattice tagger lattice)
      (let ((head (%mecab:lattice-get-bos-node lattice)))
        (loop for node-ptr = (claw:ptr head) then (claw:c-ref node-ptr %mecab:node-t :next)
              until (claw:null-pointer-p node-ptr)
              nconc (claw:c-let ((node %mecab:node-t :from node-ptr))
                      (alexandria:switch ((node :stat) :test #'=)
                        (%mecab:+bos-node+ nil)
                        (%mecab:+eos-node+ nil)
                        (t (list (make-morph node))))))))))
