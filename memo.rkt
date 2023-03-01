#lang racket

(require racket/cmdline)

(define file-modify-time  file-or-directory-modify-seconds)

(define (files-under-dir dir)
  ;; DirPath -> List<AbsFilePath>
  ;; 返回指定目录下的文件列表
  ;; 不考虑子文件夹中的文件与符号链接
  (filter (lambda (dorf) (eq? (file-or-directory-type dorf) 'file)) (map (lambda (x) (path->complete-path (build-path dir x))) (directory-list dir))))

(define (sort-filepaths-bytime paths)
  ;; List<AbsFilePath> -> SortedByTime List<AbsFilePath>
  (sort paths (lambda (x y) (> (file-modify-time x) (file-modify-time y)))))

;; ListZipper<T> = ([T], [T])

(define (mkListZipper l)
  (cons empty l))

(define (lheadLZ lz)
  (first (cdr lz)))

 (define (rheadLZ lz)
  (first (car lz)))

(define (ltailLZ lz)
  (rest (cdr lz)))

(define (lemptyLZ? lz)
  (empty? (cdr lz)))
  
(define (leftLZ lz)
  (let ([x  (lheadLZ lz)]
        [xs (ltailLZ lz)])
    (cons (cons x (car lz)) xs)))

(define (takeNCE l n)
  ;; take without contract exception to some extent
  ;; e.g takeNCE '(1 2) 3 => '(1 2)
  (cond
    [(empty? l) '()]
    [(zero? n)  '()]
    [else (cons (first l) (takeNCE (rest l) (- n 1)))]))

(define (memo-traverse lz target n)
  ;; ListZipper<AbsFilePath> -> AbsFilePath -> ListZipper<AbsFilePath>
  (cond 
    [(lemptyLZ? lz) '()]
    [(equal? (lheadLZ lz) target)
     ;; 取前后n个文件名
     ;; racket的take在列表元素不足时会抛出异常，等会再修
     (append (reverse (takeNCE (car lz) n)) (takeNCE (cdr lz) (+ n 1)))]
    [else (memo-traverse (leftLZ lz) target n)]))

(define (application dirpath target n)
  ;; String -> String -> List<AbsFilePath>
  (memo-traverse (mkListZipper (sort-filepaths-bytime (files-under-dir (string->path dirpath)))) 
                 (path->complete-path (build-path (string->path dirpath) (string->path target)))
                 n))

(define search-range (make-parameter 1))
(define target-file (make-parameter ""))

(define dir-to-processing
  (command-line
   #:program "memo"
   #:once-each
    [("-n" "--search-range") n
     "指定前后搜索范围"
     (search-range (string->number n))]
    [("-f") file
     "target file"
     (target-file file)]
    #:args (dirname)
    dirname))

(print (application dir-to-processing (target-file) (search-range)))

