#lang racket

(require web-server/servlet
         web-server/servlet-env)
(require web-server/private/url-param)
(require net/uri-codec)
(require web-server/http/request-structs)
(require db)

(define pgc
  (virtual-connection
    (connection-pool
       (lambda ()
        (mysql-connect
              #:user "your_user"
              #:database "your_database"
              #:server "localhost"
              #:password "your_password"
        )
      )
    )
  )
)
(define (search-teams)
         (query-list pgc (~a "SELECT name FROM teams LIMIT 8") )
    )
(define (search-team team)
         (query-list pgc (~a "SELECT name FROM teams WHERE id = " team) )
    )
(define (insert-team team)
  (query pgc "INSERT INTO copa_mundo_racket.teams VALUES (DEFAULT, ?)" team)
  )

(define (update-team id team)
  (query pgc "UPDATE teams SET name=? WHERE id=?" team id) 
  )

(define (delete-all-teams)
  (query pgc "DELETE FROM teams") 
  )

(define (delete-one-team id)
  (query pgc "DELETE FROM teams WHERE id =" id)
  )

(define (form-servlet req)  
  (define uri (request-uri req))
  (define path (map path/param-path (url-path uri)))    
  (define page (car path))
  
  (cond 
    ; /form-edit
    [(equal? page "form-edit")
     (define id (bytes->string/utf-8
                 (binding:form-value
                  (bindings-assq
                   (string->bytes/utf-8 "id") (request-bindings/raw req)))))
     (print  id)
     (response/xexpr
      `(html
        (body
         (form ([method "POST"] [action "/print-form-data-edit"])
               "ID do Time: " (input ([type "text"] [name "id"] [readonly "true"] [value ,id]))
               (br)
               "Novo nome do Time: " (input ([type "text"] [name "team"]))
               (br)
               (input ([type "submit"]))))))]
    
    ; /print-form-data-edit
    [(equal? page "print-form-data-edit")
  
     ; extract the form data:
     (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
     
     ; convert to an alist:
     (define form-data (form-urlencoded->alist post-data))

     ; pull out the user and comment:
     (define name    (cdr (assq 'team form-data)))
     (define id    (cdr (assq 'id form-data)))

     (update-team id name)

      ; send back the extracted data:
     (response/xexpr
      `(html
        (body
         (p "Your name: ",name ))))]

    ; /
    [(equal? page "")
     (define teams(search-teams))
     (define (createstring team)
       (~a ""  team))
     (response/xexpr
      `(html
        (body
         (h1 "Nome dos times",@(for/list ([team teams])
                                 `(h3, (createstring team))))
         )))]
    
    ; /form
    [(equal? page "form")
     (response/xexpr
      `(html
        (body
         (form ([method "POST"] [action "/print-form-data"])
               "Nome do Time: " (input ([type "text"] [name "team"]))
               (br)
               (input ([type "submit"]))))))]
    
    ; /print-form-data
    [(equal? page "print-form-data")
  
     ; extract the form data:
     (define post-data (bytes->string/utf-8 (request-post-data/raw req)))
     
     ; convert to an alist:
     (define form-data (form-urlencoded->alist post-data))

     ; pull out the user and comment:
     (define name    (cdr (assq 'team form-data)))

     (insert-team name)

     
     ; send back the extracted data:
     (response/xexpr
      `(html
        (body
         (p "Your name: " ))))]
    
    ; another page?
    [else
     (response/xexpr
      `(html
        (body
         (p "Page not found!"))))]))
         
(serve/servlet form-servlet
               #:servlet-regexp #rx""
               #:servlet-path "/")
