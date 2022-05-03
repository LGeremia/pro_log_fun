#lang racket

(require web-server/servlet
         web-server/servlet-env)
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
         (query-rows pgc (~a "SELECT * FROM teams LIMIT 8") )
    )
(define (search-team team)
         (query-rows pgc (~a "SELECT * FROM teams WHERE id = " team) )
    )
(define (insert-team team)
  (query pgc "INSERT INTO teams VALUES (DEFAULT, ?)" team)
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
    ; /remove-all
    [(equal? page "remove-all")
     
     (delete-all-teams)
     (response/xexpr
      `(html
        (body
         (p "Time removido com sucesso! " )
         '(a ((href "http://localhost:8000/")) "Voltar para o Início"))))]
    
     ; /remove-team
    [(equal? page "remove-team")
     (define id (bytes->string/utf-8
                 (binding:form-value
                  (bindings-assq
                   (string->bytes/utf-8 "id") (request-bindings/raw req)))))
     (delete-one-team  id)
     (response/xexpr
      `(html
        (body
         (p "Time removido com sucesso! " )
         '(a ((href "http://localhost:8000/")) "Voltar para o Início"))))]
    
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
               (input ([type "submit"]))
               (br)
               '(a ((href "http://localhost:8000/")) "Voltar para o Início")))))]
    
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
         (p "Time atualizado com sucesso! ",name )
         '(a ((href "http://localhost:8000/")) "Voltar para o Início"))))]

    ; /generate-matches-semi
    [(equal? page "generate-matches-semi")
     (define teams(search-teams))
     (print teams)
     (response/xexpr
      `(html
        (body
         (h1 "Nome dos times",@(for/list ([(team) teams])
                                 `(h3, (createstring (vector->values team 1 2))
                                 '(a ((href, (~a "http://localhost:8000/remove-team/?id=" (vector->values team 0 1)))) "Excluir Time")
                                 "  "
                                 '(a ((href, (~a "http://localhost:8000/form-edit/?id=" (vector->values team 0 1)))) "Editar Time"))))
         '(a ((href "http://localhost:8000/form")) "Cadastrar Time")
         "   "
         '(a ((href "http://localhost:8000/generate-matches")) "Gerar semifinais")
         "   "
         '(a ((href "http://localhost:8000/remove-all")) "Excluir todos os times")
         )))]
    
    ; /
    [(equal? page "")
     (define teams(search-teams))
     (print teams)
     (define (createstring team)
       (~a ""  team "  "))
     (response/xexpr
      `(html
        (body
         (h1 "Nome dos times",@(for/list ([(team) teams])
                                 `(h3, (createstring (vector->values team 1 2))
                                 '(a ((href, (~a "http://localhost:8000/remove-team/?id=" (vector->values team 0 1)))) "Excluir Time")
                                 "  "
                                 '(a ((href, (~a "http://localhost:8000/form-edit/?id=" (vector->values team 0 1)))) "Editar Time"))))
         '(a ((href "http://localhost:8000/form")) "Cadastrar Time")
         "   "
         '(a ((href "http://localhost:8000/generate-matches")) "Gerar semifinais")
         "   "
         '(a ((href "http://localhost:8000/remove-all")) "Excluir todos os times")
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
         (p "Time cadastrado com sucesso! " ,name)
         '(a ((href "http://localhost:8000/")) "Voltar para o Início"))))]
    
    ; another page?
    [else
     (response/xexpr
      `(html
        (body
         (p "Page not found!"))))]))
         
(serve/servlet form-servlet
               #:servlet-regexp #rx""
               #:servlet-path "/")
