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
     (define teams(shuffle (search-teams)))
     (define jogo_1_time_1(list (list-ref teams 0)))
     (define jogo_1_time_2(list (list-ref teams 1)))
     (define jogo_2_time_1(list (list-ref teams 2)))
     (define jogo_2_time_2(list (list-ref teams 3)))
     (define jogo_3_time_1(list (list-ref teams 4)))
     (define jogo_3_time_2(list (list-ref teams 5)))
     (define jogo_4_time_1(list (list-ref teams 6)))
     (define jogo_4_time_2(list (list-ref teams 7)))
     (print jogo_1_time_1)
     (response/xexpr
      `(html
        (body
         (form ([method "POST"] [action "/form-gera-resultado"])
               (h1 "Jogo 1",@(for/list ([(team) jogo_1_time_1])
                                 `(h3, (vector->values team 1 2)
                                     " " (input ([type "text"] [name "jogo_1_time_1"] [placeholder "Gols Marcados"]))))
                   " X ",@(for/list ([(team) jogo_1_time_2])
                                 `(h3, (vector->values team 1 2)
                                     " " (input ([type "text"] [name "jogo_1_time_2"] [placeholder "Gols Marcados"])))))
               (h1 "Jogo 2",@(for/list ([(team) jogo_2_time_1])
                                 `(h3, (vector->values team 1 2)
                                     " " (input ([type "text"] [name "jogo_2_time_1"] [placeholder "Gols Marcados"]))))
                   " X ",@(for/list ([(team) jogo_2_time_2])
                                 `(h3, (vector->values team 1 2)
                                      " " (input ([type "text"] [name "jogo_2_time_2"] [placeholder "Gols Marcados"])))))
               (h1 "Jogo 3",@(for/list ([(team) jogo_3_time_1])
                                 `(h3, (vector->values team 1 2)
                                     " " (input ([type "text"] [name "jogo_3_time_1"] [placeholder "Gols Marcados"]))))
                   " X ",@(for/list ([(team) jogo_3_time_2])
                                 `(h3, (vector->values team 1 2)
                                     " " (input ([type "text"] [name "jogo_3_time_2"] [placeholder "Gols Marcados"])))))
               (h1 "Jogo 4",@(for/list ([(team) jogo_4_time_1])
                                 `(h3, (vector->values team 1 2)
                                     " " (input ([type "text"] [name "jogo_4_time_1"] [placeholder "Gols Marcados"]))))
                   " X ",@(for/list ([(team) jogo_4_time_2])
                                 `(h3, (vector->values team 1 2)
                                     " " (input ([type "text"] [name "jogo_4_time_2"] [placeholder "Gols Marcados"])))))
               (input ([type "submit"])))
         '(a ((href "http://localhost:8000/")) "Voltar para o Início")
         )))]
    
    ; /
    [(equal? page "")
     (define teams(search-teams))
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
         '(a ((href "http://localhost:8000/generate-matches-semi")) "Gerar semifinais")
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
