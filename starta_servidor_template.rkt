#lang web-server/insta
(require conecta_banco)
(
    define (start request) (
         response/xexpr '(
             html (
                head (
                    title "My Blog"
                )
             ) (
                body (
                    h1 "Under construction")
                ) 
             ) 
         ) 
     )

