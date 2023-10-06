# Using the OpenAI and Anthropic APIs in Racket

TBD

## Introduction to Large Language Models

TBD

## Using the OpenAI APIs in Racket

```racket
#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(define (question prompt)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"messages\": [ {\"role\": \"user\","
             " \"content\": \"Answer the question: "
             prompt
             "\"}], \"model\": \"gpt-3.5-turbo\"}"))))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set*
                   headers
                   'authorization
                   (string-join
                    (list
                     "Bearer "
                     (getenv "OPENAI_API_KEY")))
                   'content-type "application/json")
                  params)))
         (p
          (post
           "https://api.openai.com/v1/chat/completions"
           #:auth auth
           #:data prompt-data))
         (r (response-json p)))
    (hash-ref
     (hash-ref (first (hash-ref r 'choices)) 'message)
     'content)))

(println
  (question "Mary is 30 and Harry is 25. Who is older?"))

(define (completion prompt)
  (question
   (string-append
    "Continue writing from the following text: "
    prompt)))

(println
 (completion
  "Frank bought a new sports car. Frank drove"))
```

The output looks like:

```
> (question "Mary is 30 and Harry is 25. Who is older?")
"Mary is older than Harry."
> (displayln
    (completion
      "Frank bought a new sports car. Frank drove"))
Frank bought a new sports car. Frank drove it out of the dealership with a wide grin on his face. The sleek, aerodynamic design of the car hugged the road as he accelerated, feeling the power under his hands. The adrenaline surged through his veins, and he couldn't help but let out a triumphant shout as he merged onto the highway.

As he cruised down the open road, the wind whipping through his hair, Frank couldn't help but reflect on how far he had come. It had been a lifelong dream of his to own a sports car, a symbol of success and freedom in his eyes. He had worked tirelessly, saving every penny, making sacrifices along the way to finally make this dream a reality.

With the sun drenching the car's glossy exterior, Frank felt a sense of pride and accomplishment wash over him. The car represented more than just a mode of transportation; it embodied his perseverance and determination to achieve his goals. It was a symbol of all the late nights spent working overtime, the countless hours researching, and the sacrifices of those small luxuries he had foregone in order to save every dollar.

Lost in his thoughts and the thrill of the ride, Frank decided to take a detour off the highway and explore the curving backroads. He craved the freedom and adventure the open road offered, hoping to leave behind the stresses and worries of everyday life, even if just for a little while.

As he navigated the twists and turns, Frank's senses were heightened. The engine's roar echoed through the secluded countryside, the scent of fresh air mingled with the intoxicating smell of burnt rubber. The adrenaline rush, combined with the beautiful scenery passing by, made Frank feel as though he were in his own personal paradise.

Hours slipped away as Frank indulged in this extraordinary experience. The car, perfectly engineered, effortlessly glided through each hairpin turn, making Frank feel like a professional race car driver. The worries that plagued his mind seemed distant and insignificant, replaced by the pure joy of the present moment.

With the sun slowly setting on the horizon, casting a golden glow over the landscape, Frank's journey was coming to an end. He found himself back on the highway, headed home with a heart full of contentment and a smile that couldn't be wiped away.

As Frank parked his sports car in the driveway, he took a moment to admire it once more. It was more than just a machine; it was a symbol of his hard work and dedication. In that moment, he realized that the true value of the car lay not in its exterior or performance but in the journey it represented.

Frank knew that from that day forward, every drive in his new sports car would remind him of the endless possibilities that await when one dares to dream big. And as he turned off the engine and stepped out of the car, Frank couldn't wait to see where his next adventure would take him.
> 
```

## Using the Anthropic APIs in Racket


TBD

```racket
#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(provide question completion)

(define (question prompt max-tokens)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \"\\n\\nHuman: "
             prompt
             "\\n\\nAssistant: \", \"max_tokens_to_sample\": "
             (number->string  max-tokens)
             ", \"model\": \"claude-instant-1\" }"))))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set*
                   headers
                   'x-api-key
                     (getenv "ANTHROPIC_API_KEY")
                   'anthropic-version "2023-06-01"
                   'content-type "application/json")
                  params)))
         (p
          (post
           "https://api.anthropic.com/v1/complete"
           #:auth auth
           #:data prompt-data))
         (r (response-json p)))
    (string-trim (hash-ref r 'completion))))

(define (completion prompt max-tokens)
  (question
   (string-append
    "Continue writing from the following text: "
    prompt)
   max-tokens))


;; (displayln (question "Mary is 30 and Harry is 25. Who is older?" 20))
;; (displayln (completion "Frank bought a new sports car. Frank drove" 200))
```

We will try the same examples we used with OpenAI APIs in the previous section:

```racket
$ racket
> (require "anthropic.rkt")
> (question "Mary is 30 and Harry is 25. Who is older?" 20)
"Mary is older than Harry. Mary is 30 years old and Harry is 25 years old."
> (completion "Frank bought a new sports car. Frank drove" 200)
"Here is a possible continuation of the story:\n\nFrank bought a new sports car. Frank drove excitedly to show off his new purchase. The sleek red convertible turned heads as he cruised down the street with the top down. While stopping at a red light, Frank saw his neighbor Jane walking down the sidewalk. He pulled over and called out to her, \"Hey Jane, check out my new ride! Want to go for a spin?\" Jane smiled and said \"Wow that is one nice car! I'd love to go for a spin.\" She hopped in and they sped off down the road, the wind in their hair. Frank was thrilled to show off his new sports car and even more thrilled to share it with his beautiful neighbor Jane. Little did he know this joyride would be the beginning of something more between them."
> 
```

TBD
