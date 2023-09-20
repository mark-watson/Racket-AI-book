# Using the OpenAI APIs in Racket


```racket
#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(define (question prompt)
  (let* ((prompt-data (string-join (list "{\"messages\": [ {\"role\": \"user\", \"content\": \"Answer the question: "
                                         prompt
                                         "\"}], \"model\": \"gpt-3.5-turbo\"}")))
         ;;(temp0 (displayln prompt-data))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set* headers
                             'authorization (string-join (list "Bearer " (getenv "OPENAI_API_KEY")))
                             'content-type "application/json") params)))
         (p
          (post "https://api.openai.com/v1/chat/completions" ;; "https://api.openai.com/v1/engines/davinci/completions"
                #:auth auth
                #:data prompt-data))
         ;;(temp (println p))
         (r (response-json p)))
    ;;(println r)
    (hash-ref (hash-ref (first (hash-ref r 'choices)) 'message) 'content)))

(define answer2 (question "Mary is 30 and Harry is 25. Who is older?"))
answer2


(define (completion prompt)
  (question (string-append "Continue writieng from the following text: " prompt)))

(println (completion "Frank bought a new sports car. Frank drove"))
```

The output looks like:

```
"Mary is older."
"his new sports car out of the dealership with a mix of excitement and anticipation. The sleek, red exterior gleamed under the bright sun, turning heads as he cruised through the city streets. The powerful engine purred as he accelerated, relishing in the adrenaline rush that came with every twist and turn of the road.\n\nAs he drove, Frank couldn't help but think about all the adventures he would embark on with his new car. He had always dreamed of feeling the wind in his hair as he zoomed down scenic coastal highways, or the thrill of pushing the car to its limits on a racetrack. The possibilities seemed endless.\n\nWith a smirk on his face, Frank decided to take a detour from his usual route and explore a winding road that snaked through the outskirts of town. As the surrounding scenery transformed into lush green fields and picturesque hills, he felt a sense of liberation. Each curve in the road challenged his driving skills, allowing him to unleash the true potential of his sports car.\n\nThe drive became a symphony of precision and control. Frank found himself in awe of the car's responsive handling, gliding effortlessly through each turn and hugging the road with impeccable precision. It was as though the car and Frank were in perfect harmony, dancing together as he shifted gears and effortlessly maneuvered.\n\nIn that moment, Frank realized that his new sports car was not just a vehicle, but an extension of his own spirit. It reflected his love for adventure, his admiration for speed, and his desire to live life on the edge. With his hands gripping the steering wheel tightly and a wide grin on his face, Frank embraced the freedom that came with his new car, savoring every second of the drive.\n\nAs the sun began to set, Frank reluctantly turned back towards the city. The exhilarating ride may have come to an end for now, but he knew this was just the beginning of a thrilling journey with his beloved sports car. With a renewed sense of passion and excitement, Frank promised himself that he would continue to push the limits, explore new roads, and create unforgettable memories with his new companion."
> 
```
