# Using the OpenAI, Anthropic, Mistral, and Local Hugging Face Large Language Model APIs in Racket

As I write this chapter in October 2023, Peter Norvig and Blaise Agüera y Arcas just wrote an article [Artificial General Intelligence Is Already Here](https://www.noemamag.com/artificial-general-intelligence-is-already-here/) making the case that we might already have Artificial General Intelligence (AGI) because of the capabilities of Large Language Models (LLMs) to solve new tasks.

In the development of practical AI systems LLMs like those provided by OpenAI, Anthropic, and Hugging Face have emerged as pivotal tools for numerous applications including natural language processing, generation, and understanding. These models, powered by deep learning architectures, encapsulate a wealth of knowledge and computational capabilities. As a Racket Scheme enthusiast embarking on the journey of intertwining the elegance of Racket with the power of these modern language models, you are opening a gateway to a realm of possibilities that we begin to explore here.

The OpenAI and Anthropic APIs serve as gateways to some of the most advanced language models available today. By accessing these APIs, developers can harness the power of these models for a variety of applications. Here, we delve deeper into the distinctive features and capabilities that these APIs offer, which could be harnessed through a Racket interface.

OpenAI provides an API for developers to access models like GPT-4. The OpenAI API is designed with simplicity and ease of use in mind, making it a favorable choice for developers. It provides endpoints for different types of interactions, be it text completion, translation, or semantic search among others. We will use the completion API in this chapter. The robustness and versatility of the OpenAI API make it a valuable asset for anyone looking to integrate advanced language understanding and generation capabilities into their applications.

On the other hand, Anthropic is a newer entrant in the field but with a strong emphasis on building models that are not only powerful but also understandable and steerable. The Anthropic API serves as a portal to access their language models. While the detailed offerings and capabilities might evolve, the core ethos of Anthropic is to provide models that developers can interact with in a more intuitive and controlled manner. This aligns with a growing desire within the AI community for models that are not black boxes, but instead, offer a level of interpretability and control that makes them safer and more reliable to use in different contexts. We will use the Anthropic completion API.

What if you want the total control of running open LLMs on your own computers? The company [Hugging Face](https://huggingface.co) maintains a huge repository of pre-trained models.  Some of these models are licensed for research only but many are licensed (e.g., using Apache 2) for any commercial use.  Many of the Hugging Face models are derived from Meta and other companys. We will use the [llama.cpp server](https://github.com/ggerganov/llama.cpp/tree/master) at the end of this chapter to run our own LLM on a laptop and access it via Racket code.

Lastly, this chapter will delve into practical examples showing the synergy between systems developed in Racket and the LLMs. Whether it’s automating creative writing, conducting semantic analysis, or building intelligent chatbots, the fusion of Racket with OpenAI, Anthropic, and Hugging Face's LLMs provides many opportunities for you, dear reader, to write innovative software that utilizes the power of LLMs.

## Introduction to Large Language Models

Large Language Models (LLMs) represent a huge advance in the evolution of artificial intelligence, particularly in the domain of natural language processing (NLP). They are trained on vast corpora of text data, learning to predict subsequent words in a sequence, which imbues them with the ability to generate human-like text, comprehend the semantics of language, and perform a variety of language-related tasks. The architecture of these models, typically based on deep learning paradigms such as Transformer, empowers them to encapsulate intricate patterns and relationships within language. These models are trained utilizing substantial computational resources.

The utility of LLMs extends across a broad spectrum of applications including but not limited to text generation, translation, summarization, question answering, and sentiment analysis. Their ability to understand and process natural language makes them indispensable tools in modern AI-driven solutions. However, with great power comes great responsibility. The deployment of LLMs raises imperative considerations regarding ethics, bias, and the potential for misuse. Moreover, the black-box nature of these models presents challenges in interpretability and control, which are active areas of research in the quest to make LLMs more understandable and safe. The advent of LLMs has undeniably propelled the field of NLP to new heights, yet the journey towards fully responsible and transparent utilization of these powerful models is an ongoing endeavor. I recommend reading material at [Center for Humane Technology](https://www.humanetech.com/key-issues) for issues of the safe use of AI. You might also be interested in a book I wrote in April 2023 [Safe For Humans AI: A "humans-first" approach to designing and building AI systems](https://leanpub.com/safe-for-humans-AI/read) (link for reading my book free online).

## Using the OpenAI APIs in Racket

We will now have some fun using Racket Scheme and OpenAI's APIs. The combination of Racket's language features and programming environment with OpenAI's linguistic models opens up many possibilities for developing sophisticated AI-driven applications.

Our goal is straightforward interaction with OpenAI's APIs. The communication between your Racket code and OpenAI's models is orchestrated through well-defined API requests and responses, allowing for a seamless exchange of data. The following sections will show the technical aspects of interfacing Racket with OpenAI's APIs, showcasing how requests are formulated, transmitted, and how the JSON responses are handled. Whether your goal is to automate content generation, perform semantic analysis on text data, or build intelligent systems capable of engaging in natural language interactions, the code snippets and explanations provided will serve as a valuable resource in understanding and leveraging the power of AI through Racket and OpenAI's APIs.

The Racket code listed below defines two functions, **question** and **completion**, aimed at interacting with the OpenAI API to leverage the GPT-3.5 Turbo model for text generation. The function **question** accepts a **prompt** argument and constructs a JSON payload following the OpenAI's chat models schema. It constructs a value for **prompt-data** string containing a user message that instructs the model to "Answer the question" followed by the provided prompt. The **auth** lambda function within **question** is utilized to set necessary headers for the HTTP request, including the authorization header populated with the OpenAI API key obtained from the environment variable **OPENAI_API_KEY**. The function **post** from the **net/http-easy** library is employed to issue a POST request to the OpenAI API endpoint "https://api.openai.com/v1/chat/completions" with the crafted JSON payload and authentication headers. The response from the API is then parsed as JSON, and the content of the message from the first choice is extracted and returned.

The function **completion**, on the other hand, serves a specific use case of continuing text from a given **prompt**. It reformats the prompt to prepend the phrase "Continue writing from the following text: " to the provided text, and then calls the function **question** with this modified prompt. This setup encapsulates the task of text continuation in a separate function, making it straightforward for developers to request text extensions from the OpenAI API by merely providing the initial text to the function **completion**. Through these functions, the code provides a structured mechanism to generate responses or text continuations from the GPT-3.5 Turbo model using a Racket Scheme programming environment.

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

(define (completion prompt)
  (question
   (string-append
    "Continue writing from the following text: "
    prompt)))
```

The output looks like (output from the second example shortened for brevity):

```
> (question "Mary is 30 and Harry is 25. Who is older?")
"Mary is older than Harry."
> (displayln
    (completion
      "Frank bought a new sports car. Frank drove"))
Frank bought a new sports car. Frank drove it out of the dealership with a wide grin on his face. The sleek, aerodynamic design of the car hugged the road as he accelerated, feeling the power under his hands. The adrenaline surged through his veins, and he couldn't help but let out a triumphant shout as he merged onto the highway.

As he cruised down the open road, the wind whipping through his hair, Frank couldn't help but reflect on how far he had come. It had been a lifelong dream of his to own a sports car, a symbol of success and freedom in his eyes. He had worked tirelessly, saving every penny, making sacrifices along the way to finally make this dream a reality.
...
> 
```

## Using the Anthropic APIs in Racket

The Racket code listed below defines two functions, **question** and **completion**, which facilitate interaction with the Anthropic API to access a language model named **claude-instant-1** for text generation purposes. The function **question** takes two arguments: a **prompt** and a **max-tokens** value, which are used to construct a JSON payload that will be sent to the Anthropic API. Inside the function, several Racket libraries are utilized for handling HTTP requests and processing data. A POST request is initiated to the Anthropic API endpoint "https://api.anthropic.com/v1/complete" with the crafted JSON payload. This payload includes the prompt text, maximum tokens to sample, and specifies the model to be used. The **auth** lambda function is used to inject necessary headers for authentication and specifying the API version. Upon receiving the response from the API, it extracts the **completion** field from the JSON response, trims any leading or trailing whitespace, and returns it.

The function **completion** is defined to provide a more specific use-case scenario, where it is intended to continue text from a given **prompt**. It also accepts a **max-tokens** argument to limit the length of the generated text. This function internally calls the function **question** with a modified prompt that instructs the model to continue writing from the provided text. By doing so, it encapsulates the common task of text continuation, making it easy to request text extensions by simply providing the initial text and desired maximum token count. Through these defined functions, the code offers a structured way to interact with the Anthropic API for generating text responses or completions in a Racket Scheme environment.

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

## Using a Local Hugging Face Llama2-13b-orca Model with Llama.cpp Server

Now we look at an approach to run LLMs locally on your own computers.

Diving into AI unveils many ways where modern language models play a pivotal role in bridging the gap between machines and human language. Among the many open and public models, I chose Hugging Face's Llama2-13b-orca model because of its support for natural language processing tasks. To truly harness the potential of Llama2-13b-orca, an interface to Racket code is essential. This is where we use the Llama.cpp Server as a conduit between the local instance of the Hugging Face model and the applications that seek to utilize it. The combination of Llama2-13b-orca with the llama.cpp server code will meet our requirements for local deployment and ease of installation and use.

### Installing and Running Llama.cpp server with a Llama2-13b-orca Model

The **llama.cpp** server acts as a conduit for translating REST API requests to the respective language model APIs. By setting up and running the **llama.cpp** server, a channel of communication is established, allowing Racket code to interact with these language models in a seamless manner. There is also a Python library to encapsulate running models inside a Python program (a subject I leave to my Python AI books).

I run the **llama.cpp** service easily on a M2 Mac with 16G of memory. Start by cloning the **llama.cpp** project and building it:

```bash
git clone https://github.com/ggerganov/llama.cpp.git
make
mkdir models
```

Then get a model file from [https://huggingface.co/TheBloke/OpenAssistant-Llama2-13B-Orca-8K-3319-GGUF](https://huggingface.co/TheBloke/OpenAssistant-Llama2-13B-Orca-8K-3319-GGUF) and copy to **./models** directory:

```bash
$ ls -lh models
8.6G openassistant-llama2-13b-orca-8k-3319.Q5_K_M.gguf
```

Note that there are many different variations of this model that trade off quality for memory use. I am using one of the larger models. If you only have 8G of memory try a smaller model.

Run the REST server:

```bash
./server -m models/openassistant-llama2-13b-orca-8k-3319.Q5_K_M.gguf -c 2048
```

We can test the REST server using the **curl** utility:

```bash
 $ curl --request POST \
    --url http://localhost:8080/completion \
    --header "Content-Type: application/json" \
    --data '{"prompt": "Answer the question: Mary is 30 years old and Sam is 25. Who is older and by how much?","n_predict": 128, "top_k": 1}'
{"content":"\nAnswer: Mary is older than Sam by 5 years.","generation_settings":{"frequency_penalty":0.0,"grammar":"","ignore_eos":false,"logit_bias":[],"mirostat":0,"mirostat_eta":0.10000000149011612,"mirostat_tau":5.0,"model":"models/openassistant-llama2-13b-orca-8k-3319.Q5_K_M.gguf","n_ctx":2048,"n_keep":0,"n_predict":128,"n_probs":0,"penalize_nl":true,"presence_penalty":0.0,"repeat_last_n":64,"repeat_penalty":1.100000023841858,"seed":4294967295,"stop":[],"stream":false,"temp":0.800000011920929,"tfs_z":1.0,"top_k":1,"top_p":0.949999988079071,"typical_p":1.0},"model":"models/openassistant-llama2-13b-orca-8k-3319.Q5_K_M.gguf","prompt":"Answer the question: Mary is 30 years old and Sam is 25. Who is older and by how much?","stop":true,"stopped_eos":true,"stopped_limit":false,"stopped_word":false,"stopping_word":"","timings":{"predicted_ms":960.595,"predicted_n":13,"predicted_per_second":13.53327885321077,"predicted_per_token_ms":73.89192307692308,"prompt_ms":539.3580000000001,"prompt_n":27,"prompt_per_second":50.05951520140611,"prompt_per_token_ms":19.976222222222223},"tokens_cached":40,"tokens_evaluated":27,"tokens_predicted":13,"truncated":false}
```

The important part of the output is:

```
"content":"Answer: Mary is older than Sam by 5 years."
```

### A Racket Library for Using a Local Llama.cpp server with a Llama2-13b-orca Model


The following Racket code is designed to interface with a local instance of a Llama.cpp server to interact with a language model for generating text completions. This setup is particularly beneficial when there's a requirement to have a local language model server, reducing latency and ensuring data privacy. We start by requiring libraries for handling HTTP requests and responses. The functionality of this code is encapsulated in three functions: **helper**, **question**, and **completion**, each serving a unique purpose in the interaction with the Llama.cpp server.

The **helper** function provides common functionality, handling the core logic of constructing the HTTP request, sending it to the Llama.cpp server, and processing the response. It accepts a **prompt** argument which forms the basis of the request payload. A JSON string is constructed with three key fields: **prompt**, **n_predict**, and **top_k**, which respectively contain the text prompt, the number of tokens to generate, and a parameter to control the diversity of the generated text. A debug line with `displayln` is used to output the constructed JSON payload to the console, aiding in troubleshooting. The function **post** is employed to send a POST request to the Llama.cpp server hosted locally on port 8080 at the **/completion** endpoint, with the constructed JSON payload as the request body. Upon receiving the response, it's parsed into a Racket hash data structure, and the **content** field, which contains the generated text, is extracted and returned.

The **question** and **completion** functions serve as specialized interfaces to the **helper** function, crafting specific prompts aimed at answering a question and continuing a text, respectively. The **question** function prefixes the provided question text with "Answer: " to guide the model's response, while the **completion** function prefixes the provided text with a phrase instructing the model to continue from the given text. Both functions then pass these crafted prompts to the **helper** function, which in turn handles the interaction with the Llama.cpp server and extracts the generated text from the response.

The following code is in the file **llama_local.rkt**:

```racket
#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(define (helper prompt)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \""
             prompt
             "\", \"n_predict\": 256, \"top_k\": 1}"))))
         (ignore (displayln prompt-data))
         (p
          (post
           "http://localhost:8080/completion"
           #:data prompt-data))
         (r (response-json p)))
    (hash-ref r 'content)))

(define (question question)
  (helper (string-append "Answer: " question)))

(define (completion prompt)
  (helper
   (string-append
    "Continue writing from the following text: "
    prompt)))
```

We can try this in a Racket REPL (output of the second example is edited for brevity):

```racket
> (question "Mary is 30 and Harry is 25. Who is older?")
{"prompt": "Answer: Mary is 30 and Harry is 25. Who is older?", "n_predict": 256, "top_k": 1}
"\nAnswer: Mary is older than Harry."
> (completion "Frank bought a new sports car. Frank drove")
{"prompt": "Continue writing from the following text: Frank bought a new sports car. Frank drove", "n_predict": 256, "top_k": 1}
" his new sports car to work every day. He was very happy with his new sports car. One day, while he was driving his new sports car, he saw a beautiful girl walking on the side of the road. He stopped his new sports car and asked her if she needed a ride. The beautiful girl said yes, so Frank gave her a ride in his new sports car. They talked about many things during the ride to work. When they arrived at work, Frank asked the beautiful girl for her phone number. She gave him her phone number, and he promised to call her later that day...."
> (question "Mary is 30 and Harry is 25. Who is older and by how much?")
{"prompt": "Answer: Mary is 30 and Harry is 25. Who is older and by how much?", "n_predict": 256, "top_k": 1}
"\nAnswer: Mary is older than Harry by 5 years."
> 
```

## Using a Local Mistral-7B Model with Ollama.ai

Now we look at another approach to run LLMs locally on your own computers. The [Ollama.ai project](https://ollama.ai) supplies a simple to install application for macOS and Linux (Windows support expected soon). When you download and run the application, it will install a command line tool **ollama** that we use here.


### Installing and Running Ollama.ai server with a Mistral-7B Model

The Mistral model is the best 7B LLM that I have used (as I write this chapter in October 2023). When you run the **ollama** command line tool it will download and cache for future use the requested model.

For example, the first time we run **ollama** requesting the **mistral** LLM, you see that it is downloading the model:

```bash
 $ ollama run mistral
pulling manifest
pulling 6ae280299950... 100% |███████████████████████████████████████████████| (4.1/4.1 GB, 13 MB/s)           
pulling fede2d8d6c1f... 100% |██████████████████████████████████████████████████████| (29/29 B, 20 B/s)        
pulling b96850d2e482... 100% |███████████████████████████████████████████████████| (307/307 B, 170 B/s)        
verifying sha256 digest
writing manifest
removing any unused layers
success
>>> Mary is 30 and Bill is 25. Who is older and by how much?
Mary is older than Bill by 5 years.

>>> /?
Available Commands:
  /set         Set session variables
  /show        Show model information
  /bye         Exit
  /?, /help    Help for a command

Use """ to begin a multi-line message.

>>>
```

When you run the **ollama** command line tool, it also runs a REST API serve which we use later. The next time you run the **mistral** model, there is no download delay:

```bash
$ ollama run mistral
>>> ^D
(base) Marks-Mac-mini:~ $ ollama run mistral
>>> If I am driving between Sedona Arizona and San Diego, what sites should I visit as a tourist?
    
There are many great sites to visit when driving from Sedona, Arizona to San Diego. Here are some 
suggestions:

* Grand Canyon National Park - A must-see attraction in the area, the Grand Canyon is a massive and 
awe-inspiring natural wonder that offers countless opportunities for outdoor activities such as hiking, 
camping, and rafting.
* Yuma Territorial Prison State Historic Park - Located in Yuma, Arizona, this former prison was once the 
largest and most secure facility of its kind in the world. Today, visitors can explore the site and learn 
about its history through exhibits and guided tours.
* Joshua Tree National Park - A unique and otherworldly landscape in southern California, Joshua Tree 
National Park is home to a variety of natural wonders, including towering trees, giant boulders, and 
scenic trails for hiking and camping.
* La Jolla Cove - Located just north of San Diego, La Jolla Cove is a beautiful beach and tidal pool area 
that offers opportunities for snorkeling, kayaking, and exploring marine life.
* Balboa Park - A cultural and recreational hub in the heart of San Diego, Balboa Park is home to numerous
museums, gardens, theaters, and other attractions that offer a glimpse into the city's history and culture.

>>> 
```

While we use the **mistral** LLM here, there are many more available models listed in the GitHub repository for Ollama.ai: [https://github.com/jmorganca/ollama](https://github.com/jmorganca/ollama).

### A Racket Library for Using a Local Ollama.ai REST Server with a Mistral-7B Model

The example code in the file **ollama_ai_local.rkt** is very similar to the example code in the last section:

```racket
(require net/http-easy)
(require racket/set)
(require pprint)

(define (helper prompt)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \""
             prompt
             "\", \"model\": \"mistral\", \"stream\": false}"))))
         (ignore (displayln prompt-data))
         (p
          (post
           "http://localhost:11434/api/generate"
           #:data prompt-data))
         (r (response-json p)))
    (hash-ref r 'response)))

(define (question question)
  (helper (string-append "Answer: " question)))

(define (completion prompt)
  (helper
   (string-append
    "Continue writing from the following text: "
    prompt)))
```

We will run the same examples we used in the last section for comparison:

```
> (question "Mary is 30 and Harry is 25. Who is older and by how much?")
{"prompt": "Answer: Mary is 30 and Harry is 25. Who is older and by how much?", "model": "mistral", "stream": false}
"Answer: Mary is older than Harry by 5 years."
> (completion "Frank bought a new sports car. Frank drove")
{"prompt": "Continue writing from the following text: Frank bought a new sports car. Frank drove", "model": "mistral", "stream": false}
"Frank drove his new sports car around town, enjoying the sleek design and powerful engine. The car was a bright red, which caught the attention of everyone on the road. Frank couldn't help but smile as he cruised down the highway, feeling the wind in his hair and the sun on his face.\n\nAs he drove, Frank couldn't resist the urge to test out the car's speed and agility. He weaved through traffic, expertly maneuvering the car around curves and turns. The car handled perfectly, and Frank felt a rush of adrenaline as he pushed it to its limits.\n\nEventually, Frank found himself at a local track where he could put his new sports car to the test. He revved up the engine and took off down the straightaway, hitting top speeds in no time. The car handled like a dream, and Frank couldn't help but feel a sense of pride as he crossed the finish line.\n\nAfterwards, Frank parked his sports car and walked over to a nearby café to grab a cup of coffee. As he sat outside, sipping his drink and watching the other cars drive by, he couldn't help but think about how much he loved his new ride. It was the perfect addition to his collection of cars, and he knew he would be driving it for years to come."
> 
```
