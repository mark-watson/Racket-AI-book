# Building a MicroGPT in Racket

In the next chapters we will use production-grade Large Language Models like GPT-5, Gemini, Claude, etc. Here we look at optional material: Karpathy’s MicroGPT re-implemented in Racket. Dear reader, this chapter has the practical value of introducing you to the low level ideas for implementing LLMs. Feel free to skip this chapter for now and proceed to the following LLM application-oriented chapters.

## Introduction

In recent years, the artificial intelligence landscape has been completely transformed by large language models (LLMs) built upon the foundational Transformer architecture. Models like GPT-5, LLaMA, Gemma 3, and Claude contain tens of billions to trillions of parameters. Their capabilities are profound, ranging from generating sophisticated poetry to writing complex computer code. However, the sheer size and complexity of these production models can make them feel like impenetrable black boxes to developers and researchers trying to grasp their inner workings.

To understand how these massive systems operate, it can be beneficial to strip away the complexities of distributed training, massive datasets, and heavily optimized C++/CUDA backend libraries. One of the most famous educational projects in this domain is Andrej Karpathy's MicroGPT written in Python (often associated with his broader `nanoGPT` and `micrograd` repositories). Karpathy's work distills the essence of a Generative Pre-trained Transformer into a bare-bones, dependency-free implementation, allowing anyone to trace the mathematical operations from start to finish.

In this chapter, we will explore a complete, dependency-free port of MicroGPT written entirely in Racket. Racket is a general-purpose programming language in the Lisp-Scheme family, known for its powerful macro system and "language-oriented programming" philosophy. Its functional nature, expressive syntax, and excellent tool support make it a uniquely powerful tool for exploring complex algorithmic architectures. By stepping away from the heavy machinery of modern Python frameworks like PyTorch, TensorFlow, or JAX, we are forced to construct the foundational components from scratch. We will build an automatic differentiation engine (autograd), construct neural network layers, implement the multi-head self-attention mechanism, and design the training loop. This hands-on approach not only demystifies how modern AI systems learn but also beautifully demonstrates the expressive power and mathematical elegance of Racket.

## Demystifying the Core Components

To build a functioning, end-to-end language model from scratch, we need to orchestrate several interacting systems. Our Racket implementation elegantly encapsulates these systems into a single, cohesive file. Let us break down the architecture into its primary constituents: the autograd engine, model parameter initialization, the Transformer architecture, the training loop, and the inference generator. Understanding each of these pieces is crucial for grasping the holistic behavior of a language model.

### The Autograd Engine

At the heart of any modern neural network training process is the ability to automatically compute the gradients of the loss function with respect to the model's millions (or in this case, hundreds) of parameters. This is achieved through a technique known as reverse-mode automatic differentiation. 

In our Racket code, this mechanism is implemented using a simple `struct` called `val`. A `val` structure encapsulates a scalar piece of data, its current gradient, the child nodes from which it was computed, and the local gradients with respect to those children. This means that every time we perform a mathematical operation, we are not merely calculating a result; we are dynamically building a computational graph.

When operations like addition (`u+`), multiplication (`u*`), exponentiation (`upow`), or non-linearities like ReLU (`urelu`) are executed, they instantiate new `val` objects. These objects maintain pointers to their operands. For example, if `c = (u+ a b)`, then `c` knows that it was created from `a` and `b`, and it stores the local derivatives of the addition operation.

Once a forward pass through the network is complete and a scalar loss value is computed (representing how wrong the network's predictions were), the `back` function takes over. It performs a topological sort of the entire computational graph. Then, traversing this sorted list in reverse order, it applies the chain rule from calculus to propagate gradients backward from the loss down to every individual weight of the model. This dependency-free approach, heavily inspired by Karpathy's `micrograd`, clearly illustrates that automatic differentiation is fundamentally just an automated, programmatic application of the chain rule over a directed acyclic graph.

### Model Parameters and Initialization

A neural network is essentially an enormous collection of parameters—weights and biases—that are iteratively optimized over time. In our Racket implementation, these parameters are stored in a centralized hash table called `*state-dict*`. 

The `init-mod` function defines the architectural shape of our MicroGPT. We initialize several distinct sets of matrices:
- **`wte`**: The token embedding matrix, which translates discrete character IDs into dense continuous vectors.
- **`wpe`**: The positional embedding matrix, which gives the model information about where a token is located in the sequence.
- **`lm_head`**: The final language modeling head, a linear layer that projects the internal representations back into the vocabulary space to predict the next character.
- **Transformer Layers**: For each layer, we initialize matrices for the attention mechanism (`attn_wq`, `attn_wk`, `attn_wv`, `attn_wo`) and the multi-layer perceptron (`mlp_fc1`, `mlp_fc2`).

Each parameter is initialized using a Gaussian distribution (`random-gauss`). This small amount of random noise provides the network with an asymmetrical starting state, breaking symmetry and allowing gradient descent to effectively navigate the loss landscape. Crucially, every weight generated is a `val` structure, ensuring that every calculation utilizing these weights is automatically tracked by the autograd engine.

### The Transformer Architecture

The `gp` function serves as the central processing unit of the model. It implements the forward pass of a decoder-only Transformer. As data flows through this function, it undergoes a series of sophisticated transformations:

1. **Embeddings:** Each discrete token (in our case, characters) and its position in the sequence are converted into dense vectors using our embedding matrices. These two vectors are added together to form the initial representation of the input sequence.
2. **Layer Normalization:** We employ RMSNorm (Root Mean Square Normalization) to stabilize the neural activations. RMSNorm is computationally simpler than standard Layer Normalization because it does not require mean-centering, yet it is highly effective. It has become a standard technique in modern, highly optimized models like Meta's LLaMA.
3. **Multi-Head Self-Attention:** This is the defining feature of the Transformer. The model linearly projects the input into three distinct representations: Queries (Q), Keys (K), and Values (V). For each attention head, the model computes the dot product of the Query and Key. This dot product determines how much "attention" the current token should pay to all past tokens. The attention scores are normalized using a `usoftmax` function and are then multiplied by the Value vector. To significantly speed up inference, we maintain a Key-Value (KV) cache. This cache stores past K and V computations, preventing the model from redundantly recalculating past context for every newly generated token.
4. **Multi-Layer Perceptron (MLP):** Following the attention mechanism, the data is passed through a simple feed-forward neural network equipped with a ReLU activation function. While the attention mechanism allows tokens to communicate with one another, the MLP allows each individual token to process and refine the information it has gathered.
5. **Residual Connections:** You will notice in the code that the outputs of the attention layer and the MLP layer are added back to their respective inputs (`set! x (map u+ x xr)`). These residual connections create "shortcuts" for gradients to flow backward through the network, mitigating the vanishing gradient problem and enabling the training of deeper architectures.
6. **Language Modeling Head:** Finally, the refined representations are projected back into the vocabulary space. The output is a set of raw scores, or "logits," representing the model's prediction for what the next token in the sequence should be.

### Training the Model: Learning from Data

The `run-train` function orchestrates the intricate dance of the learning process. The model trains by reading lines of text—in our specific example, from a file called `names.txt`—processing it character by character. 

The training loop proceeds iteratively over many steps:
1. **The Forward Pass:** The model ingests a sequence of characters and attempts to predict the subsequent character for every position in the sequence simultaneously.
2. **Loss Calculation:** We utilize the cross-entropy loss function. By applying the softmax function to our raw logits, we convert them into a normalized probability distribution. The loss is calculated as the negative log-likelihood of the correct next character. If the model assigns a high probability to the correct character, the loss is low. If it assigns a low probability, the loss is high.
3. **The Backward Pass:** The `back` function is invoked on the final scalar loss value. This single function call ripples backward through the computational graph, computing the exact gradient for every parameter in the network.
4. **Optimization:** We update the parameters using the Adam optimization algorithm. Unlike simple stochastic gradient descent, Adam maintains running averages of both the gradients and the squared gradients. This allows it to adaptively tune the learning rate for each individual parameter, resulting in faster and more stable convergence.

By executing this loop over hundreds or thousands of steps, the model incrementally adjusts its internal weights. It slowly learns to assign higher probabilities to the correct sequences of characters, thereby absorbing the underlying statistical structure of the training data.

### Inference: Hallucinating New Data

Once the model has completed its training phase and internalized the patterns of the dataset, we can use it to generate novel text via the `run-inference` function. 

Starting with a special Beginning-Of-Sequence (BOS) token, we feed this initial context into the model to obtain the probabilities for the first character. We then sample from this probability distribution (using the `random-choice` function) to select a character. This newly generated character is appended to our context and fed back into the model to predict the *next* character. This autoregressive generation process continues in a loop until the model predicts the BOS token again, which we use to indicate the end of the sequence.

Because we sample from a probability distribution rather than greedily picking the single most likely token at every step, the model can generate diverse, creative, and sometimes surprising outputs. It effectively "hallucinates" new names or words that strongly resemble the stylistic patterns of the training data but were never explicitly contained within it.

## Conclusion

This complete Racket implementation of MicroGPT vividly demonstrates that the core concepts underpinning modern large language models are not inscrutable magic. By methodically breaking down the architecture into its fundamental mathematical operations and constructing a rudimentary but fully functional autograd engine from scratch, we gain a profound, demystified understanding of how these powerful systems learn and operate. 

Racket, with its unparalleled flexibility and expressive syntax, proves to be an exceptionally capable and elegant language for this endeavor. Its interactive nature makes it a joy to use for defining complex computational graphs and experimenting with novel neural network architectures.

In the following section, you will find the complete source code for our dependency-free MicroGPT. Reading carefully through this program listing will help solidify the theoretical concepts discussed in this chapter and provide you with a robust, transparent foundation for further experimentation and exploration in the fascinating world of language modeling.

## Complete Source Code Listing for `microgpt.rkt`

```racket
#lang racket

;;; microgpt.rkt — Karpathy's microGPT in dependency-free Racket

(require racket/math)

(provide run-microgpt)

;; ============================================================
;; UTILITIES
;; ============================================================

(define (random-gauss mean std)
  (+ mean (* std (sqrt (* -2 (log (max (random) 1e-15)))) (cos (* 2 pi (random))))))

(define (shuffle-list lst)
  (sort lst (lambda (a b) (< (random) 0.5))))

(define (read-lines file-path)
  (if (file-exists? file-path)
      (with-input-from-file file-path
        (lambda ()
          (let loop ([lines '()])
            (let ([line (read-line)])
              (if (eof-object? line)
                  (reverse lines)
                  (let ([tr (string-trim line)])
                    (if (positive? (string-length tr))
                        (loop (cons tr lines))
                        (loop lines))))))))
      '()))

(define *docs* '())
(define *uchars* "")
(define *bos* 0)
(define *vocab-size* 0)
(define *n-layer* 1)
(define *n-embd* 16)
(define *block-size* 16)
(define *n-head* 4)
(define *head-dim* 4)
(define *state-dict* (make-hash))
(define *params* '())

;; ============================================================
;; AUTOGRAD
;; ============================================================

(struct val (data grad children local-grads) #:mutable #:transparent)

(define (make-val d [ch '()] [lg '()])
  (val (if (val? d) (val-data d) (real->double-flonum d))
       0.0 ch lg))

(define (ens v)
  (if (val? v) v (make-val v)))

(define (u+ a b)
  (let ([a (ens a)] [b (ens b)])
    (make-val (+ (val-data a) (val-data b)) (list a b) '(1.0 1.0))))

(define (u* a b)
  (let ([a (ens a)] [b (ens b)])
    (make-val (* (val-data a) (val-data b)) (list a b) (list (val-data b) (val-data a)))))

(define (upow v p)
  (let ([v (ens v)])
    (make-val (expt (val-data v) p) (list v)
                (list (real->double-flonum (* p (expt (val-data v) (- p 1))))))))

(define (ulog v)
  (let ([v (ens v)])
    (make-val (log (val-data v)) (list v) (list (/ 1.0 (val-data v))))))

(define (uexp v)
  (let ([v (ens v)])
    (make-val (exp (val-data v)) (list v) (list (exp (val-data v))))))

(define (urelu v)
  (let ([v (ens v)])
    (make-val (max 0.0 (val-data v)) (list v) (list (if (positive? (val-data v)) 1.0 0.0)))))

(define (uneg v) (u* v -1.0))
(define (u- a b) (u+ a (uneg b)))
(define (u/ a b) (u* a (upow b -1.0)))

(define (back v)
  (define topo '())
  (define seen (make-hasheq))
  (define (build n)
    (unless (hash-has-key? seen n)
      (hash-set! seen n #t)
      (for-each build (val-children n))
      (set! topo (cons n topo))))
  (build v)
  (set-val-grad! v 1.0)
  (for ([n topo])
    (let ([grad (val-grad n)])
      (for-each (lambda (c g)
                  (set-val-grad! c (+ (val-grad c) (* g grad))))
                (val-children n)
                (val-local-grads n)))))

;; ============================================================
;; MODEL PARAMETERS
;; ============================================================

(define (make-matr nout nin [std 0.08])
  (for/vector ([i (in-range nout)])
    (for/vector ([j (in-range nin)])
      (let ([v (make-val (random-gauss 0 std))])
        (set! *params* (cons v *params*))
        v))))

(define (init-mod)
  (set! *state-dict* (make-hash))
  (set! *params* '())
  (define (mat k r c) (hash-set! *state-dict* k (make-matr r c)))
  (mat "wte" *vocab-size* *n-embd*)
  (mat "wpe" *block-size* *n-embd*)
  (mat "lm_head" *vocab-size* *n-embd*)
  (for ([i (in-range *n-layer*)])
    (define (lmat s r c) (hash-set! *state-dict* (format "layer~a.~a" i s) (make-matr r c)))
    (lmat "attn_wq" *n-embd* *n-embd*)
    (lmat "attn_wk" *n-embd* *n-embd*)
    (lmat "attn_wv" *n-embd* *n-embd*)
    (lmat "attn_wo" *n-embd* *n-embd*)
    (lmat "mlp_fc1" (* 4 *n-embd*) *n-embd*)
    (lmat "mlp_fc2" *n-embd* (* 4 *n-embd*)))
  (set! *params* (reverse *params*)))

;; ============================================================
;; ARCHITECTURE
;; ============================================================

(define (usum vs) (foldl u+ (make-val 0.0) vs))

(define (ulinear x w)
  (for/list ([row (in-vector w)])
    (foldl u+ (make-val 0.0) (map u* (vector->list row) x))))

(define (usoftmax logits)
  (let* ([mx (apply max (map val-data logits))]
         [exps (map (lambda (v) (uexp (u- v mx))) logits)]
         [sum (usum exps)])
    (map (lambda (e) (u/ e sum)) exps)))

(define (urmsnorm x)
  (let* ([x2 (map (lambda (xi) (u* xi xi)) x)]
         [mean-x2 (u/ (usum x2) (length x))]
         [sc (upow (u+ mean-x2 1e-5) -0.5)])
    (map (lambda (xi) (u* xi sc)) x)))

(define (gp tok-id pos-id keys-vec vals-vec)
  (let* ([wte (hash-ref *state-dict* "wte")]
         [wpe (hash-ref *state-dict* "wpe")]
         [x (map u+ (vector->list (vector-ref wte tok-id))
                    (vector->list (vector-ref wpe pos-id)))]
         [x (urmsnorm x)])
    (for ([li (in-range *n-layer*)])
      (let* ([xr x]
             [x-norm (urmsnorm x)]
             [wq (hash-ref *state-dict* (format "layer~a.attn_wq" li))]
             [wk (hash-ref *state-dict* (format "layer~a.attn_wk" li))]
             [wv (hash-ref *state-dict* (format "layer~a.attn_wv" li))]
             [wo (hash-ref *state-dict* (format "layer~a.attn_wo" li))]
             [q (ulinear x-norm wq)]
             [k (ulinear x-norm wk)]
             [v (ulinear x-norm wv)])
        
        (vector-set! keys-vec li (append (vector-ref keys-vec li) (list k)))
        (vector-set! vals-vec li (append (vector-ref vals-vec li) (list v)))
        
        (let* ([heads 
                (for/list ([h (in-range *n-head*)])
                  (let* ([hs (* h *head-dim*)]
                         [he (+ hs *head-dim*)]
                         [q-h (take (drop q hs) *head-dim*)]
                         [k-h (map (lambda (ki) (take (drop ki hs) *head-dim*)) (vector-ref keys-vec li))]
                         [v-h (map (lambda (vi) (take (drop vi hs) *head-dim*)) (vector-ref vals-vec li))]
                         [scores (for/list ([kt k-h])
                                   (u/ (usum (map u* q-h kt)) (sqrt *head-dim*)))]
                         [aw (usoftmax scores)])
                    (for/list ([j (in-range *head-dim*)])
                      (usum (map (lambda (w vt) (u* w (list-ref vt j))) aw v-h)))))])
          (set! x (ulinear (apply append heads) wo))
          (set! x (map u+ x xr)))
        
        (let ([xr x]
              [x-norm (urmsnorm x)]
              [fc1 (hash-ref *state-dict* (format "layer~a.mlp_fc1" li))]
              [fc2 (hash-ref *state-dict* (format "layer~a.mlp_fc2" li))])
          (set! x (map urelu (ulinear x-norm fc1)))
          (set! x (ulinear x fc2))
          (set! x (map u+ x xr)))))
    (ulinear x (hash-ref *state-dict* "lm_head"))))

;; ============================================================
;; TRAINING
;; ============================================================

(define (string-index str ch)
  (for/first ([i (in-range (string-length str))]
              #:when (char=? (string-ref str i) ch))
    i))

(define (run-train)
  (let* ([lr 0.01] [b1 0.85] [b2 0.99] [eps 1e-8] [steps 1000]
         [np (length *params*)]
         [m (make-vector np 0.0)]
         [mv (make-vector np 0.0)])
    (for ([step (in-range steps)])
      (let* ([doc (list-ref *docs* (modulo step (length *docs*)))]
             [tokens (append (list *bos*)
                             (for/list ([ch (in-string doc)])
                               (string-index *uchars* ch))
                             (list *bos*))]
             [n (min *block-size* (- (length tokens) 1))]
             [keys (for/vector ([i (in-range *n-layer*)]) '())]
             [vals (for/vector ([i (in-range *n-layer*)]) '())]
             [losses '()])
        
        (for ([pos (in-range n)])
          (let* ([logits (gp (list-ref tokens pos) pos keys vals)]
                 [probs (usoftmax logits)])
            (set! losses (cons (uneg (ulog (list-ref probs (list-ref tokens (+ pos 1))))) losses))))
        
        (let* ([loss (u/ (usum losses) n)]
               [t+1 (+ step 1)])
          ;; Reset grads
          (for ([p *params*]) (set-val-grad! p 0.0))
          (back loss)
          
          (for ([i (in-range np)]
                [p *params*])
            (let* ([grad (val-grad p)]
                   [m-val (+ (* b1 (vector-ref m i)) (* (- 1 b1) grad))]
                   [mv-val (+ (* b2 (vector-ref mv i)) (* (- 1 b2) (expt grad 2)))])
              (vector-set! m i m-val)
              (vector-set! mv i mv-val)
              (let* ([mh (/ m-val (- 1 (expt b1 t+1)))]
                     [vh (/ mv-val (- 1 (expt b2 t+1)))])
                (set-val-data! p (- (val-data p) (/ (* lr (- 1 (/ step steps)) mh) (+ (sqrt vh) eps)))))))
          
          (when (or (zero? (modulo t+1 10)) (= t+1 1))
            (printf "step ~a/~a | loss ~a\n" t+1 steps (val-data loss))
            (flush-output)))))))

;; ============================================================
;; INFERENCE
;; ============================================================

(define (random-choice weights)
  (let* ([total (apply + weights)]
         [r (* (random) total)])
    (let loop ([i 0] [acc 0.0])
      (if (>= i (- (length weights) 1))
          i
          (let ([new-acc (+ acc (list-ref weights i))])
            (if (< r new-acc)
                i
                (loop (+ i 1) new-acc)))))))

(define (run-inference)
  (printf "\n--- inference (new, hallucinated names) ---\n")
  (for ([i (in-range 20)])
    (let ([keys (for/vector ([l (in-range *n-layer*)]) '())]
          [vals (for/vector ([l (in-range *n-layer*)]) '())]
          [tok *bos*]
          [sample '()])
      (let loop ([pos 0])
        (when (< pos *block-size*)
          (let* ([logits (gp tok pos keys vals)]
                 [lt (map (lambda (l) (/ (val-data l) 0.5)) logits)]
                 [mx (apply max lt)]
                 [exps (map (lambda (v) (exp (- v mx))) lt)]
                 [sum-exps (apply + exps)]
                 [probs (map (lambda (e) (/ e sum-exps)) exps)])
            (set! tok (random-choice probs))
            (unless (= tok *bos*)
              (set! sample (cons (string-ref *uchars* tok) sample))
              (loop (+ pos 1))))))
      (printf "sample ~a: ~a\n" (+ i 1) (list->string (reverse sample))))))

;; ============================================================
;; MAIN
;; ============================================================

(define (run-microgpt)
  (set! *docs* (shuffle-list (read-lines "names.txt")))
  (printf "num docs: ~a\n" (length *docs*))
  (let ([chars '()])
    (for ([doc *docs*])
      (for ([ch (in-string doc)])
        (unless (member ch chars)
          (set! chars (cons ch chars)))))
    (set! *uchars* (list->string (sort chars char<?)))
    (set! *bos* (string-length *uchars*))
    (set! *vocab-size* (+ *bos* 1)))
  (printf "vocab size: ~a\n" *vocab-size*)
  (init-mod)
  (printf "num params: ~a\n" (length *params*))
  (run-train)
  (run-inference))

(module+ main
  (run-microgpt))
```

Let’s run the code:

```bash
$ racket microgpt.rkt
num docs: 32033
vocab size: 27
num params: 4192
step 1/1000 | loss 3.3421
step 2/1000 | loss 3.5779
...
step 997/1000 | loss 2.2916
step 998/1000 | loss 1.7724
step 999/1000 | loss 2.3279
step 1000/1000 | loss 2.3213

--- inference (new, hallucinated names) ---
sample 1: lereri
sample 2: nahand
sample 3: eleni
sample 4: amica
sample 5: calina
sample 6: alla
sample 7: marona
sample 8: chidynn
sample 9: kanan
sample 10: brann
sample 11: kalen
sample 12: jorena
```


## Wrap Up

This concludes our exploration of MicroGPT implemented from scratch in Racket. By studying the complete source code listing provided above, you have seen firsthand how the abstract mathematical concepts of reverse-mode automatic differentiation, self-attention, and autoregressive language modeling translate into concrete, functional code.

I highly encourage you to load `microgpt.rkt` into your own Racket REPL or DrRacket and run it. Try experimenting with the hyperparameters—adjust the `*n-layer*`, `*n-embd*`, or `*n-head*` variables and observe how they impact both the training time and the quality of the hallucinated outputs. You can also swap out the `names.txt` dataset for your own text files to see how the model adapts to entirely different linguistic patterns. By actively tinkering with this minimalistic implementation, you will cement your understanding of how modern generative AI truly works under the hood.
