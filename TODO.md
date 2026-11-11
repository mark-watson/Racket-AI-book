# TODO: Suggested Improvements for the Book and Example Code

Review date: 2026-11-10, against Racket v9.3 [cs] on macOS.

How this list was produced:

- Every tracked `.rkt` file was compiled with `raco make`.
- The offline test suites were run (results in the table below).
- Each chapter's Racket listings were compared line by line against the files in `source-code/`.
- The older chapters (preface, tutorial, datastores, web scraping, NLP, KGN, conclusion) were read in full. The longer, newer chapters (coding agent, OPS5, math, RL, Malt, category theory, Ollama tools, RAG) were checked for structure, listings and openings only, so they have fewer prose-level items here. That is a gap in the review, not a sign they need no editing.

Items are ordered by priority within each section.

## 1. Fix first: things that are broken or wrong

- [ ] **`manuscript/conclusion.md`: remove the stray Mermaid diagram.** The chapter ends with a `{format: mermaid}` flowchart of a Python `estimate_pi` function (with `ValueError` handling). It has nothing to do with the conclusion or with Racket.
- [ ] **`source-code/RAG/info.rkt` uses `#lang racket`; it must be `#lang info`.** As written, `raco test tests.rkt` fails with `get-info: info file has illegal #lang or #reader`, and `raco pkg install` will fail the same way. (`racket tests.rkt` works: 16 tests pass.)
- [ ] **`manuscript/kgn.md`: rewrite the explanation of `@string-append`.** The text says it "seems like a custom or pseudo-syntax" and that "the mechanism for replacing these placeholders … is not explicitly shown". Both are wrong: this is standard `#lang at-exp racket` reader syntax, and `@person-uri` is ordinary interpolation. Explain the at-exp reader in two or three sentences instead.
- [ ] **`manuscript/tutorial.md`: the hash table listing is out of date and shows two bugs that the source file has already fixed.**
  - `(make-hash '(("dog" '("friendly" 5)) …))` has a nested quote, which is why the printed output shows `('("friendly" 5))`.
  - `(println '("key:" k "value:" v))` prints the literal symbols `k` and `v`; the sample output shows this three times.
  - `misc_code/hash_tests.rkt` now uses `(list "key:" k "value:" v)` and no nested quote. Re-paste the listing and regenerate the output.
- [ ] **Remove `sqlite-table` from the book.** `tutorial.md`, `datastores.md` and `misc_code/README.md` tell readers to `raco pkg install sqlite-table` and `(require sqlite-table)`, but `misc_code/sqlite.rkt` only uses the built-in `db` library. Use a package the book really needs (`http-easy` or `html-parsing`) as the "installing packages" example.
- [ ] **Empty source files.** `source-code/misc_code/lists_tests.rkt` and `source-code/kgn/dev.rkt` are zero bytes and fail to compile. `misc_code/README.md` tells readers to run `racket lists_tests.rkt`. Either write them or delete them and their references (`kgn/Makefile` also lists `dev.rkt` and a non-existent `ui.rkt`).
- [ ] **`source-code/search_brave/brave_search.rkt` makes a live API call at module top level.** Anyone who `require`s it triggers a Brave search (or an error if the key is unset). Move the example into `(module+ main …)`, as `search_tavily/tavily_search.rkt` already does.
- [ ] **License mismatch for the coding agent.** `source-code/coding-agent-harness/LICENSE` is AGPL v3, while the preface, both READMEs and `source-code/LICENSE.txt` say all example code is Apache 2. Either relicense that directory or state the exception in the preface and the chapter.
- [ ] **`manuscript/nlp.md` wrap-up link is a 404.** `https://github.com/mark-watson/Racket-AI-book/source-code` should be `…/Racket-AI-book/tree/main/source-code`.
- [ ] **`manuscript/tutorial.md` wrap-up link is reversed.** `[https://docs.racket-lang.org/guide/](The Racket Guide)` should be `[The Racket Guide](https://docs.racket-lang.org/guide/)`.

## 2. Repository metadata that has fallen behind `Book.txt`

`Book.txt` now has 21 chapters in six parts. Several index files still describe a much earlier book.

- [ ] **`manuscript/TOC.md`** lists 11 chapters. Missing: MicroGPT, Ollama tools, coding agent, Malt, category theory, reinforcement learning, OPS5, Racklog, pi and e. Chapter titles and numbers are also stale (the LLM chapter title no longer matches). The README sends GitHub readers to this file, so it is the first thing they see.
- [ ] **`SOURCE_CODE_MAP.md`** is missing the same chapters plus `RAG.md`. It also maps `datastores.md` to `misc_code` only, although that chapter's main listing is `sparql/sparql.rkt`. Consider generating both `TOC.md` and this file from `Book.txt` with a small script so they cannot drift again.
- [ ] **`manuscript/Makefile`** `edit` target opens files that no longer exist (`deeplearning.md`, `opennlp.md`, `anomaly_detection.md`, `libpython.md`, `semantic_web.md`, `jena_wrapper.md`). It looks copied from another book.
- [ ] **Preface project list** covers 10 directories and omits most of the current book. It lists `pdf_chat` and `search_brave`, which no chapter covers. Rewrite it to follow the six parts.
- [ ] **Preface `software.png` diagram and its caption** describe packages and tools deployed to `~/bin`; check that it still reflects the examples.
- [ ] **Copyright years disagree**: preface says 2022-2024, README says 2022-2025, newer source files say 2026.
- [ ] **Root `README.md`**: the long "advantages / disadvantages of a single repository" section is not useful to readers. Replace it with a short setup section (Racket version, API keys, how to run the tests). Also fix "Apace 2" and "assited".
- [ ] **`source-code/README.md`** is nine lines. Add a table of directories with chapter, required packages, required environment variables and whether the example runs offline.
- [ ] **`source-code/CODING_IDEAS.md`** has an empty first section and refers to "Idea 5" and `ai_service`, which do not exist. Finish it or move it out of the published tree.
- [ ] **Unused images** in `manuscript/resources/images`: `ga_crossover.png`, `ga_sample_function.png`, `genetic-uml.png`, `nn_sigmoid.png`, `IMG_0827.jpeg`. The first three belong to the unpublished genetic programming chapter; the last two look like leftovers.

## 3. Examples that exist but are not in the book (decide: add, or mark clearly)

- [ ] **`genetic_programming/`** has a 377-line `CHAPTER.md` and working code but is marked "still working on this". It would fit Part 6 or Part 4. This is the closest thing to a free new chapter.
- [ ] **`llm-agent/llb-agent.rkt`**: the filename is a typo for `llm-agent.rkt`. Its README points at the LLM chapter, which never mentions it. It also `require`s `rackunit` and provides `all-defined-out` in library code. Fold it into the LLM or coding-agent chapter, or mark it as not in the book.
- [ ] **`pdf_chat/`**: the README says "experimental"; `llm.md` and the preface mention it, but no chapter explains it. It would make a natural short section at the end of `embeddings.md` ("chat with a PDF").
- [ ] **`search_brave/` and `search_tavily/`**: no chapter. A short "web search tools" section in the LLM or Ollama tools chapter would connect them to tool calling. The coding agent has its own `search.rkt`; consider sharing one implementation.
- [ ] **`Bridge_Game_webkit/`** is marked `NOT_IN_BOOK.md` with the body "... not yet...". It has the largest test suite in the repository (18,555 checks pass). Say what the plan is.
- [ ] **`AGENT_SKILLS_README.md` and `AGENT_SKILLS_ONE_TIME_SETUP.md`** (about 740 lines in `source-code/`) are not referenced from the book. Mention them in the preface or the coding-agent chapter, or move them.

## 4. Example code improvements

### Build and test status found in this review

| Suite | Result |
|---|---|
| `math-pi-e/math-tests.rkt` | 246 pass |
| `ollama_tools/tests.rkt` | 10 pass |
| `simple_RDF_SPARQL/tests.rkt` | 15 pass |
| `embeddingsdb/tests.rkt` | 11 pass |
| `ReinforcementLearning/tests.rkt` | 30 pass |
| `Bridge_Game_webkit/tests.rkt` | 18,555 pass |
| `RAG/tests.rkt` | 16 pass with `racket`; fails under `raco test` (see section 1) |

Not compiled here because packages were not installed on the review machine: `Malt_deep_learning` (needs `malt`), `kgn` (needs the local `nlp` package installed), `pdf_chat` (needs `pdf-read`), `racket_llm_language` (needs `llm`). These are not bugs, but none of the first three READMEs lists the install step in a way a script could follow.

### Across all examples

- [ ] **Add a top-level `make test`** in `source-code/Makefile` that runs every offline suite, plus a `make compile` that runs `raco make` on everything. Add a GitHub Actions workflow that runs both. The suites already exist and are fast; nothing runs them together.
- [ ] **Add an `info.rkt` with `deps` to each example directory.** Only `RAG` has one (and it is broken). With correct `info.rkt` files, `raco pkg install --auto` installs dependencies and the per-chapter "install these packages" prose can shrink to one line.
- [ ] **Pick one way to depend on sibling examples.** `embeddingsdb`, `RAG` and `ollama_tools` use relative paths (`"../llmapis/llmapis.rkt"`), while `pdf_chat` and `kgn` require installed collections (`llmapis`, `embeddingsdb`, `nlp`). A reader who follows one chapter's setup hits "collection not found" in another. Relative paths need no install step and are the simpler choice.
- [ ] **Put default model names in one place.** Model strings are scattered across about ten files. Some are clearly old: `text-embedding-ada-002` (in `embeddingsdb.rkt` and three chapters), `mistral-tiny`, `yi:34b`, `mistral:7b-instruct`, and the `gpt4o-mini` and `phi3` modules in `racket_llm_language`. Define defaults once in `llmapis.rkt`, allow environment-variable overrides everywhere (some files already honor `OPENAI_MODEL`, `GEMINI_MODEL`, `OLLAMA_MODEL`), and have chapters refer to "the default model" instead of naming one in prose.
- [ ] **Standardize API key variable names** and document them in one table. The code reads `OPENAI_API_KEY` and `OPENAI_KEY`, `GEMINI_API_KEY` and `GOOGLE_API_KEY`, plus Brave, Tavily, Exa, Mistral, Anthropic and Fireworks keys.
- [ ] **Remove the duplicated NLP data.** `kgn/data` is a byte-identical copy of `nlp/data` (about 110,000 lines of name lists and a 2 MB `tag.dat`). `kgn` already requires `nlp`, so its copy can be deleted. Also remove the stray `data/p.py`.
- [ ] **Use one file name for tests.** Currently `tests.rkt`, `test.rkt` (webscrape, llmapis) and `math-tests.rkt`.
- [ ] **Add offline tests where there are none**: `nlp` (tagger and name finder are deterministic), `webscrape` (parse a saved HTML string), `lightpanda` (link extraction from a fixture), `sparql` (`json->listvals` and `gd` on canned JSON), `Racklog_expert_systems_logic`, `OPS5_in_Racket` (run `draw.ops` and `monkey.ops` and compare the firing trace), `Karpathy_MicroGPT` (autograd gradient check), `deep_learning_category_theory`.

### `embeddingsdb/embeddingsdb.rkt`

- [ ] The SQLite database is opened when the module is required, as `test.db` in whatever the current directory happens to be. Make the path a parameter and open it lazily.
- [ ] Errors are swallowed in two places: `CREATE TABLE` failures and every embedding failure inside `create-document` are caught and ignored. A wrong API key produces an empty index and no message. Use `CREATE TABLE IF NOT EXISTS` and report embedding errors.
- [ ] Calling `create-document` twice on the same file inserts duplicate rows. Delete existing rows for that path first.
- [ ] Remove dead code: `read-file`, `join-strings`, `truncate-string`, `interleave`, `execute-to-list`, and `string-to-list` (a duplicate of `string->floats`). Remove the placeholder comments `;; ... database setup, error handling, and queries ...` from the real source file.
- [ ] `break-into-chunks` cuts every 200 characters with no overlap and no regard for word boundaries. `rag_extensions.rkt` already has better chunking; make it the default so the chapter's first example is not the weak one.
- [ ] The `0.7` dot-product cutoff in `semantic-match` was tuned for one embedding model. Switch the default to top-k retrieval, which the chapter's own "Retrieval, Without Thresholds" section argues for.

### `sparql/sparql.rkt` and `kgn/`

- [ ] `gd` handles one to four variables with four hand-written lambdas. `(apply map list jd)` handles any number. This is already practice problem 2 in the datastores chapter; fix the library and replace the exercise.
- [ ] Person names and URIs are interpolated straight into SPARQL strings. A name containing a double quote breaks the query. Add a small escaping helper and mention why.
- [ ] `sparql-query->hash` has no error handling for timeouts or non-JSON replies; DBpedia returns HTML error pages under load.
- [ ] `kgn.rkt` uses `htdp/gui` together with `racket/gui/base`. `htdp/gui` is a teaching library; use `racket/gui` alone.
- [ ] The KGN README and chapter both say "work in progress" and "only who/where queries are handled". Either finish the organization and relationship search, or reduce the chapter's claims to match (see section 5).

### `misc_code/` and `webscrape/`

- [ ] `webscrape.rkt` requires `srfi/13` "for strings", but `string-normalize-spaces`, `string-join` and `string-prefix?` come from `racket/string`. Remove the require and the matching claim in the chapter.
- [ ] `web-uri->links` drops relative links, and `web-uri->xexp` has no status check or timeout. Both are currently practice problems; at least the status check belongs in the library.
- [ ] `sqlite.rkt` is 14 lines. Extend it with parameterized queries, a transaction and `query-maybe-row` so the SQLite section has something to teach.

### `llmapis/`

- [ ] The directory contains both the uniform `llmapis.rkt` (1,081 lines) and five older per-provider files. Check whether `openai.rkt`, `mistral.rkt` and `ollama_ai_local.rkt` still add anything beyond the uniform API; if not, reduce them to thin wrappers or remove them, and keep only the ones with provider-specific features (`gemini.rkt` search grounding, `anthropic.rkt` web search).
- [ ] `test.rkt` appears to need live keys. Split it into an offline suite (request building, response parsing, error mapping against canned JSON) and an opt-in live suite.

### Larger examples

- [ ] **`OPS5_in_Racket/ops5.rkt`** is a single 3,610-line file with no tests. A regression test that runs both `.ops` programs and checks the output would protect a file this size.
- [ ] **`coding-agent-harness/agent.rkt`** is 994 lines. The chapter already describes it as separate concerns (intent classification, context management, REPL, CLI); splitting the file along those lines would make the listings shorter too. There are no tests; `tools.rkt` path handling and `approval.rkt` diff generation are easy to test offline.
- [ ] **`Malt_deep_learning/PROBLEMS.md`** and **`ReinforcementLearning/PROBLEMS.md`** contain resolved debugging notes. The Malt one is already told in the chapter's "Interlude". Trim them to open issues only.
- [ ] **`Malt_deep_learning/joint_recommendation.rkt`**: `PROBLEMS.md` notes that a dot-product scoring head would match the bilinear ground truth better than concat plus linear (MAE is stuck near 0.09). Either make that change or turn it into an exercise.

## 5. Chapter improvements

### Whole-book consistency

- [ ] **Part pages are a single heading each.** `PART1.md` to `PART6.md` have no text. Add two or three sentences per part saying what the chapters have in common and which can be skipped.
- [ ] **Chapter order in Part 4.** `kgn.md` says it uses "the NLP library from the last chapter", which is true, but Part 4 puts Malt, category theory and RL before NLP and KGN, which have nothing to do with deep learning. Consider moving NLP and KGN into Part 1 (next to the RDF chapters they build on) or into their own part.
- [ ] **Two RAG chapters use different stacks.** `embeddings.md` uses OpenAI and SQLite; `RAG.md` uses Gemini and its own vector store. Add a short paragraph at the start of `RAG.md` saying why it does not reuse `embeddingsdb`, or make it reuse it.
- [ ] **Exercises.** Thirteen chapters end with "Optional Practice Problems", two use "Exercises" (`ReinforcementLearning.md`, `Racklog_expert_systems_logic.md`), and three have none (`tutorial.md`, `Malt_deep_learning.md`, `deep_learning_category_theory.md`). Use one heading and add problems to the three.
- [ ] **Architecture diagrams.** The 12 older chapters each have an `*_architecture.jpg`; the 9 newer chapters have no images at all (`RAG`, `Malt`, coding agent, RL, pi and e, Racklog, category theory, OPS5, conclusion). The coding agent, RAG agent loop and Rete network would each benefit most from a diagram.
- [ ] **Code that overflows the page.** Lines over 75 characters inside code blocks: `coding-agent-harness.md` 234, `math-pi-e.md` 124, `Ollama_Tools.md` 50, `kgn.md` 39, `embeddings.md` 36, `deep_learning_category_theory.md` 36, `llm.md` 31. These wrap or get cut in PDF and EPUB.
- [ ] **Chapters that are mostly listing.** `coding-agent-harness.md` (15,400 words, about 2,090 code lines) and `math-pi-e.md` (11,100 words, about 1,400 code lines) reprint nearly the whole program; `karpathy.md` appends the complete 300-line source after already walking through it. Show the instructive excerpts and link to the repository for the rest. `ReinforcementLearning.md` is the opposite: about 2,800 words of prose and no Racket listings at all for 2,700 lines of code. A few short excerpts (the Q-learning update, the policy gradient step, the replay buffer) would help.
- [ ] **Old Leanpub attribute syntax.** `nlp.md` and `kgn.md` still use `{lang=racket, linenos=off}` with `~~~~~~~~` fences; every other chapter uses fenced blocks with a language. Convert these so they are highlighted the same way.
- [ ] **Stale version banners.** REPL transcripts show Racket 8.10, 8.12 and 8.14. Regenerate them on a current release when the listings are next touched.
- [ ] **Hard-wrapped versus unwrapped source.** Five chapters are hard-wrapped at about 72 columns, the rest are one line per paragraph. Not visible to readers, but it makes diffs noisy; pick one.
- [ ] **"Work in progress" statements.** `nlp.md` says so twice, `kgn.md` twice plus "will be updated … when I finish writing the KGN application". These have been there across several editions; finish the work or delete the sentences.
- [ ] **Add a setup chapter or appendix.** There is no single place that covers installing Racket, the package list, API keys, Ollama, and how to run the tests. Each chapter currently repeats part of it.

### Per chapter

**`preface.md`**
- [ ] Says "the chapters can be read in any order", but KGN depends on NLP and the datastores chapter, RAG depends on embeddings, and Racklog depends on OPS5. State the real dependencies.
- [ ] Typo: "It offers for interact ing with different LLM providers".
- [ ] "Racket code that I wrote for my own use and I am wrapping in a book" predates the newer tutorial-style chapters; update the description of who the book is for.

**`tutorial.md`**
- [ ] The NLP REPL example repeats the same `find-place-names` call twice with identical output. Replace one with `find-human-names`.
- [ ] The list explaining `(map + …)` contains two empty bullets (lone `-` lines).
- [ ] Typos: "Th* function", "a lising".
- [ ] The chapter stops at structs and HTTP. The rest of the book relies heavily on parameters (`make-parameter`, `parameterize`), keyword arguments, `module+ main` and `module+ test`, `with-handlers`, `match`, `for/list` and friends, `define-runtime-path`, and JSON as hash tables. A short section on each would pay off in every later chapter.
- [ ] The HTTP example duplicates the opening of `webscrape.md` almost line for line. Keep one.

**`datastores.md`**
- [ ] "Let's try an example in a Racket REPL" shows output with no input expression. Add the `(sparql-dbpedia (sparql-dbpedia-person-uri "Steve Jobs"))` call.
- [ ] The RDF and SPARQL background is delegated to another book. Half a page here (triples, URIs, a `SELECT` with one pattern) would let the chapter stand alone, and would set up the next chapter.
- [ ] The SQLite session creates a temporary table `the_numbers` that is never used.
- [ ] "I might add examples for WikiData and Jena in future versions": add one WikiData query or drop the sentence.
- [ ] Spelling of "SQlite" (should be SQLite) and "DBPedia" (should be DBpedia) varies; "accesing".

**`RDF_Datastore_SPARQL_Implementation.md`**
- [ ] Six listing lines no longer match the source (the `triple-matches?` style pattern test and a sample result). Re-sync.
- [ ] Say explicitly what subset of SPARQL is supported and what is not (OPTIONAL, UNION, prefixes), with one example of an unsupported query and the error it gives.

**`webscrape.md`**
- [ ] The `webscrape.rkt` listing is an older version of the file (about half its lines differ from the source, which now uses `define` and `for/list`). Re-paste it.
- [ ] A REPL block starts with a stray `#<procedure:string-normalize-spaces>` output line with no input.
- [ ] The prose says `se-path/list`; the code uses `se-path*/list`.
- [ ] The final `raco pkg install --scope user` line is indented three spaces, so it renders as body text, not code.
- [ ] Typos: "outoput", "make more sense to your", "as a lisp" (list).
- [ ] Mention `robots.txt` and rate limiting alongside the existing copyright note, and cross-reference the Lightpanda chapter for JavaScript-rendered pages.

**`lightpanda.md`**
- [ ] Eight listing lines differ from the source (binary path and example URLs). Re-sync.
- [ ] Add a short comparison: the same page fetched with `webscrape` and with Lightpanda, showing what the first one misses.

**`karpathy.md`**
- [ ] Remove the full source listing at the end, or remove the excerpts before it; currently the reader sees most of the code twice.
- [ ] Add expected training time and a sample of generated names so readers know what a successful run looks like.

**`llm.md`**
- [ ] The title in the chapter, in `TOC.md` and in the directory READMEs are three different strings.
- [ ] The history section has a broken sentence: "…modern hardware like GPUs. solution arrived in 2017".
- [ ] The opening is two paragraphs of general statements about LLMs before any Racket appears; the uniform API starts at line 47. Consider moving the history section to the end or cutting it down.
- [ ] The `#lang llm` section at the end uses `gpt4o-mini` and `phi3` modules and Racket 8.12 transcripts. Verify the package still installs and update, or cut the section.
- [ ] The chapter links `ollama.ai` and `github.com/ggerganov/llama.cpp`; both projects have moved (`ollama.com`, `ggml-org/llama.cpp`).

**`Ollama_Tools.md`**
- [ ] The opening says "The registry, handlers, and schemas below are unchanged; only the transport moved." That is a changelog note for returning readers; a new reader has no "before". Rephrase.
- [ ] The chapter says it uses the OpenAI-compatible route through `llmapis.rkt`, which means most of it is not specific to Ollama. Say so, and show the one-line change to run the same tools against a hosted model.

**`coding-agent-harness.md`**
- [ ] At 15,400 words this is 17% of the book. Split into two chapters (provider clients and tool registry; REPL, approval and CLI) or cut listings as suggested above.
- [ ] State the AGPL license in the chapter if it stays.
- [ ] Add a short section on what the agent cannot do safely (shell tool scope, path escapes), mirroring the "Safety and Sandboxing" section in the Ollama tools chapter.

**`embeddings.md`**
- [ ] The chapter title is "Retrieval Augmented Generation of Text Using Embeddings" and the next chapter is "Agentic RAG"; the first could be titled around embeddings and a vector store to make the distinction clearer.
- [ ] The first listing presents the weakest design (fixed 200-character chunks, threshold retrieval, swallowed errors) and later sections explain why each is a poor choice. Reorder, or fix the base code as listed in section 4, so readers do not copy the first version.

**`RAG.md`**
- [ ] Three listing lines differ from the source (two `write-proc` struct lines and a `sort` call). Re-sync.
- [ ] Add a cost note: number of LLM calls per question for the default iteration bound.

**`Malt_deep_learning.md`, `deep_learning_category_theory.md`**
- [ ] Neither has exercises. For Malt, the dot-product head from `PROBLEMS.md` is a ready-made one.
- [ ] The category theory chapter introduces five frameworks (Para and lenses, Markov categories, colimits, toposes, natural transformations) in about 2,900 words. Add a one-paragraph "what you need to know first" and a pointer to an introductory reference.
- [ ] `Malt_deep_learning.md` ends with "Files in this directory" and no wrap-up.

**`ReinforcementLearning.md`**
- [ ] Add code excerpts (see above).
- [ ] Add a results table or learning-curve figure; the chapter describes outcomes in prose only.

**`nlp.md`**
- [ ] The chapter is two long listings with about 770 words around them. It never explains the tag set (what `NNP`, `VBD`, `DT` mean), where `tag.dat` came from, or how accurate the tagger is.
- [ ] The `fasttag.rkt` listing carries a "Copyright 2002" header from the original Java and Common Lisp code; explain or update.
- [ ] "Overall, the code is fairly optimized for its purpose" is not supported by anything in the chapter. Cut it or add a timing.
- [ ] Add a comparison with an LLM doing the same extraction, with a note on when the rule-based version is still the better choice (speed, cost, offline). This would tie the chapter to Part 3.

**`kgn.md`**
- [ ] The opening paragraph ends with the fragment "KGN uses the SPARQL queries." Finish or delete the sentence.
- [ ] "is previous books" should be "in previous books"; "look at either for more details" has no clear referent.
- [ ] "Entity Types Handled" says people and places only; the next section says the application processes "Person, Place, and Organization names" and finds "relationships between them". The code does people and places. Make the text consistent.
- [ ] The prose says `find-human-names` and `find-place-names` are "defined in the earlier section on SPARQL"; they are from the NLP chapter.
- [ ] The SPARQL client listing is repeated from the datastores chapter ("We already saw most of the following code listing"). Reference it instead of reprinting it.
- [ ] Four listings near the end (`dialog-utils.rkt`, `sparql-utils.rkt`, `kgn.rkt`) are introduced with one sentence each and no explanation.
- [ ] Ten listing lines differ from the source. Re-sync.

**`OPS5_in_Racket.md`, `Racklog_expert_systems_logic.md`**
- [ ] The OPS5 chapter opens by saying the author no longer recommends rule-based systems for practical work, twice in two paragraphs. Keep the candor, but say once what the reader gains by reading on (Rete as an algorithm, a worked large Lisp port).
- [ ] Add a side-by-side table at the end of the Racklog chapter comparing the two solutions (lines of code, forward versus backward chaining, how each handles the monkey's state).

**`math-pi-e.md`**
- [ ] Cut the listings to the algorithms that teach something distinct; the registry, reporting and command-line sections (Parts 5 to 7, about 700 lines of manuscript) are scaffolding.
- [ ] The chapter sits in a book on AI with no stated connection to it. One paragraph on why it is here (exact rational arithmetic and bignums as a Racket strength) would help.

**`conclusion.md`**
- [ ] After removing the Mermaid block, the chapter is about 200 words and unchanged from the early editions. Add a paragraph that reflects the current book (LLMs, agents, symbolic AI) and suggests what to read or build next.

## 6. Possible new material

Lower priority; listed because the existing code is close to supporting each one.

- [ ] Publish the genetic programming chapter (code and draft already exist).
- [ ] A "chat with a PDF" section built on `pdf_chat` and the embeddings chapter.
- [ ] A local-only path through Part 3: one page showing how to run every LLM chapter against Ollama with no API keys, including a local embedding model for `embeddingsdb` and `RAG`.
- [ ] Structured output: JSON-schema-constrained responses through `llmapis.rkt`, used to redo the NLP chapter's entity extraction.
- [ ] An LLM-generated SPARQL example for KGN (natural language to query), contrasted with the template approach the chapter uses now.
- [ ] An evaluation section for the RAG chapters: a small question set with expected answers and a script that scores retrieval hit rate.
