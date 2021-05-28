# referring to a figure by caption, with a link

You add an id to an element by putting {#some-id} before the element on a line by itself. So, if I had a figure in an image called figure32.jpg, it would look like this:

========
# Chapter 1

See [figure 32](#figure-32) for an example.

# Chapter 2

{#figure-32}
![Figure 32](images/figure32.jpg)

=======

What we don't do (yet) is provide auto-numbering of figures and captions.

# Figure with centered caption

![This is the Image Caption](images/LeanpubLogo1200x610_300ppi.png)

# Refering to sections and chapter titles

# Chapter 1
Blah, blah, blah.
And if you think that was good, wait until you see the figure in
[Chapter 2](#chapter-2)!
# Chapter 2 {#chapter-2}
Lorem ipsum dolor. 

