Brainf*ck interpreter.

Uses monad transformers to run the IO commands `,` and `.`. I haven't tested
this much, and it's pretty fragile at the moment. The hello-world program from
Wikipedia (as a single line of code) runs perfectly, though.

Uses a jump table (from [this](http://stackoverflow.com/a/3041005) page) to deal
with the loop commands `[` and `]`.
