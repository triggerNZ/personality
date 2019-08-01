# personality

This is a little program to parse the results of the Big 5 test output HTML and post them to a JSON service.

## Strategy

The instructions say to 'save your results as text', but as far as I could tell, there was no such option so I pulled the
data out of the HTML, to have the following high-level structure:

HTML => Relevant Text => PersonalityTestResult => JSON

Each of these steps has its own error types, which are combined into top level error messages. I have tried to make the 
types tight in order to reduce bugs.

The libraries used were:
- tagsoup (for parsing and text HTML)
- aeson (for JSON generation)
- wreq (for HTTP requests)

## Building and running:

```
stack build && stack exec personality <email> results.html 
```

Where results.html is your results file. Mine is included in the repo.

## Testing:

```
stack test
```