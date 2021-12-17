# advent2021

Running a specific puzzle:

```sh
spago run --node-args '--day $DAY --part $PART --input $PATH_TO_INPUT_FILE'
```

To run a puzzle with a larger call stack (to bypass stack safety issues):

```sh
spago bundle-app
node --stack-size=$STACK_SIZE_IN_KB \
  --stack-trace-limit=$STACK_TRACE_LIMIT \
  index.js \
  --day $DAY \
  --part $PART \
  --input $PATH_TO_INPUT_FILE
```
