# Teller

# Usage

```sh
build/teller -i $PATH_TO_PDF_STATEMENT -o $PATH_TO_TSV_OUTPUT
```

# Gotchas
This project uses jdn-loader, which allows you to load jdns as if they were .janet files. This is convenient, but it also means janet's build caching can trip you up. `jpm build` can actually be a no-op, because no janet file changed. If this happens, `jpm clean` before `jpm build`.

# Compile

`jpm clean`
`jpm deps`
`jpm build`
(optionally...)
`jpm install`
(or...)
`cp ./build/teller $DIR_IN_PATH`

# Debugging
pdftotext -nopgbrk -layout -upw $PASSWORD $PATH_TO_PDF -   

# Roadmap
- [ ] Breaking change: Dates should normalize to ISO.