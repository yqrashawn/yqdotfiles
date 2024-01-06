const documentClone = document.cloneNode(true)
const readerable = tri._readability.isProbablyReaderable(documentClone)
if (readerable) {
  const article = new tri._readability.Readability(documentClone, {
    debug: false,
  })
  const parsed = article.parse()
  parsed.url = location.href
  console.log(parsed)
}
