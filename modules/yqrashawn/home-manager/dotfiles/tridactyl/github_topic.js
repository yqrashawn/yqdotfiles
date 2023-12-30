let loadMoreButtons

function updateLoadMoreButtons() {
  loadMoreButtons = Array.from(document.querySelectorAll("button")).filter(
    (button) => button.innerText === "Load more…",
  )
}

;(async () => {
  updateLoadMoreButtons()
  try {
    while (loadMoreButtons?.length > 0) {
      loadMoreButtons.forEach((b) => (console.log(b), b.click()))
      await tri.sleep(2000)
      updateLoadMoreButtons()
    }
  } catch (err) {}
})()
