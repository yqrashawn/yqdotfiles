// Keep one pinned tab per container for URL; never open duplicates.
// Run with jsb (background context).
;(async () => {
  const URL = "https://claude.ai/new#settings/usage"
  // match patterns ignore the #fragment, so filter on the full url below
  const MATCH = "*://claude.ai/*"
  const CONTAINERS = ["p", "Personal", "Work"]

  // Right after browser start, wait for session restore to settle so restored
  // pinned tabs are found instead of duplicated (tab count stable 3s, max 30s)
  if (performance.now() < 60000) {
    let last = -1
    for (let i = 0, stable = 0; i < 30 && stable < 3; i++) {
      const n = (await browser.tabs.query({})).length
      stable = n === last ? stable + 1 : 0
      last = n
      await new Promise((r) => setTimeout(r, 1000))
    }
  }

  const identities = await browser.contextualIdentities.query({})
  for (const name of CONTAINERS) {
    const ci =
      identities.find((c) => c.name === name) ||
      (await browser.contextualIdentities.create({
        name,
        color: "blue",
        icon: "fingerprint",
      }))
    const tabs = await browser.tabs.query({
      cookieStoreId: ci.cookieStoreId,
      url: MATCH,
    })
    // any pinned claude.ai tab counts (user may have navigated inside it);
    // only adopt an unpinned tab that is on the usage page
    const tab =
      tabs.find((t) => t.pinned) || tabs.find((t) => t.url.startsWith(URL))
    if (tab) {
      if (!tab.pinned) await browser.tabs.update(tab.id, { pinned: true })
    } else {
      await browser.tabs.create({
        url: URL,
        cookieStoreId: ci.cookieStoreId,
        pinned: true,
        active: false,
      })
    }
  }
})()
