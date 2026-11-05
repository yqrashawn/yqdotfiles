// Keep one pinned tab per container for URL; never open duplicates.
// Run with jsb (background context).
;(async () => {
  const URL = "https://mail.google.com/"
  const MATCH = "*://mail.google.com/*"
  const CONTAINERS = ["p", "Personal", "Work"]

  const identities = await browser.contextualIdentities.query({})
  for (const name of CONTAINERS) {
    const ci =
      identities.find((c) => c.name === name) ||
      (await browser.contextualIdentities.create({
        name,
        color: "blue",
        icon: "fingerprint",
      }))
    const [tab] = await browser.tabs.query({
      cookieStoreId: ci.cookieStoreId,
      url: MATCH,
    })
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
