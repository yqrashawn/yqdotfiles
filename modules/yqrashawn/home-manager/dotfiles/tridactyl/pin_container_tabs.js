// Keep one pinned tab per container for each site; never open duplicates.
// Run with jsb (background context).
;(async () => {
  const SITES = [
    {
      url: "https://claude.ai/new#settings/usage",
      // match patterns ignore the #fragment; full url only matters for unpinned tabs
      match: "*://claude.ai/*",
      containers: ["p", "Personal", "Work"],
    },
    {
      url: "https://pc.0g.ai/dashboard/overview",
      match: "*://pc.0g.ai/*",
      containers: ["Personal", "Work"],
    },
  ]

  // On TriStart (flag set by the autocmd), wait for session restore to settle
  // so restored pinned tabs are found instead of duplicated (tab count stable
  // 3s, max 30s)
  const startup = window.__pinContainerTabsStartup
  window.__pinContainerTabsStartup = false
  if (startup) {
    let last = -1
    for (let i = 0, stable = 0; i < 30 && stable < 3; i++) {
      const n = (await browser.tabs.query({})).length
      stable = n === last ? stable + 1 : 0
      last = n
      await new Promise((r) => setTimeout(r, 1000))
    }
  }

  const identities = await browser.contextualIdentities.query({})
  for (const site of SITES) {
    for (const name of site.containers) {
      let ci = identities.find((c) => c.name === name)
      if (!ci) {
        ci = await browser.contextualIdentities.create({
          name,
          color: "blue",
          icon: "fingerprint",
        })
        // later sites may want the same name; without this they would
        // look up the stale list and mint a duplicate container
        identities.push(ci)
      }
      const tabs = await browser.tabs.query({
        cookieStoreId: ci.cookieStoreId,
        url: site.match,
      })
      // any pinned tab on the site counts (user may have navigated inside it);
      // only adopt an unpinned tab that is on the target page
      const tab =
        tabs.find((t) => t.pinned) ||
        tabs.find((t) => t.url.startsWith(site.url))
      if (tab) {
        if (!tab.pinned) await browser.tabs.update(tab.id, { pinned: true })
      } else {
        await browser.tabs.create({
          url: site.url,
          cookieStoreId: ci.cookieStoreId,
          pinned: true,
          active: false,
        })
      }
    }
  }
})()
