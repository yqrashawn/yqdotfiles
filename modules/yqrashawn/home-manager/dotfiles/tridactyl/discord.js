tri.userIdle("discord click first server", 1 * 1000 * 60, () =>
  document
    .querySelector('div[aria-label="Servers"]')
    .querySelectorAll('div[role="treeitem"]')?.[0]
    ?.click(),
)
