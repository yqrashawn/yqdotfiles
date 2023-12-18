try {
  const targetTab = parseInt(
    new URL(location.href).searchParams.get("tridactylrctab"),
    10,
  )
  if (Number.isInteger(targetTab)) {
    tri.excmds.tabclose()
    tri.excmds.tab(targetTab)
  }
} catch (_err) {}
