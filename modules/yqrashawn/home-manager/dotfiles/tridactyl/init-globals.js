tri.quitVoila = () =>
  document.querySelector('a[title="Esc"].Voila__IconButton')?.click()
tri.quitThings = () => {
  tri.quitVoila()
}
tri.userIdle = (id, ms, fn) => {
  const realId = `${id}_idle_timer_defined`
  if (tri[realId]) return () => {}
  tri[realId] = true
  let timeoutId

  // Reset the timeout whenever there is user activity
  function resetTimer() {
    clearTimeout(timeoutId)
    timeoutId = setTimeout(fn, ms)
  }

  // Call the resetTimer function whenever there is user activity
  document.addEventListener("mousemove", resetTimer)
  document.addEventListener("keydown", resetTimer)
  document.addEventListener("scroll", resetTimer)

  return () => clearTimeout(timeoutId)
}
