try {
  tri.excmds.jsb(
    `browser.history.deleteUrl({url: "http://macmini.donkey-clownfish.ts.net:6155/login.html?tridactylrctab=1"})`,
  )
  tri.excmds.jsb(
    `browser.history.deleteUrl({url: "http://macmini.donkey-clownfish.ts.net:6155/login.html?tridactylrctab=2"})`,
  )
  tri.excmds.jsb(
    `browser.history.deleteUrl({url: "http://macmini.donkey-clownfish.ts.net:6155/login.html?tridactylrctab=3"})`,
  )
} catch (err) {}
