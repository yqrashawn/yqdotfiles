#!/usr/bin/env babel

/* -*- mode: js2 -*- */
/**
 * @fileOverview phoenix configuration file
 * @name .phoenix.js
 * @author Rashawn Zhang <Rashawn@namy.19@gmail.com>
 */
g = this;

const screens = Screen.all();
const screenLeft = screens[0];
const screenRight = screens[1];

var WINDOW_LIST = Window.all();

const IGNORED_APP = [
  "Contexts",
  "Qbserve",
  "Unclutter",
  "Emacs",
  "Google Chrome",
  "iTerm2",
  "Finder",
  "Phoenix",
  "BearyChat",
  "Karabiner-Elements",
  "Karabiner-EventViewer",
  "Alfred 3",
  "CleanMyMac 3 Menu",
  "Bartender 2",
  "WeChat",
  "Dash",
  "Code",
  "Mail",
];

const filterList = function(w) {
  WINDOW_LIST = WINDOW_LIST.filter(function(window) {
    if (window && window.app()) {
      if (!window.title().length) return false;
      if (IGNORED_APP.indexOf(window.app().name()) !== -1) return false;
      if (w && window.hash() !== w.hash()) return true;
      if (!w) return true;
    }
  });
};

const refresh = function() {
  WINDOW_LIST = Window.all();
  filterList();
};

const windowOpen = new Event("windowDidOpen", function(w) {
  refresh();
});

const windowFocus = new Event("windowDidFocus", function(w) {
  refresh();
});

const windowDidClose = new Event("windowDidClose", function(w) {
  refresh();
});

const appDidTerminate = new Event("appDidTerminate", function(app) {
  refresh();
});

const tmodal = new Modal();
g.tmodal = tmodal;
tmodal.duration = 0;
tmodal.origin = { x: 0, y: 600 };

var switchToSecondPrevWindow = new Key(
  "f16",
  ["alt", "ctrl", "shift"],
  function() {
    if (WINDOW_LIST[2]) return WINDOW_LIST[2].focus();
    if (WINDOW_LIST[1]) return WINDOW_LIST[1].focus();
  }
);

const trucate = function(str) {
  if (str.length > 15) {
    str = str.substr(0, 15);
    const spaceIdx = str.lastIndexOf(" ");
    if (spaceIdx !== -1) {
      str = str.substr(0, spaceIdx + 1);
    }
  }

  return str;
};

const numToHomeRow = ["space", "a", "s", "d", "f", "h", "j", "k", "l"];

var keys = [];

var switchTo = new Key("f16", ["alt", "ctrl"], function() {
  filterList();
  tmodal.text = WINDOW_LIST.map(function(window, index) {
    return `[${
      numToHomeRow[index]
    }] - ${window.app().name()} - ${trucate(window.title())}`;
  })
    .toString()
    .replace(/,/g, "\n");
  if (WINDOW_LIST.length === 0) {
    tmodal.text = "no uncontrolled window";
    tmodal.duration = 1;
  } else {
    tmodal.duration = 0;
  }
  keys.forEach(function(keyHandler) {
    keyHandler.disable();
  });
  WINDOW_LIST.forEach(function(window, index) {
    keys.push(
      new Key(numToHomeRow[index], [], function(key) {
        WINDOW_LIST[index].raise();
        WINDOW_LIST[index].focus();
        tmodal.close();
        keys.forEach(function(keyHandler) {
          keyHandler.disable();
        });
      })
    );
  });

  keys.push(
    new Key("escape", [], function() {
      tmodal.close();
      keys.forEach(function(keyHandler) {
        keyHandler.disable();
      });
    })
  );

  keys.push(
    new Key("g", ["ctrl"], function() {
      tmodal.close();
      keys.forEach(function(keyHandler) {
        keyHandler.disable();
      });
    })
  );
  tmodal.show();
});
