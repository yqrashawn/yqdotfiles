// https://github.com/gajus/eslint-plugin-flowtype
// https://github.com/prettier/eslint-config-prettier
// https://github.com/yannickcr/eslint-plugin-react
// https://github.com/yannickcr/eslint-plugin-react
module.exports = {
  globals: {
    Bimsop: true,
    Zlib: true,
    base64: true,
    THREE: true,
    describe: true,
    it: true,
    Hammer: true,
  },
  parser: "babel-eslint",
  parserOptions: {
    ecmaFeatures: {
      jsx: true,
    },
  },
  env: {
    browser: true,
    es6: true,
    node: true,
  },
  // extends: "google",
  extends: ["wesbos"],
  parserOptions: {
    sourceType: "module",
    ecmaVersion: 8,
  },
  settings: {},
  rules: {},
};
