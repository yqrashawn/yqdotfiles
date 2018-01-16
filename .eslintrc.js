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
    Hammer: true
  },
  parser: "babel-eslint",
  parserOptions: {
    ecmaFeatures: {
      jsx: true
    }
  },
  env: {
    browser: true,
    es6: true,
    node: true
  },
  // extends: "google",
  extends: [
    "standard",
    "prettier",
    "plugin:flowtype/recommended",
    "eslint:recommended",
    "plugin:react/recommended",
    "prettier/flowtype",
    "prettier/react",
    "prettier/standard"
  ],
  parserOptions: {
    sourceType: "module",
    ecmaVersion: 8
  },
  plugins: ["prettier", "flowtype", "react"],
  settings: {
    flowtype: {
      onlyFilesWithFlowAnnotation: true
    }
  },
  rules: {
    "prettier/prettier": "error"
  }
};
