module.exports = {
  globals: {
    Cesium: true,
    map3d: true,
    describe: true,
    it: true,
  },
  parser: 'babel-eslint',
  env: {
    browser: true,

    // es6: true,
    node: true,
  },
  extends: 'airbnb-base',
  parserOptions: {
    sourceType: 'module',
    ecmaVersion: 8,

    // 'ecmaVersion': 2017,
  },
  plugins: ['babel'],
  rules: {
    'babel/generator-star-spacing': 1,
    'linebreak-style': [0],
    'babel/arrow-parens': 1,
    indent: [
      'off',
      2,
    ],
    'arrow-parens': [
      'off',
    ],
    'func-names': [
      'off',
    ],
    'no-plusplus': [
      'off',
    ],
    'valid-jsdoc': [
      'error',
      {
        requireReturn: false,
      },

      // {
      //   preferType: {
      //     Boolean: 'boolean',
      //     Number: 'number',
      //     object: 'Object',
      //     String: 'string',
      //   },
      // },
    ],
    'babel/arrow-parens': [
      'off',
    ],
    'new-cap': [
      'off',
    ],
    'import/no-unresolved': [
      'off',
    ],
    'no-useless-escape': [
      'off',
    ],
    'import/no-extraneous-dependencies': [
      'off',
    ],
    'global-require': [
      'off',
    ],
    'no-lonely-if': [
      'off',
    ],
    'no-iterator': [
      'error',
    ],
    'no-alert': [
      'off',
    ],
    'no-multi-str': [
      'off',
    ],
    'comma-dangle': [
      'off',
    ],
    'no-restricted-syntax': [
      'off',
      'ForInStatement',
    ],
    'no-console': [
      'off',
    ],
    curly: [
      'off',
    ],
    'no-unused-expressions': [
      'off',
    ],
    'no-unused-vars': [
      'off',
    ],
    'spaced-comment': [
      'warn',
    ],
    'no-use-before-define': [
      'off',
    ],
    'no-param-reassign': [
      'off',
    ],
    'no-mixed-operators': [
      'off',
    ],
    'no-shadow': [
      'off',
    ],
    'no-restricted-syntax': [
      'warn',
    ],
    'no-else-return': [
      'warn',
    ],
    'no-underscore-dangle': [
      'off',
    ],
    quotes: [
      'off',
      'single',
    ],
    semi: [
      'error',
      'always',
    ],
    'no-prototype-builtins': [
      'off',
    ],
    'consistent-return': [
      'off',
    ],
    'max-len': ['error', 300],
  },
};
