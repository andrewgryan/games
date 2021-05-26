const colors = require("tailwindcss/colors")

module.exports = {
  purge: ["./index.html", "src/**/*.elm"],
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
        colors: {
            teal: colors.teal
        }
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
