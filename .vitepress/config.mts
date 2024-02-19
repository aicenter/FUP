import { defineConfig } from 'vitepress'
import markdownItMathjax3 from 'markdown-it-mathjax3'
import markdownItFootnote from 'markdown-it-footnote'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  title: "Functional Programming",
  description: "Course materials for the functional programming course at the Czech Technical University.",

  base: "/FUP/",

  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      { text: 'Organization', link: '/organization' },
      { text: 'Lectures', link: '/lectures/' },
      { text: 'Labs', link: '/labs/' },
      { text: 'Homeworks', link: '/homeworks/' },
      { text: 'Exams', link: '/exams/' },
    ],

    sidebar: [

      { text: 'Organization', link: '/organization' },

      {
        text: 'Lectures',
        link: '/lectures/',
        items: [
          { text: '01: Introduction', link: '/lectures/lecture01'}
        ]
      },

      {
        text: 'Labs',
        link: '/labs/',
        items: [
          { text: '01: Introduction to Racket', link: '/labs/lab01' },
        ]
      },

      {
        text: 'Homeworks',
        link: '/homeworks/',
        items: [
          { text: '01: ASCII Art', link: '/homeworks/hw01' },
        ]
      },
    ],

    socialLinks: [
      { icon: 'github', link: 'https://github.com/aicenter/FUP' }
    ],

    editLink: {
      pattern: 'https://github.com/aicenter/FUP/edit/main/:path'
    },

    search: {
      provider: 'local'
    },
  },

  markdown: {
    config: (md) => {
      md.use(markdownItMathjax3);
      md.use(markdownItFootnote)
    }
  }
})
