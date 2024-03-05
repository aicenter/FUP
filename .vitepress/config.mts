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
      { text: 'Homework', link: '/homework/' },
      { text: 'Exams', link: '/exams/' },
    ],

    sidebar: [

      { text: 'Organization', link: '/organization' },

      {
        text: 'Lectures',
        link: '/lectures/',
        items: [
          { text: '01: Introduction', link: '/lectures/lecture01'},
          { text: '02: Lists & Trees', link: '/lectures/lecture02'},
          { text: '03: Higher Order Functions', link: '/lectures/lecture03'},
          { text: '07: Haskell', link: '/lectures/lecture07'},
        ]
      },

      {
        text: 'Labs',
        link: '/labs/',
        items: [
          { text: '01: Introduction to Racket', link: '/labs/lab01' },
          { text: '02: Lists & Trees', link: '/labs/lab02' },
          { text: '03: Higher Order Functions', link: '/labs/lab03' },
        ]
      },

      {
        text: 'Homework',
        link: '/homework/',
        items: [
          { text: '01: ASCII Art', link: '/homework/hw01' },
        ]
      },

      {
        text: 'Exams',
        link: '/exams/',
        collapsed: true,
        items: [
          { text: '1.  Minesweeper', link: '/exams/minesweeper/' },
          { text: '2.  Cheap Flights', link: '/exams/cheap-flights/' },
          { text: '3.  Manhattan Distance', link: '/exams/manhattan-distance/' },
          { text: '4.  N²-Knights', link: '/exams/n2-knights/' },
          { text: '5.  Filetree', link: '/exams/filetree/' },
          { text: '6.  Text Justification', link: '/exams/justify/' },
          { text: '7.  Photographing Skyscrapers', link: '/exams/photo-skyscraper/' },
          { text: '8.  Non-deterministic Finite Automata', link: '/exams/finite-automata/' },
          { text: '9.  Least Common Anchestor', link: '/exams/least-common-ancestor/' },
          { text: '10. Building Trees', link: '/exams/building-trees/' },
          // { text: '11. Square Code', link: '/exams/filetree' },
          // { text: '12. Rock, Paper, Scissors', link: '/exams/filetree' },
          // { text: '13. Sierpinski Carpet', link: '/exams/filetree' },
          // { text: '14. Spiral Matrix', link: '/exams/filetree' },
          // { text: '15. Unit Propagation', link: '/exams/filetree' },
          // { text: '16. Balanced Binary Tree', link: '/exams/filetree' },
          // { text: '17. Minimum Spanning Tree', link: '/exams/filetree' },
          // { text: '18. Pretty Printing Binary Numbers', link: '/exams/filetree' },
          // { text: '19. Fermat Primality Test', link: '/exams/filetree' },
          // { text: '20. Convex Hull', link: '/exams/filetree' },
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
