import { defineConfig } from 'vitepress'
import markdownItMathjax3 from 'markdown-it-mathjax3'
import markdownItFootnote from 'markdown-it-footnote'
import { withMermaid } from 'vitepress-plugin-mermaid'

export default withMermaid(
  // https://vitepress.dev/reference/site-config
  defineConfig({
    vue: {
      template: {
        compilerOptions: {
          isCustomElement: (tag) => tag.startsWith('mjx-')
        }
      }
    },
    title: "Functional Programming",
    description: "Course materials for the functional programming course at the Czech Technical University.",
    base: "/FUP/",
    head: [
      ['link', { rel: 'icon', type: 'image/svg+xml', href: '/FUP/lambda_auto.svg' }],
      ['link', {rel:"preconnect", href:"https://fonts.googleapis.com"}],
      ['link', {rel:"preconnect", href:"https://fonts.gstatic.com", crossorigin:''}],
      ['link', {href:"https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap", rel:"stylesheet"}]
    ],
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
            { text: '04: Pattern Matching & Lazy Evaluation', link: '/lectures/lecture04'},
            { text: '05: Macros & Interpreters', link: '/lectures/lecture05'},
            { text: '06: Lambda Calculus', link: '/lectures/lecture06'},
            { text: '07: Haskell Basics', link: '/lectures/lecture07'},
            { text: '08: Haskell Types', link: '/lectures/lecture08'},
            { text: '09: Type Classes', link: '/lectures/lecture09'},
            { text: '10: IO & Monads', link: '/lectures/lecture10'},
            { text: '11: Monadic Parsing', link: '/lectures/lecture11'},
            { text: '12: State Monad', link: '/lectures/lecture12'},
            { text: '13: Monoids & Foldables', link: '/lectures/lecture13'},
          ]
        },

        {
          text: 'Labs',
          link: '/labs/',
          items: [
            { text: '01: Introduction to Racket', link: '/labs/lab01' },
            { text: '02: Lists & Trees', link: '/labs/lab02' },
            { text: '03: Higher Order Functions I', link: '/labs/lab03' },
            { text: '04: Higher Order Functions II', link: '/labs/lab04' },
            { text: '05: Streams & Graphs', link: '/labs/lab05' },
            { text: '06: Brainf*ck', link: '/labs/lab06' },
            { text: '07: Lambda Calculus', link: '/labs/lab07' },
            { text: '08: Haskell Basics', link: '/labs/lab08' },
            { text: '09: Haskell Types', link: '/labs/lab09' },
            { text: '10: Polymorphic functions', link: '/labs/lab10' },
            { text: '11: Functors and IO', link: '/labs/lab11' },
            { text: '12: Monads in action', link: '/labs/lab12' },
            { text: '13: State Monad', link: '/labs/lab13' },
          ]
        },

        {
          text: 'Homework',
          link: '/homework/',
          items: [
            { text: '01: ASCII Art', link: '/homework/hw01' },
            { text: '02: SVGen Interpreter', link: '/homework/hw02' },
            { text: '03: λ-Calculus Evaluator', link: '/homework/hw03' },
            { text: '04: Parser of λ-programs', link: '/homework/hw04' },
          ]
        },

        {
          text: 'Exams',
          link: '/exams/',
          items: [
            { text: '01: Minesweeper', link: '/exams/minesweeper/' },
            { text: '02: Cheap Flights', link: '/exams/cheap-flights/' },
            { text: '03: Manhattan Distance', link: '/exams/manhattan-distance/' },
            { text: '04: N²-Knights', link: '/exams/n2-knights/' },
            { text: '05: Filetree', link: '/exams/filetree/' },
            { text: '06: Text Justification', link: '/exams/justify/' },
            { text: '07: Photographing Skyscrapers', link: '/exams/photo-skyscraper/' },
            { text: '08: Non-deterministic Finite Automata', link: '/exams/finite-automata/' },
            { text: '09: Least Common Ancestor', link: '/exams/least-common-ancestor/' },
            { text: '10: Building Trees', link: '/exams/building-trees/' },
            { text: '11: Square Code', link: '/exams/square-code/' },
            { text: '12: Rock, Paper, Scissors', link: '/exams/rock-paper-scissors/' },
            { text: '13: Sierpinski Carpet', link: '/exams/sierpinski-carpet/' },
            { text: '14: Spiral Matrix', link: '/exams/spiral-matrix/' },
            { text: '15: Unit Propagation', link: '/exams/unit-propagation/' },
            { text: '16: Balanced Binary Tree', link: '/exams/balanced-tree/' },
            { text: '17: Minimum Spanning Tree', link: '/exams/minimum-spanning-tree/' },
            { text: '18: Pretty Printing Binary Numbers', link: '/exams/pretty-binary-numbers/' },
            { text: '19: Fermat Primality Test', link: '/exams/fermat-primality/' },
            { text: '20: Convex Hull', link: '/exams/convex-hull/' },
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
    },
  }),
);

// make sure that .rkt/.hs files are linked correctly (without additional .html at the end)
process.env.VITE_EXTRA_EXTENSIONS = 'rkt,hs'  // comma separated list: 'foo,bar,baz'

